;;; ellama.el --- Tool for interacting with LLMs -*- lexical-binding: t -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.

;; Author: Sergey Kostyaev <sskostyaev@gmail.com>
;; URL: http://github.com/s-kostyaev/ellama
;; Keywords: help local tools
;; Package-Requires: ((emacs "28.1") (llm "0.6.0") (spinner "1.7.4"))
;; Version: 0.4.5
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Created: 8th Oct 2023

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Ellama is a tool for interacting with large language models from Emacs.
;; It allows you to ask questions and receive responses from the
;; LLMs.  Ellama can perform various tasks such as translation, code
;; review, summarization, enhancing grammar/spelling or wording and
;; more through the Emacs interface.  Ellama natively supports streaming
;; output, making it effortless to use with your preferred text editor.
;;

;;; Code:

(require 'json)
(require 'llm)
(require 'spinner)
(eval-when-compile (require 'rx))

(defcustom ellama-buffer "*ellama*"
  "Default ellama buffer."
  :group 'tools
  :type 'string)

(defcustom ellama-user-nick "User"
  "User nick in logs."
  :group 'tools
  :type 'string)

(defcustom ellama-assistant-nick "Ellama"
  "Assistant nick in logs."
  :group 'tools
  :type 'string)

(defcustom ellama-buffer-mode (if (fboundp 'markdown-mode)
				  'markdown-mode
				'text-mode)
  "Major mode for ellama logs."
  :group 'tools
  :type 'function)

(defcustom ellama-language "English"
  "Language for ellama translation."
  :group 'tools
  :type 'string)

(defcustom ellama-provider
  (progn
    (declare-function make-llm-ollama "llm-ollama")
    (require 'llm-ollama)
    (make-llm-ollama
     :chat-model "zephyr" :embedding-model "zephyr"))
  "Backend LLM provider."
  :group 'tools
  :type '(sexp :validate 'cl-struct-p))

(defcustom ellama-providers nil
  "LLM provider list for fast switching."
  :group 'tools
  :type '(alist :key-type string))

(defcustom ellama-spinner-type 'progress-bar
  "Spinner type for ellama."
  :group 'tools
  :type `(choice ,@(mapcar
		    (lambda (type)
		      `(const ,(car type)))
		    spinner-types)))

(defcustom ellama-keymap-prefix "C-c e"
  "Key sequence for Ellama Commands."
  :type 'string
  :group 'tools)

(defcustom ellama-ollama-binary (executable-find "ollama")
  "Path to ollama binary."
  :type 'string
  :group 'tools)

(defcustom ellama-auto-scroll nil
  "If enabled ellama buffer will scroll automatically during generation."
  :type 'boolean
  :group 'tools)

(defvar-local ellama--chat-prompt nil)

(defvar-local ellama--change-group nil)

(defconst ellama--code-prefix
  (rx (minimal-match
       (zero-or-more anything) (literal "```") (zero-or-more anything) line-end)))

(defconst ellama--code-suffix
  (rx (minimal-match
       (literal "```") (zero-or-more anything))))

(defun ellama--code-filter (text)
  "Filter code prefix/suffix from TEXT."
  (string-trim text ellama--code-prefix ellama--code-suffix))

(defun ellama-setup-keymap ()
  "Set up the Ellama keymap and bindings."
  (interactive)
  (defvar ellama-keymap (make-sparse-keymap)
    "Keymap for Ellama Commands")

  (define-key global-map (kbd ellama-keymap-prefix) ellama-keymap)

  (let ((key-commands
	 '(;; code
	   ("c c" ellama-code-complete "Code complete")
	   ("c a" ellama-code-add "Code add")
	   ("c e" ellama-code-edit "Code edit")
	   ("c i" ellama-code-improve "Code improve")
	   ("c r" ellama-code-review "Code review")
	   ;; summarize
	   ("s s" ellama-summarize "Summarize")
	   ("s w" ellama-summarize-webpage "Summarize webpage")
	   ;; improve
	   ("i w" ellama-improve-wording "Improve wording")
	   ("i g" ellama-improve-grammar "Improve grammar and spelling")
	   ("i c" ellama-improve-conciseness "Improve conciseness")
	   ;; make
	   ("m l" ellama-make-list "Make list")
	   ("m t" ellama-make-table "Make table")
	   ("m f" ellama-make-format "Make format")
	   ;; ask
	   ("a a" ellama-ask-about "Ask about")
	   ("a i" ellama-ask-interactive "Ask interactively")
	   ("a l" ellama-ask-line "Ask current line")
	   ("a s" ellama-ask-selection "Ask selection")
	   ;; text
	   ("t t" ellama-translate "Text translate")
	   ("t c" ellama-complete "Text complete")
	   ;; define
	   ("d w" ellama-define-word "Define word")
	   ;; provider
	   ("p s" ellama-provider-select "Provider select"))))
    (dolist (key-command key-commands)
      (define-key ellama-keymap (kbd (car key-command)) (cadr key-command)))))

(defcustom ellama-enable-keymap t
  "Enable or disable Ellama keymap."
  :type 'boolean
  :group 'tools
  :set (lambda (symbol value)
	 (set symbol value)
	 (if value
	     (ellama-setup-keymap)
	   ;; If ellama-enable-keymap is nil, remove the key bindings
	   (define-key global-map (kbd ellama-keymap-prefix) nil))))

(defun ellama-stream (prompt &rest args)
  "Query ellama for PROMPT.
ARGS contains keys for fine control.

:buffer BUFFER -- BUFFER is the buffer (or `buffer-name') to insert ellama reply
in.  Default value is (current-buffer).

:point POINT -- POINT is the point in buffer to insert ellama reply at.

:filter FILTER -- FILTER is a function that's applied to (partial) response
strings before they're inserted into the BUFFER.

:session SESSION -- if session is non-nil, the PROMPT will be appended to the
current (BUFFER-local) conversation.

:on-error ON-ERROR -- ON-ERROR a function that's called with an error message on
failure (with BUFFER current).

:on-done ON-DONE -- ON-DONE a function that's called with the full response text
when the request completes (with BUFFER current)."
  (let* ((buffer (or (plist-get args :buffer) (current-buffer)))
	 (point (or (plist-get args :point)
		    (with-current-buffer buffer (point))))
	 (filter (or (plist-get args :filter) #'identity))
	 (session (plist-get args :session))
	 (errcb (or (plist-get args :on-error) (lambda (msg) (error "Error calling the LLM: %s" msg))))
	 (donecb (or (plist-get args :on-done) #'ignore)))
    (with-current-buffer buffer
      (if (and session ellama--chat-prompt)
	  (llm-chat-prompt-append-response
	   ellama--chat-prompt prompt)
	(setq ellama--chat-prompt (llm-make-simple-chat-prompt prompt)))
      (unwind-protect
	  (save-excursion
	    (let* ((start (make-marker))
		   (end (make-marker))
		   (window (selected-window))
		   (insert-text
		    (lambda (text)
		      ;; Erase and insert the new text between the marker cons.
		      (with-current-buffer (marker-buffer start)
			(let ((pt (point)))
			  (save-excursion
			    (goto-char start)
			    (delete-region start end)
			    (insert (funcall filter text))
			    (fill-region start (point)))
			  (goto-char pt))
			(when ellama-auto-scroll
			  (select-window (get-window-with-predicate
					  (lambda (_)
					    (eq (current-buffer)
						(get-buffer buffer))))
					 t)
			  (goto-char (point-max))
			  (recenter -1)
			  (select-window window))))))
	      (setq ellama--change-group (prepare-change-group))
	      (activate-change-group ellama--change-group)
	      (set-marker start point)
	      (set-marker end point)
	      (set-marker-insertion-type start nil)
	      (set-marker-insertion-type end t)
	      (spinner-start ellama-spinner-type)
	      (llm-chat-streaming ellama-provider
				  ellama--chat-prompt
				  insert-text
				  (lambda (text)
				    (funcall insert-text text)
				    (with-current-buffer buffer
				      (undo-amalgamate-change-group ellama--change-group)
				      (accept-change-group ellama--change-group)
				      (spinner-stop)
				      (funcall donecb text)))
				  (lambda (_ msg)
				    (with-current-buffer buffer
				      (cancel-change-group ellama--change-group)
				      (spinner-stop)
				      (funcall errcb msg))))))))))

;;;###autoload
(defun ellama-chat (prompt)
  "Send PROMPT to ellama chat with conversation history."
  (interactive "sAsk ellama: ")
  (when (not (buffer-live-p (get-buffer ellama-buffer)))
    (get-buffer-create ellama-buffer)
    (with-current-buffer ellama-buffer
      (funcall ellama-buffer-mode)))
  (display-buffer ellama-buffer)
  (with-current-buffer ellama-buffer
    (save-excursion
      (goto-char (point-max))
      (insert "## " ellama-user-nick ":\n" prompt "\n\n"
	      "## " ellama-assistant-nick ":\n")
      (ellama-stream prompt
		     :session t
		     :on-done (lambda (_) (save-excursion
				      (goto-char (point-max))
				      (insert "\n\n")))))))

;;;###autoload
(defalias 'ellama-ask 'ellama-chat)

;;;###autoload
(defalias 'ellama-code-complete 'ellama-complete-code)

;;;###autoload
(defalias 'ellama-code-add 'ellama-add-code)

;;;###autoload
(defalias 'ellama-code-edit 'ellama-change-code)

;;###autoload
(defalias 'ellama-code-improve 'ellama-enhance-code)

;;;###autoload
(defalias 'ellama-improve-wording 'ellama-enhance-wording)

;;;###autoload
(defalias 'ellama-improve-grammar 'ellama-enhance-grammar-spelling)

;;;###autoload
(defalias 'ellama-improve-conciseness 'ellama-make-concise)

;;;###autoload
(defalias 'ellama-make-format 'ellama-render)

;;;###autoload
(defalias 'ellama-ask-interactive 'ellama-ask)

;;;###autoload
(defun ellama-ask-about ()
  "Ask ellama about selected region or current buffer."
  (interactive)
  (let ((input (read-string "Ask ellama about this text: "))
	(text (if (region-active-p)
		  (buffer-substring-no-properties (region-beginning) (region-end))
		(buffer-substring-no-properties (point-min) (point-max)))))
    (ellama-chat (format "Text:\n%s\nRegarding this text, %s" text input))))

;;;###autoload
(defun ellama-ask-selection ()
  "Send selected region or current buffer to ellama chat."
  (interactive)
  (let ((text (if (region-active-p)
		  (buffer-substring-no-properties (region-beginning) (region-end))
		(buffer-substring-no-properties (point-min) (point-max)))))
    (ellama-chat text)))

;;;###autoload
(defun ellama-complete ()
  "Complete text in current buffer."
  (interactive)
  (let* ((beg (if (region-active-p)
		  (region-beginning)
		(point-min)))
	 (end (if (region-active-p)
		  (region-end)
		(point)))
	 (text (buffer-substring-no-properties beg end)))
    (ellama-stream text)))

;;;###autoload
(defun ellama-ask-line ()
  "Send current line to ellama chat."
  (interactive)
  (let ((text (thing-at-point 'line)))
    (ellama-chat text)))

(defun ellama-instant (prompt)
  "Prompt ellama for PROMPT to reply instantly."
  (let ((buffer (get-buffer-create (make-temp-name ellama-buffer))))
    (display-buffer buffer)
    (ellama-stream prompt :buffer buffer (point-min))))

;;;###autoload
(defun ellama-translate ()
  "Ask ellama to translate selected region or word at point."
  (interactive)
  (if (region-active-p)
      (ellama-instant
       (format "Translate the following text to %s:\n%s"
	       ellama-language
	       (buffer-substring-no-properties (region-beginning) (region-end))))
    (ellama-instant
     (format "Translate %s to %s" (thing-at-point 'word) ellama-language))))

;;;###autoload
(defun ellama-define-word ()
  "Find definition of current word."
  (interactive)
  (ellama-instant (format "Define %s" (thing-at-point 'word))))

;;;###autoload
(defun ellama-summarize ()
  "Summarize selected region or current buffer."
  (interactive)
  (let ((text (if (region-active-p)
		  (buffer-substring-no-properties (region-beginning) (region-end))
		(buffer-substring-no-properties (point-min) (point-max)))))
    (ellama-instant (format "Text:\n%s\nSummarize it." text))))

;;;###autoload
(defun ellama-code-review ()
  "Review code in selected region or current buffer."
  (interactive)
  (let ((text (if (region-active-p)
		  (buffer-substring-no-properties (region-beginning) (region-end))
		(buffer-substring-no-properties (point-min) (point-max)))))
    (ellama-instant (format "Review the following code and make concise suggestions:\n```\n%s\n```" text))))

;;;###autoload
(defun ellama-change (change)
  "Change selected text or text in current buffer according to provided CHANGE."
  (interactive "sWhat needs to be changed: ")
  (let* ((beg (if (region-active-p)
		  (region-beginning)
		(point-min)))
	 (end (if (region-active-p)
		  (region-end)
		(point-max)))
	 (text (buffer-substring-no-properties beg end)))
    (kill-region beg end)
    (ellama-stream
     (format
      "Change the following text, %s, just output the final text without additional quotes around it:\n%s"
      change text)
     :point beg)))

;;;###autoload
(defun ellama-enhance-grammar-spelling ()
  "Enhance the grammar and spelling in the currently selected region or buffer."
  (interactive)
  (ellama-change "improve grammar and spelling"))

;;;###autoload
(defun ellama-enhance-wording ()
  "Enhance the wording in the currently selected region or buffer."
  (interactive)
  (ellama-change "use better wording"))

;;;###autoload
(defun ellama-make-concise ()
  "Make the text of the currently selected region or buffer concise and simple."
  (interactive)
  (ellama-change "make it as simple and concise as possible"))

;;;###autoload
(defun ellama-change-code (change)
  "Change selected code or code in current buffer according to provided CHANGE."
  (interactive "sWhat needs to be changed in this code: ")
  (let* ((beg (if (region-active-p)
		  (region-beginning)
		(point-min)))
	 (end (if (region-active-p)
		  (region-end)
		(point-max)))
	 (text (buffer-substring-no-properties beg end)))
    (kill-region beg end)
    (ellama-stream
     (format
      "Regarding the following code, %s, only ouput the result code in format ```language\n...\n```:\n```\n%s\n```"
      change text)
     :filter #'ellama--code-filter
     :point beg)))

;;;###autoload
(defun ellama-enhance-code ()
  "Change selected code or code in current buffer according to provided CHANGE."
  (interactive)
  (let* ((beg (if (region-active-p)
		  (region-beginning)
		(point-min)))
	 (end (if (region-active-p)
		  (region-end)
		(point-max)))
	 (text (buffer-substring-no-properties beg end)))
    (kill-region beg end)
    (ellama-stream
     (format
      "Enhance the following code, only ouput the result code in format ```language\n...\n```:\n```\n%s\n```"
      text)
     :filter #'ellama--code-filter
     :point beg)))

;;;###autoload
(defun ellama-complete-code ()
  "Complete selected code or code in current buffer."
  (interactive)
  (let* ((beg (if (region-active-p)
		  (region-beginning)
		(point-min)))
	 (end (if (region-active-p)
		  (region-end)
		(point)))
	 (text (buffer-substring-no-properties beg end)))
    (ellama-stream
     (format
      "Continue the following code, only write new code in format ```language\n...\n```:\n```\n%s\n```"
      text)
     :filter #'ellama--code-filter
     :point end)))

;;;###autoload
(defun ellama-add-code (description)
  "Add new code according to DESCRIPTION.
Code will be generated with provided context from selected region or current
buffer."
  (interactive "sDescribe the code to be generated: ")
  (let* ((beg (if (region-active-p)
		  (region-beginning)
		(point-min)))
	 (end (if (region-active-p)
		  (region-end)
		(point-max)))
	 (text (buffer-substring-no-properties beg end)))
    (ellama-stream
     (format
      "Context: \n```\n%s\n```\nBased on this context, %s, only ouput the result in format ```\n...\n```"
      text description)
     :filter #'ellama--code-filter)))


;;;###autoload
(defun ellama-render (needed-format)
  "Render selected text or text in current buffer as NEEDED-FORMAT."
  (interactive "sSpecify required format: ")
  (let* ((beg (if (region-active-p)
		  (region-beginning)
		(point-min)))
	 (end (if (region-active-p)
		  (region-end)
		(point-max)))
	 (text (buffer-substring-no-properties beg end)))
    (kill-region beg end)
    (ellama-stream
     (format
      "Render the following text as a %s:\n%s"
      needed-format text)
     :point beg)))

;;;###autoload
(defun ellama-make-list ()
  "Create markdown list from active region or current buffer."
  (interactive)
  (ellama-render "markdown list"))

;;;###autoload
(defun ellama-make-table ()
  "Create markdown table from active region or current buffer."
  (interactive)
  (ellama-render "markdown table"))

(defun ellama-summarize-webpage (url)
  "Summarize webpage fetched from URL."
  (interactive "sEnter URL you want to summarize: ")
  (let ((buffer-name (url-retrieve-synchronously url t)))
    ;; (display-buffer buffer-name)
    (with-current-buffer buffer-name
      (goto-char (point-min))
      (search-forward "<!DOCTYPE")
      (beginning-of-line)
      (kill-region (point-min) (point))
      (shr-insert-document (libxml-parse-html-region (point-min) (point-max)))
      (goto-char (point-min))
      (search-forward "<!DOCTYPE")
      (beginning-of-line)
      (kill-region (point) (point-max))
      (ellama-summarize))))

(defun ellama-get-ollama-local-model ()
  "Return llm provider for interactively selected ollama model."
  (interactive)
  (let ((model-name
	 (completing-read "Select ollama model: "
			  (mapcar (lambda (s)
				    (car (split-string s)))
				  (seq-drop
				   (process-lines ellama-ollama-binary "ls") 1)))))
    (make-llm-ollama
     :chat-model model-name :embedding-model model-name)))

;;;###autoload
(defun ellama-provider-select ()
  "Select ellama provider."
  (interactive)
  (let* ((providers (if (and ellama-ollama-binary
			     (file-exists-p ellama-ollama-binary))
			(push '("ollama model" . (ellama-get-ollama-local-model))
			      ellama-providers)
		      ellama-providers))
	 (variants (mapcar #'car providers)))
    (setq ellama-provider
	  (eval (alist-get
		 (completing-read "Select model: " variants)
		 providers nil nil #'string=)))))

(provide 'ellama)
;;; ellama.el ends here.
