;;; ellama.el --- Tool for interacting with LLMs -*- lexical-binding: t -*-

;; Copyright (C) 2023 Sergey Kostyaev

;; Author: Sergey Kostyaev <sskostyaev@gmail.com>
;; URL: http://github.com/s-kostyaev/ellama
;; Keywords: help local tools
;; Package-Requires: ((emacs "28.1")(llm "0.5.0")(spinner "1.7.4"))
;; Version: 0.1.0
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Ellama is a tool for interacting with large language models from Emacs.
;; It allows you to ask questions and receive responses from the
;; LLMs. Ellama can perform various tasks such as translation, code
;; review, summarization, enhancing grammar/spelling or wording and
;; more through the Emacs interface. Ellama natively supports streaming
;; output, making it effortless to use with your preferred text editor.
;;

;;; Code:

(require 'json)
(require 'llm)
(require 'llm-ollama)
(require 'spinner)

(defgroup ellama nil
  "Tool for interacting with LLMs for Emacs."
  :group 'ellama)

(defcustom ellama-buffer "*ellama*" "Default ellama buffer."
  :group 'ellama
  :type 'string)

(defcustom ellama-user-nick "User" "User nick in logs."
  :group 'ellama
  :type 'string)

(defcustom ellama-assistant-nick "Ellama" "Assistant nick in logs."
  :group 'ellama
  :type 'string)

(defcustom ellama-buffer-mode 'markdown-mode "Major mode for ellama logs."
  :group 'ellama
  :type 'function)

(defcustom ellama-language "English" "Language for ellama translation."
  :group 'ellama
  :type 'string)

(defcustom ellama-provider
  (make-llm-ollama
   :chat-model "zephyr" :embedding-model "zephyr")
  "Backend LLM provider."
  :group 'ellama
  :type '(sexp :validate 'cl-struct-p))

(defcustom ellama-spinner-type 'progress-bar "Spinner type for ellama."
  :group 'ellama
  :type 'symbol)

(defvar-local ellama--chat-prompt nil)

(defvar ellama--code-prefix
  (rx (minimal-match
       (zero-or-more anything) (literal "```") (zero-or-more anything) line-end)))

(defvar ellama--code-suffix
  (rx (minimal-match
       (literal "```") (zero-or-more anything))))

(defun ellama-stream (prompt &rest args)
  "Query ellama for PROMPT.
ARGS contains keys for fine control.

:buffer BUFFER -- BUFFER is the buffer (or `buffer-name') to insert ellama reply
in. Default value is (current-buffer).

:point POINT -- POINT is the point in buffer to insert ellama reaply at."
  (let* ((buffer (or (plist-get args :buffer) (current-buffer)))
	 (point (or (plist-get args :point)
		    (with-current-buffer buffer (point)))))
    (with-current-buffer buffer
      (save-excursion
	(let* ((start (make-marker))
	       (end (make-marker))
	       (insert-text
		(lambda (text)
		  ;; Erase and insert the new text between the marker cons.
		  (with-current-buffer (marker-buffer start)
		    (save-excursion
		      (goto-char start)
		      (delete-region start end)
		      (insert text))))))
          (set-marker start point)
          (set-marker end point)
          (set-marker-insertion-type start nil)
          (set-marker-insertion-type end t)
	  (spinner-start ellama-spinner-type)
	  (llm-chat-streaming ellama-provider
			      (llm-make-simple-chat-prompt prompt)
			      insert-text
			      (lambda (text)
				(funcall insert-text text)
				(with-current-buffer buffer
				  (spinner-stop)))
			      (lambda (_ msg) (error "Error calling the LLM: %s" msg))))))))

(defun ellama-stream-filter (prompt prefix suffix buffer point)
  "Query ellama for PROMPT with filtering.
In BUFFER at POINT will be inserted result between PREFIX and SUFFIX."
  (with-current-buffer buffer
    (save-excursion
      (let* ((start (make-marker))
             (end (make-marker))
	     (insert-text (lambda (text)
			    ;; Erase and insert the new text between the marker cons.
			    (with-current-buffer (marker-buffer start)
			      (save-excursion
				(goto-char start)
				(delete-region start end)
				;; remove prefix and suffix parts
				(insert (string-trim-right
					 (string-trim-left text prefix)
					 suffix)))))))
        (set-marker start point)
        (set-marker end point)
        (set-marker-insertion-type start nil)
        (set-marker-insertion-type end t)
	(spinner-start ellama-spinner-type)
	(llm-chat-streaming ellama-provider
			    (llm-make-simple-chat-prompt prompt)
			    insert-text
			    (lambda (text)
			      (funcall insert-text text)
			      (with-current-buffer buffer
				(spinner-stop)))
			    (lambda (_ msg) (error "Error calling the LLM: %s" msg)))))))

;;;###autoload
(defun ellama-chat (prompt)
  "Send PROMPT to ellama chat with conversation history."
  (interactive "sAsk ellama: ")
  (while (not (buffer-live-p (get-buffer ellama-buffer)))
    (get-buffer-create ellama-buffer)
    (with-current-buffer ellama-buffer
      (funcall ellama-buffer-mode)))
  (with-current-buffer ellama-buffer
    (display-buffer ellama-buffer)
    (if ellama--chat-prompt
	(llm-chat-prompt-append-response
	 ellama--chat-prompt prompt)
      (setq ellama--chat-prompt (llm-make-simple-chat-prompt prompt)))
    (save-excursion
      (goto-char (point-max))
      (insert "## " ellama-user-nick ":\n" prompt "\n\n"
	      "## " ellama-assistant-nick ":\n")
      (let* ((start (make-marker))
	     (end (make-marker))
	     (point (point-max))
	     (insert-text
	      (lambda (text)
		;; Erase and insert the new text between the marker cons.
		(with-current-buffer (marker-buffer start)
		  (save-excursion
		    (goto-char start)
		    (delete-region start end)
		    (insert text))))))
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
			      (with-current-buffer ellama-buffer
				(save-excursion
				  (goto-char (point-max))
				  (insert "\n\n"))
				(spinner-stop)))
			    (lambda (_ msg) (error "Error calling the LLM: %s" msg)))))))

;;;###autoload
(defalias 'ellama-ask 'ellama-chat)

;;;###autoload
(defun ellama-ask-about ()
  "Ask ellama about selected region or current buffer."
  (interactive)
  (let ((input (read-string "Ask ellama about this text: "))
	(text (if (region-active-p)
		  (buffer-substring-no-properties (region-beginning) (region-end))
		(buffer-substring-no-properties (point-min) (point-max)))))
    (ellama-chat (format "Text:\n%s\nRegarding this text, %s" text input))))

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
    (ellama-stream-filter
     (format
      "Regarding the following code, %s, only ouput the result code in format ```language\n...\n```:\n```\n%s\n```"
      change text)
     ellama--code-prefix
     ellama--code-suffix
     (current-buffer)
     beg)))

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
    (ellama-stream-filter
     (format
      "Enhance the following code, only ouput the result code in format ```language\n...\n```:\n```\n%s\n```"
      text)
     ellama--code-prefix
     ellama--code-suffix
     (current-buffer)
     beg)))

;;;###autoload
(defun ellama-complete-code ()
  "Change selected code or code in current buffer according to provided CHANGE."
  (interactive)
  (let* ((beg (if (region-active-p)
		  (region-beginning)
		(point-min)))
	 (end (if (region-active-p)
		  (region-end)
		(point-max)))
	 (text (buffer-substring-no-properties beg end)))
    (ellama-stream-filter
     (format
      "Continue the following code, only write new code in format ```language\n...\n```:\n```\n%s\n```"
      text)
     ellama--code-prefix
     ellama--code-suffix
     (current-buffer)
     end)))

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
    (ellama-stream-filter
     (format
      "Context: \n```\n%s\n```\nBased on this context, %s, only ouput the result in format ```\n...\n```"
      text description)
     ellama--code-prefix
     ellama--code-suffix
     (current-buffer)
     end)))


;;;###autoload
(defun ellama-render (needed-format)
  "Render selected text or text in current buffer as NEEDED-FORMAT."
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

(provide 'ellama)
;;; ellama.el ends here.
