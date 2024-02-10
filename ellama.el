;;; ellama.el --- Tool for interacting with LLMs -*- lexical-binding: t -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.

;; Author: Sergey Kostyaev <sskostyaev@gmail.com>
;; URL: http://github.com/s-kostyaev/ellama
;; Keywords: help local tools
;; Package-Requires: ((emacs "28.1") (llm "0.6.0") (spinner "1.7.4") (dash "2.19.1"))
;; Version: 0.7.7
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
(require 'dash)
(eval-when-compile (require 'rx))

(defgroup ellama nil
  "Tool for interacting with LLMs."
  :group 'tools)

(defcustom ellama-user-nick "User"
  "User nick in logs."
  :group 'ellama
  :type 'string)

(defcustom ellama-assistant-nick "Ellama"
  "Assistant nick in logs."
  :group 'ellama
  :type 'string)

(defcustom ellama-nick-prefix "**"
  "User and assistant nick prefix in logs."
  :group 'ellama
  :type 'string)

(defcustom ellama-language "English"
  "Language for ellama translation."
  :group 'ellama
  :type 'string)

(defcustom ellama-provider
  (progn
    (declare-function make-llm-ollama "llm-ollama")
    (require 'llm-ollama)
    (make-llm-ollama
     :chat-model "zephyr" :embedding-model "zephyr"))
  "Backend LLM provider."
  :group 'ellama
  :type '(sexp :validate 'cl-struct-p))

(defcustom ellama-providers nil
  "LLM provider list for fast switching."
  :group 'ellama
  :type '(alist :key-type string))

(defcustom ellama-spinner-type 'progress-bar
  "Spinner type for ellama."
  :group 'ellama
  :type `(choice ,@(mapcar
		    (lambda (type)
		      `(const ,(car type)))
		    spinner-types)))

(defun ellama-setup-keymap ()
  "Set up the Ellama keymap and bindings."
  (interactive)
  (when (boundp 'ellama-keymap-prefix)
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
	     ("a i" ellama-chat "Ask interactively")
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
	(define-key ellama-keymap (kbd (car key-command)) (cadr key-command))))))

(defcustom ellama-keymap-prefix "C-c e"
  "Key sequence for Ellama Commands."
  :type 'string
  :set (lambda (symbol value)
	 (custom-set-default symbol value)
	 (when value
	   (ellama-setup-keymap)))
  :group 'ellama)

(defcustom ellama-ollama-binary (executable-find "ollama")
  "Path to ollama binary."
  :type 'string
  :group 'ellama)

(defcustom ellama-auto-scroll nil
  "If enabled ellama buffer will scroll automatically during generation."
  :type 'boolean
  :group 'ellama)

(defcustom ellama-fill-paragraphs '(text-mode)
  "When to wrap paragraphs."
  :group 'ellama
  :type `(choice
          (const :tag "Never fill paragraphs" nil)
          (const :tag "Always fill paragraphs" t)
          (function :tag "By predicate")
          (repeat :tag "In specific modes" (symbol))))

(defcustom ellama-name-prompt-words-count 5
  "Count of words in prompt to generate name."
  :group 'ellama
  :type 'integer)

(defcustom ellama-ask-about-prompt-template "Text:\n%s\nRegarding this text, %s"
  "Prompt template for `ellama-ask-about'."
  :group 'ellama
  :type 'string)

(defcustom ellama-translate-word-prompt-template "Translate %s to %s"
  "Promp template for `ellama-translate' with single word."
  :group 'ellama
  :type 'string)

(defcustom ellama-translate-region-prompt-template "Translate the following text to %s:\n%s"
  "Promp template for `ellama-translate' with active region."
  :group 'ellama
  :type 'string)

(defcustom ellama-define-word-prompt-template "Define %s"
  "Prompt template for `ellama-define-word'."
  :group 'ellama
  :type 'string)

(defcustom ellama-summarize-prompt-template "Text:\n%s\nSummarize it."
  "Prompt template for `ellama-summarize'."
  :group 'ellama
  :type 'string)

(defcustom ellama-code-review-prompt-template "Review the following code and make concise suggestions:\n```\n%s\n```"
  "Prompt template for `ellama-code-review'."
  :group 'ellama
  :type 'string)

(defcustom ellama-change-prompt-template "Change the following text, %s, just output the final text without additional quotes around it:\n%s"
  "Prompt template for `ellama-change'."
  :group 'ellama
  :type 'string)

(defcustom ellama-improve-grammar-prompt-template "improve grammar and spelling"
  "Prompt template for `ellama-improve-grammar'."
  :group 'ellama
  :type 'string)

(defcustom ellama-improve-wording-prompt-template "use better wording"
  "Prompt template for `ellama-improve-wording'."
  :group 'ellama
  :type 'string)

(defcustom ellama-improve-conciseness-prompt-template "make it as simple and concise as possible"
  "Prompt template for `ellama-improve-conciseness'."
  :group 'ellama
  :type 'string)

(defcustom ellama-code-edit-prompt-template "Regarding the following code, %s, only output the result code in format ```language\n...\n```:\n```\n%s\n```"
  "Prompt template for `ellama-code-edit'."
  :group 'ellama
  :type 'string)

(defcustom ellama-code-improve-prompt-template "Enhance the following code, only output the result code in format ```language\n...\n```:\n```\n%s\n```"
  "Prompt template for `ellama-code-improve'."
  :group 'ellama
  :type 'string)

(defcustom ellama-code-complete-prompt-template "Continue the following code, only write new code in format ```language\n...\n```:\n```\n%s\n```"
  "Prompt template for `ellama-code-complete'."
  :group 'ellama
  :type 'string)

(defcustom ellama-code-add-prompt-template "Context: \n```\n%s\n```\nBased on this context, %s, only output the result in format ```\n...\n```"
  "Prompt template for `ellama-code-add'."
  :group 'ellama
  :type 'string)

(defcustom ellama-make-format-prompt-template "Render the following text as a %s:\n%s"
  "Prompt template for `ellama-make-format'."
  :group 'ellama
  :type 'string)

(defcustom ellama-make-list-prompt-template "markdown list"
  "Prompt template for `ellama-make-list'."
  :group 'ellama
  :type 'string)

(defcustom ellama-make-table-prompt-template "markdown table"
  "Prompt template for `ellama-make-table'."
  :group 'ellama
  :type 'string)

(defcustom ellama-chat-done-callback nil
  "Callback that will be called on ellama chat response generation done.
It should be a function with single argument generated text string."
  :group 'ellama
  :type 'function)

(defcustom ellama-major-mode 'org-mode
  "Major mode for ellama commands."
  :group 'ellama
  :type 'symbol)

(defcustom ellama-long-lines-length 100
  "Long lines length for fill paragraph call.
Too low value can break generated code by splitting long comment lines."
  :group 'ellama
  :type 'integer)

(defcustom ellama-session-auto-save t
  "Automatically save ellama sessions if set."
  :group 'ellama
  :type 'boolean)

(define-minor-mode ellama-session-mode
  "Minor mode for ellama session buffers."
  :interactive nil
  (if ellama-session-mode
      (progn
        (add-hook 'after-save-hook 'ellama--save-session nil t)
        (add-hook 'kill-buffer-hook 'ellama--session-deactivate nil t))
    (remove-hook 'kill-buffer-hook 'ellama--session-deactivate)
    (remove-hook 'after-save-hook 'ellama--save-session)
    (ellama--session-deactivate)))

(define-minor-mode ellama-request-mode
  "Minor mode for ellama buffers with active request to llm."
  :interactive nil
  :keymap '(([remap keyboard-quit] . ellama--cancel-current-request-and-quit))
  (if ellama-request-mode
      (add-hook 'kill-buffer-hook 'ellama--cancel-current-request nil t)
    (remove-hook 'kill-buffer-hook 'ellama--cancel-current-request)
    (ellama--cancel-current-request)))

(defvar-local ellama--change-group nil)

(defvar-local ellama--current-request nil)

(defconst ellama--code-prefix
  (rx (minimal-match
       (zero-or-more anything) (literal "```") (zero-or-more anything) line-end)))

(defconst ellama--code-suffix
  (rx (minimal-match
       (literal "```") (zero-or-more anything))))

(defun ellama--code-filter (text)
  "Filter code prefix/suffix from TEXT."
  ;; Trim left first as `string-trim' trims from the right and ends up deleting all the code.
  (string-trim-right (string-trim-left text ellama--code-prefix) ellama--code-suffix))

(defun ellama--fill-string (s)
  "Fill string S."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert s)
    (fill-region (point-min) (point-max))
    (buffer-substring (point-min) (point-max))))

(defun ellama--fill-long-lines (text)
  "Fill long lines only in TEXT."
  (--> text
       (split-string it "\n")
       (-map (lambda (el)
	       (if (> (length el)
		      ellama-long-lines-length)
		   (ellama--fill-string el)
		 el)) it)
       (string-join it "\n")))

(defun ellama--translate-markdown-to-org-filter (text)
  "Filter to translate code blocks from markdown syntax to org syntax in TEXT.
This filter contains only subset of markdown syntax to be good enough."
  (->> text
       ;; code blocks
       (replace-regexp-in-string "^```\\(.+\\)$" "#+BEGIN_SRC \\1")
       (replace-regexp-in-string "^<!-- language: \\(.+\\) -->\n```" "#+BEGIN_SRC \\1")
       (replace-regexp-in-string "^```$" "#+END_SRC")
       ;; lists
       (replace-regexp-in-string "^\\* " "+ ")
       ;; bold
       (replace-regexp-in-string "\\*\\*\\(.+?\\)\\*\\*" "*\\1*")
       (replace-regexp-in-string "__\\(.+?\\)__" "*\\1*")
       (replace-regexp-in-string "<b>\\(.+?\\)</b>" "*\\1*")
       ;; italic
       (replace-regexp-in-string "_\\(.+?\\)_" "/\\1/")
       (replace-regexp-in-string "<i>\\(.+?\\)</i>" "/\\1/")
       ;; inline code
       (replace-regexp-in-string "`\\(.+?\\)`" "~\\1~")
       ;; underlined
       (replace-regexp-in-string "<u>\\(.+?\\)</u>" "_\\1_")
       ;; strikethrough
       (replace-regexp-in-string "~~\\(.+?\\)~~" "+\\1+")
       (replace-regexp-in-string "<s>\\(.+?\\)</s>" "+\\1+")
       ;; headings
       (replace-regexp-in-string "^# " "* ")
       (replace-regexp-in-string "^## " "** ")
       (replace-regexp-in-string "^### " "*** ")
       (replace-regexp-in-string "^#### " "**** ")
       (replace-regexp-in-string "^##### " "***** ")
       (replace-regexp-in-string "^###### " "***** ")
       ;; badges
       (replace-regexp-in-string "\\[\\!\\[.*?\\](\\(.*?\\))\\](\\(.*?\\))" "[[\\2][file:\\1]]")
       ;;links
       (replace-regexp-in-string "\\[\\(.*?\\)\\](\\(.*?\\))" "[[\\2][\\1]]")
       ;; filling long lines
       (ellama--fill-long-lines)))

(defcustom ellama-enable-keymap t
  "Enable or disable Ellama keymap."
  :type 'boolean
  :group 'ellama
  :set (lambda (symbol value)
	 (custom-set-default symbol value)
	 (if value
	     (ellama-setup-keymap)
	   ;; If ellama-enable-keymap is nil, remove the key bindings
	   (define-key global-map (kbd ellama-keymap-prefix) nil))))

(defcustom ellama-session-file-extension "org"
  "File extension for saving ellama session."
  :type 'string
  :group 'ellama)

(defcustom ellama-sessions-directory (file-truename
				      (file-name-concat
				       user-emacs-directory
				       "ellama-sessions"))
  "Directory for saved ellama sessions."
  :type 'string
  :group 'ellama)

(defvar-local ellama--current-session nil)

(defvar ellama--current-session-id nil)

(defvar ellama--active-sessions (make-hash-table :test #'equal))

(cl-defstruct ellama-session
  "A structure represent ellama session.

ID is an unique identifier of session, string.

PROVIDER is an llm provider of session.

FILE is a path to file contains string representation of this session, string.

PROMPT is a variable contains last prompt in this session.

CONTEXT contains context for next request."
  id provider file prompt context)

(defun ellama-get-session-buffer (id)
  "Return ellama session buffer by provided ID."
  (gethash id ellama--active-sessions))

(defun ellama-generate-name (provider action prompt)
  "Generate name for ellama ACTION by PROVIDER according to PROMPT."
  (let* ((cleaned-prompt (replace-regexp-in-string "/" "_" prompt))
         (prompt-words (split-string cleaned-prompt)))
    (string-join
     (flatten-tree
      (list (split-string (format "%s" action) "-")
	    (seq-take prompt-words ellama-name-prompt-words-count)
	    (if (> (length prompt-words) ellama-name-prompt-words-count)
		"..."
	      nil)
	    (format "(%s)" (llm-name provider))))
     " ")))

(defun ellama-new-session (provider prompt &optional ephemeral)
  "Create new ellama session with unique id.
Provided PROVIDER and PROMPT will be used in new session.
If EPHEMERAL non nil new session will not be associated with any file."
  (let* ((name (ellama-generate-name provider 'ellama prompt))
	 (count 1)
	 (name-with-suffix (format "%s %d" name count))
	 (id (if (not (ellama-get-session-buffer name))
		 name
	       (while (ellama-get-session-buffer name-with-suffix)
		 (setq count (+ count 1))
		 (setq name-with-suffix (format "%s %d" name count)))
	       name-with-suffix))
	 (file-name (when (and (not ephemeral)
			       ellama-session-auto-save)
		      (file-name-concat
		       ellama-sessions-directory
		       (concat id "." ellama-session-file-extension))))
	 (session (make-ellama-session
		   :id id :provider provider :file file-name :context nil))
	 (buffer (if file-name
		     (progn
		       (make-directory ellama-sessions-directory t)
		       (find-file-noselect file-name))
		   (get-buffer-create id))))
    (setq ellama--current-session-id id)
    (puthash id buffer ellama--active-sessions)
    (with-current-buffer buffer
      (funcall ellama-major-mode)
      (setq ellama--current-session session)
      (ellama-session-mode +1))
    session))

(defun ellama--cancel-current-request ()
  "Cancel current running request."
  (when ellama--current-request
    (llm-cancel-request ellama--current-request)
    (setq ellama--current-request nil)))

(defun ellama--cancel-current-request-and-quit ()
  "Cancel the current request and quit."
  (interactive)
  (ellama--cancel-current-request)
  (keyboard-quit))

(defun ellama--session-deactivate ()
  "Deactivate current session."
  (ellama--cancel-current-request)
  (when-let* ((session ellama--current-session)
              (id (ellama-session-id session)))
    (when (string= (buffer-name)
                   (buffer-name (ellama-get-session-buffer id)))
      (remhash id ellama--active-sessions)
      (when (equal ellama--current-session-id id)
	(setq ellama--current-session-id nil)))))

(defun ellama--get-session-file-name (file-name)
  "Get ellama session file name for FILE-NAME."
  (let* ((base-name (file-name-nondirectory file-name))
	 (dir (file-name-directory file-name))
	 (session-file-name
	  (file-name-concat
	   dir
	   (concat "." base-name ".session.el"))))
    session-file-name))

(defun ellama--save-session ()
  "Save current ellama session."
  (when ellama--current-session
    (let* ((session ellama--current-session)
	   (file-name (ellama-session-file session))
	   (session-file-name (ellama--get-session-file-name file-name)))
      (with-temp-file session-file-name
	(insert (prin1-to-string session))))))

;;;###autoload
(defun ellama-load-session ()
  "Load ellama session from file."
  (interactive)
  (when-let* ((dir (if current-prefix-arg
		       (read-directory-name
			"Select directory containing sessions: "
			ellama-sessions-directory)
		     ellama-sessions-directory))
	      (file-name (file-name-concat
			  ellama-sessions-directory
			  (completing-read
			   "Select session to load: "
			   (directory-files
			    ellama-sessions-directory nil "^[^\.].*"))))
	      (session-file-name (ellama--get-session-file-name file-name))
	      (session-file-exists (file-exists-p session-file-name))
	      (buffer (find-file-noselect file-name))
	      (session-buffer (find-file-noselect session-file-name)))
    (with-current-buffer session-buffer
      (goto-char (point-min))
      ;; old sessions support
      (when (string= "(setq "
		     (buffer-substring-no-properties 1 7))
	(goto-char (point-min))
	;; skip "("
	(forward-char)
	;; skip setq
	(forward-sexp)
	;; skip ellama--current-session
	(forward-sexp)
	;; skip space
	(forward-char)
	;; remove all above
	(kill-region (point-min) (point))
	(goto-char (point-max))
	;; remove ")"
	(delete-char -1)
	;; save session in new format
	(save-buffer)
	(goto-char (point-min))))
    (with-current-buffer buffer
      (setq ellama--current-session (read session-buffer))
      (setq ellama--current-session-id (ellama-session-id ellama--current-session))
      (puthash (ellama-session-id ellama--current-session)
	       buffer ellama--active-sessions)
      (ellama-session-mode +1))
    (kill-buffer session-buffer)
    (display-buffer buffer)))

;;;###autoload
(defun ellama-session-remove ()
  "Remove ellama session."
  (interactive)
  (let* ((id (completing-read
	      "Select session to remove: "
	      (hash-table-keys ellama--active-sessions)))
	 (buffer (ellama-get-session-buffer id))
	 (file (buffer-file-name buffer))
	 (session-file (ellama--get-session-file-name file)))
    (kill-buffer buffer)
    (delete-file file t)
    (delete-file session-file t)))

;;;###autoload
(defun ellama-session-switch ()
  "Change current active session."
  (interactive)
  (let* ((id (completing-read
	      "Select session to activate: "
	      (hash-table-keys ellama--active-sessions)))
	 (buffer (ellama-get-session-buffer id)))
    (setq ellama--current-session-id id)
    (display-buffer buffer)))

;;;###autoload
(defun ellama-session-rename ()
  "Rename current ellama session."
  (interactive)
  (when-let* ((id (if ellama--current-session
		      (ellama-session-id ellama--current-session)
		    ellama--current-session-id))
	      (buffer (ellama-get-session-buffer id))
	      (session (with-current-buffer buffer
			 ellama--current-session))
	      (file-name (buffer-file-name buffer))
	      (file-ext (file-name-extension file-name))
	      (dir (file-name-directory file-name))
	      (session-file-name (ellama--get-session-file-name file-name))
	      (new-id (read-string
		       "New session name: "
		       id))
	      (new-file-name (file-name-concat
			      dir
			      (concat new-id "." file-ext)))
	      (new-session-file-name
	       (ellama--get-session-file-name new-file-name)))
    (with-current-buffer buffer
      (set-visited-file-name new-file-name))
    (when (file-exists-p file-name)
      (rename-file file-name new-file-name))
    (when (file-exists-p session-file-name)
      (rename-file session-file-name new-session-file-name))
    (setf (ellama-session-id session) new-id)
    (when (equal ellama--current-session-id id)
      (setq ellama--current-session-id new-id))
    (remhash id ellama--active-sessions)
    (puthash new-id buffer ellama--active-sessions)))

;;;###autoload
(defun ellama-context-add-file ()
  "Add file to current session."
  (interactive)
  (if-let* ((id ellama--current-session-id)
	    (session (with-current-buffer (ellama-get-session-buffer id)
		       ellama--current-session))
	    (file-name (read-file-name "Select file: " nil nil t)))
      (push (cons 'file file-name) (ellama-session-context session))
    (user-error "Empty current ellama session")))

;;;###autoload
(defun ellama-context-add-buffer ()
  "Add file to current session."
  (interactive)
  (if-let* ((id ellama--current-session-id)
	    (session (with-current-buffer (ellama-get-session-buffer id)
		       ellama--current-session))
	    (buf (read-buffer "Select buffer: " nil t)))
      (push (cons 'buffer buf) (ellama-session-context session))
    (user-error "Empty current ellama session")))

;;;###autoload
(defun ellama-context-add-region ()
  "Add file to current session."
  (interactive)
  (if-let* ((id ellama--current-session-id)
	    (session (with-current-buffer (ellama-get-session-buffer id)
		       ellama--current-session))
	    ((region-active-p))
	    (content (buffer-substring-no-properties (region-beginning) (region-end))))
      (push (cons 'text content) (ellama-session-context session))
    (user-error "Empty current ellama session")))

(defun ellama-stream (prompt &rest args)
  "Query ellama for PROMPT.
ARGS contains keys for fine control.

:provider PROVIDER -- PROVIDER is an llm provider for generation.

:buffer BUFFER -- BUFFER is the buffer (or `buffer-name') to insert ellama reply
in.  Default value is (current-buffer).

:point POINT -- POINT is the point in buffer to insert ellama reply at.

:filter FILTER -- FILTER is a function that's applied to (partial) response
strings before they're inserted into the BUFFER.

:session SESSION -- SESSION is a ellama conversation session.

:ephemeral-session BOOL -- if BOOL is set session will not be saved to named
file by default.

:on-error ON-ERROR -- ON-ERROR a function that's called with an error message on
failure (with BUFFER current).

:on-done ON-DONE -- ON-DONE a function that's called with the full response text
when the request completes (with BUFFER current)."
  (let* ((provider (or (plist-get args :provider) ellama-provider))
	 (session (plist-get args :session))
	 (buffer (or (plist-get args :buffer)
		     (when (ellama-session-p session)
		       (ellama-get-session-buffer (ellama-session-id session)))
		     (current-buffer)))
	 (point (or (plist-get args :point)
		    (with-current-buffer buffer (point))))
	 (filter (or (plist-get args :filter) #'identity))
	 (errcb (or (plist-get args :on-error)
		    (lambda (msg)
		      (error "Error calling the LLM: %s" msg))))
	 (donecb (or (plist-get args :on-done) #'ignore))
	 (llm-prompt (if session
			 (if (llm-chat-prompt-p (ellama-session-prompt session))
			     (progn
			       (llm-chat-prompt-append-response
				(ellama-session-prompt session)
				prompt)
			       (ellama-session-prompt session))
			   (setf (ellama-session-prompt session)
				 (llm-make-simple-chat-prompt prompt)))
		       (llm-make-simple-chat-prompt prompt))))
    (with-current-buffer buffer
      (ellama-request-mode +1)
      (let* ((start (make-marker))
	     (end (make-marker))
	     (insert-text
	      (lambda (text)
		;; Erase and insert the new text between the marker cons.
		(with-current-buffer buffer
		  ;; Manually save/restore point as save-excursion doesn't
		  ;; restore the point into the middle of replaced text.
		  (let ((pt (point)))
		    (goto-char start)
		    (delete-region start end)
		    (insert (funcall filter text))
                    (when (pcase ellama-fill-paragraphs
                            ((cl-type function) (funcall ellama-fill-paragraphs))
                            ((cl-type boolean) ellama-fill-paragraphs)
                            ((cl-type list) (and (apply #'derived-mode-p
							ellama-fill-paragraphs)
						 (not (equal major-mode 'org-mode)))))
                      (fill-region start (point)))
		    (goto-char pt))
		  (when-let ((ellama-auto-scroll)
			     (window (get-buffer-window buffer)))
		    (with-selected-window window
		      (goto-char (point-max))
		      (recenter -1)))
		  (undo-amalgamate-change-group ellama--change-group)))))
	(setq ellama--change-group (prepare-change-group))
	(activate-change-group ellama--change-group)
	(set-marker start point)
	(set-marker end point)
	(set-marker-insertion-type start nil)
	(set-marker-insertion-type end t)
	(spinner-start ellama-spinner-type)
	(setq ellama--current-request
	      (llm-chat-streaming provider
				  llm-prompt
				  insert-text
				  (lambda (text)
				    (funcall insert-text text)
				    (with-current-buffer buffer
				      (accept-change-group ellama--change-group)
				      (spinner-stop)
				      (funcall donecb text)
				      (setq ellama--current-request nil)
				      (ellama-request-mode -1)))
				  (lambda (_ msg)
				    (with-current-buffer buffer
				      (cancel-change-group ellama--change-group)
				      (spinner-stop)
				      (funcall errcb msg)
				      (setq ellama--current-request nil)
				      (ellama-request-mode -1)))))))))

(defun ellama-chat-done (text)
  "Chat done.
Will call `ellama-chat-done-callback' on TEXT."
  (save-excursion
    (goto-char (point-max))
    (insert "\n\n")
    (when ellama-session-auto-save
      (save-buffer)))
  (when ellama-chat-done-callback
    (funcall ellama-chat-done-callback text)))

;;;###autoload
(defun ellama-chat (prompt &optional create-session)
  "Send PROMPT to ellama chat with conversation history.

If CREATE-SESSION set, creates new session even if there is an active session."
  (interactive "sAsk ellama: ")
  (let* ((providers (progn
		      (push '("default model" . ellama-provider)
			    ellama-providers)
		      (if (and ellama-ollama-binary
			       (file-exists-p ellama-ollama-binary))
			  (push '("ollama model" . (ellama-get-ollama-local-model))
				ellama-providers)
			ellama-providers)))
	 (variants (mapcar #'car providers))
	 (provider (if current-prefix-arg
		       (eval (alist-get
			      (completing-read "Select model: " variants)
			      providers nil nil #'string=))
		     ellama-provider))
	 (session (if (or create-session
			  current-prefix-arg
			  (and (not ellama--current-session)
			       (not ellama--current-session-id)))
		      (ellama-new-session provider prompt)
		    (or ellama--current-session
			(with-current-buffer (ellama-get-session-buffer
					      ellama--current-session-id)
			  ellama--current-session))))
	 (buffer (ellama-get-session-buffer
		  (ellama-session-id session))))
    (display-buffer buffer)
    (with-current-buffer buffer
      (save-excursion
	(goto-char (point-max))
	(insert ellama-nick-prefix " " ellama-user-nick ":\n" prompt "\n\n"
		ellama-nick-prefix " " ellama-assistant-nick ":\n")
	(ellama-stream prompt
		       :session session
		       :on-done #'ellama-chat-done
		       :filter (when (derived-mode-p 'org-mode)
				 #'ellama--translate-markdown-to-org-filter))))))

;;;###autoload
(defun ellama-ask-about ()
  "Ask ellama about selected region or current buffer."
  (interactive)
  (let ((input (read-string "Ask ellama about this text: "))
	(text (if (region-active-p)
		  (buffer-substring-no-properties (region-beginning) (region-end))
		(buffer-substring-no-properties (point-min) (point-max)))))
    (ellama-chat (format ellama-ask-about-prompt-template text input))))

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
  (let* ((buffer-name (ellama-generate-name ellama-provider real-this-command prompt))
	 (buffer (get-buffer-create (if (get-buffer buffer-name)
					(make-temp-name (concat buffer-name " "))
				      buffer-name)))
	 filter)
    (with-current-buffer buffer
      (funcall ellama-major-mode)
      (when (derived-mode-p 'org-mode)
	(setq filter 'ellama--translate-markdown-to-org-filter)))
    (display-buffer buffer)
    (ellama-stream prompt
		   :buffer buffer
		   :filter filter)))

;;;###autoload
(defun ellama-translate ()
  "Ask ellama to translate selected region or word at point."
  (interactive)
  (if (region-active-p)
      (ellama-instant
       (format ellama-translate-region-prompt-template
	       ellama-language
	       (buffer-substring-no-properties (region-beginning) (region-end))))
    (ellama-instant
     (format ellama-translate-word-prompt-template
	     (thing-at-point 'word) ellama-language))))

;;;###autoload
(defun ellama-define-word ()
  "Find definition of current word."
  (interactive)
  (ellama-instant (format ellama-define-word-prompt-template
			  (thing-at-point 'word))))

;;;###autoload
(defun ellama-summarize ()
  "Summarize selected region or current buffer."
  (interactive)
  (let ((text (if (region-active-p)
		  (buffer-substring-no-properties (region-beginning) (region-end))
		(buffer-substring-no-properties (point-min) (point-max)))))
    (ellama-instant (format ellama-summarize-prompt-template text))))

;;;###autoload
(defun ellama-code-review ()
  "Review code in selected region or current buffer."
  (interactive)
  (let ((text (if (region-active-p)
		  (buffer-substring-no-properties (region-beginning) (region-end))
		(buffer-substring-no-properties (point-min) (point-max)))))
    (ellama-instant (format ellama-code-review-prompt-template text))))

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
      ellama-change-prompt-template
      change text)
     :point beg)))

;;;###autoload
(defun ellama-improve-grammar ()
  "Enhance the grammar and spelling in the currently selected region or buffer."
  (interactive)
  (ellama-change ellama-improve-grammar-prompt-template))

;;;###autoload
(defun ellama-improve-wording ()
  "Enhance the wording in the currently selected region or buffer."
  (interactive)
  (ellama-change ellama-improve-wording-prompt-template))

;;;###autoload
(defun ellama-improve-conciseness ()
  "Make the text of the currently selected region or buffer concise and simple."
  (interactive)
  (ellama-change ellama-improve-conciseness-prompt-template))

;;;###autoload
(defun ellama-code-edit (change)
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
      ellama-code-edit-prompt-template
      change text)
     :filter #'ellama--code-filter
     :point beg)))

;;;###autoload
(defun ellama-code-improve ()
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
      ellama-code-improve-prompt-template
      text)
     :filter #'ellama--code-filter
     :point beg)))

;;;###autoload
(defun ellama-code-complete ()
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
      ellama-code-complete-prompt-template
      text)
     :filter #'ellama--code-filter
     :point end)))

;;;###autoload
(defun ellama-code-add (description)
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
      ellama-code-add-prompt-template
      text description)
     :filter #'ellama--code-filter)))


;;;###autoload
(defun ellama-make-format (needed-format)
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
      ellama-make-format-prompt-template
      needed-format text)
     :point beg)))

;;;###autoload
(defun ellama-make-list ()
  "Create markdown list from active region or current buffer."
  (interactive)
  (ellama-make-format ellama-make-list-prompt-template))

;;;###autoload
(defun ellama-make-table ()
  "Create markdown table from active region or current buffer."
  (interactive)
  (ellama-make-format ellama-make-table-prompt-template))

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
  (declare-function llm-ollama-p "ext:llm-ollama")
  (declare-function llm-ollama-host "ext:llm-ollama")
  (declare-function llm-ollama-port "ext:llm-ollama")
  (let ((model-name
	 (completing-read "Select ollama model: "
			  (mapcar (lambda (s)
				    (car (split-string s)))
				  (seq-drop
				   (process-lines ellama-ollama-binary "ls") 1))))
	(host (when (llm-ollama-p ellama-provider)
		(llm-ollama-host ellama-provider)))
	(port (when (llm-ollama-p ellama-provider)
		(llm-ollama-port ellama-provider))))
    (make-llm-ollama
     :chat-model model-name :embedding-model model-name :host host :port port)))

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
