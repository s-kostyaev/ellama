;;; ellama.el --- Tool for interacting with LLMs -*- lexical-binding: t -*-

;; Copyright (C) 2023, 2024  Free Software Foundation, Inc.

;; Author: Sergey Kostyaev <sskostyaev@gmail.com>
;; URL: http://github.com/s-kostyaev/ellama
;; Keywords: help local tools
;; Package-Requires: ((emacs "28.1") (llm "0.6.0") (spinner "1.7.4"))
;; Version: 0.8.14
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
(require 'info)
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

(defcustom ellama-nick-prefix-depth 2
  "Prefix depth."
  :group 'ellama
  :type 'integer)

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

(defcustom ellama-chat-translation-enabled nil
  "Enable chat translations."
  :group 'ellama
  :type 'boolean)

(defcustom ellama-translation-provider nil
  "LLM provider for chat translation."
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

(defcustom ellama-command-map
  (let ((map (make-sparse-keymap)))
    ;; code
    (define-key map (kbd "c c") 'ellama-code-complete)
    (define-key map (kbd "c a") 'ellama-code-add)
    (define-key map (kbd "c e") 'ellama-code-edit)
    (define-key map (kbd "c i") 'ellama-code-improve)
    (define-key map (kbd "c r") 'ellama-code-review)
    ;; summarize
    (define-key map (kbd "s s") 'ellama-summarize)
    (define-key map (kbd "s w") 'ellama-summarize-webpage)
    ;; session
    (define-key map (kbd "s l") 'ellama-load-session)
    (define-key map (kbd "s r") 'ellama-session-rename)
    (define-key map (kbd "s d") 'ellama-session-remove)
    (define-key map (kbd "s a") 'ellama-session-switch)
    ;; improve
    (define-key map (kbd "i w") 'ellama-improve-wording)
    (define-key map (kbd "i g") 'ellama-improve-grammar)
    (define-key map (kbd "i c") 'ellama-improve-conciseness)
    ;; make
    (define-key map (kbd "m l") 'ellama-make-list)
    (define-key map (kbd "m t") 'ellama-make-table)
    (define-key map (kbd "m f") 'ellama-make-format)
    ;; ask
    (define-key map (kbd "a a") 'ellama-ask-about)
    (define-key map (kbd "a i") 'ellama-chat)
    (define-key map (kbd "a l") 'ellama-ask-line)
    (define-key map (kbd "a s") 'ellama-ask-selection)
    ;; text
    (define-key map (kbd "t t") 'ellama-translate)
    (define-key map (kbd "t b") 'ellama-translate-buffer)
    (define-key map (kbd "t c") 'ellama-complete)
    (define-key map (kbd "t e") 'ellama-chat-translation-enable)
    (define-key map (kbd "t d") 'ellama-chat-translation-disable)
    ;; define
    (define-key map (kbd "d w") 'ellama-define-word)
    ;; context
    (define-key map (kbd "x b") 'ellama-context-add-buffer)
    (define-key map (kbd "x f") 'ellama-context-add-file)
    (define-key map (kbd "x s") 'ellama-context-add-selection)
    (define-key map (kbd "x i") 'ellama-context-add-info-node)
    ;; provider
    (define-key map (kbd "p s") 'ellama-provider-select)
    map)
  "Keymap for ellama commands."
  :group 'ellama
  :type 'keymap)

(defun ellama-setup-keymap ()
  "Set up the Ellama keymap and bindings."
  (interactive)
  (when (boundp 'ellama-keymap-prefix)
    (defvar ellama-keymap (make-sparse-keymap)
      "Keymap for Ellama Commands")

    (when ellama-keymap-prefix
      (define-key global-map (kbd ellama-keymap-prefix) ellama-command-map))))

(defcustom ellama-keymap-prefix nil
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

(defcustom ellama-naming-scheme 'ellama-generate-name-by-words
  "How to name sessions.
If you choose custom function, that function should accept PROVIDER, ACTION
and PROMPT arguments.

PROVIDER is an llm provider.

ACTION is a symbol, current command.

PROMPT is a prompt string."
  :group 'ellama
  :type `(choice
          (const :tag "By first N words of prompt" ellama-generate-name-by-words)
          (const :tag "By current time" ellama-generate-name-by-time)
	  (const :tag "By generating name with LLM based on prompt." ellama-generate-name-by-llm)
          (function :tag "By custom function")))

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

(defcustom ellama-get-name-template "I will get you user query, you should return short topic only, what this conversation about. NEVER respond to query itself. Topic must be short and concise.
For example:
Query: Why is sky blue?
Topic: Blue sky

Query: %s
Topic:"
  "Prompt template for `ellama-get-name'."
  :group 'ellama
  :type 'string)

(defcustom ellama-translation-template "Translate this text to %s.
Original text:
%s
Translation to %s:
"
  "Translation template."
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
       (zero-or-more anything) (literal "```") (zero-or-more anything) (+ (or "\n" "\r")))))

(defconst ellama--code-suffix
  (rx (minimal-match
       (literal "```") (zero-or-more anything))))

(defun ellama--code-filter (text)
  "Filter code prefix/suffix from TEXT."
  ;; Trim left first as `string-trim' trims from the right and ends up deleting all the code.
  (string-trim-right (string-trim-left text ellama--code-prefix) ellama--code-suffix))

(defun ellama--fill-long-lines (text)
  "Fill long lines only in TEXT."
  (with-temp-buffer
    (insert (propertize text 'hard t))
    (let ((fill-column ellama-long-lines-length)
	  (use-hard-newlines t))
      (fill-region (point-min) (point-max) nil t t))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun ellama--translate-markdown-to-org-filter (text)
  "Filter to translate code blocks from markdown syntax to org syntax in TEXT.
This filter contains only subset of markdown syntax to be good enough."
  (thread-last text
       ;; code blocks
       (replace-regexp-in-string "^[[:space:]]*```\\(.+\\)$" "#+BEGIN_SRC \\1")
       (replace-regexp-in-string "^<!-- language: \\(.+\\) -->\n```" "#+BEGIN_SRC \\1")
       (replace-regexp-in-string "^[[:space:]]*```$" "#+END_SRC")
       ;; lists
       (replace-regexp-in-string "^\\* " "+ ")
       ;; bold
       (replace-regexp-in-string "\\*\\*\\(.+?\\)\\*\\*" "*\\1*")
       (replace-regexp-in-string "__\\(.+?\\)__" "*\\1*")
       (replace-regexp-in-string "<b>\\(.+?\\)</b>" "*\\1*")
       ;; italic
       ;; (replace-regexp-in-string "_\\(.+?\\)_" "/\\1/") ;; most of the time it breaks code blocks, so disable it
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

(defcustom ellama-sessions-directory (file-truename
				      (file-name-concat
				       user-emacs-directory
				       "ellama-sessions"))
  "Directory for saved ellama sessions."
  :type 'string
  :group 'ellama)

(defcustom ellama-naming-provider nil
  "LLM provider for generating names."
  :group 'ellama
  :type '(sexp :validate 'cl-struct-p))

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

(defun ellama-generate-name-by-words (provider action prompt)
  "Generate name for ACTION by PROVIDER by getting first N words from PROMPT."
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

(defun ellama-get-name (prompt)
  "Generate session name by LLM based on PROMPT."
  (let ((provider (or ellama-naming-provider ellama-provider)))
    (string-trim-right
     (string-trim
      (seq-first
       (split-string
	(llm-chat provider (llm-make-simple-chat-prompt
			    (format ellama-get-name-template prompt)))
	"\n")))
     "\\.")))

(defun ellama-generate-name-by-llm (provider _action prompt)
  "Generate name for ellama ACTION by PROVIDER and PROMPT by LLM."
  (format "%s (%s)"
	  (ellama-get-name prompt)
	  (llm-name provider)))

(defun ellama-get-current-time ()
  "Return string representation of current time."
  (replace-regexp-in-string
   "\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\'" "\\1:\\2"
   (format-time-string "%FT%T%z" (current-time))))

(defun ellama-generate-name-by-time (_provider _action _prompt)
  "Generate name for ellama session by current time."
  (ellama-get-current-time))

(defun ellama-generate-name (provider action prompt)
  "Generate name for ellama ACTION by PROVIDER according to PROMPT."
  (replace-regexp-in-string "/" "_" (funcall ellama-naming-scheme provider action prompt)))

(defvar ellama--new-session-context nil)

(defun ellama-get-nick-prefix-for-mode ()
  "Return preferred header prefix char based om the current mode.
Defaults to #, but supports `org-mode'.  Depends on `ellama-major-mode'."
  (let* ((prefix-char
          (cond ((provided-mode-derived-p ellama-major-mode 'org-mode) ?*)
                (t ?#))))
    (make-string ellama-nick-prefix-depth prefix-char)))

(defun ellama-get-session-file-extension ()
  "Return file extension based om the current mode.
Defaults to md, but supports org.  Depends on \"ellama-major-mode.\""
  (cond ((provided-mode-derived-p ellama-major-mode 'org-mode) "org")
        (t "md")))

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
		       (concat id "." (ellama-get-session-file-extension)))))
	 (session (make-ellama-session
		   :id id :provider provider :file file-name :context ellama--new-session-context))
	 (buffer (if file-name
		     (progn
		       (make-directory ellama-sessions-directory t)
		       (find-file-noselect file-name))
		   (get-buffer-create id))))
    (setq ellama--new-session-context nil)
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

(defun ellama--get-translation-file-name (file-name)
  "Get ellama translation file name for FILE-NAME."
  (let* ((base-name (file-name-base file-name))
	 (ext (file-name-extension file-name))
	 (dir (file-name-directory file-name))
	 (translation-file-name
	  (file-name-concat
	   dir
	   (concat base-name ".translation"
		   (when ext
		     (concat "." ext))))))
    translation-file-name))

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
      (let ((session (read session-buffer)))
	(setq ellama--current-session
	      (make-ellama-session
	       :id (ellama-session-id session)
	       :provider (ellama-session-provider session)
	       :file (ellama-session-file session)
	       :prompt (ellama-session-prompt session)
	       :context ellama--new-session-context)))
      (setq ellama--new-session-context nil)
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
	 (session-file (ellama--get-session-file-name file))
	 (translation-file (ellama--get-translation-file-name file)))
    (kill-buffer buffer)
    (delete-file file t)
    (delete-file session-file t)
    (mapc
     (lambda (buf)
       (when (and (buffer-file-name buf)
		  (file-equal-p (buffer-file-name buf)
				translation-file))
	 (kill-buffer buf)))
     (buffer-list))
    (when (file-exists-p translation-file)
      (delete-file translation-file t))))

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
  "Add file to context."
  (interactive)
  (if-let* ((id ellama--current-session-id)
	    (session (with-current-buffer (ellama-get-session-buffer id)
		       ellama--current-session))
	    (file-name (read-file-name "Select file: " nil nil t)))
      (push (cons 'file file-name) (ellama-session-context session))
    (push (cons 'file file-name) ellama--new-session-context)))

;;;###autoload
(defun ellama-context-add-buffer (buf)
  "Add BUF to context."
  (interactive "bSelect buffer: ")
  (if-let* ((id ellama--current-session-id)
	    (session (with-current-buffer (ellama-get-session-buffer id)
		       ellama--current-session)))
      (push (cons 'buffer buf) (ellama-session-context session))
    (push (cons 'buffer buf) ellama--new-session-context)))

;;;###autoload
(defun ellama-context-add-selection ()
  "Add file to context."
  (interactive)
  (if-let* ((id ellama--current-session-id)
	    (session (with-current-buffer (ellama-get-session-buffer id)
		       ellama--current-session))
	    ((region-active-p))
	    (content (buffer-substring-no-properties (region-beginning) (region-end))))
      (push (cons 'text content) (ellama-session-context session))
    (push (cons 'text content) ellama--new-session-context)))

;;;###autoload
(defun ellama-context-add-info-node (node)
  "Add info NODE to context."
  (interactive (list (Info-copy-current-node-name)))
  (if-let* ((id ellama--current-session-id)
	    (session (with-current-buffer (ellama-get-session-buffer id)
		       ellama--current-session)))
      (push (cons 'info node) (ellama-session-context session))
    (push (cons 'info node) ellama--new-session-context)))

(defun ellama--translate-string (s)
  "Translate string S to `ellama-language' syncronously."
  (llm-chat
   (or ellama-translation-provider ellama-provider)
   (llm-make-simple-chat-prompt
    (format ellama-translation-template
	    ellama-language
	    s
	    ellama-language))))

(defun ellama--org-format-context-element (elt)
  "Format context ELT for org mode."
  (pcase (car elt)
    ('file
     (format "[[file:%s][%s]]" (cdr elt) (cdr elt)))
    ('buffer
     (format "[[elisp:(display-buffer \"%s\")][%s]]" (cdr elt) (cdr elt)))
    ('text
     (cdr elt))
    ('info
     (format "[[%s][%s]]"
	     (replace-regexp-in-string
	      "(\\(.?*\\)) \\(.*\\)" "info:\\1#\\2" (cdr elt))
	     (if (and ellama-chat-translation-enabled
		      (not ellama--current-session))
		 (ellama--translate-string (cdr elt))
	       (cdr elt))))
    (_
     (user-error "Unsupported context element"))))

(defun ellama--md-format-context-element (elt)
  "Format context ELT for org mode."
  (pcase (car elt)
    ('file
     (format "[%s](<%s>)"
	     (cdr elt)
	     (cdr elt)))
    ('buffer
     (format "```emacs-lisp\n(display-buffer \"%s\")\n```\n" (cdr elt)))
    ('text
     (cdr elt))
    ('info
     (format "```emacs-lisp\n(info \"%s\")\n```\n" (cdr elt)))
    (_
     (user-error "Unsupported context element"))))

(defun ellama--extract-context-element (elt)
  "Extract context ELT content."
  (pcase (car elt)
    ('text (cdr elt))
    ('buffer (with-current-buffer (cdr elt)
	       (buffer-substring-no-properties (point-min) (point-max))))
    ('file (with-temp-buffer
	     (find-file-literally (cdr elt))
	     (buffer-substring-no-properties (point-min) (point-max))))
    ('info (with-temp-buffer
	     (info (cdr elt) (current-buffer))
	     (buffer-substring-no-properties (point-min) (point-max))))
    (_
     (user-error "Unsupported context element"))))

(defun ellama--format-context (session)
  "Format SESSION context for chat buffer."
  (if-let* ((context (ellama-session-context session)))
      (concat (string-join
	       (cons "Context:"
		     (if (derived-mode-p 'org-mode)
			 (mapcar #'ellama--org-format-context-element context)
		       (mapcar #'ellama--md-format-context-element context)))
	       "\n")
	      "\n\n")
    ""))

(defun ellama--prompt-with-context (prompt)
  "Add context to PROMPT for sending to llm."
  (if-let* ((session ellama--current-session)
	    (context (ellama-session-context session)))
      (concat (string-join
	       (cons "Context:"
		     (mapcar #'ellama--extract-context-element context))
	       "\n")
	      "\n\n"
	      prompt)
    prompt))

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
  (let* ((session (plist-get args :session))
	 (provider (if session
		       (ellama-session-provider session)
		     (or (plist-get args :provider) ellama-provider)))
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
	 (prompt-with-ctx (ellama--prompt-with-context prompt))
	 (llm-prompt (if session
			 (if (llm-chat-prompt-p (ellama-session-prompt session))
			     (progn
			       (llm-chat-prompt-append-response
				(ellama-session-prompt session)
				prompt-with-ctx)
			       (ellama-session-prompt session))
			   (setf (ellama-session-prompt session)
				 (llm-make-simple-chat-prompt prompt-with-ctx)))
		       (llm-make-simple-chat-prompt prompt-with-ctx))))
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
	(when session
	  (setf (ellama-session-context session) nil))
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

(defun ellama--translate-generated-text-on-done (translation-buffer)
  "Translate generated text into TRANSLATION-BUFFER."
  (lambda (generated)
    (ellama-chat-done generated)
    (display-buffer translation-buffer)
    (with-current-buffer translation-buffer
      (save-excursion
	(goto-char (point-max))
	(ellama-stream
	 (format ellama-translation-template
		 ellama-language
		 generated
		 ellama-language)
	 :provider (or ellama-translation-provider ellama-provider)
	 :on-done #'ellama-chat-done
	 :filter (when (derived-mode-p 'org-mode)
		   #'ellama--translate-markdown-to-org-filter))))))

(defun ellama--call-llm-with-translated-prompt (buffer session translation-buffer)
  "Call llm with translated text in BUFFER with SESSION from TRANSLATION-BUFFER."
  (lambda (result)
    (ellama-chat-done result)
    (save-excursion
      (goto-char (point-max))
      (delete-char -2)
      (delete-char (- (length result))))
    (display-buffer buffer)
    (with-current-buffer buffer
      (save-excursion
	(goto-char (point-max))
	(insert (ellama-get-nick-prefix-for-mode) " " ellama-user-nick ":\n"
		(ellama--format-context session) result "\n\n"
		(ellama-get-nick-prefix-for-mode) " " ellama-assistant-nick ":\n")
	(ellama-stream result
		       :session session
		       :on-done (ellama--translate-generated-text-on-done translation-buffer)
		       :filter (when (derived-mode-p 'org-mode)
				 #'ellama--translate-markdown-to-org-filter))))))

(defun ellama--translate-interaction (prompt translation-buffer buffer session)
  "Translate chat PROMPT in TRANSLATION-BUFFER for BUFFER with SESSION."
  (display-buffer translation-buffer)
  (with-current-buffer translation-buffer
    (save-excursion
      (goto-char (point-max))
      (insert (ellama-get-nick-prefix-for-mode) " " ellama-user-nick ":\n"
	      (ellama--format-context session) prompt "\n\n"
	      (ellama-get-nick-prefix-for-mode) " " ellama-assistant-nick ":\n")
      (ellama-stream
       (format ellama-translation-template
	       "english"
	       prompt
	       "english")
       :provider (or ellama-translation-provider ellama-provider)
       :filter (when (derived-mode-p 'org-mode)
		 #'ellama--translate-markdown-to-org-filter)
       :on-done
       (ellama--call-llm-with-translated-prompt buffer session translation-buffer)))))

;;;###autoload
(defun ellama-chat (prompt &optional create-session &rest args)
  "Send PROMPT to ellama chat with conversation history.

If CREATE-SESSION set, creates new session even if there is an active session.
ARGS contains keys for fine control.

:provider PROVIDER -- PROVIDER is an llm provider for generation."
  (interactive "sAsk ellama: ")
  (let* ((providers (append
                     `(("default model" . ellama-provider)
		               ,(if (and ellama-ollama-binary (file-exists-p ellama-ollama-binary))
			                '("ollama model" . (ellama-get-ollama-local-model))))
                     ellama-providers))
	     (variants (mapcar #'car providers))
	     (provider (if current-prefix-arg
		               (eval (alist-get
			                  (completing-read "Select model: " variants)
			                  providers nil nil #'string=))
		             (or (plist-get args :provider)
			             ellama-provider)))
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
		          (ellama-session-id session)))
	     (file-name (ellama-session-file session))
	     (translation-buffer (when ellama-chat-translation-enabled
			                   (if file-name
				                   (progn
				                     (find-file-noselect
				                      (ellama--get-translation-file-name file-name)))
				                 (get-buffer-create (ellama-session-id session))))))
    (if ellama-chat-translation-enabled
	(ellama--translate-interaction prompt translation-buffer buffer session)
      (display-buffer buffer)
      (with-current-buffer buffer
	(save-excursion
	  (goto-char (point-max))
	  (insert (ellama-get-nick-prefix-for-mode) " " ellama-user-nick ":\n"
		  (ellama--format-context session) prompt "\n\n"
		  (ellama-get-nick-prefix-for-mode) " " ellama-assistant-nick ":\n")
	  (ellama-stream prompt
			 :session session
			 :on-done #'ellama-chat-done
			 :filter (when (derived-mode-p 'org-mode)
				   #'ellama--translate-markdown-to-org-filter)))))))

;;;###autoload
(defun ellama-ask-about ()
  "Ask ellama about selected region or current buffer."
  (interactive)
  (let ((input (read-string "Ask ellama about this text: ")))
    (if (region-active-p)
	(ellama-context-add-selection)
      (ellama-context-add-buffer (buffer-name (current-buffer))))
    (ellama-chat input)))

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

(defun ellama-instant (prompt &rest args)
  "Prompt ellama for PROMPT to reply instantly.

ARGS contains keys for fine control.

:provider PROVIDER -- PROVIDER is an llm provider for generation."
  (let* ((provider (or (plist-get args :provider)
		       ellama-provider))
	 (buffer-name (ellama-generate-name provider real-this-command prompt))
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
		   :filter filter
		   :provider provider)))

;;;###autoload
(defun ellama-translate ()
  "Ask ellama to translate selected region or word at point."
  (interactive)
  (let ((text (if (region-active-p)
		  (buffer-substring-no-properties (region-beginning) (region-end))
		(thing-at-point 'word))))
    (ellama-instant
     (format ellama-translation-template
	     ellama-language text ellama-language)
     :provider ellama-translation-provider)))

;;;###autoload
(defun ellama-translate-buffer ()
  "Ask ellama to translate current buffer."
  (interactive)
  (let ((text (buffer-substring-no-properties (point-min) (point-max))))
    (ellama-instant
     (format ellama-translation-template
	     ellama-language text ellama-language)
     :provider ellama-translation-provider)))

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
    (if host
	(make-llm-ollama
	 :chat-model model-name :embedding-model model-name :host host :port port)
      (make-llm-ollama
       :chat-model model-name :embedding-model model-name))))

;;;###autoload
(defun ellama-provider-select ()
  "Select ellama provider."
  (interactive)
  (let* ((providers (append
                     `(("default model" . ellama-provider)
		               ,(if (and ellama-ollama-binary (file-exists-p ellama-ollama-binary))
			                '("ollama model" . (ellama-get-ollama-local-model))))
                     ellama-providers))
	     (variants (mapcar #'car providers)))
    (setq ellama-provider
	  (eval (alist-get
		 (completing-read "Select model: " variants)
		 providers nil nil #'string=)))
    (setq ellama--current-session-id nil)))

;;;###autoload
(defun ellama-chat-translation-enable ()
  "Enable chat translation."
  (interactive)
  (setq ellama-chat-translation-enabled t))

;;;###autoload
(defun ellama-chat-translation-disable ()
  "Enable chat translation."
  (interactive)
  (setq ellama-chat-translation-enabled nil))

(provide 'ellama)
;;; ellama.el ends here.
