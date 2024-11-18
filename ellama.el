;;; ellama.el --- Tool for interacting with LLMs -*- lexical-binding: t -*-

;; Copyright (C) 2023, 2024  Free Software Foundation, Inc.

;; Author: Sergey Kostyaev <sskostyaev@gmail.com>
;; URL: http://github.com/s-kostyaev/ellama
;; Keywords: help local tools
;; Package-Requires: ((emacs "28.1") (llm "0.6.0") (spinner "1.7.4") (transient "0.7.6") (compat "29.1"))
;; Version: 0.12.4
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

(require 'eieio)
(require 'json)
(require 'llm)
(require 'llm-provider-utils)
(require 'spinner)
(require 'transient)
(require 'info)
(require 'shr)
(require 'eww)
(require 'vc)
(require 'compat)
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
  :type '(sexp :validate 'llm-standard-provider-p))

(defcustom ellama-chat-translation-enabled nil
  "Enable chat translations."
  :group 'ellama
  :type 'boolean)

(defcustom ellama-translation-provider nil
  "LLM provider for chat translation."
  :group 'ellama
  :type '(sexp :validate 'llm-standard-provider-p))

(defcustom ellama-summarization-provider nil
  "LLM provider for summarization."
  :group 'ellama
  :type '(sexp :validate 'llm-standard-provider-p))

(defcustom ellama-coding-provider nil
  "LLM provider for coding tasks."
  :group 'ellama
  :type '(sexp :validate 'llm-standard-provider-p))

(defcustom ellama-providers nil
  "LLM provider list for fast switching."
  :group 'ellama
  :type '(alist :key-type string
		:value-type (sexp :validate 'llm-standard-provider-p)))

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
    (define-key map (kbd "c m") 'ellama-generate-commit-message)
    ;; summarize
    (define-key map (kbd "s s") 'ellama-summarize)
    (define-key map (kbd "s w") 'ellama-summarize-webpage)
    (define-key map (kbd "s c") 'ellama-summarize-killring)
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

(defcustom ellama-ollama-binary "ollama"
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

(defcustom ellama-summarize-prompt-template "<INSTRUCTIONS>
You are a summarizer. You write a summary of the input **IN THE SAME LANGUAGE AS ORIGINAL INPUT TEXT** using following steps:
1.) Analyze the input text and generate 5 essential questions that, when answered, capture the main points and core meaning of the text.
2.) When formulating your questions:
 a. Address the central theme or argument
 b. Identify key supporting ideas
 c. Highlight important facts or evidence
 d. Reveal the author's purpose or perspective
 e. Explore any significant implications or conclusions.
3.) Answer all of your generated questions one-by-one in detail.
</INSTRUCTIONS>
<INPUT>
%s
</INPUT>"
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

(defcustom ellama-code-edit-prompt-template "Regarding the following code, %s, only output the result code in format ```language\n...\n```:\n```\n%s\n```\nWrite all the code in single code block."
  "Prompt template for `ellama-code-edit'."
  :group 'ellama
  :type 'string)

(defcustom ellama-code-improve-prompt-template "Enhance the following code, only output the result code in format ```language\n...\n```:\n```\n%s\n```\nWrite all the code in single code block."
  "Prompt template for `ellama-code-improve'."
  :group 'ellama
  :type 'string)

(defcustom ellama-code-complete-prompt-template "Continue the following code, only write new code in format ```language\n...\n```:\n```\n%s\n```\nWrite all the code in single code block."
  "Prompt template for `ellama-code-complete'."
  :group 'ellama
  :type 'string)

(defcustom ellama-code-add-prompt-template "Context: \n```\n%s\n```\nBased on this context, %s, only output the result in format ```\n...\n```\nWrite all the code in single code block."
  "Prompt template for `ellama-code-add'."
  :group 'ellama
  :type 'string)

(defcustom ellama-generate-commit-message-template "<INSTRUCTIONS>
You are professional software developer.

Write concise commit message based on diff in the following format:
<FORMAT>
First line should contain short title described major change in functionality.
Then one empty line. Then detailed description of all changes.
</FORMAT>
<EXAMPLE>
Improve abc

Improved abc feature by adding new xyz module.
</EXAMPLE>

**Reply with commit message only without any quotes.**
</INSTRUCTIONS>

<DIFF>
%s
</DIFF>"
  "Prompt template for `ellama-generate-commit-message'."
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
<example>
Query: Why is sky blue?
Topic: Blue sky
</example>
<query>
%s
</query>
Topic:
"
  "Prompt template for `ellama-get-name'."
  :group 'ellama
  :type 'string)

(defcustom ellama-translation-template "<INSTRUCTIONS>
You are expert text translator. Translate input text to %s. Do
not explain what you are doing. Do not self reference. You are an
expert translator that will be tasked with translating and
improving the spelling/grammar/literary quality of a piece of
text. Please rewrite the translated text in your tone of voice
and writing style. Ensure that the meaning of the original text
is not changed.
</INSTRUCTIONS>
<INPUT>
%s
</INPUT>"
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

(defcustom ellama-show-quotes nil
  "Show quotes in chat context."
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

(defun ellama--replace-first-begin-src (text)
  "Replace first begin src in TEXT."
  (if (not (string-match-p (rx (literal "#+BEGIN_SRC")) text))
      (replace-regexp-in-string "^[[:space:]]*```\\(\\(.\\|\n\\)*\\)" "#+BEGIN_SRC\\1" text)
    text))

(defun ellama--replace-bad-code-blocks (text)
  "Replace code src blocks in TEXT."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    ;; skip good code blocks
    (while (re-search-forward "#\\+BEGIN_SRC\\(.\\|\n\\)*?#\\+END_SRC" nil t))
    (while (re-search-forward "#\\+END_SRC\\(\\(.\\|\n\\)*?\\)#\\+END_SRC" nil t)
      (replace-match "#+BEGIN_SRC\\1#+END_SRC"))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun ellama--replace (from to beg end)
  "Replace FROM to TO in region BEG END."
  (goto-char beg)
  (while (re-search-forward from end t)
    (replace-match to)))

(defun ellama--apply-transformations (beg end)
  "Apply md to org transformations for region BEG END."
  ;; headings
  (ellama--replace "^# " "* " beg end)
  (ellama--replace "^## " "** " beg end)
  (ellama--replace "^### " "*** " beg end)
  (ellama--replace "^#### " "**** " beg end)
  (ellama--replace "^##### " "***** " beg end)
  (ellama--replace "^###### " "****** " beg end)
  ;; bold
  (ellama--replace "__\\(.+?\\)__" "*\\1*" beg end)
  (ellama--replace "\\*\\*\\(.+?\\)\\*\\*" "*\\1*" beg end)
  (ellama--replace "<b>\\(.+?\\)</b>" "*\\1*" beg end)
  ;; italic
  (ellama--replace "_\\(.+?\\)_" "/\\1/" beg end)
  (ellama--replace "<i>\\(.+?\\)</i>" "/\\1/" beg end)
  ;; underlined
  (ellama--replace "<u>\\(.+?\\)</u>" "_\\1_" beg end)
  ;; inline code
  (ellama--replace "`\\(.+?\\)`" "~\\1~" beg end)
  ;; lists
  (ellama--replace "^\\* " "+ " beg end)
  ;; strikethrough
  (ellama--replace "~~\\(.+?\\)~~" "+\\1+" beg end)
  (ellama--replace "<s>\\(.+?\\)</s>" "+\\1+" beg end)
  ;; badges
  (ellama--replace "\\[\\!\\[.*?\\](\\(.*?\\))\\](\\(.*?\\))" "[[\\2][file:\\1]]" beg end)
  ;;links
  (ellama--replace "\\[\\(.*?\\)\\](\\(.*?\\))" "[[\\2][\\1]]" beg end)

  ;; filling long lines
  (goto-char beg)
  (let ((fill-column ellama-long-lines-length)
	(use-hard-newlines t))
    (fill-region beg end nil t t)))

(defun ellama--replace-outside-of-code-blocks (text)
  "Replace some markdown elements to org in TEXT outside of code blocks."
  (with-temp-buffer
    (insert (propertize text 'hard t))
    (goto-char (point-min))
    ;; apply transformations outside of code blocks
    (let ((beg (point-min))
	  (end (or (re-search-forward "#\\+BEGIN_SRC" nil t)
		   (point-max))))
      (ellama--apply-transformations beg end)
      (goto-char beg)
      (re-search-forward "#\\+BEGIN_SRC" nil t)
      (while (when-let ((beg (re-search-forward "#\\+END_SRC" nil t))
			(end (or (re-search-forward "#\\+BEGIN_SRC" nil t)
				 (point-max))))
	       (ellama--apply-transformations beg end)
	       (goto-char beg))))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun ellama--translate-markdown-to-org-filter (text)
  "Filter to translate code blocks from markdown syntax to org syntax in TEXT.
This filter contains only subset of markdown syntax to be good enough."
  (thread-last
    text
    ;; code blocks
    (replace-regexp-in-string "^[[:space:]]*```\\(.+\\)$" "#+BEGIN_SRC \\1")
    (ellama--replace-first-begin-src)
    (replace-regexp-in-string "^<!-- language: \\(.+\\) -->\n```" "#+BEGIN_SRC \\1")
    (replace-regexp-in-string "^[[:space:]]*```$" "#+END_SRC")
    (replace-regexp-in-string "^[[:space:]]*```" "#+END_SRC\n")
    (replace-regexp-in-string "```" "\n#+END_SRC\n")
    (ellama--replace-bad-code-blocks)
    (ellama--replace-outside-of-code-blocks)))

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
  :type '(sexp :validate 'llm-standard-provider-p))

(defcustom ellama-always-show-chain-steps nil
  "Always show ellama chain buffers."
  :type 'boolean
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
 	 (previous-session
	  (when ellama--current-session-id
	    (with-current-buffer
		(ellama-get-session-buffer ellama--current-session-id)
	      ellama--current-session)))
	 (session (make-ellama-session
		   :id id :provider provider :file file-name
		   :context (if previous-session
				(ellama-session-context previous-session)
			      ellama--new-session-context)))
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

(defun ellama-activate-session (id)
  "Change current active session to session with ID."
  (setq ellama--current-session-id id))

;;;###autoload
(defun ellama-session-switch ()
  "Change current active session."
  (interactive)
  (let* ((id (completing-read
	      "Select session to activate: "
	      (hash-table-keys ellama--active-sessions)))
	 (buffer (ellama-get-session-buffer id)))
    (ellama-activate-session id)
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

;; Context elements

(defclass ellama-context-element () ()
  "A structure for holding information about a context element.")

(cl-defgeneric ellama-context-element-add (element)
  "Add the ELEMENT to the Ellama context.")

(cl-defgeneric ellama-context-element-extract (element)
  "Extract the content of the context ELEMENT.")

(cl-defgeneric ellama-context-element-format (element mode)
  "Format the context ELEMENT for the major MODE.")

(cl-defmethod ellama-context-element-add ((element ellama-context-element))
  "Add the ELEMENT to the Ellama context."
  (if-let* ((id ellama--current-session-id)
	    (session (with-current-buffer (ellama-get-session-buffer id)
		       ellama--current-session)))
      (push element (ellama-session-context session))
    (push element ellama--new-session-context)))

;; Buffer context element

(defclass ellama-context-element-buffer (ellama-context-element)
  ((name :initarg :name :type string))
  "A structure for holding information about a context element.")

(cl-defmethod ellama-context-element-extract
  ((element ellama-context-element-buffer))
  "Extract the content of the context ELEMENT."
  (with-slots (name) element
    (with-current-buffer name
      (buffer-substring-no-properties (point-min) (point-max)))))

(cl-defmethod ellama-context-element-format
  ((element ellama-context-element-buffer) (mode (eql 'markdown-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (with-slots (name) element
    (format "```emacs-lisp\n(display-buffer \"%s\")\n```\n" name)))

(cl-defmethod ellama-context-element-format
  ((element ellama-context-element-buffer) (mode (eql 'org-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (with-slots (name) element
    (format "[[elisp:(display-buffer \"%s\")][%s]]" name name)))

;; File context element

(defclass ellama-context-element-file (ellama-context-element)
  ((name :initarg :name :type string))
  "A structure for holding information about a context element.")

(cl-defmethod ellama-context-element-extract
  ((element ellama-context-element-file))
  "Extract the content of the context ELEMENT."
  (with-slots (name) element
    (with-temp-buffer
      (insert-file-contents name)
      (buffer-substring-no-properties (point-min) (point-max)))))

(cl-defmethod ellama-context-element-format
  ((element ellama-context-element-file) (mode (eql 'markdown-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (with-slots (name) element
    (format "[%s](<%s>)" name name)))

(cl-defmethod ellama-context-element-format
  ((element ellama-context-element-file) (mode (eql 'org-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (with-slots (name) element
    (format "[[file:%s][%s]]" name name)))

;; Info node context element

(defclass ellama-context-element-info-node (ellama-context-element)
  ((name :initarg :name :type string))
  "A structure for holding information about a context element.")

(cl-defmethod ellama-context-element-extract
  ((element ellama-context-element-info-node))
  "Extract the content of the context ELEMENT."
  (with-slots (name) element
    (with-temp-buffer
      (info name (current-buffer))
      (buffer-substring-no-properties (point-min) (point-max)))))

(cl-defmethod ellama-context-element-format
  ((element ellama-context-element-info-node) (mode (eql 'markdown-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (with-slots (name) element
    (format "```emacs-lisp\n(info \"%s\")\n```\n" name)))

(cl-defmethod ellama-context-element-format
  ((element ellama-context-element-info-node) (mode (eql 'org-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (with-slots (name) element
    (format "[[%s][%s]]"
	    (replace-regexp-in-string
	     "(\\(.?*\\)) \\(.*\\)" "info:\\1#\\2" name)
	    (if (and ellama-chat-translation-enabled
		     (not ellama--current-session))
		(ellama--translate-string name)
	      name))))

;; Text context element

(defclass ellama-context-element-text (ellama-context-element)
  ((content :initarg :content :type string))
  "A structure for holding information about a context element.")

(cl-defmethod ellama-context-element-extract
  ((element ellama-context-element-text))
  "Extract the content of the context ELEMENT."
  (oref element content))

(cl-defmethod ellama-context-element-format
  ((element ellama-context-element-text) (mode (eql 'markdown-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (oref element content))

(cl-defmethod ellama-context-element-format
  ((element ellama-context-element-text) (mode (eql 'org-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (oref element content))

;; Webpage quote context elements

(defclass ellama-context-element-webpage-quote (ellama-context-element)
  ((name :initarg :name :type string)
   (url :initarg :url :type string)
   (content :initarg :content :type string))
  "A structure for holding information about a context element.")

(cl-defmethod ellama-context-element-extract
  ((element ellama-context-element-webpage-quote))
  "Extract the content of the context ELEMENT."
  (oref element content))

(defun ellama--quote-buffer (quote)
  "Return buffer name for QUOTE."
  (let* ((buf-name (concat (make-temp-name "*ellama-quote-") "*"))
	 (buf (get-buffer-create buf-name t)))
    (with-current-buffer buf
      (with-silent-modifications
	(insert quote)))
    buf-name))

(cl-defmethod ellama-context-element-format
  ((element ellama-context-element-webpage-quote) (mode (eql 'markdown-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (with-slots (name url content) element
    (if ellama-show-quotes
	(format "[%s](%s):\n%s\n\n"
		name url
		(ellama--md-quote content))
      (format
       "[%s](%s):\n```emacs-lisp\n(display-buffer \"%s\")\n```\n"
       name url (ellama--quote-buffer content)))))

(defun ellama--md-quote (content)
  "Return quoted CONTENT for markdown."
  (with-temp-buffer
    (insert (propertize content 'hard t))
    (let ((fill-prefix "> ")
	  (fill-column ellama-long-lines-length)
	  (use-hard-newlines t)
	  (comment-start ">")
	  (comment-empty-lines t))
      (comment-region (point-min) (point-max) ">")
      (fill-region (point-min) (point-max) nil t t))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun ellama--org-quote (content)
  "Return transformed CONTENT for org quotes."
  (replace-regexp-in-string "^*" " *" content))

(cl-defmethod ellama-context-element-format
  ((element ellama-context-element-webpage-quote) (mode (eql 'org-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (with-slots (name url content) element
    (if ellama-show-quotes
	(format "[[%s][%s]]:\n#+BEGIN_QUOTE\n%s\n#+END_QUOTE\n"
		url name (ellama--org-quote content))
      (format "[[%s][%s]] [[elisp:(display-buffer \"%s\")][show]]"
	      url name (ellama--quote-buffer content)))))

;; Info node quote context elements

(defclass ellama-context-element-info-node-quote (ellama-context-element)
  ((name :initarg :name :type string)
   (content :initarg :content :type string))
  "A structure for holding information about a context element.")

(cl-defmethod ellama-context-element-extract
  ((element ellama-context-element-info-node-quote))
  "Extract the content of the context ELEMENT."
  (oref element content))

(cl-defmethod ellama-context-element-format
  ((element ellama-context-element-info-node-quote) (mode (eql 'markdown-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (with-slots (name content) element
    (if ellama-show-quotes
	(format "```emacs-lisp\n(info \"%s\")\n```\n%s\n\n"
		name
		(ellama--md-quote content))
      (format "```emacs-lisp\n(info \"%s\")\n```\nshow:\n```emacs-lisp\n(display-buffer \"%s\")\n```\n" name (ellama--quote-buffer content)))))

(cl-defmethod ellama-context-element-format
  ((element ellama-context-element-info-node-quote) (mode (eql 'org-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (with-slots (name content) element
    (if ellama-show-quotes
	(format "[[%s][%s]]:\n#+BEGIN_QUOTE\n%s\n#+END_QUOTE\n"
		(replace-regexp-in-string
		 "(\\(.?*\\)) \\(.*\\)" "info:\\1#\\2" name)
		(if (and ellama-chat-translation-enabled
			 (not ellama--current-session))
		    (ellama--translate-string name)
		  name)
		(ellama--org-quote content))
      (format "[[%s][%s]] [[elisp:(display-buffer \"%s\")][show]]"
	      (replace-regexp-in-string
	       "(\\(.?*\\)) \\(.*\\)" "info:\\1#\\2" name)
	      (if (and ellama-chat-translation-enabled
		       (not ellama--current-session))
		  (ellama--translate-string name)
		name)
	      (ellama--quote-buffer content)))))

;; File quote context elements

(defclass ellama-context-element-file-quote (ellama-context-element)
  ((path :initarg :path :type string)
   (content :initarg :content :type string))
  "A structure for holding information about a context element.")

(cl-defmethod ellama-context-element-extract
  ((element ellama-context-element-file-quote))
  "Extract the content of the context ELEMENT."
  (oref element content))

(cl-defmethod ellama-context-element-format
  ((element ellama-context-element-file-quote) (mode (eql 'markdown-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (with-slots (path content) element
    (if ellama-show-quotes
	(format "[%s](%s):\n%s\n\n"
		path path
		(ellama--md-quote content))
      (format "[%s](%s):\n```emacs-lisp\n(display-buffer \"%s\")"
	      path path (ellama--quote-buffer content)))))

(cl-defmethod ellama-context-element-format
  ((element ellama-context-element-file-quote) (mode (eql 'org-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (with-slots (path content) element
    (if ellama-show-quotes
	(format "[[%s][%s]]:\n#+BEGIN_QUOTE\n%s\n#+END_QUOTE\n"
		path path (ellama--org-quote content))
      (format "[[%s][%s]] [[elisp:(display-buffer \"%s\")][show]]"
	      path path (ellama--quote-buffer content)))))


;;;###autoload
(defun ellama-context-add-file ()
  "Add file to context."
  (interactive)
  (let* ((file-name (read-file-name "Select file: " nil nil t))
         (element (ellama-context-element-file :name file-name)))
    (ellama-context-element-add element)))

(defun ellama-context-add-file-quote-noninteractive (path content)
  "Add file with PATH quote CONTENT to context."
  (let ((element (ellama-context-element-file-quote
		  :path path :content content)))
    (ellama-context-element-add element)))

;;;###autoload
(defun ellama-context-add-file-quote ()
  "Add file quote to context interactively."
  (interactive)
  (let ((path (buffer-file-name (current-buffer)))
	(content (if (region-active-p)
		     (buffer-substring-no-properties
		      (region-beginning)
		      (region-end))
		   (buffer-substring-no-properties
		    (point-min)
		    (point-max)))))
    (if (not path)
	(warn "should be called from buffer associated with file")
      (ellama-context-add-file-quote-noninteractive path content))))

;;;###autoload
(defun ellama-context-add-buffer (buf)
  "Add BUF to context."
  (interactive "bSelect buffer: ")
  (let ((element (ellama-context-element-buffer :name buf)))
    (ellama-context-element-add element)))

;;;###autoload
(defun ellama-context-add-selection ()
  "Add file to context."
  (interactive)
  (if (region-active-p)
      (let* ((content (buffer-substring-no-properties (region-beginning) (region-end)))
             (element (ellama-context-element-text :content content)))
        (ellama-context-element-add element))
    (warn "No active region")))

(defun ellama-context-add-text (text)
  "Add TEXT to context."
  (let ((element (ellama-context-element-text :content text)))
    (ellama-context-element-add element)))

;;;###autoload
(defun ellama-context-add-info-node (node)
  "Add info NODE to context."
  (interactive (list (Info-copy-current-node-name)))
  (let ((element (ellama-context-element-info-node :name node)))
    (ellama-context-element-add element)))

(defun ellama-context-add-info-node-quote-noninteractive (name content)
  "Add info node with NAME quote CONTENT to context."
  (let ((element (ellama-context-element-info-node-quote
		  :name name :content content)))
    (ellama-context-element-add element)))

;;;###autoload
(defun ellama-context-add-info-node-quote ()
  "Add info node quote to context interactively."
  (interactive)
  (let ((name (Info-copy-current-node-name))
	(content (if (region-active-p)
		     (buffer-substring-no-properties
		      (region-beginning)
		      (region-end))
		   (buffer-substring-no-properties
		    (point-min)
		    (point-max)))))
    (if (not name)
	(warn "should be called from `info' buffer")
      (ellama-context-add-info-node-quote-noninteractive name content))))

(defun ellama-context-add-webpage-quote-noninteractive (name url content)
  "Add webpage with NAME and URL quote CONTENT to context."
  (let ((element (ellama-context-element-webpage-quote
		  :name name :url url :content content)))
    (ellama-context-element-add element)))

;;;###autoload
(defun ellama-context-add-webpage-quote-eww ()
  "Add webpage quote to context interactively from `eww'."
  (interactive)
  (if (eq major-mode 'eww-mode)
      (let* ((name (plist-get eww-data :title))
	     (url (eww-current-url))
	     (content (if (region-active-p)
			  (buffer-substring-no-properties
			   (region-beginning)
			   (region-end))
			(buffer-substring-no-properties
			 (point-min)
			 (point-max)))))
	(ellama-context-add-webpage-quote-noninteractive name url content))
    (warn "Should be called from `eww'.")))

(defun ellama--translate-string (s)
  "Translate string S to `ellama-language' syncronously."
  (llm-chat
   (or ellama-translation-provider ellama-provider)
   (llm-make-simple-chat-prompt
    (format ellama-translation-template
	    ellama-language
	    s
	    ellama-language))))

(defun ellama--format-context (session)
  "Format SESSION context for chat buffer."
  (let ((mode (if (derived-mode-p 'org-mode) 'org-mode 'markdown-mode)))
    (if-let* ((context (ellama-session-context session)))
        (concat (string-join
	         (cons "Context:"
                       (mapcar (lambda (elt)
                                 (ellama-context-element-format elt mode))
                               context))
	         "\n")
	        "\n\n")
      "")))

(defun ellama--prompt-with-context (prompt)
  "Add context to PROMPT for sending to llm."
  (if-let* ((session ellama--current-session)
	    (context (ellama-session-context session)))
      (concat (string-join
	       (cons "Context:"
		     (mapcar #'ellama-context-element-extract context))
	       "\n")
	      "\n\n"
	      prompt)
    prompt))

(defun ellama-chat-buffer-p (buffer)
  "Return non-nil if BUFFER is an ellama chat buffer."
  (with-current-buffer buffer
    (not (not ellama--current-session))))

(defun ellama-get-current-session-id ()
  "Return current session id.
If buffer contains ellama session return its id.
Otherwire return id of current active session."
  (if ellama--current-session
      (ellama-session-id ellama--current-session)
    ellama--current-session-id))

(defun ellama-get-current-session ()
  "Return current session.
If buffer contains ellama session return it.
Otherwire return current active session."
  (if ellama--current-session
      ellama--current-session
    (when ellama--current-session-id
      (with-current-buffer (ellama-get-session-buffer ellama--current-session-id)
	ellama--current-session))))

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

:session-id ID -- ID is a ellama session unique identifier.

:ephemeral-session BOOL -- if BOOL is set session will not be saved to named
file by default.

:on-error ON-ERROR -- ON-ERROR a function that's called with an error message on
failure (with BUFFER current).

:on-done ON-DONE -- ON-DONE a function or list of functions that's called with
 the full response text when the request completes (with BUFFER current)."
  (let* ((session-id (plist-get args :session-id))
	 (session (or (plist-get args :session)
		      (when session-id
			(with-current-buffer (ellama-get-session-buffer session-id)
			  ellama--current-session))))
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
		    (when (ellama-chat-buffer-p buffer)
		      (with-selected-window window
			(goto-char (point-max))
			(recenter -1))))
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
				    (funcall insert-text (string-trim text))
				    (with-current-buffer buffer
				      (accept-change-group ellama--change-group)
				      (spinner-stop)
				      (if (and (listp donecb)
					       (functionp (car donecb)))
					  (mapc (lambda (fn) (funcall fn text))
						donecb)
					(funcall donecb text))
				      (setq ellama--current-request nil)
				      (ellama-request-mode -1)))
				  (lambda (_ msg)
				    (with-current-buffer buffer
				      (cancel-change-group ellama--change-group)
				      (spinner-stop)
				      (funcall errcb msg)
				      (setq ellama--current-request nil)
				      (ellama-request-mode -1)))))))))

(defun ellama-chain (initial-prompt forms &optional acc)
  "Call chain of FORMS on INITIAL-PROMPT.
ACC will collect responses in reverse order (previous answer will be on top).
Each form is a plist that can contain different options:

:provider PROVIDER - use PROVIDER instead of `ellama-provider'.

:transform FUNCTION - use FUNCTION to transform result of previous step to new
prompt.  FUCTION will be called with two arguments INITIAL-PROMPT and ACC.

:session SESSION - use SESSION in current step.

:session-id ID -- ID is a ellama session unique identifier.

:chat BOOL - if BOOL use chat buffer, otherwise use temp buffer.  Make sense for
last step only.

:show BOOL - if BOOL show buffer for this step."
  (let* ((hd (car forms))
	 (tl (cdr forms))
	 (provider (or (plist-get hd :provider) ellama-provider))
	 (transform (plist-get hd :transform))
	 (prompt (if transform
		     (apply transform (list initial-prompt acc))
		   initial-prompt))
	 (session-id (plist-get hd :session-id))
	 (session (or (plist-get hd :session)
		      (when session-id
			(with-current-buffer (ellama-get-session-buffer session-id)
			  ellama--current-session))))
	 (chat (plist-get hd :chat))
	 (show (or (plist-get hd :show) ellama-always-show-chain-steps))
	 (buf (if (or (and (not chat)) (not session))
		  (get-buffer-create (make-temp-name
				      (ellama-generate-name provider real-this-command prompt)))
		(ellama-get-session-buffer ellama--current-session-id))))
    (when show
      (display-buffer buf))
    (with-current-buffer buf
      (funcall ellama-major-mode))
    (if chat
	(ellama-chat
	 prompt
	 nil
	 :provider provider
	 :on-done (lambda (res)
		    (when tl
		      (ellama-chain res tl (cons res acc)))))
      (ellama-stream
       prompt
       :provider provider
       :buffer buf
       :session session
       :filter (when (derived-mode-p 'org-mode)
		 #'ellama--translate-markdown-to-org-filter)
       :on-done (lambda (res)
		  (when tl
		    (ellama-chain res tl (cons res acc))))))))

;;;###autoload
(defun ellama-solve-reasoning-problem (problem)
  "Solve reasoning PROBLEM with absctraction of thought.
Problem will be solved with the chain of questions to LLM."
  (interactive "sProblem: ")
  (ellama-chain
   problem
   '((:chat t
	    :transform (lambda (problem _)
			 (format "Problem:
%s

Let's think logically and provide abstract higher order plan how to solve this kind
of problems. Don't dive into small details only provide high-level plan." problem)))
     (:chat t
	    :transform (lambda (_ _)
			 "Provide more detailed plan. On what details should we pay attention?"))
     (:chat t
	    :transform (lambda (_ _)
			 "Now revise the plan and provide the final solution."))
     (:chat t
	    :transform (lambda (_ _)
			 "Provide short final answer based on final solution.")))))

;;;###autoload
(defun ellama-solve-domain-specific-problem (problem)
  "Solve domain-specific PROBLEM with `ellama-chain'."
  (interactive "sProblem: ")
  (ellama-chain
   problem
   `((:transform (lambda (problem _)
		   (format "Problem:
%s

Which specialist suits better for solving this kind of problems?"
			   problem)))
     (:transform (lambda (res _)
		   (format "Message:
%s

Extract profession from this message. Be short and concise."
			   res)))
     (:chat t
	    :transform (lambda (profession _)
			 (format
			  "You are professional %s. Do your best and create detailed plan how to solve this problem:
%s"
			  (string-trim profession) ,problem)))
     (:chat t
	    :transform (lambda (_ _)
			 "Now revise the plan and provide the final solution."))
     (:chat t
	    :transform (lambda (_ _)
			 "Provide short final answer based on final solution.")))))

(defun ellama-chat-done (text &optional on-done)
  "Chat done.
Will call `ellama-chat-done-callback' and ON-DONE on TEXT."
  (save-excursion
    (goto-char (point-max))
    (insert "\n\n")
    (when ellama-session-auto-save
      (save-buffer)))
  (when ellama-chat-done-callback
    (funcall ellama-chat-done-callback text))
  (when on-done
    (funcall on-done text)))

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

:provider PROVIDER -- PROVIDER is an llm provider for generation.

:session SESSION -- SESSION is a ellama conversation session.

:session-id ID -- ID is a ellama session unique identifier.

:on-done ON-DONE -- ON-DONE a function that's called with
the full response text when the request completes (with BUFFER current)."
  (interactive "sAsk ellama: ")
  (let* ((ollama-binary (executable-find ellama-ollama-binary))
	 (providers (append
                     `(("default model" . ellama-provider)
		       ,(if (and ollama-binary
				 (file-exists-p ollama-binary))
			    '("ollama model" . (ellama-get-ollama-local-model))))
                     ellama-providers))
	 (variants (mapcar #'car providers))
	 (donecb (plist-get args :on-done))
	 (provider (if current-prefix-arg
		       (eval (alist-get
			      (completing-read "Select model: " variants)
			      providers nil nil #'string=))
		     (or (plist-get args :provider)
			 ellama-provider)))
	 (session (or (plist-get args :session)
		      (if (or create-session
			      current-prefix-arg
			      (and provider
				   (or (plist-get args :provider)
				       (not (equal provider ellama-provider)))
				   ellama--current-session-id
				   (with-current-buffer (ellama-get-session-buffer
							 ellama--current-session-id)
				     (not (equal
					   provider
					   (ellama-session-provider ellama--current-session)))))
			      (and (not ellama--current-session)
				   (not ellama--current-session-id)))
			  (ellama-new-session provider prompt)
			(or ellama--current-session
			    (with-current-buffer (ellama-get-session-buffer
						  (or (plist-get args :session-id)
						      ellama--current-session-id))
			      ellama--current-session)))))
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
		  (ellama--format-context session) (ellama--fill-long-lines prompt) "\n\n"
		  (ellama-get-nick-prefix-for-mode) " " ellama-assistant-nick ":\n")
	  (ellama-stream prompt
			 :session session
			 :on-done (if donecb (list 'ellama-chat-done donecb)
				    'ellama-chat-done)
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

(defun ellama--diff-cached ()
  "Diff staged."
  (let* ((default-directory
	  (if (string= ".git"
		       (car (reverse
			     (cl-remove
			      ""
			      (file-name-split default-directory)
			      :test #'string=))))
	      (file-name-parent-directory default-directory)
	    default-directory))
	 (vc-git-diff-switches "--cached")
	 (diff (with-temp-buffer
		 (vc-diff-internal
		  nil (vc-deduce-fileset t) nil nil nil (current-buffer))
		 (buffer-substring-no-properties (point-min) (point-max)))))
    (if (string-empty-p diff)
	nil
      diff)))

(defun ellama--diff ()
  "Diff unstaged."
  (let* ((default-directory
	  (if (string= ".git"
		       (car (reverse
			     (cl-remove
			      ""
			      (file-name-split default-directory)
			      :test #'string=))))
	      (file-name-parent-directory default-directory)
	    default-directory))
	 (vc-git-diff-switches t)
	 (diff (with-temp-buffer
		 (vc-diff-internal
		  nil (vc-deduce-fileset t) nil nil nil (current-buffer))
		 (buffer-substring-no-properties (point-min) (point-max)))))
    (if (string-empty-p diff)
	nil
      diff)))

;;;###autoload
(defun ellama-generate-commit-message ()
  "Generate commit message based on diff."
  (interactive)
  (save-window-excursion
    (when-let* ((default-directory
		 (if (string= ".git"
			      (car (reverse
				    (cl-remove
				     ""
				     (file-name-split default-directory)
				     :test #'string=))))
		     (file-name-parent-directory default-directory)
		   default-directory))
		(diff (or (ellama--diff-cached)
			  (ellama--diff))))
      (ellama-stream
       (format ellama-generate-commit-message-template diff)
       :provider ellama-coding-provider))))

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
    (ellama-instant (format ellama-summarize-prompt-template text)
		    :provider (or ellama-summarization-provider ellama-provider))))

;;;###autoload
(defun ellama-summarize-killring ()
  "Summarize text from the kill ring."
  (interactive)
  (let ((text (current-kill 0)))
    (if (string-empty-p text)
        (message "No text in the kill ring to summarize.")
      (ellama-instant (format ellama-summarize-prompt-template text)
		      :provider (or ellama-summarization-provider ellama-provider)))))

;;;###autoload
(defun ellama-code-review ()
  "Review code in selected region or current buffer."
  (interactive)
  (let ((text (if (region-active-p)
		  (buffer-substring-no-properties (region-beginning) (region-end))
		(buffer-substring-no-properties (point-min) (point-max)))))
    (ellama-instant (format ellama-code-review-prompt-template text)
                    :provider ellama-coding-provider)))

;;;###autoload
(defun ellama-change (change &optional edit-template)
  "Change selected text or text in current buffer according to provided CHANGE.
When the value of EDIT-TEMPLATE is 4, or with one `universal-argument' as
prefix (\\[universal-argument]), prompt the user to amend the template."
  (interactive "sWhat needs to be changed: \np")
  (let* ((beg (if (region-active-p)
		  (region-beginning)
		(point-min)))
	 (end (if (region-active-p)
		  (region-end)
		(point-max)))
         (template-orig (format ellama-change-prompt-template change "%s"))
         (template (if (= edit-template 4)
                       (read-from-minibuffer "Template: " template-orig)
                     template-orig))
	 (text (buffer-substring-no-properties beg end)))
    (kill-region beg end)
    (ellama-stream
     (format template text)
     :point beg)))

;;;###autoload
(defun ellama-improve-grammar (&optional edit-template)
  "Enhance the grammar and spelling in the currently selected region or buffer.
When the value of EDIT-TEMPLATE is 4, or with one `universal-argument' as
prefix (\\[universal-argument]), prompt the user to amend the template."
  (interactive "p")
  (ellama-change ellama-improve-grammar-prompt-template edit-template))

;;;###autoload
(defun ellama-improve-wording (&optional edit-template)
  "Enhance the wording in the currently selected region or buffer.
When the value of EDIT-TEMPLATE is 4, or with one `universal-argument' as
prefix (\\[universal-argument]), prompt the user to amend the template."
  (interactive "p")
  (ellama-change ellama-improve-wording-prompt-template edit-template))

;;;###autoload
(defun ellama-improve-conciseness (&optional edit-template)
  "Make the text of the currently selected region or buffer concise and simple.
When the value of EDIT-TEMPLATE is 4, or with one `universal-argument' as
prefix (\\[universal-argument]), prompt the user to amend the template."
  (interactive "p")
  (ellama-change ellama-improve-conciseness-prompt-template edit-template))

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
     :provider ellama-coding-provider
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
     :provider ellama-coding-provider
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
     :provider ellama-coding-provider
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
     :provider ellama-coding-provider
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
  "Summarize webpage fetched from URL.

Summarize the URL at point if `thing-at-point' is present, or using
`shr-url-at-point' if a URL is at point in modes like `eww' or `elfeed',
otherwise prompt user for URL to summarize."
  (interactive
   (list
    (if-let ((url (or (and (fboundp 'thing-at-point) (thing-at-point 'url))
                      (shr-url-at-point nil))))
        url
      (read-string "Enter URL you want to summarize: "))))
  (let ((buffer-name (url-retrieve-synchronously url t)))
    ;; (display-buffer buffer-name)
    (with-current-buffer buffer-name
      (goto-char (point-min))
      (or (search-forward "<!DOCTYPE" nil t)
          (search-forward "<html" nil))
      (beginning-of-line)
      (kill-region (point-min) (point))
      (shr-insert-document (libxml-parse-html-region (point-min) (point-max)))
      (goto-char (point-min))
      (or (search-forward "<!DOCTYPE" nil t)
          (search-forward "<html" nil))
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
				   (process-lines
				    (executable-find ellama-ollama-binary) "ls")
				   1))))
	(host (when (llm-ollama-p ellama-provider)
		(llm-ollama-host ellama-provider)))
	(port (when (llm-ollama-p ellama-provider)
		(llm-ollama-port ellama-provider))))
    (if host
	(make-llm-ollama
	 :chat-model model-name :embedding-model model-name :host host :port port)
      (make-llm-ollama
       :chat-model model-name :embedding-model model-name))))

(transient-define-prefix ellama-transient-code-menu ()
  "Code Commands."
  [["Code Commands"
    ("c" "Complete" ellama-code-complete)
    ("a" "Add" ellama-code-add)
    ("e" "Edit" ellama-code-edit)
    ("i" "Improve" ellama-code-improve)
    ("r" "Review" ellama-code-review)
    ("m" "Generate Commit Message" ellama-generate-commit-message)]
   ["Quit" ("q" "Quit" transient-quit-one)]])

(transient-define-prefix ellama-transient-summarize-menu ()
  "Summarize Commands."
  [["Summarize Commands"
    ("s" "Summarize" ellama-summarize)
    ("w" "Summarize Webpage" ellama-summarize-webpage)
    ("k" "Summarize Killring" ellama-summarize-killring)]
   ["Quit" ("q" "Quit" transient-quit-one)]])

(transient-define-prefix ellama-transient-session-menu ()
  "Session Commands."
  [["Session Commands"
    ("l" "Load Session" ellama-load-session)
    ("r" "Rename Session" ellama-session-rename)
    ("d" "Remove Session" ellama-session-remove)
    ("a" "Activate Session" ellama-session-switch)]
   ["Quit" ("q" "Quit" transient-quit-one)]])

(transient-define-prefix ellama-transient-improve-menu ()
  "Improve Commands."
  [["Improve Commands"
    ("w" "Improve Wording" ellama-improve-wording)
    ("g" "Improve Grammar" ellama-improve-grammar)
    ("c" "Improve Conciseness" ellama-improve-conciseness)]
   ["Quit" ("q" "Quit" transient-quit-one)]])

(transient-define-prefix ellama-transient-make-menu ()
  "Make Commands."
  [["Make Commands"
    ("l" "Make List" ellama-make-list)
    ("t" "Make Table" ellama-make-table)
    ("f" "Make Format" ellama-make-format)]
   ["Quit" ("q" "Quit" transient-quit-one)]])

(transient-define-prefix ellama-transient-ask-menu ()
  "Ask Commands."
  [["Ask Commands"
    ("l" "Ask Line" ellama-ask-line)
    ("s" "Ask Selection" ellama-ask-selection)
    ("a" "Ask About" ellama-ask-about)]
   ["Quit" ("q" "Quit" transient-quit-one)]])

(transient-define-prefix ellama-transient-translate-menu ()
  "Translate Commands."
  [["Translate Commands"
    ("t" "Translate Text" ellama-translate)
    ("b" "Translate Buffer" ellama-translate-buffer)
    ("e" "Enable Translation" ellama-chat-translation-enable)
    ("d" "Disable Translation" ellama-chat-translation-disable)]
   ["Quit" ("q" "Quit" transient-quit-one)]])

(transient-define-prefix ellama-transient-context-menu ()
  "Context Commands."
  [["Context Commands"
    ("b" "Add Buffer" ellama-context-add-buffer)
    ("f" "Add File" ellama-context-add-file)
    ("s" "Add Selection" ellama-context-add-selection)
    ("i" "Add Info Node" ellama-context-add-info-node)]
   ["Quit" ("q" "Quit" transient-quit-one)]])

(transient-define-prefix ellama-transient-main-menu ()
  "Main Menu."
  [["Main"
    ("c" "Chat" ellama-chat)
    ("a" "Ask Commands" ellama-transient-ask-menu)
    ("C" "Code Commands" ellama-transient-code-menu)]]
  [["Text"
    ("s" "Summarize Commands" ellama-transient-summarize-menu)
    ("i" "Improve Commands" ellama-transient-improve-menu)
    ("t" "Translate Commands" ellama-transient-translate-menu)
    ("m" "Make Commands" ellama-transient-make-menu)
    ("k" "Text Complete" ellama-complete)]]
  [["System"
    ("S" "Session Commands" ellama-transient-session-menu)
    ("x" "Context Commands" ellama-transient-context-menu)
    ("p" "Provider selection" ellama-provider-select)]]
  [["Problem solving"
    ("R" "Solve reasoning problem" ellama-solve-reasoning-problem)
    ("D" "Solve domain specific problem" ellama-solve-domain-specific-problem)]]
  [["Quit" ("q" "Quit" transient-quit-one)]])

;;;###autoload
(defun ellama-provider-select ()
  "Select ellama provider."
  (interactive)
  (let* ((ollama-binary (executable-find ellama-ollama-binary))
	 (providers (append
                     `(("default model" . ellama-provider)
		       ,(if (and ollama-binary
				 (file-exists-p ollama-binary))
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
