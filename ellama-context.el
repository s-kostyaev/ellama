;;; ellama-context.el --- Working with context -*- lexical-binding: t; package-lint-main-file: "ellama.el"; -*-

;; Copyright (C) 2023-2025  Free Software Foundation, Inc.

;; Author: Sergey Kostyaev <sskostyaev@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

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
(require 'ellama)

(defcustom ellama-context-line-always-visible nil
  "Make context header or mode line always visible, even with empty context."
  :group 'ellama
  :type 'boolean)

(defcustom ellama-context-posframe-enabled nil
  "Enable showing posframe with ellama context."
  :group 'ellama
  :type 'boolean)

(defcustom ellama-context-poshandler 'posframe-poshandler-frame-top-center
  "Position handler for displaying context buffer."
  :group 'ellama
  :type 'function)

(defcustom ellama-context-border-width 1
  "Border width for the context buffer."
  :group 'ellama
  :type 'integer)

(defface ellama-context-line-face '((t (:inherit (mode-line-buffer-id ellama-face))))
  "Face for ellama context line."
  :group 'ellama)

(defface ellama-key-face '((t (:inherit help-key-binding)))
  "Face for ellama context line."
  :group 'ellama)

(defvar ellama-context-global nil
  "Global context.")

(defvar ellama--context-buffer " *ellama-context*")

(defvar ellama-context-buffer "*ellama-context*")

;;;###autoload
(defun ellama-context-reset ()
  "Clear global context."
  (interactive)
  (setq ellama-context-global nil)
  (with-current-buffer ellama--context-buffer
    (erase-buffer))
  (ellama-context-update-show))

(defun ellama-context--element-remove-by-name (name)
  "Remove all context element that matches by NAME."
  (setq ellama-context-global
	(cl-remove-if (lambda (el)
			(string= name (ellama-context-element-display el)))
		      ellama-context-global)))

;;;###autoload
(defun ellama-context-element-remove-by-name ()
  "Remove a context element by its name from the global context.
This function prompts the user to select a context element from
the list of unique elements currently present in the global
context and removes it.  After removal, it updates the display of
the context."
  (interactive)
  (ellama-context--element-remove-by-name
   (completing-read
    "Remove context element: "
    (seq-uniq (mapcar #'ellama-context-element-display ellama-context-global))))
  (ellama-context-update-show))

(defun ellama-context-update-show ()
  "Update and show context in posframe of header line."
  (declare-function posframe-show "ext:posframe")
  (declare-function posframe-hide "ext:posframe")
  (with-current-buffer (get-buffer-create ellama--context-buffer)
    (erase-buffer)
    (if ellama-context-global
	(insert (format
		 " ellama ctx: %s"
		 (string-join
		  (mapcar
		   (lambda (el)
		     (ellama-context-element-display el))
		   ellama-context-global)
		  "  ")))
      (insert " ellama ctx")))
  (when ellama-context-posframe-enabled
    (require 'posframe)
    (if ellama-context-global
	(posframe-show
	 ellama--context-buffer
	 :poshandler ellama-context-poshandler
	 :internal-border-width ellama-context-border-width)
      (posframe-hide ellama--context-buffer)))
  (ellama-context-update-header-line))

(declare-function ellama-transient-context-menu "ellama-transient")

(defun ellama-context-line ()
  "Return current global context line."
  (propertize (with-current-buffer ellama--context-buffer
		(buffer-substring-no-properties
		 (point-min) (point-max)))
	      'help-echo "mouse-1: manage ellama context"
	      'mouse-face 'header-line-format
	      'face 'ellama-context-line-face
	      'keymap (let ((m (make-sparse-keymap)))
			(define-key m [header-line mouse-1] #'ellama-transient-context-menu)
			(define-key m [mode-line mouse-1] #'ellama-transient-context-menu)
			m)))

;;;###autoload
(define-minor-mode ellama-context-header-line-mode
  "Toggle Ellama Context header line mode."
  :group 'ellama
  (ellama-context-update-show)
  (add-hook 'window-state-change-hook #'ellama-context-update-header-line)
  (if ellama-context-header-line-mode
      (ellama-context-update-header-line)
    (when (listp header-line-format)
      (setq header-line-format (delete '(:eval (ellama-context-line)) header-line-format)))))

;;;###autoload
(define-globalized-minor-mode ellama-context-header-line-global-mode
  ellama-context-header-line-mode
  ellama-context-header-line-mode
  :group 'ellama)

(defun ellama-context-update-header-line ()
  "Update and display context information in the header line."
  (when (listp header-line-format)
    (if (and ellama-context-header-line-mode
	     (or ellama-context-line-always-visible
		 ellama-context-global))
	(add-to-list 'header-line-format '(:eval (ellama-context-line)) t)
      (setq header-line-format (delete '(:eval (ellama-context-line)) header-line-format)))))

;;;###autoload
(define-minor-mode ellama-context-mode-line-mode
  "Toggle Ellama Context mode line mode."
  :group 'ellama
  (ellama-context-update-show)
  (add-hook 'window-state-change-hook #'ellama-context-update-mode-line)
  (if ellama-context-mode-line-mode
      (ellama-context-update-mode-line)
    (setq mode-line-format (delete '(:eval (ellama-context-line)) mode-line-format))))

;;;###autoload
(define-globalized-minor-mode ellama-context-mode-line-global-mode
  ellama-context-mode-line-mode
  ellama-context-mode-line-mode
  :group 'ellama)

(defun ellama-context-update-mode-line ()
  "Update and display context information in the mode line."
  (if (and ellama-context-mode-line-mode
	   (or ellama-context-line-always-visible
	       ellama-context-global))
      (add-to-list 'mode-line-format '(:eval (ellama-context-line)) t)
    (setq mode-line-format (delete '(:eval (ellama-context-line)) mode-line-format))))

(defcustom ellama-context-manage-display-action-function #'display-buffer-same-window
  "Display action function for `ellama-render-context'."
  :group 'ellama
  :type 'function)

(defvar-keymap ellama-context-mode-map
  :doc "Local keymap for Ellama context mode buffers."
  :full t
  :parent special-mode-map
  "n"       #'next-line
  "p"       #'previous-line
  "q"       #'quit-window
  "g"       #'ellama-context-manage
  "a"       #'ellama-transient-context-menu
  "d"       #'ellama-context-remove-element-at-point
  "RET"     #'ellama-context-preview-element-at-point)

(define-derived-mode ellama-context-mode
  fundamental-mode
  "ellama-ctx"
  "Toggle Ellama Context mode."
  :keymap ellama-context-mode-map
  :group 'ellama)

(defun ellama-context-update-buffer ()
  "Update ellama context buffer."
  (let* ((buf (get-buffer-create ellama-context-buffer))
	 (inhibit-read-only t))
    (with-current-buffer buf
      (read-only-mode +1)
      (ellama-context-mode)
      (erase-buffer)
      (dolist (el ellama-context-global)
	(insert (ellama-context-element-display el))
	(put-text-property (pos-bol) (pos-eol) 'context-element el)
	(insert "\n"))
      (goto-char (point-min)))))

;;;###autoload
(defun ellama-context-manage ()
  "Manage the global context."
  (interactive)
  (ellama-context-update-buffer)
  (display-buffer
   ellama-context-buffer
   (when ellama-context-manage-display-action-function
     `((ignore . (,ellama-context-manage-display-action-function))))))

(defvar-keymap ellama-context-preview-mode-map
  :doc "Local keymap for Ellama preview context mode buffers."
  :full t
  :parent special-mode-map
  "q"       #'ellama-kill-current-buffer)

(define-minor-mode ellama-context-preview-mode
  "Toggle Ellama Preview Context mode."
  :keymap ellama-context-preview-mode-map
  :group 'ellama
  (setq header-line-format
	(concat (propertize (substitute-command-keys
			     "`\\[ellama-kill-current-buffer]'")
			    'face 'ellama-key-face)
		" to quit")))

(defcustom ellama-context-preview-element-display-action-function nil
  "Display action function for `ellama-context-preview-element'."
  :group 'ellama
  :type 'function)

(defun ellama-context-preview-element (element)
  "Preview context ELEMENT content."
  (let* ((name
	  (concat (make-temp-name
		   (concat " *ellama-context-"
			   (ellama-context-element-display element)
			   "-"))
		  "*"))
	 (buf (get-buffer-create name)))
    (with-current-buffer buf
      (insert (ellama-context-element-extract element))
      (read-only-mode +1)
      (ellama-context-preview-mode +1)
      (display-buffer
       buf
       (when ellama-context-preview-element-display-action-function
	 `((ignore . (,ellama-context-preview-element-display-action-function))))))))

(defun ellama-context-remove-element (element)
  "Remove context ELEMENT from global context."
  (setf ellama-context-global
	(cl-remove element ellama-context-global :test #'equal-including-properties)))

;;;###autoload
(defun ellama-context-preview-element-at-point ()
  "Preview ellama context element at point."
  (interactive)
  (when-let ((elt (get-text-property (point) 'context-element)))
    (ellama-context-preview-element elt)))

;;;###autoload
(defun ellama-context-remove-element-at-point ()
  "Remove ellama context element at point from global context."
  (interactive)
  (when-let ((elt (get-text-property (point) 'context-element)))
    (ellama-context-remove-element elt)
    (ellama-context-manage)
    (ellama-context-update-show)))

;; Context elements

(defclass ellama-context-element () ()
  "A structure for holding information about a context element.")

(cl-defgeneric ellama-context-element-add (element)
  "Add the ELEMENT to the Ellama context.")

(cl-defgeneric ellama-context-element-extract (element)
  "Extract the content of the context ELEMENT.")

(cl-defgeneric ellama-context-element-display (element)
  "Display the context ELEMENT.")

(cl-defgeneric ellama-context-element-format (element mode)
  "Format the context ELEMENT for the major MODE.")

(cl-defmethod ellama-context-element-add ((element ellama-context-element))
  "Add the ELEMENT to the Ellama context."
  (setf ellama-context-global (nreverse ellama-context-global))
  (cl-pushnew element ellama-context-global
	      :test #'equal-including-properties)
  (setf ellama-context-global (nreverse ellama-context-global))
  (get-buffer-create ellama--context-buffer t)
  (ellama-context-update-show))

;; Buffer context element

(defclass ellama-context-element-buffer (ellama-context-element)
  ((name :initarg :name :type string))
  "A structure for holding information about a context element.")

(cl-defmethod ellama-context-element-extract
  ((element ellama-context-element-buffer))
  "Extract the content of the context ELEMENT."
  (with-slots (name) element
    (with-current-buffer name
      (let* ((data (buffer-substring-no-properties (point-min) (point-max)))
	     (content (if (derived-mode-p 'org-mode)
			  (ellama-convert-org-to-md data)
			data)))
	content))))

(cl-defmethod ellama-context-element-display
  ((element ellama-context-element-buffer))
  "Display the context ELEMENT."
  (with-slots (name) element
    name))

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

;; Buffer quote context elements

(defclass ellama-context-element-buffer-quote (ellama-context-element)
  ((name :initarg :name :type string)
   (content :initarg :content :type string))
  "A structure for holding information about a context element.")

(cl-defmethod ellama-context-element-extract
  ((element ellama-context-element-buffer-quote))
  "Extract the content of the context ELEMENT."
  (oref element content))

(cl-defmethod ellama-context-element-display
  ((element ellama-context-element-buffer-quote))
  "Display the context ELEMENT."
  (oref element name))

(cl-defmethod ellama-context-element-format
  ((element ellama-context-element-buffer-quote) (mode (eql 'markdown-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (with-slots (name content) element
    (if ellama-show-quotes
	(format "[%s](%s):\n%s\n\n"
		name name
		(ellama-context--md-quote content))
      (format "[%s](%s):\n```emacs-lisp\n(display-buffer \"%s\")"
	      name name (ellama-context--quote-buffer content)))))

(cl-defmethod ellama-context-element-format
  ((element ellama-context-element-buffer-quote) (mode (eql 'org-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (with-slots (name content) element
    (if ellama-show-quotes
	(format "[[%s][%s]]:\n#+BEGIN_QUOTE\n%s\n#+END_QUOTE\n"
		name name (ellama-context--org-quote content))
      (format "[[%s][%s]] [[elisp:(display-buffer \"%s\")][show]]"
	      name name (ellama-context--quote-buffer content)))))

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
      (let* ((data (buffer-substring-no-properties (point-min) (point-max)))
	     (ext (file-name-extension name)))
	(if (string= ext "org")
	    (ellama-convert-org-to-md data)
	  data)))))

(cl-defmethod ellama-context-element-display
  ((element ellama-context-element-file))
  "Display the context ELEMENT."
  (with-slots (name) element
    (file-name-nondirectory name)))

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

(cl-defmethod ellama-context-element-display
  ((element ellama-context-element-info-node))
  "Display the context ELEMENT."
  (with-slots (name) element
    (format "(info \"%s\")" name)))

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

(defcustom ellama-text-display-limit 15
  "Limit for text display in context elements."
  :group 'ellama
  :type 'integer)

(cl-defmethod ellama-context-element-display
  ((element ellama-context-element-text))
  "Display the context ELEMENT."
  (with-slots (content) element
    (format "\"%s\"" (concat
		      (string-limit
		       content
		       ellama-text-display-limit)
		      "..."))))

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

(cl-defmethod ellama-context-element-display
  ((element ellama-context-element-webpage-quote))
  "Display the context ELEMENT."
  (with-slots (name) element
    name))

(cl-defmethod ellama-context-element-display
  ((element ellama-context-element-webpage-quote))
  "Display the context ELEMENT."
  (with-slots (name) element
    name))

(defun ellama-context--quote-buffer (quote)
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
		(ellama-context--md-quote content))
      (format
       "[%s](%s):\n```emacs-lisp\n(display-buffer \"%s\")\n```\n"
       name url (ellama-context--quote-buffer content)))))

(defun ellama-context--md-quote (content)
  "Return quoted CONTENT for markdown."
  (with-temp-buffer
    (insert (propertize content 'hard t))
    (let ((fill-prefix "> ")
	  (use-hard-newlines t)
	  (comment-start ">")
	  (comment-empty-lines t))
      (comment-region (point-min) (point-max) ">")
      (fill-region (point-min) (point-max) nil t t))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun ellama-context--org-quote (content)
  "Return transformed CONTENT for org quotes."
  (replace-regexp-in-string "^*" " *" content))

(cl-defmethod ellama-context-element-format
  ((element ellama-context-element-webpage-quote) (mode (eql 'org-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (with-slots (name url content) element
    (if ellama-show-quotes
	(format "[[%s][%s]]:\n#+BEGIN_QUOTE\n%s\n#+END_QUOTE\n"
		url name (ellama-context--org-quote content))
      (format "[[%s][%s]] [[elisp:(display-buffer \"%s\")][show]]"
	      url name (ellama-context--quote-buffer content)))))

;; Info node quote context elements

(defclass ellama-context-element-info-node-quote (ellama-context-element)
  ((name :initarg :name :type string)
   (content :initarg :content :type string))
  "A structure for holding information about a context element.")

(cl-defmethod ellama-context-element-extract
  ((element ellama-context-element-info-node-quote))
  "Extract the content of the context ELEMENT."
  (oref element content))

(cl-defmethod ellama-context-element-display
  ((element ellama-context-element-info-node-quote))
  "Display the context ELEMENT."
  (with-slots (name) element
    (format "(info \"%s\")" name)))

(cl-defmethod ellama-context-element-format
  ((element ellama-context-element-info-node-quote) (mode (eql 'markdown-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (with-slots (name content) element
    (if ellama-show-quotes
	(format "```emacs-lisp\n(info \"%s\")\n```\n%s\n\n"
		name
		(ellama-context--md-quote content))
      (format "```emacs-lisp\n(info \"%s\")\n```\nshow:\n```emacs-lisp\n(display-buffer \"%s\")\n```\n" name (ellama-context--quote-buffer content)))))

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
		(ellama-context--org-quote content))
      (format "[[%s][%s]] [[elisp:(display-buffer \"%s\")][show]]"
	      (replace-regexp-in-string
	       "(\\(.?*\\)) \\(.*\\)" "info:\\1#\\2" name)
	      (if (and ellama-chat-translation-enabled
		       (not ellama--current-session))
		  (ellama--translate-string name)
		name)
	      (ellama-context--quote-buffer content)))))

;; File quote context elements

(defclass ellama-context-element-file-quote (ellama-context-element)
  ((path :initarg :path :type string)
   (content :initarg :content :type string))
  "A structure for holding information about a context element.")

(cl-defmethod ellama-context-element-extract
  ((element ellama-context-element-file-quote))
  "Extract the content of the context ELEMENT."
  (oref element content))

(cl-defmethod ellama-context-element-display
  ((element ellama-context-element-file-quote))
  "Display the context ELEMENT."
  (with-slots (path) element
    (file-name-nondirectory path)))

(cl-defmethod ellama-context-element-format
  ((element ellama-context-element-file-quote) (mode (eql 'markdown-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (with-slots (path content) element
    (if ellama-show-quotes
	(format "[%s](%s):\n%s\n\n"
		path path
		(ellama-context--md-quote content))
      (format "[%s](%s):\n```emacs-lisp\n(display-buffer \"%s\")"
	      path path (ellama-context--quote-buffer content)))))

(cl-defmethod ellama-context-element-format
  ((element ellama-context-element-file-quote) (mode (eql 'org-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (with-slots (path content) element
    (if ellama-show-quotes
	(format "[[%s][%s]]:\n#+BEGIN_QUOTE\n%s\n#+END_QUOTE\n"
		path path (ellama-context--org-quote content))
      (format "[[%s][%s]] [[elisp:(display-buffer \"%s\")][show]]"
	      path path (ellama-context--quote-buffer content)))))


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
  (let* ((buffer-name (if (stringp buf)
			  buf
			(buffer-name buf)))
	 (element (ellama-context-element-buffer :name buffer-name)))
    (ellama-context-element-add element)))

;;;###autoload
(defun ellama-context-add-projectile-project nil
  "Add all files in current projectile project to context."
  (interactive)
  (unless (boundp 'projectile-current-project-files)
    (user-error "Projectile is not avaliable"))
  (dolist (file-name (projectile-current-project-files))
    (let ((element (ellama-context-element-file :name file-name)))
      (ellama-context-element-add element))))

;;;###autoload
(defun ellama-context-add-directory (dir)
  "Add all files in DIR to the context."
  (interactive "DSelect directory: ")
  (dolist (file-name (directory-files dir t "^[^\.].*"))
    (unless (file-directory-p file-name)
      (let ((element (ellama-context-element-file :name file-name)))
	(ellama-context-element-add element)))))

;;;###autoload
(defun ellama-context-add-selection ()
  "Add active region to context."
  (interactive)
  (if (region-active-p)
      (let* ((data (buffer-substring-no-properties (region-beginning) (region-end)))
	     (content (if (derived-mode-p 'org-mode)
			  (ellama-convert-org-to-md data)
			data))
	     (file-name (buffer-file-name))
	     (buffer-name (buffer-name (current-buffer)))
	     (element (if file-name
			  (ellama-context-element-file-quote :path file-name
							     :content content)
			(ellama-context-element-buffer-quote :name buffer-name :content content))))
	(ellama-context-element-add element))
    (warn "No active region")))

(defun ellama-context-add-text (text)
  "Add TEXT to context."
  (let ((element (ellama-context-element-text :content text)))
    (ellama-context-element-add element)))

(declare-function Info-copy-current-node-name "info")

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
  (defvar eww-data)
  (declare-function eww-current-url "eww")
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

;;;###autoload
(defun ellama-context-format (_)
  "Format context for chat buffer."
  (let ((mode (if (derived-mode-p 'org-mode) 'org-mode 'markdown-mode)))
    (if-let* ((context ellama-context-global))
	(concat (string-join
		 (cons "Context:"
		       (mapcar (lambda (elt)
				 (ellama-context-element-format elt mode))
			       context))
		 "\n")
		"\n\n")
      "")))

;;;###autoload
(defun ellama-context-prompt-with-context (prompt)
  "Add context to PROMPT for sending to llm."
  (let* ((context ellama-context-global))
    (if context
	(concat (string-join
		 (cons "Context:"
		       (mapcar #'ellama-context-element-extract context))
		 "\n")
		"\n\n"
		prompt)
      prompt)))

(provide 'ellama-context)
;;; ellama-context.el ends here.
