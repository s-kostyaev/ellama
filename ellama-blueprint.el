;;; ellama-blueprint.el --- Working with blueprints -*- lexical-binding: t; package-lint-main-file: "ellama.el"; -*-

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

(defcustom ellama-blueprints nil
  "User defined blueprints."
  :group 'ellama
  :type 'plist)

(defvar-keymap ellama-blueprint-mode-map
  :doc "Local keymap for Ellama blueprint mode buffers."
  :parent global-map
  "C-c C-c" #'ellama-send-buffer-to-new-chat-then-kill
  "C-c C-k" #'ellama-kill-current-buffer
  "C-c c" #'ellama-blueprint-create
  "C-c v" #'ellama-blueprint-fill-variables)

(defvar ellama-blueprint-font-lock-keywords
  '(("{\\([^}]+\\)}" 1 'font-lock-keyword-face))
  "Highlight variables in curly braces for Ellama Blueprint Mode.")

;;;###autoload
(define-derived-mode ellama-blueprint-mode
  text-mode
  "ellama-blueprint"
  "Toggle Ellama Blueprint mode."
  :keymap ellama-blueprint-mode-map
  :group 'ellama
  (setq font-lock-defaults '((("{\\([^}]+\\)}" 1 font-lock-keyword-face t))))
  (setq header-line-format
	(substitute-command-keys
	 "`\\[ellama-send-buffer-to-new-chat-then-kill]' to send `\\[ellama-kill-current-buffer]' to cancel `\\[ellama-blueprint-create]' to create new blueprint `\\[ellama-blueprint-fill-variables]' to fill variables")))

(defvar ellama-blueprint-buffer "*ellama-blueprint-buffer*"
  "Buffer for prompt blueprint.")

;;;###autoload
(defun ellama-blueprint-run (blueprint &optional args)
  "Run chat with llm starting with BLUEPRINT with prefilled variables.

ARGS contains plist with variables to prefill."
  (let* ((collection (seq-union
		      ellama-blueprints
		      (ellama-community-prompts-ensure)
		      (lambda (blueprint1 blueprint2)
			(string=
			 (plist-get blueprint1 :act)
			 (plist-get blueprint2 :act)))))
	 (prompt (cl-find-if (lambda (el)
			       (string= blueprint (plist-get el :act)))
			     collection))
	 (content (plist-get prompt :prompt)))
    (with-temp-buffer
      (insert content)
      (when args
	(dolist (var (ellama-blueprint-get-variable-list))
	  (ellama-blueprint-set-variable
	   var
	   (plist-get args (intern (concat ":" var))))))
      (ellama-send-buffer-to-new-chat))))

;;;###autoload
(defun ellama-blueprint-select (&optional args)
  "Select a prompt from the prompt collection.
The user is prompted to choose a role, and then a
corresponding prompt is inserted into a blueprint buffer.
ARGS contains keys for fine control.

:for-devs filters prompts for developers.

:source filters prompts for source:
- `user' will show user defined blueprints only;
- `community' will show blueprints from community;
- otherwise all blueprints will be shown."
  (interactive)
  (declare-function ellama-community-prompts-ensure "ellama-community-prompts")
  (let* ((for-devs (plist-get args :for-devs))
	 (source (plist-get args :source))
	 (acts '())
	 (collection (pcase source
		       ('user ellama-blueprints)
		       ('community (ellama-community-prompts-ensure))
		       (_ (seq-union
			   ellama-blueprints
			   (ellama-community-prompts-ensure)
			   (lambda (blueprint1 blueprint2)
			     (string=
			      (plist-get blueprint1 :act)
			      (plist-get blueprint2 :act)))))))
	 selected-act
	 selected-prompt)
    ;; Collect unique acts from the filtered collection
    (dolist (prompt collection)
      (when (or (not for-devs) (eq for-devs (plist-get prompt :for-devs)))
	(cl-pushnew (plist-get prompt :act) acts)))
    ;; Prompt user to select an act
    (setq selected-act (completing-read "Select Act: " acts))
    ;; Find the corresponding prompt
    (catch 'found-prompt
      (dolist (prompt collection)
	(when (and (string= selected-act (plist-get prompt :act))
		   (or (not for-devs) (eq for-devs (plist-get prompt :for-devs))))
	  (setq selected-prompt (plist-get prompt :prompt))
	  (throw 'found-prompt nil))))
    ;; Create a new buffer and insert the selected prompt
    (with-current-buffer (get-buffer-create ellama-blueprint-buffer)
      (erase-buffer)
      (let ((hard-newline t))
	(insert selected-prompt)
	(ellama-blueprint-mode))
      (switch-to-buffer (current-buffer))
      (ellama-blueprint-fill-variables))))

;;;###autoload
(defun ellama-blueprint-select-user-defined-blueprint ()
  "Select a prompt from the user defined prompt collection.
The user is prompted to choose a role, and then a
corresponding prompt is inserted into a blueprint buffer."
  (interactive)
  (ellama-blueprint-select '(:source user)))

;;;###autoload
(defun ellama-blueprint-create ()
  "Create blueprint from current buffer."
  (interactive)
  (let* ((name (read-string "Name: "))
	 (for-devs (y-or-n-p "For developers? "))
	 (content (buffer-substring-no-properties (point-min) (point-max)))
	 (blueprint `(:act ,name :prompt ,content :for-devs ,for-devs)))
    (add-to-list 'ellama-blueprints blueprint t)
    (customize-save-variable 'ellama-blueprints ellama-blueprints)))

;;;###autoload
(defun ellama-blueprint-new ()
  "Create new blueprint."
  (interactive)
  (let* ((content (when (region-active-p)
		    (buffer-substring-no-properties (region-beginning) (region-end))))
	 (name (concat (make-temp-name "*ellama-blueprint-") "*"))
	 (buf (get-buffer-create name)))
    (switch-to-buffer buf t t)
    (with-current-buffer buf
      (when content (insert content))
      (ellama-blueprint-mode))))

(defun ellama-blueprint-get-variable-list ()
  "Return a deduplicated list of variables found in the current buffer."
  (save-excursion
    (let ((vars '()))
      (goto-char (point-min))
      (while (re-search-forward "\{\\([^}]+\\)}" nil t)
	(push (match-string 1) vars))
      (seq-uniq vars))))

(defun ellama-blueprint-set-variable (var value)
  "Replace VAR with VALUE in blueprint buffer."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward (format "{%s}" var) nil t)
      (replace-match value))))

;;;###autoload
(defun ellama-blueprint-fill-variables ()
  "Prompt user for values of variables found in current buffer and fill them."
  (interactive)
  (let ((vars (ellama-blueprint-get-variable-list)))
    (dolist (var vars)
      (let ((value (read-string (format "Enter value for {%s}: " var))))
	(ellama-blueprint-set-variable var value)))))

(provide 'ellama-blueprint)
;;; ellama-blueprint.el ends here.
