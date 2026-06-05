;;; extract-commands.el --- Extract documented public commands -*- lexical-binding: t -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; This script reads Ellama source files as forms.  It intentionally does not
;; load the project, because loading depends on user packages and local runtime
;; state and can silently miss commands in batch checks.

(require 'cl-lib)

(defconst check-commands--extra-public-commands
  '(ellama-summarize-webpage
    ellama-tools-dlp-clear-session-bypasses
    ellama-tools-dlp-reset-runtime-state
    ellama-tools-dlp-show-incident-stats)
  "Interactive public commands without autoload cookies.")

(defconst check-commands--ignored-commands
  '(ellama-blueprint-chat-with-system-kill-buffer
    ellama-blueprint-create
    ellama-blueprint-edit-system-message
    ellama-blueprint-new
    ellama-blueprint-remove
    ellama-blueprint-select
    ellama-blueprint-select-user-defined-blueprint
    ellama-blueprint-set-system-kill-buffer
    ellama-chat-with-system-from-buffer
    ellama-context-add-file-quote
    ellama-context-add-info-node-quote
    ellama-context-add-webpage-quote-eww
    ellama-context-element-remove-by-name
    ellama-eval-run-hypothesis-suite-interactive
    ellama-kill-current-buffer
    ellama-manual-export
    ellama-send-buffer-to-new-chat
    ellama-send-buffer-to-new-chat-then-kill
    ellama-transient-ask-menu
    ellama-transient-blueprint-menu
    ellama-transient-blueprint-mode-menu
    ellama-transient-code-menu
    ellama-transient-context-menu
    ellama-transient-improve-menu
    ellama-transient-main-menu
    ellama-transient-make-menu
    ellama-transient-session-menu
    ellama-transient-summarize-menu
    ellama-transient-tools-menu
    ellama-transient-translate-menu)
  "Interactive commands intentionally excluded from README Commands coverage.")

(defun check-commands--source-files (dir)
  "Return Ellama source .el files in DIR."
  (sort
   (directory-files dir t "\\`ellama.*\\.el\\'")
   #'string<))

(defun check-commands--public-command-name-p (name)
  "Return non-nil when NAME is an Ellama command name worth documenting."
  (and (symbolp name)
       (string-prefix-p "ellama-" (symbol-name name))
       (not (string-prefix-p "ellama--" (symbol-name name)))
       (not (memq name check-commands--ignored-commands))))

(defun check-commands--interactive-body-p (body)
  "Return non-nil when BODY starts with an `interactive' form."
  (when (stringp (car body))
    (setq body (cdr body)))
  (while (and (consp (car body))
              (eq (caar body) 'declare))
    (setq body (cdr body)))
  (and (consp (car body))
       (eq (caar body) 'interactive)))

(defun check-commands--command-form-name (form autoloadp)
  "Return public command name from FORM when AUTOLOADP marks it as public."
  (when (consp form)
    (let ((head (car form))
          (name (cadr form)))
      (cond
       ((memq head '(defun cl-defun))
        (let ((body (cdddr form)))
          (when (and autoloadp
                     (check-commands--public-command-name-p name)
                     (check-commands--interactive-body-p body))
            name)))
       ((eq head 'transient-define-prefix)
        (when (and autoloadp
                   (check-commands--public-command-name-p name))
          name))))))

(defun check-commands--extract-file (file)
  "Return public command symbols found in FILE."
  (let (commands)
    (with-temp-buffer
      (insert-file-contents file)
      (emacs-lisp-mode)
      (goto-char (point-min))
      (while (not (eobp))
        (condition-case nil
            (let* ((comment-start (point))
                   (_ (forward-comment (point-max)))
                   (autoloadp
                    (string-match-p
                     ";;;###autoload"
                     (buffer-substring-no-properties
                      comment-start (point))))
                   (form (unless (eobp)
                           (read (current-buffer))))
                   (name (check-commands--command-form-name form autoloadp)))
              (when name
                (push name commands)))
          (end-of-file
           (goto-char (point-max)))
          (invalid-read-syntax
           (forward-char 1))
          (error
           (forward-char 1)))))
    commands))

(defun check-commands--extract-public (dir)
  "Extract public command names from Ellama sources in DIR."
  (let ((commands (copy-sequence check-commands--extra-public-commands)))
    (dolist (file (check-commands--source-files dir))
      (setq commands (append (check-commands--extract-file file) commands)))
    (dolist (command (sort (delete-dups commands)
                           (lambda (left right)
                             (string< (symbol-name left)
                                      (symbol-name right)))))
      (princ (format "%s\n" command)))))

(when (and noninteractive (getenv "ELLAMA_DIR"))
  (check-commands--extract-public (getenv "ELLAMA_DIR")))

(provide 'extract-commands)
;;; extract-commands.el ends here
