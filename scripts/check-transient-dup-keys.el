;;; check-transient-dup-keys.el --- Check transient menus for duplicate keys

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; This script reads ellama-transient.el as forms to find duplicate keys
;; within transient menus.  It does not load the project, because loading
;; depends on user packages and local runtime state.

;; Run with:
;;   ELLAMA_DIR=/path/to/project emacs -Q -batch -l scripts/check-transient-dup-keys.el

(require 'cl-lib)

(defun check-transient-dup-keys--parse-file (file)
  "Parse FILE looking for transient-define-prefix definitions.
Prints any duplicate keys found within each menu."
  (let ((menu-entries nil)
        (form-count 0)
        (prefix-count 0))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (emacs-lisp-mode)

      ;; Parse forms one by one
      (while (not (eobp))
        (condition-case err
            (progn
              (forward-comment (point-max))
              (unless (eobp)
                (let ((form (read (current-buffer))))
                  (setq form-count (1+ form-count))
                  (when (and (consp form)
                             (eq (car form) 'transient-define-prefix))
                    (setq prefix-count (1+ prefix-count))
                    (let* ((menu-name (cadr form))
                           (menu-body (cddr form))
                           (this-menu-keys
                            (check-transient-dup-keys--extract-keys menu-body)))
                      (message "Menu %s: found %d keys"
                               (symbol-name menu-name)
                               (length this-menu-keys))
                      (when this-menu-keys
                        (push (cons (symbol-name menu-name) this-menu-keys)
                              menu-entries)))))))
          (error (message "Error at position %d: %s" (point) err))))

      ;; Report results
      (message "Parsed %d forms, found %d transient-define-prefix definitions"
               form-count prefix-count)

      (when menu-entries
        (check-transient-dup-keys--report-duplicates menu-entries)))
    )
  )

(defun check-transient-dup-keys--is-key-entry (sexp)
  "Return t if SEXp looks like a transient key binding entry.
A key binding has the form (\"key\" \"description\" command ...)."
  (when (and (consp sexp)
             (>= (length sexp) 3)
             (stringp (car sexp))
             (stringp (cadr sexp)))
    (let ((key (car sexp)))
      (unless (string= key "-")
        t))))

(defun check-transient-dup-keys--get-binding (sexp)
  "Extract (key . command) from a transient key binding SEXp.
Returns a cons cell (key . command-symbol) or nil if not a valid binding."
  (let ((key (car sexp))
        (cmd nil))
    ;; Find the first symbol that's not a keyword (the command)
    (dolist (item (cdr sexp))
      (unless cmd
        (when (and (symbolp item)
                   (not (keywordp item)))
          (setq cmd item))))
    (when cmd
      (cons key cmd))))

(defun check-transient-dup-keys--extract-keys (sexp)
  "Extract key-command pairs from SEXp recursively.
Traverses nested lists and vectors to find transient entry patterns.
Returns list of (key . command) cons cells."
  (let ((result nil))
    ;; Check if this is a key binding entry (list case)
    (when (check-transient-dup-keys--is-key-entry sexp)
      (let ((binding (check-transient-dup-keys--get-binding sexp)))
        (when binding
          (push binding result))))
    
    ;; Recurse into all elements of lists and vectors
    (cond
     ;; Handle vectors (transient menus use vectors for sections)
     ((vectorp sexp)
      (dotimes (i (length sexp))
        (let ((item (aref sexp i)))
          (when (or (consp item) (vectorp item))
            (let ((sub-keys (check-transient-dup-keys--extract-keys item)))
              (setq result (append sub-keys result)))))))
     
     ;; Handle lists
     ((consp sexp)
      (dolist (item sexp)
        (when (or (consp item) (vectorp item))
          (let ((sub-keys (check-transient-dup-keys--extract-keys item)))
            (setq result (append sub-keys result)))))))
    
    (nreverse result)))

(defun check-transient-dup-keys--report-duplicates (menu-entries)
  "Report duplicate keys from MENU-ENTRIES.
MENU-ENTRIES is (menu-name (key . command) ...)."
  (let ((found-any nil))
    (dolist (menu menu-entries)
      (let* ((menu-name (car menu))
             (keys (cdr menu))
             (key-maps (check-transient-dup-keys--find-duplicates keys)))
        (when key-maps
          (setq found-any t)
          (message "=== %s ===" menu-name)
          (dolist (dup key-maps)
            (message "  key '%s: %s" (car dup)
                     (mapconcat #'identity (cdr dup) ", "))))))
    (unless found-any
      (message "No duplicate keys found."))))

(defun check-transient-dup-keys--find-duplicates (keys)
  "Find duplicate keys in KEYS list.
KEYS is a list of (key . command) cons cells.
Returns list of (key string string ...) where strings are command names."
  (let ((seen (make-hash-table :test #'equal))
        (results nil))
    (dolist (key-entry keys)
      (let* ((key (car key-entry))
             (cmd (cdr key-entry))
             (cmd-name (symbol-name cmd)))
        (if (gethash key seen)
            ;; Duplicate - add to list if not already there
            (let ((existing (gethash key seen)))
              (unless (member cmd-name existing)
                (puthash key (cons cmd-name existing) seen)))
          ;; First occurrence - store as single-element list
          (puthash key (list cmd-name) seen))))
    (maphash (lambda (key cmd-list)
               (when (cdr cmd-list)
                 (push (cons key (nreverse cmd-list)) results)))
             seen)
    results))

(when (and noninteractive (getenv "ELLAMA_DIR"))
  (let* ((dir (getenv "ELLAMA_DIR"))
         (transient-file (expand-file-name "ellama-transient.el" dir)))
    (when (file-exists-p transient-file)
      (check-transient-dup-keys--parse-file transient-file))))

(provide 'check-transient-dup-keys)
;;; check-transient-dup-keys.el ends here
