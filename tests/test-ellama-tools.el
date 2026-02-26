;;; test-ellama-tools.el --- Ellama tools tests -*- lexical-binding: t; package-lint-main-file: "../ellama.el"; -*-

;; Copyright (C) 2023-2026  Free Software Foundation, Inc.

;; Author: Sergey Kostyaev <sskostyaev@gmail.com>

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
;; Ellama tools tests.
;;

;;; Code:

(require 'cl-lib)
(require 'ellama)
(require 'ert)

(defconst ellama-test-root
  (expand-file-name
   ".."
   (file-name-directory (or load-file-name buffer-file-name)))
  "Project root directory for test assets.")

(ert-deftest test-ellama--append-tool-error-to-prompt-uses-llm-message ()
  (let (captured)
    (cl-letf (((symbol-function 'llm-chat-prompt-append-response)
               (lambda (_prompt msg role)
                 (setq captured (list msg role)))))
      (ellama--append-tool-error-to-prompt
       'prompt
       "Unknown tool 'search' called"))
    (should (equal captured
                   '("Unknown tool 'search' called" system)))))

(ert-deftest test-ellama--tool-call-error-p ()
  (unless (get 'ellama-test-tool-call-error-2 'error-conditions)
    (define-error 'ellama-test-tool-call-error-2
                  "Tool call test error"
                  'llm-tool-call-error))
  (should (ellama--tool-call-error-p 'ellama-test-tool-call-error-2))
  (should-not (ellama--tool-call-error-p 'error))
  (should-not (ellama--tool-call-error-p nil)))

(ert-deftest test-ellama--error-handler-retry-on-tool-call-error ()
  (unless (get 'ellama-test-tool-call-error-3 'error-conditions)
    (define-error 'ellama-test-tool-call-error-3
                  "Tool call retry error"
                  'llm-tool-call-error))
  (let ((retry-called nil)
        (err-called nil)
        (appended nil))
    (with-temp-buffer
      (cl-letf (((symbol-function 'ellama--append-tool-error-to-prompt)
                 (lambda (_prompt msg)
                   (setq appended msg))))
        (let ((handler
               (ellama--error-handler
                (current-buffer)
                (lambda (_msg) (setq err-called t))
                'prompt
                (lambda () (setq retry-called t)))))
          (funcall handler 'ellama-test-tool-call-error-3 "tool failed"))))
    (should retry-called)
    (should-not err-called)
    (should (equal appended "tool failed"))))

(ert-deftest test-ellama--error-handler-calls-errcb-for-non-tool-errors ()
  (let ((err-msg nil)
        (request-mode-arg nil)
        (spinner-stop-called nil)
        (ellama--change-group (prepare-change-group))
        (ellama-spinner-enabled t))
    (with-temp-buffer
      (setq-local ellama--current-request 'request)
      (activate-change-group ellama--change-group)
      (cl-letf (((symbol-function 'cancel-change-group)
                 (lambda (_cg) nil))
                ((symbol-function 'spinner-stop)
                 (lambda () (setq spinner-stop-called t)))
                ((symbol-function 'ellama-request-mode)
                 (lambda (arg)
                   (setq request-mode-arg arg))))
        (let ((handler
               (ellama--error-handler
                (current-buffer)
                (lambda (msg) (setq err-msg msg))
                'prompt
                (lambda () (error "Retry should not run")))))
          (funcall handler 'error "bad")))
      (should (null ellama--current-request)))
    (should (equal err-msg "bad"))
    (should (equal request-mode-arg -1))
    (should spinner-stop-called)))
(defun ellama-test--ensure-local-ellama-tools ()
  "Ensure tests use local `ellama-tools.el' from project root."
  (unless (and (fboundp 'ellama-tools--sanitize-tool-text-output)
               (fboundp 'ellama-tools--command-argv))
    (load-file (expand-file-name "ellama-tools.el" ellama-test-root))))

(defun ellama-test--clear-srt-policy-cache ()
  "Clear local `srt' policy cache used by tool tests."
  (ellama-test--ensure-local-ellama-tools)
  (if (fboundp 'ellama-tools--srt-policy-clear-cache)
      (ellama-tools--srt-policy-clear-cache)
    (setq ellama-tools--srt-policy-cache nil)))

(defmacro ellama-test--with-temp-srt-settings (json &rest body)
  "Run BODY with SETTINGS-FILE bound to a temp `srt' config JSON string."
  (declare (indent 1))
  `(let ((settings-file (make-temp-file "ellama-srt-settings-" nil ".json")))
     (unwind-protect
         (progn
           (with-temp-file settings-file
             (insert ,json))
           (ellama-test--clear-srt-policy-cache)
           ,@body)
       (ellama-test--clear-srt-policy-cache)
       (when (file-exists-p settings-file)
         (delete-file settings-file)))))

(defun ellama-test--wait-shell-command-result (cmd)
  "Run shell tool CMD and wait for a result string."
  (ellama-test--ensure-local-ellama-tools)
  (let ((result :pending)
        (deadline (+ (float-time) 3.0)))
    (ellama-tools-shell-command-tool
     (lambda (res)
       (setq result res))
     cmd)
    (while (and (eq result :pending)
                (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (when (eq result :pending)
      (ert-fail (format "Timeout while waiting result for: %s" cmd)))
    result))

(defun ellama-test--named-tool-no-args ()
  "Return constant string."
  "zero")

(defun ellama-test--named-tool-one-arg (arg)
  "Return ARG with prefix."
  (format "one:%s" arg))

(defun ellama-test--named-tool-two-args (arg1 arg2)
  "Return ARG1 and ARG2 with prefix."
  (format "two:%s:%s" arg1 arg2))

(defun ellama-test--make-confirm-wrapper-old (function)
  "Make wrapper for FUNCTION using old confirm call style."
  (lambda (&rest args)
    (apply #'ellama-tools-confirm function args)))

(defun ellama-test--make-confirm-wrapper-new (function name)
  "Make wrapper for FUNCTION and NAME using wrapper factory."
  (ellama-tools--make-confirm-wrapper function name))

(defun ellama-test--invoke-confirm-with-yes (wrapper &rest args)
  "Call WRAPPER with ARGS and auto-answer confirmation with yes.
Return list with result and prompt."
  (let ((ellama-tools-confirm-allowed (make-hash-table))
        (ellama-tools-allow-all nil)
        (ellama-tools-allowed nil)
        result
        prompt)
    (cl-letf (((symbol-function 'read-char-choice)
               (lambda (message _choices)
                 (setq prompt message)
                 ?y)))
      (setq result (apply wrapper args)))
    (list result prompt)))

(ert-deftest test-ellama-shell-command-tool-empty-success-output ()
  (should
   (string=
    (ellama-test--wait-shell-command-result "sh -c 'true'")
    "Command completed successfully with no output.")))

(ert-deftest test-ellama-shell-command-tool-empty-failure-output ()
  (should
   (string-match-p
    "Command failed with exit code 7 and no output\\."
    (ellama-test--wait-shell-command-result "sh -c 'exit 7'"))))

(ert-deftest test-ellama-shell-command-tool-returns-stdout ()
  (should
   (string=
    (ellama-test--wait-shell-command-result "printf 'ok\\n'")
    "ok")))

(ert-deftest test-ellama-shell-command-tool-rejects-binary-output ()
  (should
   (string-match-p
    "binary data"
    (ellama-test--wait-shell-command-result
     "awk 'BEGIN { printf \"%c\", 0 }'"))))

(ert-deftest test-ellama-tools-command-argv-wraps-with-srt ()
  (ellama-test--ensure-local-ellama-tools)
  (let ((ellama-tools-use-srt t)
        (ellama-tools-srt-program "srt")
        (ellama-tools-srt-args '("--debug")))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_program) "/tmp/fake-srt")))
      (should
       (equal
        (ellama-tools--command-argv "sh" "-c" "printf ok")
        '("/tmp/fake-srt" "--debug" "sh" "-c" "printf ok"))))))

(ert-deftest test-ellama-shell-command-tool-errors-when-srt-missing ()
  (ellama-test--ensure-local-ellama-tools)
  (let ((ellama-tools-use-srt t)
        (ellama-tools-srt-program "srt")
        callback-called)
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_program) nil)))
      (should-error
       (ellama-tools-shell-command-tool
        (lambda (_result)
          (setq callback-called t))
        "printf ok")
       :type 'user-error))
    (should-not callback-called)))

(ert-deftest test-ellama-tools-srt-settings-file-resolution ()
  (ellama-test--ensure-local-ellama-tools)
  (should (equal (let ((ellama-tools-srt-args nil))
                   (ellama-tools--srt-settings-file))
                 (expand-file-name "~/.srt-settings.json")))
  (should (equal (let ((ellama-tools-srt-args '("--settings" "/tmp/a.json")))
                   (ellama-tools--srt-settings-file))
                 "/tmp/a.json"))
  (should (equal (let ((ellama-tools-srt-args '("-s" "/tmp/b.json")))
                   (ellama-tools--srt-settings-file))
                 "/tmp/b.json"))
  (should (equal (let ((ellama-tools-srt-args '("--settings=/tmp/c.json")))
                   (ellama-tools--srt-settings-file))
                 "/tmp/c.json")))

(ert-deftest test-ellama-tools-srt-settings-file-errors-on-missing-value ()
  (ellama-test--ensure-local-ellama-tools)
  (should-error
  (let ((ellama-tools-srt-args '("-s")))
     (ellama-tools--srt-settings-file))
   :type 'user-error)
  (should-error
   (let ((ellama-tools-srt-args '("--settings")))
     (ellama-tools--srt-settings-file))
   :type 'user-error))

(ert-deftest test-ellama-tools-srt-policy-load-invalid-json ()
  (ellama-test--ensure-local-ellama-tools)
  (ellama-test--with-temp-srt-settings
      "{not-json"
    (let ((ellama-tools-srt-args (list "--settings" settings-file)))
      (should-error (ellama-tools--srt-policy-current)
                    :type 'user-error))))

(ert-deftest test-ellama-tools-srt-policy-load-malformed-filesystem-shape ()
  (ellama-test--ensure-local-ellama-tools)
  (ellama-test--with-temp-srt-settings
      "{\"filesystem\":{\"denyRead\":\"/tmp/x\"}}"
    (let ((ellama-tools-srt-args (list "--settings" settings-file)))
      (should-error (ellama-tools--srt-policy-current)
                    :type 'user-error))))

(ert-deftest test-ellama-tools-srt-check-access-read-write-and-precedence ()
  (ellama-test--ensure-local-ellama-tools)
  (let* ((dir (make-temp-file "ellama-srt-policy-" t))
         (secret (expand-file-name "secret.txt" dir))
         (work-dir (expand-file-name "work" dir))
         (allowed-file (expand-file-name "note.txt" work-dir))
         (blocked-file (expand-file-name "blocked.txt" work-dir)))
    (unwind-protect
        (progn
          (make-directory work-dir)
          (with-temp-file secret (insert "secret"))
          (with-temp-file blocked-file (insert "blocked"))
          (ellama-test--with-temp-srt-settings
              (format
               "{\"filesystem\":{\"denyRead\":[%S],\"allowWrite\":[%S],\"denyWrite\":[%S]}}"
               secret work-dir blocked-file)
            (let ((default-directory dir)
                  (ellama-tools-use-srt t)
                  (ellama-tools-srt-args (list "--settings" settings-file)))
              (should (string-match-p "denyRead"
                                      (ellama-tools--srt-check-access
                                       secret 'read)))
              (should-not (ellama-tools--srt-check-access
                           allowed-file 'write))
              (should (string-match-p "denyWrite"
                                      (ellama-tools--srt-check-access
                                       blocked-file 'write))))))
      (when (file-exists-p dir)
        (delete-directory dir t)))))

(ert-deftest
    test-ellama-tools-srt-check-access-denywrite-literal-new-file-parity ()
  (ellama-test--ensure-local-ellama-tools)
  (let* ((dir (make-temp-file "ellama-srt-denywrite-new-" t))
         (work-dir (expand-file-name "work" dir))
         (blocked-file (expand-file-name "blocked.txt" work-dir)))
    (unwind-protect
        (progn
          (make-directory work-dir)
          (ellama-test--with-temp-srt-settings
              (format
               "{\"filesystem\":{\"allowWrite\":[%S],\"denyWrite\":[%S]}}"
               work-dir blocked-file)
            (let ((default-directory dir)
                  (ellama-tools-use-srt t)
                  (ellama-tools-srt-args (list "--settings" settings-file)))
              (should-not (file-exists-p blocked-file))
              ;; Parity note: behavior differs by platform.
              (if (eq system-type 'darwin)
                  (should-not (ellama-tools--srt-check-access
                               blocked-file 'write))
                (should (string-match-p
                         "denyWrite"
                         (ellama-tools--srt-check-access
                          blocked-file 'write)))))))
      (when (file-exists-p dir)
        (delete-directory dir t)))))

(ert-deftest test-ellama-tools-srt-check-access-default-write-deny ()
  (ellama-test--ensure-local-ellama-tools)
  (let ((dir (make-temp-file "ellama-srt-defaults-" t)))
    (unwind-protect
        (ellama-test--with-temp-srt-settings
            "{}"
          (let ((default-directory dir)
                (ellama-tools-use-srt t)
                (ellama-tools-srt-args (list "--settings" settings-file)))
            (should-not (ellama-tools--srt-check-access "missing.txt" 'read))
            (should (string-match-p
                     "allowWrite"
                     (ellama-tools--srt-check-access "missing.txt" 'write)))))
      (when (file-exists-p dir)
        (delete-directory dir t)))))

(ert-deftest test-ellama-tools-srt-check-access-resolves-relative-rules ()
  (ellama-test--ensure-local-ellama-tools)
  (let* ((dir (make-temp-file "ellama-srt-relative-" t))
         (file (expand-file-name "secret.txt" dir)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "x"))
          (ellama-test--with-temp-srt-settings
              "{\"filesystem\":{\"denyRead\":[\"secret.txt\"]}}"
            (let ((default-directory dir)
                  (ellama-tools-use-srt t)
                  (ellama-tools-srt-args (list "--settings" settings-file)))
              (should (string-match-p "denyRead"
                                      (ellama-tools--srt-check-access
                                       file 'read))))))
      (when (file-exists-p dir)
        (delete-directory dir t)))))

(ert-deftest test-ellama-tools-srt-check-access-normalizes-new-write-path ()
  (ellama-test--ensure-local-ellama-tools)
  (let* ((dir (make-temp-file "ellama-srt-new-write-" t))
         (out-dir (expand-file-name "out" dir))
         (target (expand-file-name "new.txt" out-dir)))
    (unwind-protect
        (progn
          (make-directory out-dir)
          (ellama-test--with-temp-srt-settings
              (format "{\"filesystem\":{\"allowWrite\":[%S]}}" out-dir)
            (let ((default-directory dir)
                  (ellama-tools-use-srt t)
                  (ellama-tools-srt-args (list "--settings" settings-file)))
              (should-not (file-exists-p target))
              (should-not (ellama-tools--srt-check-access target 'write)))))
      (when (file-exists-p dir)
        (delete-directory dir t)))))

(ert-deftest
    test-ellama-tools-srt-check-access-keeps-missing-parent-segments ()
  (ellama-test--ensure-local-ellama-tools)
  (let* ((dir (make-temp-file "ellama-srt-nested-write-" t))
         (out-dir (expand-file-name "out" dir))
         (target (expand-file-name "deep/path/new.txt" out-dir)))
    (unwind-protect
        (progn
          (make-directory out-dir)
          (ellama-test--with-temp-srt-settings
              (format "{\"filesystem\":{\"allowWrite\":[%S]}}" target)
            (let ((default-directory dir)
                  (ellama-tools-use-srt t)
                  (ellama-tools-srt-args (list "--settings" settings-file)))
              (should-not (file-exists-p target))
              (should-not (ellama-tools--srt-check-access target 'write)))))
      (when (file-exists-p dir)
        (delete-directory dir t)))))

(ert-deftest test-ellama-tools-srt-check-access-expands-tilde ()
  (ellama-test--ensure-local-ellama-tools)
  (let* ((fake-home (make-temp-file "ellama-srt-fake-home-" t))
         (dir (expand-file-name "project" fake-home))
         (file (expand-file-name "secret.txt" dir)))
    (unwind-protect
        (progn
          (make-directory dir)
          (with-temp-file file (insert "x"))
          (let* ((orig-expand (symbol-function 'expand-file-name))
                 (tilde-rule (concat "~/" (file-relative-name file fake-home))))
            (cl-letf (((symbol-function 'expand-file-name)
                       (lambda (name &optional base)
                         (if (and (stringp name)
                                  (string-prefix-p "~/" name))
                             (funcall orig-expand
                                      (concat fake-home "/" (substring name 2))
                                      base)
                           (funcall orig-expand name base)))))
              (ellama-test--with-temp-srt-settings
                  (format "{\"filesystem\":{\"denyRead\":[%S]}}" tilde-rule)
                (let ((default-directory temporary-file-directory)
                      (ellama-tools-use-srt t)
                      (ellama-tools-srt-args
                       (list "--settings" settings-file)))
                  (should (string-match-p "denyRead"
                                          (ellama-tools--srt-check-access
                                           file 'read))))))))
      (when (file-exists-p fake-home)
        (delete-directory fake-home t)))))

(ert-deftest
    test-ellama-tools-srt-check-access-darwin-symlink-rule-parity ()
  (ellama-test--ensure-local-ellama-tools)
  (let* ((dir (make-temp-file "ellama-srt-symlink-rule-" t))
         (real (expand-file-name "real.txt" dir))
         (link (expand-file-name "link.txt" dir)))
    (unwind-protect
        (progn
          (with-temp-file real (insert "x"))
          (condition-case err
              (make-symbolic-link real link)
            (file-error
             (ert-skip (format "Cannot create symlink: %s"
                               (error-message-string err)))))
          (ellama-test--with-temp-srt-settings
              (format "{\"filesystem\":{\"denyRead\":[%S]}}" link)
            (let ((default-directory dir)
                  (ellama-tools-use-srt t)
                  (ellama-tools-srt-args (list "--settings" settings-file)))
              ;; Observed host parity on macOS: exact symlink-path denyRead
              ;; rules may not deny reads through the symlink path.
              (let ((system-type 'darwin))
                (should-not (ellama-tools--srt-check-access link 'read))))
            (let ((default-directory dir)
                  (ellama-tools-use-srt t)
                  (ellama-tools-srt-args (list "--settings" settings-file)))
              (let ((system-type 'gnu/linux))
                (should (string-match-p
                         "denyRead"
                         (ellama-tools--srt-check-access link 'read)))))))
      (when (file-exists-p dir)
        (delete-directory dir t)))))

(ert-deftest test-ellama-tools-read-file-tool-denied-by-srt-policy ()
  (ellama-test--ensure-local-ellama-tools)
  (let* ((dir (make-temp-file "ellama-srt-read-tool-" t))
         (file (expand-file-name "secret.txt" dir)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "secret"))
          (ellama-test--with-temp-srt-settings
              (format "{\"filesystem\":{\"denyRead\":[%S]}}" file)
            (let ((default-directory dir)
                  (ellama-tools-use-srt t)
                  (ellama-tools-srt-args (list "--settings" settings-file)))
              (should-error (ellama-tools-read-file-tool file)
                            :type 'user-error))))
      (when (file-exists-p dir)
        (delete-directory dir t)))))

(ert-deftest test-ellama-tools-write-file-tool-denied-by-srt-defaults ()
  (ellama-test--ensure-local-ellama-tools)
  (let ((dir (make-temp-file "ellama-srt-write-tool-" t)))
    (unwind-protect
        (ellama-test--with-temp-srt-settings
            "{}"
          (let ((default-directory dir)
                (ellama-tools-use-srt t)
                (ellama-tools-srt-args (list "--settings" settings-file)))
            (should-error
             (ellama-tools-write-file-tool
              (expand-file-name "x.txt" dir) "x")
             :type 'user-error)))
      (when (file-exists-p dir)
        (delete-directory dir t)))))

(ert-deftest test-ellama-tools-directory-tree-tool-denied-by-srt-policy ()
  (ellama-test--ensure-local-ellama-tools)
  (let ((dir (make-temp-file "ellama-srt-tree-tool-" t)))
    (unwind-protect
        (ellama-test--with-temp-srt-settings
            (format "{\"filesystem\":{\"denyRead\":[%S]}}" dir)
          (let ((default-directory temporary-file-directory)
                (ellama-tools-use-srt t)
                (ellama-tools-srt-args (list "--settings" settings-file)))
            (should-error (ellama-tools-directory-tree-tool dir)
                          :type 'user-error)))
      (when (file-exists-p dir)
        (delete-directory dir t)))))

(ert-deftest test-ellama-tools-move-file-tool-denies-destination-write ()
  (ellama-test--ensure-local-ellama-tools)
  (let* ((dir (make-temp-file "ellama-srt-move-tool-" t))
         (src-dir (expand-file-name "src" dir))
         (dst-dir (expand-file-name "dst" dir))
         (src (expand-file-name "a.txt" src-dir))
         (dst (expand-file-name "b.txt" dst-dir))
         err-msg)
    (unwind-protect
        (progn
          (make-directory src-dir)
          (make-directory dst-dir)
          (with-temp-file src (insert "x"))
          (ellama-test--with-temp-srt-settings
              (format "{\"filesystem\":{\"allowWrite\":[%S]}}" src-dir)
            (let ((default-directory dir)
                  (ellama-tools-use-srt t)
                  (ellama-tools-srt-args (list "--settings" settings-file)))
              (setq err-msg
                    (condition-case err
                        (progn
                          (ellama-tools-move-file-tool src dst)
                          nil)
                      (user-error (error-message-string err))))
              (should (stringp err-msg))
              (should (string-match-p (regexp-quote dst) err-msg)))))
      (when (file-exists-p dir)
        (delete-directory dir t)))))

(ert-deftest
    test-ellama-tools-move-file-tool-nested-destination-policy-allows ()
  (ellama-test--ensure-local-ellama-tools)
  (let* ((dir (make-temp-file "ellama-srt-move-nested-" t))
         (src-dir (expand-file-name "src" dir))
         (src (expand-file-name "a.txt" src-dir))
         (dst (expand-file-name "deep/path/b.txt" dir))
         err-sym)
    (unwind-protect
        (progn
          (make-directory src-dir)
          (with-temp-file src (insert "x"))
          (ellama-test--with-temp-srt-settings
              (format
               "{\"filesystem\":{\"allowWrite\":[%S,%S]}}"
               src dst)
            (let ((default-directory dir)
                  (ellama-tools-use-srt t)
                  (ellama-tools-srt-args (list "--settings" settings-file)))
              (setq err-sym
                    (car (should-error
                          (ellama-tools-move-file-tool src dst))))
              ;; Missing destination directories should fail in rename-file,
              ;; not in local SRT policy checks.
              (should (memq 'file-error
                            (get err-sym 'error-conditions)))
              (should-not (memq 'user-error
                                (get err-sym 'error-conditions))))))
      (when (file-exists-p dir)
        (delete-directory dir t)))))

(ert-deftest test-ellama-tools-grep-tool-uses-shared-command-helper ()
  (ellama-test--ensure-local-ellama-tools)
  (let (captured)
    (cl-letf (((symbol-function 'ellama-tools--call-command-to-string)
               (lambda (&rest args)
                 (setq captured args)
                 "a:1:match\n")))
      (should (equal (ellama-tools-grep-tool default-directory "match")
                     "\"a:1:match\"")))
    (should (equal captured
                   '("find" "." "-type" "f" "-exec"
                     "grep" "--color=never" "-nH" "-e" "match" "{}" "+")))))

(ert-deftest test-ellama-tools-grep-in-file-tool-uses-shared-command-helper ()
  (ellama-test--ensure-local-ellama-tools)
  (let ((file (make-temp-file "ellama-grep-in-file-"))
        truename
        captured)
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "hello\n"))
          (setq truename (file-truename file))
          (cl-letf (((symbol-function 'ellama-tools--call-command-to-string)
                     (lambda (&rest args)
                       (setq captured args)
                       "1:hello\n")))
            (should (equal (ellama-tools-grep-in-file-tool "hello" file)
                           "\"1:hello\\n\""))))
      (when (file-exists-p file)
        (delete-file file)))
    (should (equal captured
                   (list "grep" "--color=never" "-nh"
                         "hello" truename)))))

(ert-deftest test-ellama-read-file-tool-rejects-binary-content ()
  (ellama-test--ensure-local-ellama-tools)
  (let ((file (make-temp-file "ellama-read-file-bin-")))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'no-conversion))
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (insert "%PDF-1.5\n%")
              (insert (char-to-string 143))
              (insert "\n")
              (write-region (point-min) (point-max) file nil 'silent)))
          (let ((result (ellama-tools-read-file-tool file)))
            (should (string-match-p "binary data" result))
            (should (string-match-p "bad idea" result))))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest test-ellama-read-file-tool-accepts-utf8-markdown-text ()
  (ellama-test--ensure-local-ellama-tools)
  (let ((file (make-temp-file "ellama-read-file-utf8-" nil ".md")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "# Research Plan\n\n")
            (insert "Sub‑topics: temporal reasoning overview.\n"))
          (let ((result (ellama-tools-read-file-tool file)))
            (should-not (string-match-p "binary data" result))
            (should (string-match-p "Research Plan" result))))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest test-ellama-tools-confirm-wrapped-named-no-args-old-and-new ()
  (ellama-test--ensure-local-ellama-tools)
  (let* ((old-wrapper (ellama-test--make-confirm-wrapper-old
                       #'ellama-test--named-tool-no-args))
         (new-wrapper (ellama-test--make-confirm-wrapper-new
                       #'ellama-test--named-tool-no-args
                       "named_tool"))
         (old-call (ellama-test--invoke-confirm-with-yes old-wrapper))
         (new-call (ellama-test--invoke-confirm-with-yes new-wrapper)))
    (should (equal (car old-call) "zero"))
    (should (equal (car new-call) "zero"))
    (should
     (string-match-p
      "Allow calling ellama-test--named-tool-no-args with arguments: \\?"
      (cadr old-call)))
    (should
     (string-match-p
      "Allow calling ellama-test--named-tool-no-args with arguments: \\?"
      (cadr new-call)))))

(ert-deftest test-ellama-tools-confirm-wrapped-named-one-arg-old-and-new ()
  (ellama-test--ensure-local-ellama-tools)
  (let* ((old-wrapper (ellama-test--make-confirm-wrapper-old
                       #'ellama-test--named-tool-one-arg))
         (new-wrapper (ellama-test--make-confirm-wrapper-new
                       #'ellama-test--named-tool-one-arg
                       "named_tool"))
         (old-call (ellama-test--invoke-confirm-with-yes old-wrapper "A"))
         (new-call (ellama-test--invoke-confirm-with-yes new-wrapper "A")))
    (should (equal (car old-call) "one:A"))
    (should (equal (car new-call) "one:A"))
    (should
     (string-match-p
      "Allow calling ellama-test--named-tool-one-arg with arguments: A\\?"
      (cadr old-call)))
    (should
     (string-match-p
      "Allow calling ellama-test--named-tool-one-arg with arguments: A\\?"
      (cadr new-call)))))

(ert-deftest test-ellama-tools-confirm-wrapped-named-two-args-old-and-new ()
  (ellama-test--ensure-local-ellama-tools)
  (let* ((old-wrapper (ellama-test--make-confirm-wrapper-old
                       #'ellama-test--named-tool-two-args))
         (new-wrapper (ellama-test--make-confirm-wrapper-new
                       #'ellama-test--named-tool-two-args
                       "named_tool"))
         (old-call (ellama-test--invoke-confirm-with-yes old-wrapper "A" "B"))
         (new-call (ellama-test--invoke-confirm-with-yes new-wrapper "A" "B")))
    (should (equal (car old-call) "two:A:B"))
    (should (equal (car new-call) "two:A:B"))
    (should
     (string-match-p
      "Allow calling ellama-test--named-tool-two-args with arguments: A, B\\?"
      (cadr old-call)))
    (should
     (string-match-p
      "Allow calling ellama-test--named-tool-two-args with arguments: A, B\\?"
      (cadr new-call)))))

(ert-deftest test-ellama-tools-confirm-prompt-uses-tool-name-for-lambda ()
  (ellama-test--ensure-local-ellama-tools)
  (let* ((ellama-tools-confirm-allowed (make-hash-table))
         (ellama-tools-allow-all nil)
         (ellama-tools-allowed nil)
         (tool-plist `(:function ,(lambda (_arg) "ok")
                                 :name "mcp_tool"
                                 :args ((:name "arg" :type string))))
         (wrapped (ellama-tools-wrap-with-confirm tool-plist))
         (wrapped-func (plist-get wrapped :function))
         seen-prompt)
    (cl-letf (((symbol-function 'read-char-choice)
               (lambda (prompt _choices)
                 (setq seen-prompt prompt)
                 ?n)))
      (funcall wrapped-func "value"))
    (should
     (string-match-p
      "Allow calling mcp_tool with arguments: value\\?"
      seen-prompt))))

(ert-deftest test-ellama-tools-wrap-with-confirm-preserves-arg-types ()
  (ellama-test--ensure-local-ellama-tools)
  (let* ((tool-plist '(:function ignore
                                 :name "typed_tool"
                                 :args ((:name "a" :type string)
                                        (:name "b" :type number))))
         (wrapped (ellama-tools-wrap-with-confirm tool-plist))
         (types (mapcar (lambda (arg) (plist-get arg :type))
                        (plist-get wrapped :args))))
    (should (equal types '(string number)))))

(ert-deftest test-ellama-tools-edit-file-tool-replace-at-file-start ()
  (ellama-test--ensure-local-ellama-tools)
  (let ((file (make-temp-file "ellama-edit-start-")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "abcde"))
          (ellama-tools-edit-file-tool file "ab" "XX")
          (with-temp-buffer
            (insert-file-contents file)
            (should (equal (buffer-string) "XXcde"))))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest
    test-ellama-tools-enable-by-name-tool-missing-name-does-not-add-nil ()
  (ellama-test--ensure-local-ellama-tools)
  (let ((ellama-tools-enabled nil)
        (ellama-tools-available nil))
    (ellama-tools-enable-by-name-tool "missing")
    (should (null ellama-tools-enabled))))

(ert-deftest test-ellama-tools-confirm-answer-always-caches-approval ()
  (ellama-test--ensure-local-ellama-tools)
  (let ((ellama-tools-confirm-allowed (make-hash-table))
        (ellama-tools-allow-all nil)
        (ellama-tools-allowed nil)
        (prompt-count 0))
    (cl-letf (((symbol-function 'read-char-choice)
               (lambda (_prompt _choices)
                 (setq prompt-count (1+ prompt-count))
                 ?a)))
      (should (equal (ellama-tools-confirm 'ellama-test--named-tool-one-arg "A")
                     "one:A"))
      (should (equal (ellama-tools-confirm 'ellama-test--named-tool-one-arg "B")
                     "one:B")))
    (should (= prompt-count 1))
    (should (gethash 'ellama-test--named-tool-one-arg
                     ellama-tools-confirm-allowed))))

(ert-deftest test-ellama-tools-confirm-answer-reply-returns-user-text ()
  (ellama-test--ensure-local-ellama-tools)
  (let ((ellama-tools-confirm-allowed (make-hash-table))
        (ellama-tools-allow-all nil)
        (ellama-tools-allowed nil))
    (cl-letf (((symbol-function 'read-char-choice)
               (lambda (_prompt _choices) ?r))
              ((symbol-function 'read-string)
               (lambda (_prompt &rest _args) "custom reply")))
      (should (equal
               (ellama-tools-confirm 'ellama-test--named-tool-one-arg "A")
               "custom reply")))))

(ert-deftest test-ellama-tools-confirm-answer-no-returns-forbidden ()
  (ellama-test--ensure-local-ellama-tools)
  (let ((ellama-tools-confirm-allowed (make-hash-table))
        (ellama-tools-allow-all nil)
        (ellama-tools-allowed nil))
    (cl-letf (((symbol-function 'read-char-choice)
               (lambda (_prompt _choices) ?n)))
      (should (equal
               (ellama-tools-confirm 'ellama-test--named-tool-one-arg "A")
               "Forbidden by the user")))))

(ert-deftest test-ellama-read-file-tool-missing-file ()
  (ellama-test--ensure-local-ellama-tools)
  (let ((missing-file
         (expand-file-name "missing-file-ellama-test.txt"
                           (make-temp-name temporary-file-directory))))
    (should (string-match-p "doesn't exists"
                            (ellama-tools-read-file-tool missing-file)))))

(ert-deftest test-ellama-tools-write-append-prepend-roundtrip ()
  (ellama-test--ensure-local-ellama-tools)
  (let ((file (make-temp-file "ellama-file-tools-")))
    (unwind-protect
        (progn
          (ellama-tools-write-file-tool file "middle")
          (ellama-tools-append-file-tool file "-tail")
          (ellama-tools-prepend-file-tool file "head-")
          (with-temp-buffer
            (insert-file-contents file)
            (should (equal (buffer-string) "head-middle-tail"))))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest test-ellama-tools-directory-tree-excludes-dotfiles-and-sorts ()
  (ellama-test--ensure-local-ellama-tools)
  (let* ((dir (make-temp-file "ellama-tree-" t))
         (a-file (expand-file-name "a.txt" dir))
         (b-file (expand-file-name "b.txt" dir))
         (hidden (expand-file-name ".hidden" dir))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file b-file (insert "b"))
          (with-temp-file a-file (insert "a"))
          (with-temp-file hidden (insert "h"))
          (setq result (ellama-tools-directory-tree-tool dir))
          (should-not (string-match-p "\\.hidden" result))
          (should (< (string-match-p "a\\.txt" result)
                     (string-match-p "b\\.txt" result))))
      (when (file-exists-p dir)
        (delete-directory dir t)))))

(ert-deftest test-ellama-tools-move-file-success-and-error ()
  (ellama-test--ensure-local-ellama-tools)
  (let* ((src (make-temp-file "ellama-move-src-"))
         (dst (concat src "-dst")))
    (unwind-protect
        (progn
          (with-temp-file src (insert "x"))
          (ellama-tools-move-file-tool src dst)
          (should (file-exists-p dst))
          (should-not (file-exists-p src))
          (should-error (ellama-tools-move-file-tool src dst) :type 'error))
      (when (file-exists-p src)
        (delete-file src))
      (when (file-exists-p dst)
        (delete-file dst)))))

(ert-deftest test-ellama-tools-lines-range-boundary ()
  (ellama-test--ensure-local-ellama-tools)
  (let ((file (make-temp-file "ellama-lines-range-")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "alpha\nbeta\ngamma\n"))
          (let ((single-line
                 (json-parse-string
                  (ellama-tools-lines-range-tool file 2 2)))
                (full-range
                 (json-parse-string
                  (ellama-tools-lines-range-tool file 1 3))))
            (should (equal single-line "beta"))
            (should (equal full-range "alpha\nbeta\ngamma"))))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest test-ellama-tools-role-and-provider-resolution ()
  (ellama-test--ensure-local-ellama-tools)
  (let* ((ellama-provider 'default-provider)
         (ellama-tools-subagent-roles
          (list (list "all" :tools :all)
                (list "subset" :tools '("read_file" "task"))))
         (ellama-tools-available
          (list (llm-make-tool :name "task" :function #'ignore)
                (llm-make-tool :name "read_file" :function #'ignore)
                (llm-make-tool :name "grep" :function #'ignore))))
    (should-not
     (member "task"
             (mapcar #'llm-tool-name (ellama-tools--for-role "all"))))
    (should (equal
             (mapcar #'llm-tool-name (ellama-tools--for-role "subset"))
             '("task" "read_file")))
    (should (null (ellama-tools--for-role "missing")))
    (should (eq (ellama-tools--provider-for-role "all")
                'default-provider))))

(ert-deftest test-ellama-subagent-loop-handler-max-steps-and-continue ()
  (ellama-test--ensure-local-ellama-tools)
  (let ((updated-extra nil)
        (callback-msg nil)
        (stream-call nil))
    (let* ((session-max
            (make-ellama-session
             :id "worker-max"
             :extra (list :task-completed nil
                          :step-count 2
                          :max-steps 2
                          :result-callback (lambda (msg)
                                             (setq callback-msg msg)))))
           (ellama--current-session session-max))
      (cl-letf (((symbol-function 'ellama-tools--set-session-extra)
                 (lambda (_session extra)
                   (setq updated-extra extra)))
                ((symbol-function 'ellama-stream)
                 (lambda (prompt &rest args)
                   (setq stream-call (list prompt args)))))
        (ellama--subagent-loop-handler "ignored")
        (should (equal callback-msg "Max steps (2) reached."))
        (should (plist-get updated-extra :task-completed))
        (setq callback-msg nil)
        (setq updated-extra nil)
        (setq stream-call nil)
        (let* ((session-continue
                (make-ellama-session
                 :id "worker-continue"
                 :extra (list :task-completed nil
                              :step-count 1
                              :max-steps 3
                              :result-callback (lambda (msg)
                                                 (setq callback-msg msg)))))
               (ellama--current-session session-continue))
          (ellama--subagent-loop-handler "ignored")
          (should (equal (plist-get updated-extra :step-count) 2))
          (should (equal (car stream-call)
                         ellama-tools-subagent-continue-prompt))
          (should (eq (plist-get (cadr stream-call) :session)
                      session-continue))
          (should (eq (plist-get (cadr stream-call) :on-done)
                      #'ellama--subagent-loop-handler))
          (should (null callback-msg)))))))

(ert-deftest test-ellama-tools-task-tool-role-fallback-and-report-priority ()
  (ellama-test--ensure-local-ellama-tools)
  (let ((ellama--current-session-id "parent-1")
        (ellama-tools-subagent-default-max-steps 7)
        (worker (make-ellama-session :id "worker-1"))
        (resolved-provider nil)
        (resolved-provider-role nil)
        (resolved-tools-role nil)
        (captured-extra nil)
        (stream-call nil)
        (role-tool (llm-make-tool :name "read_file" :function #'ignore)))
    (cl-letf (((symbol-function 'ellama-tools--provider-for-role)
               (lambda (role)
                 (setq resolved-provider-role role)
                 'provider))
              ((symbol-function 'ellama-tools--for-role)
               (lambda (role)
                 (setq resolved-tools-role role)
                 (list role-tool)))
              ((symbol-function 'ellama-new-session)
               (lambda (provider _prompt ephemeral)
                 (setq resolved-provider provider)
                 (should ephemeral)
                 worker))
              ((symbol-function 'ellama-tools--set-session-extra)
               (lambda (_session extra)
                 (setq captured-extra extra)))
              ((symbol-function 'ellama-stream)
               (lambda (prompt &rest args)
                 (setq stream-call (list prompt args))))
              ((symbol-function 'message)
               (lambda (&rest _args) nil)))
      (should (null (ellama-tools-task-tool (lambda (_res) nil)
                                            "Do work"
                                            "unknown-role")))
      (should (eq resolved-provider 'provider))
      (should (equal resolved-provider-role "general"))
      (should (equal resolved-tools-role "general"))
      (should (equal (plist-get captured-extra :role)
                     "general"))
      (should (equal (car stream-call) "Do work"))
      (should (eq (plist-get (cadr stream-call) :session) worker))
      (should (equal (plist-get (cadr stream-call) :tools)
                     (plist-get captured-extra :tools)))
      (should (string=
               (llm-tool-name
                (car (plist-get captured-extra :tools)))
               "report_result"))
      (should (eq (cadr (plist-get captured-extra :tools))
                  role-tool)))))

(provide 'test-ellama-tools)

;;; test-ellama-tools.el ends here
