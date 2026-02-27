;;; test-ellama-tools-srt-integration.el --- Real srt parity tests -*- lexical-binding: t; package-lint-main-file: "../ellama.el"; -*-

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
;; Local integration tests that compare `ellama' local SRT checks with the
;; real `srt' runtime.  These tests are skipped unless both:
;; - `srt' executable is installed
;; - ELLAMA_SRT_INTEGRATION=1
;;

;;; Code:

(require 'cl-lib)
(require 'ert)

(defconst ellama-test-srt-integration-root
  (expand-file-name
   ".."
   (file-name-directory (or load-file-name buffer-file-name)))
  "Project root directory for SRT integration test assets.")

(defun ellama-test-srt-integration--ensure-local-tools ()
  "Ensure tests use local `ellama-tools.el' from project root."
  (unless (fboundp 'ellama-tools--srt-check-access)
    ;; Parity tests only need the local SRT helpers, not full llm integration.
    ;; Provide tiny stubs when `llm' is unavailable (for clean Docker images).
    (unless (featurep 'llm)
      (defun llm-make-tool (&rest plist)
        "Return PLIST as a lightweight test stub for llm tool objects."
        plist)
      (defun llm-tool-name (tool)
        "Return tool name from TOOL plist."
        (plist-get tool :name))
      (provide 'llm))
    (unless (featurep 'llm-provider-utils)
      (provide 'llm-provider-utils))
    (load-file
     (expand-file-name "ellama-tools.el" ellama-test-srt-integration-root))))

(defun ellama-test-srt-integration--enabled-p ()
  "Return non-nil when SRT integration tests should run."
  (and (executable-find "srt")
       (let ((flag (getenv "ELLAMA_SRT_INTEGRATION")))
         (and flag (not (string= flag "")) (not (string= flag "0"))))))

(defun ellama-test-srt-integration--minimal-settings-json ()
  "Return minimal valid SRT config JSON used by parity tests."
  (concat
   "{\"network\":{\"allowedDomains\":[],\"deniedDomains\":[]},"
   "\"filesystem\":{\"denyRead\":[],\"allowWrite\":[],\"denyWrite\":[]}}"))

(defun ellama-test-srt-integration--skip-unless-enabled ()
  "Skip current test unless SRT integration testing is enabled."
  (unless (ellama-test-srt-integration--enabled-p)
    (ert-skip
     "Set ELLAMA_SRT_INTEGRATION=1 and install `srt' to run parity tests."))
  (let ((preflight (ellama-test-srt-integration--preflight-error)))
    (when preflight
      (ert-skip preflight))))

(defmacro ellama-test-srt-integration--with-settings (json &rest body)
  "Run BODY with a temp SRT settings file containing JSON."
  (declare (indent 1))
  `(let ((settings-file (make-temp-file "ellama-srt-parity-" nil ".json")))
     (unwind-protect
         (progn
           (with-temp-file settings-file
             (insert ,json))
           (when (fboundp 'ellama-tools--srt-policy-clear-cache)
             (ellama-tools--srt-policy-clear-cache))
           ,@body)
       (when (fboundp 'ellama-tools--srt-policy-clear-cache)
         (ellama-tools--srt-policy-clear-cache))
       (when (file-exists-p settings-file)
         (delete-file settings-file)))))

(defun ellama-test-srt-integration--preflight-error ()
  "Return nil if real `srt' is runnable for parity tests, else a reason."
  (let ((dir (make-temp-file "ellama-srt-preflight-" t)))
    (unwind-protect
        (ellama-test-srt-integration--with-settings
         (ellama-test-srt-integration--minimal-settings-json)
         (let* ((res (ellama-test-srt-integration--call
                      dir settings-file "true"))
                (exit (plist-get res :exit))
                (stderr (plist-get res :stderr)))
           (unless (and (integerp exit) (zerop exit))
             (format "srt preflight failed (exit=%s): %s"
                     exit
                     (string-trim (or stderr ""))))))
      (when (file-exists-p dir)
        (delete-directory dir t)))))

(defun ellama-test-srt-integration--call (cwd settings-file program &rest args)
  "Run PROGRAM with ARGS under real SRT from CWD using SETTINGS-FILE.
Return plist with `:exit', `:stdout', `:stderr'."
  (let ((default-directory cwd)
        (stdout (generate-new-buffer " *ellama-srt-stdout*"))
        (stderr-file (make-temp-file "ellama-srt-stderr-")))
    (unwind-protect
        (let ((exit-code
               (apply #'call-process
                      "srt" nil (list stdout stderr-file) nil
                      "--settings" settings-file program args)))
          (list :exit exit-code
                :stdout (with-current-buffer stdout (buffer-string))
                :stderr (with-temp-buffer
                          (insert-file-contents stderr-file)
                          (buffer-string))))
      (kill-buffer stdout)
      (when (file-exists-p stderr-file)
        (delete-file stderr-file)))))

(defun ellama-test-srt-integration--real-allows-p (cwd settings-file path op)
  "Return non-nil if real SRT allows PATH for OP from CWD."
  (let ((res
         (pcase op
           ('read
            (ellama-test-srt-integration--call cwd settings-file
                                               "cat" path))
           ('list
            (ellama-test-srt-integration--call cwd settings-file
                                               "ls" "-1" path))
           ('write
            (ellama-test-srt-integration--call
             cwd settings-file shell-file-name shell-command-switch
             (format "printf x > %s" (shell-quote-argument path))))
           (_ (error "Unsupported op: %S" op)))))
    (let ((exit-code (plist-get res :exit)))
      (and (integerp exit-code) (zerop exit-code)))))

(defun ellama-test-srt-integration--local-allows-p (cwd settings-file path op)
  "Return non-nil if local ellama SRT policy check allows PATH for OP."
  (let ((default-directory cwd)
        (ellama-tools-use-srt t)
        (ellama-tools-srt-args (list "--settings" settings-file)))
    (not (ellama-tools--srt-check-access path op))))

(defun ellama-test-srt-integration--should-match (cwd settings-file path op)
  "Assert local and real SRT allow/deny decisions match for PATH and OP."
  (let* ((reason (let ((default-directory cwd)
                       (ellama-tools-use-srt t)
                       (ellama-tools-srt-args (list "--settings" settings-file)))
                   (ellama-tools--srt-check-access path op)))
         (local (not reason))
         (res
          (pcase op
            ('read
             (ellama-test-srt-integration--call cwd settings-file
                                                "cat" path))
            ('list
             (ellama-test-srt-integration--call cwd settings-file
                                                "ls" "-1" path))
            ('write
             (ellama-test-srt-integration--call
              cwd settings-file shell-file-name shell-command-switch
              (format "printf x > %s" (shell-quote-argument path))))
            (_ (error "Unsupported op: %S" op))))
         (real (let ((exit-code (plist-get res :exit)))
                 (and (integerp exit-code) (zerop exit-code)))))
    (ert-info ((format "path=%s op=%S local=%S real=%S reason=%S exit=%S stderr=%s"
                       path op local real reason (plist-get res :exit)
                       (string-trim (or (plist-get res :stderr) ""))))
              (should (eq local real)))))

(defun ellama-test-srt-integration--should-match-write-with-mkdir
    (cwd settings-file path)
  "Assert local write policy matches real SRT for nested PATH creation.
The real command creates missing parent directories before writing PATH to
avoid non-policy failures for non-existing destination parents."
  (let* ((reason (let ((default-directory cwd)
                       (ellama-tools-use-srt t)
                       (ellama-tools-srt-args (list "--settings" settings-file)))
                   (ellama-tools--srt-check-access path 'write)))
         (local (not reason))
         (parent (file-name-directory path))
         (res (ellama-test-srt-integration--call
               cwd settings-file shell-file-name shell-command-switch
               (format "mkdir -p %s && printf x > %s"
                       (shell-quote-argument
                        (directory-file-name (or parent ".")))
                       (shell-quote-argument path))))
         (real (let ((exit-code (plist-get res :exit)))
                 (and (integerp exit-code) (zerop exit-code)))))
    (ert-info ((format "path=%s op=write+mkdir local=%S real=%S reason=%S exit=%S stderr=%s"
                       path local real reason (plist-get res :exit)
                       (string-trim (or (plist-get res :stderr) ""))))
              (should (eq local real)))))

(defun ellama-test-srt-integration--should-match-move
    (cwd settings-file src dst &optional allow-darwin-write-gap)
  "Assert local move-file policy matches real SRT for `mv SRC DST'.
When ALLOW-DARWIN-WRITE-GAP is non-nil, accept the observed macOS host
behavior where real `srt' may allow some cross-directory renames despite
directory-scoped write-policy denials that local `move_file' checks enforce."
  (let* ((local-reasons
          (let ((default-directory cwd)
                (ellama-tools-use-srt t)
                (ellama-tools-srt-args (list "--settings" settings-file)))
            (delq nil
                  (list (and (ellama-tools--srt-check-access src 'read)
                             'src-read)
                        (and (ellama-tools--srt-check-access src 'write)
                             'src-write)
                        (and (ellama-tools--srt-check-access dst 'write)
                             'dst-write)))))
         (local (null local-reasons))
         (res (ellama-test-srt-integration--call cwd settings-file
                                                 "mv" src dst))
         (real (let ((exit-code (plist-get res :exit)))
                 (and (integerp exit-code) (zerop exit-code)))))
    (ert-info ((format
                "mv %s -> %s local=%S real=%S local-reasons=%S exit=%S stderr=%s"
                src dst local real local-reasons (plist-get res :exit)
                (string-trim (or (plist-get res :stderr) ""))))
              (should
               (or (eq local real)
                   (and allow-darwin-write-gap
                        (eq system-type 'darwin)
                        (not local)
                        real
                        (cl-every (lambda (reason)
                                    (memq reason '(src-write dst-write)))
                                  local-reasons)))))))

(ert-deftest test-ellama-tools-srt-integration-denyread-literal-parity ()
  (ellama-test-srt-integration--ensure-local-tools)
  (ellama-test-srt-integration--skip-unless-enabled)
  (let* ((dir (make-temp-file "ellama-srt-parity-" t))
         (allowed (expand-file-name "allowed.txt" dir))
         (denied (expand-file-name "secret.txt" dir)))
    (unwind-protect
        (progn
          (with-temp-file allowed (insert "ok"))
          (with-temp-file denied (insert "no"))
          (ellama-test-srt-integration--with-settings
           (format
            (concat
             "{\"network\":{\"allowedDomains\":[],\"deniedDomains\":[]},"
             "\"filesystem\":{\"denyRead\":[%S],"
             "\"allowWrite\":[],\"denyWrite\":[]}}")
            denied)
           (ellama-test-srt-integration--should-match
            dir settings-file allowed 'read)
           (ellama-test-srt-integration--should-match
            dir settings-file denied 'read)))
      (when (file-exists-p dir)
        (delete-directory dir t)))))

(ert-deftest
    test-ellama-tools-srt-integration-allowwrite-denywrite-precedence ()
  (ellama-test-srt-integration--ensure-local-tools)
  (ellama-test-srt-integration--skip-unless-enabled)
  (let* ((dir (make-temp-file "ellama-srt-parity-write-" t))
         (work (expand-file-name "work" dir))
         (ok (expand-file-name "ok.txt" work))
         (blocked (expand-file-name "blocked.txt" work)))
    (unwind-protect
        (progn
          (make-directory work)
          (ellama-test-srt-integration--with-settings
           (format
            (concat
             "{\"network\":{\"allowedDomains\":[],\"deniedDomains\":[]},"
             "\"filesystem\":{\"denyRead\":[],\"allowWrite\":[%S],"
             "\"denyWrite\":[%S]}}")
            work blocked)
           (ellama-test-srt-integration--should-match
            dir settings-file ok 'write)
           (ellama-test-srt-integration--should-match
            dir settings-file blocked 'write)))
      (when (file-exists-p dir)
        (delete-directory dir t)))))

(ert-deftest test-ellama-tools-srt-integration-relative-rule-parity ()
  (ellama-test-srt-integration--ensure-local-tools)
  (ellama-test-srt-integration--skip-unless-enabled)
  (let* ((dir (make-temp-file "ellama-srt-parity-rel-" t))
         (file (expand-file-name "secret.txt" dir)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "x"))
          (ellama-test-srt-integration--with-settings
           (concat
            "{\"network\":{\"allowedDomains\":[],\"deniedDomains\":[]},"
            "\"filesystem\":{\"denyRead\":[\"secret.txt\"],"
            "\"allowWrite\":[],\"denyWrite\":[]}}")
           (ellama-test-srt-integration--should-match
            dir settings-file file 'read)))
      (when (file-exists-p dir)
        (delete-directory dir t)))))

(ert-deftest test-ellama-tools-srt-integration-glob-parity ()
  (ellama-test-srt-integration--ensure-local-tools)
  (ellama-test-srt-integration--skip-unless-enabled)
  (let* ((dir (make-temp-file "ellama-srt-parity-glob-" t))
         (a (expand-file-name "secret-a.txt" dir))
         (b (expand-file-name "public.txt" dir)))
    (unwind-protect
        (progn
          (with-temp-file a (insert "a"))
          (with-temp-file b (insert "b"))
          (ellama-test-srt-integration--with-settings
           (concat
            "{\"network\":{\"allowedDomains\":[],\"deniedDomains\":[]},"
            "\"filesystem\":{\"denyRead\":[\"secret-*.txt\"],"
            "\"allowWrite\":[],\"denyWrite\":[]}}")
           (ellama-test-srt-integration--should-match
            dir settings-file a 'read)
           (ellama-test-srt-integration--should-match
            dir settings-file b 'read)))
      (when (file-exists-p dir)
        (delete-directory dir t)))))

(ert-deftest
    test-ellama-tools-srt-integration-directory-prefix-edge-parity ()
  (ellama-test-srt-integration--ensure-local-tools)
  (ellama-test-srt-integration--skip-unless-enabled)
  (let* ((dir (make-temp-file "ellama-srt-parity-dir-prefix-" t))
         (sub-dir (expand-file-name "sub" dir))
         (submarine-dir (expand-file-name "submarine" dir))
         (denied (expand-file-name "a.txt" sub-dir))
         (allowed (expand-file-name "a.txt" submarine-dir)))
    (unwind-protect
        (progn
          (make-directory sub-dir)
          (make-directory submarine-dir)
          (with-temp-file denied (insert "x"))
          (with-temp-file allowed (insert "x"))
          (ellama-test-srt-integration--with-settings
           (concat
            "{\"network\":{\"allowedDomains\":[],\"deniedDomains\":[]},"
            "\"filesystem\":{\"denyRead\":[\"sub/\"],"
            "\"allowWrite\":[],\"denyWrite\":[]}}")
           (ellama-test-srt-integration--should-match
            dir settings-file denied 'read)
           (ellama-test-srt-integration--should-match
            dir settings-file allowed 'read)))
      (when (file-exists-p dir)
        (delete-directory dir t)))))

(ert-deftest test-ellama-tools-srt-integration-tilde-parity ()
  (ellama-test-srt-integration--ensure-local-tools)
  (ellama-test-srt-integration--skip-unless-enabled)
  (let* ((fake-home (make-temp-file "ellama-srt-parity-home-" t))
         (dir (expand-file-name "project" fake-home))
         (secret (expand-file-name "secret.txt" dir))
         (rule "~/project/secret.txt")
         (process-environment (copy-sequence process-environment)))
    (unwind-protect
        (progn
          (setenv "HOME" fake-home)
          (make-directory dir)
          (with-temp-file secret (insert "x"))
          (ellama-test-srt-integration--with-settings
           (format
            (concat
             "{\"network\":{\"allowedDomains\":[],\"deniedDomains\":[]},"
             "\"filesystem\":{\"denyRead\":[%S],"
             "\"allowWrite\":[],\"denyWrite\":[]}}")
            rule)
           (ellama-test-srt-integration--should-match
            dir settings-file secret 'read)))
      (when (file-exists-p fake-home)
        (delete-directory fake-home t)))))

(ert-deftest test-ellama-tools-srt-integration-symlink-file-parity ()
  (ellama-test-srt-integration--ensure-local-tools)
  (ellama-test-srt-integration--skip-unless-enabled)
  (let* ((dir (make-temp-file "ellama-srt-parity-symlink-file-" t))
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
          (ellama-test-srt-integration--with-settings
           (format
            (concat
             "{\"network\":{\"allowedDomains\":[],\"deniedDomains\":[]},"
             "\"filesystem\":{\"denyRead\":[%S],"
             "\"allowWrite\":[],\"denyWrite\":[]}}")
            link)
           (ellama-test-srt-integration--should-match
            dir settings-file link 'read)
           (ellama-test-srt-integration--should-match
            dir settings-file real 'read)))
      (when (file-exists-p dir)
        (delete-directory dir t)))))

(ert-deftest test-ellama-tools-srt-integration-symlink-dir-parity ()
  (ellama-test-srt-integration--ensure-local-tools)
  (ellama-test-srt-integration--skip-unless-enabled)
  (let* ((dir (make-temp-file "ellama-srt-parity-symlink-dir-" t))
         (real-dir (expand-file-name "real" dir))
         (link-dir (expand-file-name "alias" dir))
         (real-file (expand-file-name "note.txt" real-dir))
         (link-file (expand-file-name "note.txt" link-dir)))
    (unwind-protect
        (progn
          (make-directory real-dir)
          (with-temp-file real-file (insert "x"))
          (condition-case err
              (make-symbolic-link real-dir link-dir)
            (file-error
             (ert-skip (format "Cannot create symlink: %s"
                               (error-message-string err)))))
          (ellama-test-srt-integration--with-settings
           (format
            (concat
             "{\"network\":{\"allowedDomains\":[],\"deniedDomains\":[]},"
             "\"filesystem\":{\"denyRead\":[%S],"
             "\"allowWrite\":[],\"denyWrite\":[]}}")
            (concat link-dir "/"))
           (ellama-test-srt-integration--should-match
            dir settings-file link-file 'read)
           (ellama-test-srt-integration--should-match
            dir settings-file real-file 'read)))
      (when (file-exists-p dir)
        (delete-directory dir t)))))

(ert-deftest
    test-ellama-tools-srt-integration-write-nested-target-with-mkdir-parity ()
  (ellama-test-srt-integration--ensure-local-tools)
  (ellama-test-srt-integration--skip-unless-enabled)
  (let* ((dir (make-temp-file "ellama-srt-parity-write-nested-" t))
         (work (expand-file-name "work" dir))
         (target (expand-file-name "deep/path/out.txt" work)))
    (unwind-protect
        (progn
          (make-directory work)
          (ellama-test-srt-integration--with-settings
           (format
            (concat
             "{\"network\":{\"allowedDomains\":[],\"deniedDomains\":[]},"
             "\"filesystem\":{\"denyRead\":[],\"allowWrite\":[%S],"
             "\"denyWrite\":[]}}")
            work)
           (ellama-test-srt-integration--should-match-write-with-mkdir
            dir settings-file target)))
      (when (file-exists-p dir)
        (delete-directory dir t)))))

(ert-deftest
    test-ellama-tools-srt-integration-denywrite-exact-nested-new-file-parity ()
  (ellama-test-srt-integration--ensure-local-tools)
  (ellama-test-srt-integration--skip-unless-enabled)
  (let* ((dir (make-temp-file "ellama-srt-parity-denywrite-nested-" t))
         (work (expand-file-name "work" dir))
         (target (expand-file-name "deep/path/blocked.txt" work)))
    (unwind-protect
        (progn
          (make-directory work)
          (ellama-test-srt-integration--with-settings
           (format
            (concat
             "{\"network\":{\"allowedDomains\":[],\"deniedDomains\":[]},"
             "\"filesystem\":{\"denyRead\":[],\"allowWrite\":[%S],"
             "\"denyWrite\":[%S]}}")
            work target)
           (ellama-test-srt-integration--should-match-write-with-mkdir
            dir settings-file target)))
      (when (file-exists-p dir)
        (delete-directory dir t)))))

(ert-deftest
    test-ellama-tools-srt-integration-write-existing-target-parity ()
  (ellama-test-srt-integration--ensure-local-tools)
  (ellama-test-srt-integration--skip-unless-enabled)
  (let* ((dir (make-temp-file "ellama-srt-parity-write-existing-" t))
         (work (expand-file-name "work" dir))
         (target (expand-file-name "existing.txt" work)))
    (unwind-protect
        (progn
          (make-directory work)
          (with-temp-file target (insert "old"))
          (ellama-test-srt-integration--with-settings
           (format
            (concat
             "{\"network\":{\"allowedDomains\":[],\"deniedDomains\":[]},"
             "\"filesystem\":{\"denyRead\":[],\"allowWrite\":[%S],"
             "\"denyWrite\":[]}}")
            work)
           (ellama-test-srt-integration--should-match
            dir settings-file target 'write)))
      (when (file-exists-p dir)
        (delete-directory dir t)))))

(ert-deftest
    test-ellama-tools-srt-integration-denywrite-exact-existing-file-parity ()
  (ellama-test-srt-integration--ensure-local-tools)
  (ellama-test-srt-integration--skip-unless-enabled)
  (let* ((dir (make-temp-file "ellama-srt-parity-denywrite-existing-" t))
         (work (expand-file-name "work" dir))
         (target (expand-file-name "blocked.txt" work)))
    (unwind-protect
        (progn
          (make-directory work)
          (with-temp-file target (insert "old"))
          (ellama-test-srt-integration--with-settings
           (format
            (concat
             "{\"network\":{\"allowedDomains\":[],\"deniedDomains\":[]},"
             "\"filesystem\":{\"denyRead\":[],\"allowWrite\":[%S],"
             "\"denyWrite\":[%S]}}")
            work target)
           (ellama-test-srt-integration--should-match
            dir settings-file target 'write)))
      (when (file-exists-p dir)
        (delete-directory dir t)))))

(ert-deftest test-ellama-tools-srt-integration-move-allowed-parity ()
  (ellama-test-srt-integration--ensure-local-tools)
  (ellama-test-srt-integration--skip-unless-enabled)
  (let* ((dir (make-temp-file "ellama-srt-parity-move-ok-" t))
         (work (expand-file-name "work" dir))
         (src (expand-file-name "src.txt" work))
         (dst (expand-file-name "dst.txt" work)))
    (unwind-protect
        (progn
          (make-directory work)
          (with-temp-file src (insert "x"))
          (ellama-test-srt-integration--with-settings
           (format
            (concat
             "{\"network\":{\"allowedDomains\":[],\"deniedDomains\":[]},"
             "\"filesystem\":{\"denyRead\":[],\"allowWrite\":[%S],"
             "\"denyWrite\":[]}}")
            work)
           (ellama-test-srt-integration--should-match-move
            dir settings-file src dst t)))
      (when (file-exists-p dir)
        (delete-directory dir t)))))

(ert-deftest test-ellama-tools-srt-integration-move-denyread-src-parity ()
  (ellama-test-srt-integration--ensure-local-tools)
  (ellama-test-srt-integration--skip-unless-enabled)
  (let* ((dir (make-temp-file "ellama-srt-parity-move-denyread-" t))
         (work (expand-file-name "work" dir))
         (src (expand-file-name "src.txt" work))
         (dst (expand-file-name "dst.txt" work)))
    (unwind-protect
        (progn
          (make-directory work)
          (with-temp-file src (insert "x"))
          (ellama-test-srt-integration--with-settings
           (format
            (concat
             "{\"network\":{\"allowedDomains\":[],\"deniedDomains\":[]},"
             "\"filesystem\":{\"denyRead\":[%S],\"allowWrite\":[%S],"
             "\"denyWrite\":[]}}")
            src work)
           (ellama-test-srt-integration--should-match-move
            dir settings-file src dst t)))
      (when (file-exists-p dir)
        (delete-directory dir t)))))

(ert-deftest test-ellama-tools-srt-integration-move-denywrite-dst-parity ()
  (ellama-test-srt-integration--ensure-local-tools)
  (ellama-test-srt-integration--skip-unless-enabled)
  (let* ((dir (make-temp-file "ellama-srt-parity-move-denydst-" t))
         (work (expand-file-name "work" dir))
         (src (expand-file-name "src.txt" work))
         (dst (expand-file-name "dst.txt" work)))
    (unwind-protect
        (progn
          (make-directory work)
          (with-temp-file src (insert "x"))
          (ellama-test-srt-integration--with-settings
           (format
            (concat
             "{\"network\":{\"allowedDomains\":[],\"deniedDomains\":[]},"
             "\"filesystem\":{\"denyRead\":[],\"allowWrite\":[%S],"
             "\"denyWrite\":[%S]}}")
            work dst)
           (ellama-test-srt-integration--should-match-move
            dir settings-file src dst t)))
      (when (file-exists-p dir)
        (delete-directory dir t)))))

(ert-deftest test-ellama-tools-srt-integration-move-denywrite-src-parity ()
  (ellama-test-srt-integration--ensure-local-tools)
  (ellama-test-srt-integration--skip-unless-enabled)
  (let* ((dir (make-temp-file "ellama-srt-parity-move-denysrc-" t))
         (work (expand-file-name "work" dir))
         (src (expand-file-name "src.txt" work))
         (dst (expand-file-name "dst.txt" work)))
    (unwind-protect
        (progn
          (make-directory work)
          (with-temp-file src (insert "x"))
          (ellama-test-srt-integration--with-settings
           (format
            (concat
             "{\"network\":{\"allowedDomains\":[],\"deniedDomains\":[]},"
             "\"filesystem\":{\"denyRead\":[],\"allowWrite\":[%S],"
             "\"denyWrite\":[%S]}}")
            work src)
           (ellama-test-srt-integration--should-match-move
            dir settings-file src dst t)))
      (when (file-exists-p dir)
        (delete-directory dir t)))))

(ert-deftest test-ellama-tools-srt-integration-move-cross-dir-allowed-parity ()
  (ellama-test-srt-integration--ensure-local-tools)
  (ellama-test-srt-integration--skip-unless-enabled)
  (let* ((dir (make-temp-file "ellama-srt-parity-move-xdir-ok-" t))
         (src-dir (expand-file-name "src" dir))
         (dst-dir (expand-file-name "dst" dir))
         (src (expand-file-name "a.txt" src-dir))
         (dst (expand-file-name "b.txt" dst-dir)))
    (unwind-protect
        (progn
          (make-directory src-dir)
          (make-directory dst-dir)
          (with-temp-file src (insert "x"))
          (ellama-test-srt-integration--with-settings
           (format
            (concat
             "{\"network\":{\"allowedDomains\":[],\"deniedDomains\":[]},"
             "\"filesystem\":{\"denyRead\":[],\"allowWrite\":[%S,%S],"
             "\"denyWrite\":[]}}")
            src-dir dst-dir)
           (ellama-test-srt-integration--should-match-move
            dir settings-file src dst t)))
      (when (file-exists-p dir)
        (delete-directory dir t)))))

(ert-deftest
    test-ellama-tools-srt-integration-move-cross-dir-dst-not-allowed-parity ()
  (ellama-test-srt-integration--ensure-local-tools)
  (ellama-test-srt-integration--skip-unless-enabled)
  (let* ((dir (make-temp-file "ellama-srt-parity-move-xdir-nodst-" t))
         (src-dir (expand-file-name "src" dir))
         (dst-dir (expand-file-name "dst" dir))
         (src (expand-file-name "a.txt" src-dir))
         (dst (expand-file-name "b.txt" dst-dir)))
    (unwind-protect
        (progn
          (make-directory src-dir)
          (make-directory dst-dir)
          (with-temp-file src (insert "x"))
          (ellama-test-srt-integration--with-settings
           (format
            (concat
             "{\"network\":{\"allowedDomains\":[],\"deniedDomains\":[]},"
             "\"filesystem\":{\"denyRead\":[],\"allowWrite\":[%S],"
             "\"denyWrite\":[]}}")
            src-dir)
           (ellama-test-srt-integration--should-match-move
            dir settings-file src dst t)))
      (when (file-exists-p dir)
        (delete-directory dir t)))))

(ert-deftest
    test-ellama-tools-srt-integration-move-cross-dir-src-not-allowed-parity ()
  (ellama-test-srt-integration--ensure-local-tools)
  (ellama-test-srt-integration--skip-unless-enabled)
  (let* ((dir (make-temp-file "ellama-srt-parity-move-xdir-nosrc-" t))
         (src-dir (expand-file-name "src" dir))
         (dst-dir (expand-file-name "dst" dir))
         (src (expand-file-name "a.txt" src-dir))
         (dst (expand-file-name "b.txt" dst-dir)))
    (unwind-protect
        (progn
          (make-directory src-dir)
          (make-directory dst-dir)
          (with-temp-file src (insert "x"))
          (ellama-test-srt-integration--with-settings
           (format
            (concat
             "{\"network\":{\"allowedDomains\":[],\"deniedDomains\":[]},"
             "\"filesystem\":{\"denyRead\":[],\"allowWrite\":[%S],"
             "\"denyWrite\":[]}}")
            dst-dir)
           (ellama-test-srt-integration--should-match-move
            dir settings-file src dst t)))
      (when (file-exists-p dir)
        (delete-directory dir t)))))

(ert-deftest
    test-ellama-tools-srt-integration-move-cross-dir-denywrite-dst-dir-parity ()
  (ellama-test-srt-integration--ensure-local-tools)
  (ellama-test-srt-integration--skip-unless-enabled)
  (let* ((dir (make-temp-file "ellama-srt-parity-move-xdir-denydstdir-" t))
         (src-dir (expand-file-name "src" dir))
         (dst-dir (expand-file-name "dst" dir))
         (src (expand-file-name "a.txt" src-dir))
         (dst (expand-file-name "b.txt" dst-dir)))
    (unwind-protect
        (progn
          (make-directory src-dir)
          (make-directory dst-dir)
          (with-temp-file src (insert "x"))
          (ellama-test-srt-integration--with-settings
           (format
            (concat
             "{\"network\":{\"allowedDomains\":[],\"deniedDomains\":[]},"
             "\"filesystem\":{\"denyRead\":[],\"allowWrite\":[%S,%S],"
             "\"denyWrite\":[%S]}}")
            src-dir dst-dir (concat dst-dir "/"))
           (ellama-test-srt-integration--should-match-move
            dir settings-file src dst t)))
      (when (file-exists-p dir)
        (delete-directory dir t)))))

(provide 'test-ellama-tools-srt-integration)

;;; test-ellama-tools-srt-integration.el ends here
