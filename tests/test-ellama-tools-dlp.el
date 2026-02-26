;;; test-ellama-tools-dlp.el --- DLP helper tests -*- lexical-binding: t; package-lint-main-file: "../ellama.el"; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

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
;; Tests for DLP helper schemas.
;;

;;; Code:

(require 'cl-lib)
(require 'ert)

(defconst ellama-test-dlp-root
  (expand-file-name
   ".."
   (file-name-directory (or load-file-name buffer-file-name)))
  "Project root directory for DLP tests.")

(unless (featurep 'ellama-tools-dlp)
  (load-file (expand-file-name "ellama-tools-dlp.el" ellama-test-dlp-root)))

(ert-deftest test-ellama-tools-dlp-make-scan-context ()
  (let ((context (ellama-tools-dlp--make-scan-context
                  :direction 'input
                  :tool-name "read_file"
                  :arg-name "path"
                  :payload-length 42
                  :truncated nil)))
    (should (ellama-tools-dlp--scan-context-p context))
    (should (eq (ellama-tools-dlp--scan-context-get context :direction)
                'input))
    (should (equal (ellama-tools-dlp--scan-context-get context :tool-name)
                   "read_file"))
    (should (equal (ellama-tools-dlp--scan-context-get context :missing "x")
                   "x"))))

(ert-deftest test-ellama-tools-dlp-scan-context-invalid-direction ()
  (should-error
   (ellama-tools-dlp--make-scan-context
    :direction 'sideways
    :tool-name "read_file"
    :payload-length 1)))

(ert-deftest test-ellama-tools-dlp-make-finding-with-span ()
  (let ((finding (ellama-tools-dlp--make-finding
                  :rule-id "api-key"
                  :detector 'regex
                  :severity 'high
                  :match-start 3
                  :match-end 12)))
    (should (ellama-tools-dlp--finding-p finding))
    (should (equal (ellama-tools-dlp--finding-get finding :rule-id)
                   "api-key"))
    (should (eq (ellama-tools-dlp--finding-get finding :detector) 'regex))))

(ert-deftest test-ellama-tools-dlp-finding-require-full-span ()
  (should-error
   (ellama-tools-dlp--make-finding
    :rule-id 'token
    :detector 'exact-secret
    :match-start 1)))

(ert-deftest test-ellama-tools-dlp-make-verdict ()
  (let* ((finding (ellama-tools-dlp--make-finding
                   :rule-id "api-key"
                   :detector 'regex))
         (verdict (ellama-tools-dlp--make-verdict
                   :action 'redact
                   :message "redacted"
                   :findings (list finding)
                   :redacted-text "[REDACTED:api-key]")))
    (should (ellama-tools-dlp--verdict-p verdict))
    (should (eq (ellama-tools-dlp--verdict-get verdict :action) 'redact))
    (should (equal (ellama-tools-dlp--verdict-get verdict :message)
                   "redacted"))))

(ert-deftest test-ellama-tools-dlp-record-incident-memory-max ()
  (let ((ellama-tools-dlp-log-targets '(memory))
        (ellama-tools-dlp-incident-log-max 2))
    (ellama-tools-dlp--clear-incident-log)
    (ellama-tools-dlp--record-incident '(:type one))
    (ellama-tools-dlp--record-incident '(:type two))
    (ellama-tools-dlp--record-incident '(:type three))
    (let ((incidents (ellama-tools-dlp--incident-log)))
      (should (= (length incidents) 2))
      (should (eq (plist-get (nth 0 incidents) :type) 'three))
      (should (eq (plist-get (nth 1 incidents) :type) 'two)))))

(ert-deftest
    test-ellama-tools-dlp-record-incident-message-target-sanitizes ()
  (let ((ellama-tools-dlp-log-targets '(message))
        (ellama-tools-dlp-message-prefix "dlp-test")
        (captured nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq captured (apply #'format fmt args)))))
      (ellama-tools-dlp--record-incident
       (list :type 'scan-error
             :tool-name (concat "bad" (string ?\n) "tool" (string 27))
             :error-type 'oops
             :detail (concat "x" (string 27) "y"))))
    (should (string-match-p "\\`dlp-test " captured))
    (should-not (string-match-p "\n" captured))
    (should-not (string-match-p (string 27) captured))
    (should (string-match-p "type=scan-error" captured))))

(ert-deftest test-ellama-tools-dlp-recent-incidents-limit-and-copy ()
  (let ((ellama-tools-dlp-log-targets '(memory))
        (ellama-tools-dlp-incident-log-max 10))
    (ellama-tools-dlp--clear-incident-log)
    (ellama-tools-dlp--record-incident '(:type one))
    (ellama-tools-dlp--record-incident '(:type two))
    (let ((one (ellama-tools-dlp-recent-incidents 1))
          (all (ellama-tools-dlp-recent-incidents)))
      (should (= (length one) 1))
      (should (eq (plist-get (car one) :type) 'two))
      (setcar all 'mutated)
      (should (eq (plist-get (car (ellama-tools-dlp--incident-log)) :type)
                  'two)))))

(ert-deftest test-ellama-tools-dlp-incident-stats-aggregates-fields ()
  (let ((ellama-tools-dlp-log-targets '(memory))
        (ellama-tools-dlp-incident-log-max 10))
    (ellama-tools-dlp--clear-incident-log)
    (ellama-tools-dlp--record-incident
     '(:type scan-decision
             :action block
             :tool-name "shell_command"
             :rule-ids ("r1" "r2")
             :truncated t))
    (ellama-tools-dlp--record-incident
     '(:type scan-decision
             :action redact
             :tool-name "read_file"
             :rule-ids ("r2")))
    (ellama-tools-dlp--record-incident
     '(:type truncation
             :tool-name "read_file"
             :truncated t))
    (let ((stats (ellama-tools-dlp-incident-stats)))
      (should (= (plist-get stats :total) 3))
      (should (= (plist-get stats :truncated-count) 2))
      (should (equal (cdr (assoc 'scan-decision (plist-get stats :by-type))) 2))
      (should (equal (cdr (assoc 'truncation (plist-get stats :by-type))) 1))
      (should (equal (cdr (assoc 'block (plist-get stats :by-action))) 1))
      (should (equal (cdr (assoc 'redact (plist-get stats :by-action))) 1))
      (should (equal (cdr (assoc "read_file" (plist-get stats :by-tool))) 2))
      (should (equal (cdr (assoc "r2" (plist-get stats :by-rule-id))) 2))
      (should (equal (cdr (assoc "r1" (plist-get stats :by-rule-id))) 1)))))

(ert-deftest test-ellama-tools-dlp-reset-runtime-state-clears-caches ()
  (let ((ellama-tools-dlp-log-targets '(memory)))
    (ellama-tools-dlp--clear-incident-log)
    (ellama-tools-dlp--clear-regex-cache)
    (setq ellama-tools-dlp--exact-secret-cache '(:signature ("X=1")))
    (ellama-tools-dlp--record-incident '(:type scan-error))
    (puthash '(dummy) '(:status ok) ellama-tools-dlp--regex-cache)
    (should (ellama-tools-dlp-reset-runtime-state))
    (should (null (ellama-tools-dlp--incident-log)))
    (should (= (hash-table-count ellama-tools-dlp--regex-cache) 0))
    (should (null ellama-tools-dlp--exact-secret-cache))))

(ert-deftest test-ellama-tools-dlp-incident-stats-report-formats-sections ()
  (let ((ellama-tools-dlp-log-targets '(memory))
        (ellama-tools-dlp-incident-log-max 10))
    (ellama-tools-dlp--clear-incident-log)
    (ellama-tools-dlp--record-incident
     '(:type scan-decision
             :action block
             :tool-name "shell_command"
             :rule-ids ("r1")
             :truncated t))
    (let ((report (ellama-tools-dlp-incident-stats-report 5)))
      (should (string-match-p "Ellama DLP Incident Stats (recent 5)" report))
      (should (string-match-p "Total incidents: 1" report))
      (should (string-match-p "Truncated incidents: 1" report))
      (should (string-match-p "By type:" report))
      (should (string-match-p "scan-decision: 1" report))
      (should (string-match-p "By action:" report))
      (should (string-match-p "block: 1" report))
      (should (string-match-p "By tool:" report))
      (should (string-match-p "shell_command: 1" report))
      (should (string-match-p "By rule id:" report))
      (should (string-match-p "r1: 1" report)))))

(ert-deftest test-ellama-tools-dlp-show-incident-stats-uses-temp-buffer ()
  (let ((buffer-name "*Ellama DLP Incident Stats*"))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    (cl-letf (((symbol-function 'ellama-tools-dlp-incident-stats-report)
               (lambda (count)
                 (format "report-%s" count))))
      (ellama-tools-dlp-show-incident-stats 7))
    (unwind-protect
        (with-current-buffer buffer-name
          (should (equal (buffer-string) "report-7\n")))
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name)))))

(ert-deftest test-ellama-tools-dlp-verdict-rejects-invalid-finding ()
  (should-error
   (ellama-tools-dlp--make-verdict
    :action 'allow
    :findings (list '(:rule-id "x" :detector nope)))))

(ert-deftest test-ellama-tools-dlp-normalize-text-line-endings-and-zero-width ()
  (let ((input (concat "a\r\nb\rc"
                       (string #x200b)
                       (string #xfeff)
                       "d")))
    (should (equal (ellama-tools-dlp--normalize-text input)
                   "a\nb\ncd"))))

(ert-deftest test-ellama-tools-dlp-normalize-text-nfkc ()
  (skip-unless
   (progn
     (require 'ucs-normalize nil t)
     (fboundp 'ucs-normalize-NFKC-string)))
  (should (equal (ellama-tools-dlp--normalize-text "ＡＢＣ１２３")
                 "ABC123")))

(ert-deftest test-ellama-tools-dlp-normalize-text-fail-open-nfkc-error ()
  (cl-letf (((symbol-function 'ucs-normalize-NFKC-string)
             (lambda (_text)
               (error "boom"))))
    (let ((input (concat "A\r" (string #x200b) "B")))
      (should (equal (ellama-tools-dlp--normalize-text input)
                     "A\nB")))))

(ert-deftest test-ellama-tools-dlp-truncate-payload-marks-context-and-logs ()
  (let ((ellama-tools-dlp-max-scan-size 5))
    (ellama-tools-dlp--clear-incident-log)
    (let* ((context (ellama-tools-dlp--make-scan-context
                     :direction 'output
                     :tool-name "shell_command"
                     :arg-name nil
                     :payload-length 0
                     :truncated nil))
           (result (ellama-tools-dlp--truncate-payload "abcdefg" context))
           (text (plist-get result :text))
           (context* (plist-get result :context))
           (incidents (ellama-tools-dlp--incident-log))
           (incident (car incidents)))
      (should (equal text "abcde"))
      (should (= (string-bytes text) 5))
      (should (eq (ellama-tools-dlp--scan-context-get context* :direction)
                  'output))
      (should (equal (ellama-tools-dlp--scan-context-get context* :tool-name)
                     "shell_command"))
      (should (= (ellama-tools-dlp--scan-context-get context* :payload-length)
                 7))
      (should (eq (ellama-tools-dlp--scan-context-get context* :truncated) t))
      (should (eq (plist-get incident :type) 'truncation))
      (should (eq (plist-get incident :direction) 'output))
      (should (equal (plist-get incident :tool-name) "shell_command"))
      (should (= (plist-get incident :payload-length) 7))
      (should (= (plist-get incident :scanned-length) 5))
      (should (eq (plist-get incident :truncated) t)))))

(ert-deftest test-ellama-tools-dlp-prepare-payload-normalizes-once ()
  (let ((normalize-calls 0))
    (cl-letf (((symbol-function 'ellama-tools-dlp--normalize-text)
               (lambda (text)
                 (setq normalize-calls (1+ normalize-calls))
                 (concat "N:" text))))
      (let* ((context (ellama-tools-dlp--make-scan-context
                       :direction 'input
                       :tool-name "read_file"
                       :arg-name "path"
                       :payload-length 0
                       :truncated nil))
             (result (ellama-tools-dlp--prepare-payload "abc" context)))
        (should (= normalize-calls 1))
        (should (equal (plist-get result :text) "N:abc"))
        (should (ellama-tools-dlp--scan-context-p
                 (plist-get result :context)))))))

(ert-deftest test-ellama-tools-dlp-detect-regex-findings-enable-disable ()
  (let* ((context (ellama-tools-dlp--make-scan-context
                   :direction 'input
                   :tool-name "shell_command"
                   :arg-name "content"
                   :payload-length 0
                   :truncated nil))
         (rules (list '(:id "disabled"
                            :pattern "SECRET"
                            :enabled nil)
                      '(:id "enabled"
                            :pattern "SECRET"))))
    (let ((findings (ellama-tools-dlp--detect-regex-findings
                     "xxSECRETyy" context rules)))
      (should (= (length findings) 1))
      (should (equal (plist-get (car findings) :rule-id) "enabled")))))

(ert-deftest test-ellama-tools-dlp-detect-regex-findings-case-fold ()
  (let* ((context (ellama-tools-dlp--make-scan-context
                   :direction 'output
                   :tool-name "read_file"
                   :arg-name nil
                   :payload-length 0
                   :truncated nil))
         (rules (list '(:id "ci"
                            :pattern "secret"
                            :case-fold t))))
    (let ((findings (ellama-tools-dlp--detect-regex-findings
                     "SECRET" context rules)))
      (should (= (length findings) 1))
      (should (equal (plist-get (car findings) :rule-id) "ci")))))

(ert-deftest test-ellama-tools-dlp-detect-regex-findings-scoping ()
  (let* ((context (ellama-tools-dlp--make-scan-context
                   :direction 'input
                   :tool-name "shell_command"
                   :arg-name "content"
                   :payload-length 0
                   :truncated nil))
         (rules (list '(:id "wrong-direction"
                            :pattern "TOKEN"
                            :directions (output))
                      '(:id "wrong-tool"
                            :pattern "TOKEN"
                            :tools ("read_file"))
                      '(:id "wrong-arg"
                            :pattern "TOKEN"
                            :args ("path"))
                      '(:id "match"
                            :pattern "TOKEN"
                            :directions (input)
                            :tools ("shell_command")
                            :args ("content")))))
    (let ((findings (ellama-tools-dlp--detect-regex-findings
                     "TOKEN" context rules)))
      (should (= (length findings) 1))
      (should (equal (plist-get (car findings) :rule-id) "match")))))

(ert-deftest test-ellama-tools-dlp-regex-cache-key-respects-scoping ()
  (ellama-tools-dlp--clear-regex-cache)
  (let* ((input-context (ellama-tools-dlp--make-scan-context
                         :direction 'input
                         :tool-name "shell_command"
                         :arg-name "content"
                         :payload-length 0
                         :truncated nil))
         (output-context (ellama-tools-dlp--make-scan-context
                          :direction 'output
                          :tool-name "shell_command"
                          :arg-name nil
                          :payload-length 0
                          :truncated nil))
         (input-rule '(:id "same"
                           :pattern "SECRET"
                           :directions (input)))
         (output-rule '(:id "same"
                            :pattern "SECRET"
                            :directions (output))))
    ;; Prime the cache with an input-scoped rule, then ensure an output-scoped
    ;; rule with the same id/pattern still matches output.
    (should (= (length (ellama-tools-dlp--detect-regex-findings
                        "SECRET" input-context (list input-rule)))
               1))
    (should (= (length (ellama-tools-dlp--detect-regex-findings
                        "SECRET" output-context (list output-rule)))
               1))))

(ert-deftest test-ellama-tools-dlp-detect-regex-findings-reports-spans ()
  (let* ((context (ellama-tools-dlp--make-scan-context
                   :direction 'output
                   :tool-name "grep"
                   :arg-name nil
                   :payload-length 0
                   :truncated nil))
         (rules (list '(:id "key"
                            :pattern "KEY")))
         (findings (ellama-tools-dlp--detect-regex-findings
                    "xxKEYyyKEY" context rules)))
    (should (= (length findings) 2))
    (should (= (plist-get (nth 0 findings) :match-start) 2))
    (should (= (plist-get (nth 0 findings) :match-end) 5))
    (should (= (plist-get (nth 1 findings) :match-start) 7))
    (should (= (plist-get (nth 1 findings) :match-end) 10))))

(ert-deftest
    test-ellama-tools-dlp-detect-regex-findings-invalid-regex-safe-log ()
  (let* ((secret "TOPSECRET-DO-NOT-LOG")
         (context (ellama-tools-dlp--make-scan-context
                   :direction 'input
                   :tool-name "shell_command"
                   :arg-name "content"
                   :payload-length 0
                   :truncated nil))
         (rules (list '(:id "bad-regex"
                            :pattern "\\("))))
    (ellama-tools-dlp--clear-incident-log)
    (ellama-tools-dlp--clear-regex-cache)
    (should-not (ellama-tools-dlp--detect-regex-findings secret context rules))
    (let* ((incident (car (ellama-tools-dlp--incident-log)))
           (serialized (prin1-to-string incident)))
      (should (eq (plist-get incident :type) 'regex-error))
      (should (equal (plist-get incident :rule-id) "bad-regex"))
      (should-not (string-match-p (regexp-quote secret) serialized)))))

(ert-deftest test-ellama-tools-dlp-exact-secret-heuristic-accept-reject ()
  (let ((process-environment
         '("MY_API_KEY=sk-test-abcdefghijklmnopqrstuvwxyz123456"
           "PATH=/usr/bin:/bin:/usr/local/bin"
           "HOME=/Users/tester"
           "EDITOR=vim")))
    (ellama-tools-dlp--clear-incident-log)
    (ellama-tools-dlp--invalidate-exact-secret-cache)
    (let* ((cache (ellama-tools-dlp--refresh-exact-secret-cache))
           (candidates (plist-get cache :candidates))
           (names (mapcar (lambda (item) (plist-get item :env-name))
                          candidates)))
      (should (member "MY_API_KEY" names))
      (should-not (member "PATH" names))
      (should-not (member "HOME" names))
      (should-not (member "EDITOR" names)))))

(ert-deftest test-ellama-tools-dlp-detect-exact-secret-encoded-variants ()
  (let* ((secret "sk-test-abcdefghijklmnopqrstuvwxyz123456")
         (process-environment (list (concat "MY_API_KEY=" secret)))
         (context (ellama-tools-dlp--make-scan-context
                   :direction 'output
                   :tool-name "read_file"
                   :arg-name nil
                   :payload-length 0
                   :truncated nil))
         (bytes (encode-coding-string secret 'utf-8 t))
         (base64 (base64-encode-string bytes t))
         (base64url (replace-regexp-in-string
                     "=+\\'" ""
                     (replace-regexp-in-string
                      "/" "_"
                      (replace-regexp-in-string "+" "-" base64 t t)
                      t t)
                     t t))
         (hex (mapconcat (lambda (byte) (format "%02x" byte))
                         (string-to-list bytes)
                         "")))
    (ellama-tools-dlp--clear-incident-log)
    (ellama-tools-dlp--invalidate-exact-secret-cache)
    (ellama-tools-dlp--refresh-exact-secret-cache)
    (dolist (payload (list secret base64 base64url hex))
      (let ((findings (ellama-tools-dlp--detect-exact-secret-findings
                       (concat "xx" payload "yy") context)))
        (should (>= (length findings) 1))
        (should (cl-some
                 (lambda (finding)
                   (and (equal (plist-get finding :rule-id)
                               "env-exact-secret")
                        (eq (plist-get finding :detector)
                            'exact-secret)))
                 findings))))))

(ert-deftest test-ellama-tools-dlp-exact-secret-stage-error-log-safe ()
  (let* ((secret "TOPSECRET-DO-NOT-LOG-123456789")
         (process-environment (list (concat "BROKEN_SECRET=" secret)))
         (ellama-tools-dlp-env-secret-heuristic-stages
          (list (lambda (_env-name _env-value)
                  (error "boom")))))
    (ellama-tools-dlp--clear-incident-log)
    (ellama-tools-dlp--invalidate-exact-secret-cache)
    (let* ((cache (ellama-tools-dlp--refresh-exact-secret-cache))
           (incident (car (ellama-tools-dlp--incident-log)))
           (serialized (prin1-to-string incident)))
      (should (null (plist-get cache :candidates)))
      (should (eq (plist-get incident :type) 'exact-secret-error))
      (should (eq (plist-get incident :error-type) 'heuristic-stage-error))
      (should (equal (plist-get incident :env-name) "BROKEN_SECRET"))
      (should-not (string-match-p (regexp-quote secret) serialized)))))

(ert-deftest test-ellama-tools-dlp-scan-text-monitor-log-only ()
  (let* ((ellama-tools-dlp-enabled t)
         (ellama-tools-dlp-mode 'monitor)
         (ellama-tools-dlp-scan-env-exact-secrets nil)
         (ellama-tools-dlp-input-default-action 'block)
         (ellama-tools-dlp-regex-rules
          '((:id "token" :pattern "SECRET")))
         (context (ellama-tools-dlp--make-scan-context
                   :direction 'input
                   :tool-name "shell_command"
                   :arg-name "content"
                   :payload-length 0
                   :truncated nil)))
    (ellama-tools-dlp--clear-incident-log)
    (let* ((result (ellama-tools-dlp--scan-text "run SECRET now" context))
           (verdict (plist-get result :verdict))
           (incident (car (ellama-tools-dlp--incident-log)))
           (serialized (prin1-to-string incident)))
      (should (eq (plist-get verdict :action) 'allow))
      (should (eq (plist-get verdict :configured-action) 'block))
      (should (string-match-p "DLP monitor input" (plist-get verdict :message)))
      (should (eq (plist-get incident :type) 'scan-decision))
      (should (eq (plist-get incident :action) 'allow))
      (should (eq (plist-get incident :configured-action) 'block))
      (should-not (string-match-p "SECRET" serialized)))))

(ert-deftest test-ellama-tools-dlp-policy-overrides-and-exceptions ()
  (let* ((ellama-tools-dlp-enabled t)
         (ellama-tools-dlp-mode 'enforce)
         (ellama-tools-dlp-scan-env-exact-secrets nil)
         (ellama-tools-dlp-input-default-action 'block)
         (ellama-tools-dlp-regex-rules
          '((:id "token" :pattern "SECRET")))
         (base-context (ellama-tools-dlp--make-scan-context
                        :direction 'input
                        :tool-name "shell_command"
                        :arg-name "content"
                        :payload-length 0
                        :truncated nil)))
    (let ((ellama-tools-dlp-policy-overrides
           '((:tool "shell_command" :direction input :arg "content"
                    :action warn))))
      (let* ((result (ellama-tools-dlp--scan-text "SECRET" base-context))
             (verdict (plist-get result :verdict)))
        (should (eq (plist-get verdict :action) 'warn))
        (should (eq (plist-get verdict :configured-action) 'warn))))
    (let ((ellama-tools-dlp-policy-overrides
           '((:tool "shell_command" :direction input :arg "content"
                    :except t))))
      (let* ((result (ellama-tools-dlp--scan-text "SECRET" base-context))
             (verdict (plist-get result :verdict)))
        (should (eq (plist-get verdict :action) 'allow))
        (should (eq (plist-get verdict :configured-action) 'allow))))
    (let* ((nested-context (ellama-tools-dlp--make-scan-context
                            :direction 'input
                            :tool-name "shell_command"
                            :arg-name "content.items[0].token"
                            :payload-length 0
                            :truncated nil))
           (ellama-tools-dlp-policy-overrides
            '((:tool "shell_command" :direction input :arg "content"
                     :action warn))))
      (let* ((result (ellama-tools-dlp--scan-text "SECRET" nested-context))
             (verdict (plist-get result :verdict)))
        (should (eq (plist-get verdict :action) 'warn))
        (should (eq (plist-get verdict :configured-action) 'warn))))
    (let* ((nested-context (ellama-tools-dlp--make-scan-context
                            :direction 'input
                            :tool-name "shell_command"
                            :arg-name "content.items[0].token"
                            :payload-length 0
                            :truncated nil))
           (ellama-tools-dlp-policy-overrides
            '((:tool "shell_command" :direction input :arg "contentx"
                     :action warn))))
      (let* ((result (ellama-tools-dlp--scan-text "SECRET" nested-context))
             (verdict (plist-get result :verdict)))
        (should (eq (plist-get verdict :action) 'block))
        (should (eq (plist-get verdict :configured-action) 'block))))))

(ert-deftest test-ellama-tools-dlp-scan-text-output-redact ()
  (let* ((ellama-tools-dlp-enabled t)
         (ellama-tools-dlp-mode 'enforce)
         (ellama-tools-dlp-scan-env-exact-secrets nil)
         (ellama-tools-dlp-output-default-action 'redact)
         (ellama-tools-dlp-regex-rules
          '((:id "api-key" :pattern "SECRET")))
         (context (ellama-tools-dlp--make-scan-context
                   :direction 'output
                   :tool-name "read_file"
                   :arg-name nil
                   :payload-length 0
                   :truncated nil))
         (result (ellama-tools-dlp--scan-text "xxSECRETyy" context))
         (verdict (plist-get result :verdict)))
    (should (eq (plist-get verdict :action) 'redact))
    (should (equal (plist-get verdict :redacted-text)
                   "xx[REDACTED:api-key]yy"))
    (should-not (string-match-p "SECRET" (plist-get verdict :message)))))

(ert-deftest test-ellama-tools-dlp-scan-text-internal-error-input-fail-open ()
  (let* ((ellama-tools-dlp-enabled t)
         (ellama-tools-dlp-input-fail-open t)
         (ellama-tools-dlp-log-targets '(memory))
         (context (ellama-tools-dlp--make-scan-context
                   :direction 'input
                   :tool-name "shell_command"
                   :arg-name "content"
                   :payload-length 0
                   :truncated nil)))
    (ellama-tools-dlp--clear-incident-log)
    (cl-letf (((symbol-function 'ellama-tools-dlp--prepare-payload)
               (lambda (_text _context)
                 (error "boom"))))
      (let* ((result (ellama-tools-dlp--scan-text "SECRET" context))
             (verdict (plist-get result :verdict))
             (incident (car (ellama-tools-dlp--incident-log))))
        (should (eq (plist-get verdict :action) 'allow))
        (should (null (plist-get verdict :message)))
        (should (eq (plist-get incident :type) 'scan-error))
        (should (eq (plist-get incident :error-type) 'internal-error))))))

(ert-deftest
    test-ellama-tools-dlp-scan-text-internal-error-output-fail-closed ()
  (let* ((ellama-tools-dlp-enabled t)
         (ellama-tools-dlp-output-fail-open nil)
         (ellama-tools-dlp-log-targets '(memory))
         (context (ellama-tools-dlp--make-scan-context
                   :direction 'output
                   :tool-name "read_file"
                   :arg-name nil
                   :payload-length 0
                   :truncated nil)))
    (ellama-tools-dlp--clear-incident-log)
    (cl-letf (((symbol-function 'ellama-tools-dlp--prepare-payload)
               (lambda (_text _context)
                 (error "boom"))))
      (let* ((result (ellama-tools-dlp--scan-text "TOPSECRET" context))
             (verdict (plist-get result :verdict))
             (serialized (prin1-to-string verdict)))
        (should (eq (plist-get verdict :action) 'block))
        (should (string-match-p "internal error" (plist-get verdict :message)))
        (should-not (string-match-p "TOPSECRET" serialized))))))

(ert-deftest test-ellama-tools-dlp-redaction-failure-block ()
  (let* ((ellama-tools-dlp-mode 'enforce)
         (context (ellama-tools-dlp--make-scan-context
                   :direction 'output
                   :tool-name "read_file"
                   :arg-name nil
                   :payload-length 4
                   :truncated nil))
         (finding (ellama-tools-dlp--make-finding
                   :rule-id "token"
                   :detector 'regex))
         (verdict (ellama-tools-dlp--apply-enforcement
                   "TEXT" context (list finding) 'redact)))
    (should (eq (plist-get verdict :action) 'block))
    (should-not (plist-get verdict :redacted-text))))

(provide 'test-ellama-tools-dlp)
;;; test-ellama-tools-dlp.el ends here
