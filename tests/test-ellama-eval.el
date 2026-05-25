;;; test-ellama-eval.el --- Ellama eval tests -*- lexical-binding: t; package-lint-main-file: "../ellama.el"; -*-

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
;; Ellama eval tests.
;;

;;; Code:

(require 'cl-lib)
(require 'ellama-eval)
(require 'ert)

(ert-deftest test-ellama-eval-balanced-edit-file-tool-rejects-broken-elisp ()
  (let ((file (make-temp-file "ellama-eval-balanced-edit-" nil ".el")))
    (unwind-protect
        (let* ((old "(defun sample ()\n  (message \"ok\"))\n")
               (new "(defun sample ()\n  (message \"ok\")\n")
               result)
          (with-temp-file file
            (insert old))
          (setq result
                (ellama-eval-balanced-edit-file-tool file old new))
          (should (string-match-p "Edit rejected" result))
          (should (string-match-p "Mode: emacs-lisp-mode" result))
          (should (string-match-p "Replacement fragment" result))
          (should (string-match-p "Missing closers" result))
          (with-temp-buffer
            (insert-file-contents file)
            (should (equal (buffer-string) old))))
      (when-let* ((buffer (get-file-buffer file)))
        (kill-buffer buffer))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest test-ellama-eval-balanced-edit-file-tool-applies-valid-elisp ()
  (let ((file (make-temp-file "ellama-eval-balanced-edit-" nil ".el")))
    (unwind-protect
        (let* ((old "(defun sample ()\n  (message \"old\"))\n")
               (new "(defun sample ()\n  (message \"new\"))\n")
               result)
          (with-temp-file file
            (insert old))
          (setq result
                (ellama-eval-balanced-edit-file-tool file old new))
          (should (string-match-p "after syntax validation" result))
          (with-temp-buffer
            (insert-file-contents file)
            (should (equal (buffer-string) new)))
          (with-current-buffer (find-file-noselect file)
            (should (equal (buffer-string) new))))
      (when-let* ((buffer (get-file-buffer file)))
        (kill-buffer buffer))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest test-ellama-eval-balanced-edit-uses-existing-buffer-mode ()
  (let ((file (make-temp-file "ellama-eval-balanced-edit-" nil ".txt")))
    (unwind-protect
        (let* ((old "(defun sample ()\n  (message \"ok\"))\n")
               (new "(defun sample ()\n  (message \"ok\")\n")
               result)
          (with-temp-file file
            (insert old))
          (with-current-buffer (find-file-noselect file)
            (emacs-lisp-mode))
          (setq result
                (ellama-eval-balanced-edit-file-tool file old new))
          (should (string-match-p "Mode: emacs-lisp-mode" result)))
      (when-let* ((buffer (get-file-buffer file)))
        (kill-buffer buffer))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest test-ellama-eval-monitored-edit-file-logs-invalid-edit ()
  (let ((file (make-temp-file "ellama-eval-monitored-edit-" nil ".el"))
        (state (make-ellama-eval--run-state
                :edit-validation-trace nil)))
    (unwind-protect
        (let* ((old "(defun sample ()\n  (message \"ok\"))\n")
               (new "(defun sample ()\n  (message \"ok\")\n")
               (ellama-eval--active-run state))
          (with-temp-file file
            (insert old))
          (should
           (string-match-p
            "Edited "
            (ellama-eval-monitored-edit-file-tool file old new nil)))
          (let ((entry
                 (car
                  (ellama-eval--run-state-edit-validation-trace state))))
            (should (equal (plist-get entry :tool) "edit_file"))
            (should (plist-get entry :candidate-found))
            (should (plist-get entry :applied))
            (should-not (plist-get entry :blocked))
            (should-not (plist-get entry :valid))
            (should (= (plist-get entry :missing-closers) 1))))
      (when-let* ((buffer (get-file-buffer file)))
        (kill-buffer buffer))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest test-ellama-eval-monitored-edit-file-logs-missing-candidate ()
  (let ((file (make-temp-file "ellama-eval-monitored-edit-" nil ".el"))
        (state (make-ellama-eval--run-state
                :edit-validation-trace nil)))
    (unwind-protect
        (let ((ellama-eval--active-run state))
          (with-temp-file file
            (insert "(defun sample () nil)\n"))
          (should
           (string-match-p
            "No replacement made"
            (ellama-eval-monitored-edit-file-tool
             file "(defun missing () nil)\n" "(defun sample () t)\n" nil)))
          (let ((entry
                 (car
                  (ellama-eval--run-state-edit-validation-trace state))))
            (should (equal (plist-get entry :tool) "edit_file"))
            (should-not (plist-get entry :candidate-found))
            (should-not (plist-get entry :applied))
            (should-not (plist-get entry :blocked))
            (should-not (plist-member entry :valid))))
      (when-let* ((buffer (get-file-buffer file)))
        (kill-buffer buffer))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest test-ellama-eval-profile-tools-switch-read-and-edit-shapes ()
  "Verify both baseline and balanced-edit use edit_file."
  (let ((ellama-tools-allow-all t)
        (ellama-tools-allowed nil)
        (ellama-tools-confirm-allowed (make-hash-table))
        (baseline (ellama-eval--make-profile-tools 'baseline))
        (balanced (ellama-eval--make-profile-tools 'balanced-edit)))
    (should (member "edit_file" (mapcar #'llm-tool-name baseline)))
    (should (member "edit_file" (mapcar #'llm-tool-name balanced)))
    (should (member "read_file" (mapcar #'llm-tool-name baseline)))
    (should (member "read_file" (mapcar #'llm-tool-name balanced)))))

(ert-deftest test-ellama-eval-run-case-scores-files-and-answer ()
  (let ((case
         '(:id "stub-case"
               :suite edit
               :prompt "Patch the file."
               :files (("sample.el" . "before\n"))
               :expected-files (("sample.el" . "after\n"))
               :answer-regexps ("done"))))
    (cl-letf (((symbol-function 'ellama-tools-task-tool)
               (lambda (callback &optional _description _role
                                 _template _template-base _arguments)
                 (with-temp-file
                     (expand-file-name "sample.el" default-directory)
                   (insert "after\n"))
                 (funcall callback "done")
                 nil)))
      (let ((result (ellama-eval-run-case case 'baseline)))
        (should (eq (plist-get result :status) 'passed))
        (should (plist-get result :success))
        (should (equal (plist-get result :report-result) "done"))
        (should
         (= (plist-get result :tool-call-count) 0))))))

(ert-deftest test-ellama-eval-run-case-can-ignore-docstring-wording ()
  (let ((case
         '(:id "docstring-case"
               :suite edit
               :prompt "Patch the file."
               :files
               (("sample.el" . "(defun sample (value limit weight)\n  \"Return VALUE scaled by WEIGHT, clamped to LIMIT.\"\n  (* value weight))\n"))
               :expected-files
               (("sample.el" . "(defun sample (value limit weight)\n  \"Return VALUE scaled by WEIGHT, clamped to LIMIT.\"\n  (* (min value limit) weight))\n"))
               :ignore-docstrings t
               :file-regexps
               (("sample.el" . ("\"[^\"]*VALUE[^\"]*LIMIT[^\"]*WEIGHT[^\"]*\""))))))
    (cl-letf (((symbol-function 'ellama-tools-task-tool)
               (lambda (callback &optional _description _role
                                 _template _template-base _arguments)
                 (with-temp-file
                     (expand-file-name "sample.el" default-directory)
                   (insert
                    "(defun sample (value limit weight)\n"
                    "  \"Return VALUE clamped to LIMIT, multiplied by WEIGHT.\"\n"
                    "  (* (min value limit) weight))\n"))
                 (funcall callback "done")
                 nil)))
      (let* ((result (ellama-eval-run-case case 'baseline))
             (file-check (car (plist-get result :file-checks)))
             (regexp-check (car (plist-get result :file-regexp-checks))))
        (should (eq (plist-get result :status) 'passed))
        (should (plist-get result :success))
        (should (plist-get file-check :ignore-docstrings))
        (should (plist-get file-check :matched))
        (should (plist-get regexp-check :matched))))))

(ert-deftest test-ellama-eval-build-result-includes-edit-validation-summary ()
  (let* ((workspace (make-temp-file "ellama-eval-result-" t))
         (file (expand-file-name "sample.el" workspace))
         (state
          (make-ellama-eval--run-state
           :status 'pending
           :trace nil
           :edit-validation-trace
           (list
            (list :tool "edit_file"
                  :file "sample.el"
                  :candidate-found t
                  :applied t
                  :blocked nil
                  :valid t)
            (list :tool "edit_file"
                  :file "sample.el"
                  :candidate-found t
                  :applied nil
                  :blocked t
                  :valid nil))
           :started-at (float-time)
           :workspace workspace
           :case '(:id "validation-summary"
                       :suite edit
                       :expected-files
                       (("sample.el" . "(defun sample () nil)\n")))
           :profile 'balanced-edit))
         result)
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "(defun sample () nil)\n"))
          (setq result (ellama-eval--build-result state 'completed))
          (should (= (plist-get result :edit-validation-count) 2))
          (should (= (plist-get result :edit-rejection-count) 1))
          (should (= (plist-get result :edit-invalid-candidate-count) 1))
          (should (= (plist-get result :edit-missing-candidate-count) 0))
          (should (= (plist-get result :edit-recovery-count) 1))
          (should (= (plist-get result :syntax-valid-final-files) 1))
          (should (= (plist-get result :syntax-invalid-final-files) 0))
          (should (= (length (plist-get result :edit-validation-trace)) 2))
          (should (= (length (plist-get result :final-syntax-checks)) 1)))
      (when-let* ((buffer (get-file-buffer file)))
        (kill-buffer buffer))
      (when (file-directory-p workspace)
        (delete-directory workspace t)))))

(ert-deftest test-ellama-eval-build-result-runs-elisp-checks ()
  (let* ((workspace (make-temp-file "ellama-eval-elisp-check-" t))
         (file (expand-file-name "sample.el" workspace))
         (state
          (make-ellama-eval--run-state
           :status 'pending
           :trace nil
           :edit-validation-trace nil
           :started-at (float-time)
           :workspace workspace
           :case '(:id "elisp-check"
                       :suite edit
                       :syntax-files ("sample.el")
                       :elisp-checks
                       (("sample.el"
                         ("sample returns expected keyword"
                          (eq (sample) :ok)))))
           :profile 'baseline))
         result)
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "(defun sample () :ok)\n"))
          (setq result (ellama-eval--build-result state 'completed))
          (should (eq (plist-get result :status) 'passed))
          (should (plist-get result :success))
          (should (= (length (plist-get result :elisp-checks)) 1))
          (should
           (plist-get (car (plist-get result :elisp-checks)) :matched))
          (should (= (plist-get result :syntax-valid-final-files) 1)))
      (when (fboundp 'sample)
        (fmakunbound 'sample))
      (when-let* ((buffer (get-file-buffer file)))
        (kill-buffer buffer))
      (when (file-directory-p workspace)
        (delete-directory workspace t)))))

(ert-deftest test-ellama-eval-async-suite-calls-progress-and-completion ()
  (let ((completed nil)
        (progress nil)
        (deadline (+ (float-time) 2.0)))
    (cl-letf (((symbol-function 'ellama-eval-start-case)
               (lambda (case profile callback &optional _provider)
                 (funcall callback
                          (list :case-id (plist-get case :id)
                                :suite (plist-get case :suite)
                                :profile profile
                                :status 'passed
                                :success t
                                :steps 1
                                :tool-call-count 2)))))
      (ellama-eval-run-hypothesis-suite-async
       nil '(baseline)
       '((:id "one" :suite edit)
         (:id "two" :suite explore))
       (lambda (results)
         (setq completed results))
       (lambda (_results count total _latest)
         (push (list count total) progress)))
      (while (and (null completed)
                  (< (float-time) deadline))
        (accept-process-output nil 0.01))
      (should (= (length completed) 2))
      (should (equal (nreverse progress)
                     '((0 2) (1 2) (2 2)))))))

(ert-deftest test-ellama-eval-timeout-run-finalizes-state ()
  (let* ((workspace (make-temp-file "ellama-eval-timeout-" t))
         (callback-result nil)
         (state
          (make-ellama-eval--run-state
           :status 'pending
           :trace nil
           :started-at (float-time)
           :workspace workspace
           :case '(:id "timeout" :suite edit)
           :profile 'baseline
           :callback (lambda (result)
                       (setq callback-result result)))))
    (let ((ellama-eval--active-run state))
      (ellama-eval--timeout-run state)
      (should (null ellama-eval--active-run)))
    (should (eq (plist-get callback-result :status) 'timeout))
    (should-not (plist-get callback-result :success))
    (should-not (file-directory-p workspace))))

(ert-deftest test-ellama-eval-summarize-results ()
  (let* ((results
          '((:suite edit :profile baseline :success t
                    :steps 2 :tool-call-count 3)
            (:suite edit :profile baseline :success nil
                    :steps 4 :tool-call-count 5)))
         (summary (ellama-eval-summarize-results results))
         (row (car summary)))
    (should (equal (plist-get row :runs) 2))
    (should (equal (plist-get row :passed) 1))
    (should (= (plist-get row :success-rate) 0.5))
    (should (= (plist-get row :mean-steps) 3.0))
    (should (= (plist-get row :mean-tool-calls) 4.0))))

(ert-deftest test-ellama-eval-render-summary-buffer ()
  (let* ((results
          '((:case-id "one" :suite edit :profile baseline :status passed
                      :success t :steps 2 :tool-call-count 3
                      :elapsed-ms 40)))
         (buffer
          (ellama-eval--render-summary-buffer results 1 4 "/tmp/out.jsonl")))
    (unwind-protect
        (with-current-buffer buffer
          (let ((text (buffer-string)))
            (should (string-match-p "Progress: 1/4 completed" text))
            (should (string-match-p "JSONL target: /tmp/out.jsonl" text))
            (should (string-match-p "Aggregate summary" text))
            (should (string-match-p "one" text))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest test-ellama-eval-write-results-jsonl-uses-arrays-and-booleans ()
  (let* ((file-name (make-temp-file "ellama-eval-results-" nil ".jsonl"))
         (results
          '((:case-id "case"
                      :suite edit
                      :profile baseline
                      :status failed
                      :success nil
                      :tool-trace
                      ((:name "grep"
                              :args ("." "needle")
                              :status ok
                              :result "match"
                              :finished-at 1.0)
                       (:name "read_file"
                              :args ("sample.el" nil)
                              :status ok
                              :result nil
                              :finished-at 2.0))
                      :file-checks
                      ((:path "sample.el"
                              :expected "expected"
                              :actual "actual"
                              :matched nil))
                      :file-regexp-checks
                      ((:path "sample.el"
                              :regexp "VALUE"
                              :matched t))
                      :elisp-checks
                      ((:path "sample.el"
                              :description "sample check"
                              :form "(sample)"
                              :result "t"
                              :matched t))
                      :answer-checks
                      ((:regexp "string"
                                :matched t))
                      :edit-validation-trace
                      ((:tool "edit_file"
                              :file "sample.el"
                              :candidate-found t
                              :applied nil
                              :blocked t
                              :valid nil
                              :original-valid t
                              :old-fragment-valid t
                              :new-fragment-valid nil))
                      :final-syntax-checks
                      ((:path "sample.el"
                              :valid nil
                              :error "Unbalanced"))
                      :workspace nil))))
    (unwind-protect
        (progn
          (ellama-eval-write-results-jsonl results file-name)
          (let* ((json-false :false)
                 (parsed
                  (json-parse-string
                   (with-temp-buffer
                     (insert-file-contents file-name)
                     (buffer-substring-no-properties
                      (point-min) (line-end-position)))
                   :object-type 'alist
                   :array-type 'list
                   :false-object json-false))
                 (trace (alist-get 'tool-trace parsed))
                 (file-checks (alist-get 'file-checks parsed))
                 (file-regexp-checks
                  (alist-get 'file-regexp-checks parsed))
                 (elisp-checks (alist-get 'elisp-checks parsed))
                 (answer-checks (alist-get 'answer-checks parsed))
                 (validation-trace
                  (alist-get 'edit-validation-trace parsed))
                 (final-syntax-checks
                  (alist-get 'final-syntax-checks parsed)))
            (should (eq (alist-get 'success parsed) json-false))
            (should (assq 'workspace parsed))
            (should (null (alist-get 'workspace parsed)))
            (should (= (length trace) 2))
            (should (equal (alist-get 'name (car trace)) "grep"))
            (should (equal (alist-get 'args (car trace))
                           '("." "needle")))
            (should (equal (alist-get 'args (cadr trace))
                           '("sample.el" nil)))
            (should (assq 'result (cadr trace)))
            (should (null (alist-get 'result (cadr trace))))
            (should (eq (alist-get 'matched (car file-checks))
                        json-false))
            (should (eq (alist-get 'matched (car file-regexp-checks))
                        t))
            (should (eq (alist-get 'matched (car elisp-checks)) t))
            (should (eq (alist-get 'matched (car answer-checks))
                        t))
            (should (= (length validation-trace) 1))
            (should (eq (alist-get 'candidate-found (car validation-trace))
                        t))
            (should (eq (alist-get 'applied (car validation-trace))
                        json-false))
            (should (eq (alist-get 'blocked (car validation-trace))
                        t))
            (should (eq (alist-get 'valid (car validation-trace))
                        json-false))
            (should (eq (alist-get 'new-fragment-valid
                                   (car validation-trace))
                        json-false))
            (should (eq (alist-get 'valid (car final-syntax-checks))
                        json-false))))
      (when (file-exists-p file-name)
        (delete-file file-name)))))

(ert-deftest test-ellama-eval-cases-for-suite-selection ()
  (let ((edit-cases (ellama-eval--cases-for-suite-selection 'edit))
        (all-cases (ellama-eval--cases-for-suite-selection 'all)))
    (should edit-cases)
    (should
     (cl-every
      (lambda (case)
        (eq (plist-get case :suite) 'edit))
      edit-cases))
    (should (= (length all-cases)
               (length ellama-eval-hypothesis-cases)))))

(ert-deftest test-ellama-eval-cases-include-nested-edit-cases ()
  (let* ((ids (mapcar (lambda (case) (plist-get case :id))
                      ellama-eval-hypothesis-cases))
         (nested-ids (seq-filter
                      (lambda (id)
                        (string-prefix-p "edit-nested-" id))
                      ids)))
    (should (>= (length nested-ids) 8))
    (dolist (id '("edit-nested-plist-construction"
                  "edit-nested-branch-plists"
                  "edit-nested-backquote-template"
                  "edit-nested-filter-lambda"
                  "edit-nested-condition-case"
                  "edit-nested-pcase-branch"
                  "edit-nested-accumulator"
                  "edit-nested-state-machine"))
      (should (member id ids)))))

(ert-deftest test-ellama-eval-read-results-file-expands-relative-selection ()
  (let* ((dir (make-temp-file "ellama-eval-results-dir-" t))
         (default-directory dir)
         (buffer-file-name (expand-file-name "active.el" dir))
         captured-args)
    (unwind-protect
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (&rest _args) t))
                  ((symbol-function 'read-file-name)
                   (lambda (&rest args)
                     (setq captured-args args)
                     "ellama-eval-results-baseline.jsonl")))
          (should
           (equal
            (ellama-eval--read-results-file)
            (expand-file-name "ellama-eval-results-baseline.jsonl" dir)))
          (should
           (equal (nth 1 captured-args) dir))
          (should
           (equal (nth 2 captured-args)
                  (expand-file-name "ellama-eval-results.jsonl" dir))))
      (when (file-directory-p dir)
        (delete-directory dir t)))))

(ert-deftest test-ellama-eval-read-results-file-refuses-current-buffer-file ()
  (let* ((dir (make-temp-file "ellama-eval-results-dir-" t))
         (default-directory dir)
         (buffer-file-name (expand-file-name "active.el" dir)))
    (unwind-protect
        (progn
          (with-temp-file buffer-file-name
            (insert "keep me"))
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (&rest _args) t))
                    ((symbol-function 'read-file-name)
                     (lambda (&rest _args) buffer-file-name)))
            (should-error
             (ellama-eval--read-results-file)
             :type 'user-error)))
      (when (file-directory-p dir)
        (delete-directory dir t)))))

(ert-deftest test-ellama-eval-select-model-uses-generic-selector ()
  (let ((ellama-coding-provider 'base-provider)
        (ellama-provider nil)
        filled-provider
        read-provider
        constructed-provider
        required-feature)
    (cl-letf (((symbol-function 'require)
               (lambda (feature &optional _filename _noerror)
                 (setq required-feature feature)
                 t))
              ((symbol-function 'ellama-fill-transient-model)
               (lambda (provider)
                 (setq filled-provider provider)))
              ((symbol-function 'ellama-transient-read-model-name)
               (lambda (provider)
                 (setq read-provider provider)
                 "selected-model"))
              ((symbol-function 'ellama-construct-provider-from-transient)
               (lambda (provider)
                 (setq constructed-provider provider)
                 (list :provider provider
                       :model ellama-transient-model-name))))
      (should (equal (ellama-eval--select-model-provider)
                     '(:provider base-provider :model "selected-model")))
      (should (eq required-feature 'ellama-transient))
      (should (eq filled-provider 'base-provider))
      (should (eq read-provider 'base-provider))
      (should (eq constructed-provider 'base-provider)))))

(provide 'test-ellama-eval)

;;; test-ellama-eval.el ends here

(ert-deftest test-ellama-eval-loop-detection-identical-calls ()
  "Test that identical tool calls trigger loop detection."
  (let ((ellama-eval--active-run nil)
        (ellama-eval-loop-detection-enabled t)
        (ellama-eval-loop-detection-repeated-threshold 3)
        (ellama-eval-loop-detection-max-traces 50)
        state)
    ;; Create a run state with loop detection enabled
    (setq state
          (make-ellama-eval--run-state
           :status 'pending
           :trace nil
           :edit-validation-trace nil
           :started-at (float-time)
           :workspace "/tmp"
           :case '(:id "test")
           :profile 'baseline
           :callback nil
           :loop-state
           (list :tool-history nil :loop-detected nil :loop-reason "")))
    (setq ellama-eval--active-run state)
    ;; Simulate 3 identical tool calls
    (let ((identical-args '(:path "/test.el")))
      (dotimes (i 3)
        (ellama-eval--trace-tool-call "read_file" identical-args 'ok "content"))
      (should (eq (ellama-eval--run-state-status state) 'loop-detected))
      (should (string-match-p "Loop detected: tool read_file called"
                              (plist-get (ellama-eval--run-state-loop-state state) :loop-reason))))))

(ert-deftest test-ellama-eval-loop-detection-different-args ()
  "Test that different tool args don't trigger loop detection."
  (let ((ellama-eval--active-run nil)
        (ellama-eval-loop-detection-enabled t)
        (ellama-eval-loop-detection-repeated-threshold 3)
        (ellama-eval-loop-detection-max-traces 50)
        state)
    (setq state
          (make-ellama-eval--run-state
           :status 'pending
           :trace nil
           :edit-validation-trace nil
           :started-at (float-time)
           :workspace "/tmp"
           :case '(:id "test")
           :profile 'baseline
           :callback nil
           :loop-state
           (list :tool-history nil :loop-detected nil :loop-reason "")))
    (setq ellama-eval--active-run state)
    ;; Simulate 3 calls with different args (should NOT trigger loop)
    (dotimes (i 3)
      (ellama-eval--trace-tool-call
       "read_file"
       (list :path (format "/test-%d.el" i))
       'ok "content"))
    (should-not (eq (ellama-eval--run-state-status state) 'loop-detected))
    (should (string= "" (plist-get (ellama-eval--run-state-loop-state state) :loop-reason)))))

(ert-deftest test-ellama-eval-loop-detection-disabled ()
  "Test that loop detection can be disabled."
  (let ((ellama-eval--active-run nil)
        (ellama-eval-loop-detection-enabled nil)
        (ellama-eval-loop-detection-repeated-threshold 3)
        (ellama-eval-loop-detection-max-traces 50)
        state)
    (setq state
          (make-ellama-eval--run-state
           :status 'pending
           :trace nil
           :edit-validation-trace nil
           :started-at (float-time)
           :workspace "/tmp"
           :case '(:id "test")
           :profile 'baseline
           :callback nil
           :loop-state
           (list :tool-history nil :loop-detected nil :loop-reason "")))
    (setq ellama-eval--active-run state)
    ;; Simulate 5 identical calls (should NOT trigger since disabled)
    (let ((identical-args '(:path "/test.el")))
      (dotimes (i 5)
        (ellama-eval--trace-tool-call "read_file" identical-args 'ok "content"))
      (should-not (eq (ellama-eval--run-state-status state) 'loop-detected)))))

(ert-deftest test-ellama-eval-loop-detection-history-trimming ()
  "Test that tool call history is trimmed at max-traces limit."
  (let ((ellama-eval--active-run nil)
        (ellama-eval-loop-detection-enabled t)
        (ellama-eval-loop-detection-repeated-threshold 3)
        (ellama-eval-loop-detection-max-traces 5)
        state)
    (setq state
          (make-ellama-eval--run-state
           :status 'pending
           :trace nil
           :edit-validation-trace nil
           :started-at (float-time)
           :workspace "/tmp"
           :case '(:id "test")
           :profile 'baseline
           :callback nil
           :loop-state
           (list :tool-history nil :loop-detected nil :loop-reason "")))
    (setq ellama-eval--active-run state)
    ;; Make 10 tool calls with different args
    (dotimes (i 10)
      (ellama-eval--trace-tool-call
       "grep"
       (list :string (format "pattern-%d" i) :dir "/tmp")
       'ok "result"))
    (let ((history (plist-get (ellama-eval--run-state-loop-state state) :tool-history)))
      (should (= (length history) 5))
      (should (equal (plist-get (car history) :name) "grep")))))

(ert-deftest test-ellama-eval-loop-detection-no-active-run ()
  "Test that loop detection gracefully handles missing active run."
  (let ((ellama-eval--active-run nil)
        (ellama-eval-loop-detection-enabled t)
        (ellama-eval-loop-detection-repeated-threshold 3))
    ;; Should not signal an error when no active run exists
    (should
     (null
      (ignore-errors
        (ellama-eval--trace-tool-call
         "read_file" '(:path "/test.el") 'ok "content")
        nil)))))

(ert-deftest test-ellama-eval-loop-detection-trace-recorded ()
  "Test that tool calls are still traced even when loop detection is active."
  (let ((ellama-eval--active-run nil)
        (ellama-eval-loop-detection-enabled t)
        (ellama-eval-loop-detection-repeated-threshold 3)
        (ellama-eval-loop-detection-max-traces 50)
        state)
    (setq state
          (make-ellama-eval--run-state
           :status 'pending
           :trace nil
           :edit-validation-trace nil
           :started-at (float-time)
           :workspace "/tmp"
           :case '(:id "test")
           :profile 'baseline
           :callback nil
           :loop-state
           (list :tool-history nil :loop-detected nil :loop-reason "")))
    (setq ellama-eval--active-run state)
    ;; Make 3 identical calls (triggers loop detection)
    (let ((identical-args '(:path "/test.el")))
      (dotimes (i 3)
        (ellama-eval--trace-tool-call "read_file" identical-args 'ok "content")))
    ;; Verify trace still has all calls
    (let ((trace (ellama-eval--run-state-trace state)))
      (should (= (length trace) 3))
      (should (seq-every-p (lambda (entry)
                             (eq (plist-get entry :status) 'ok))
                           trace)))))

(provide 'test-ellama-eval)
