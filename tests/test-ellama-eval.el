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

(ert-deftest test-ellama-eval-number-text-lines ()
  (should
   (equal
    (ellama-eval--number-text-lines "alpha\nbeta\n" 7)
    "7 | alpha\n8 | beta")))

(ert-deftest test-ellama-eval-edit-lines-tool-replaces-range ()
  (let ((file (make-temp-file "ellama-eval-edit-lines-")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "alpha\nbeta\ngamma\n"))
          (should
           (string-match-p
            "Replaced lines 2-2"
            (ellama-eval-edit-lines-tool file 2 2 "delta")))
          (with-temp-buffer
            (insert-file-contents file)
            (should (equal (buffer-string) "alpha\ndelta\ngamma\n"))))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest test-ellama-eval-profile-tools-switch-read-and-edit-shapes ()
  (let ((ellama-tools-allow-all t)
        (ellama-tools-allowed nil)
        (ellama-tools-confirm-allowed (make-hash-table))
        (baseline (ellama-eval--make-profile-tools 'baseline))
        (numbered (ellama-eval--make-profile-tools 'numbered-line-edit)))
    (should (member "edit_file" (mapcar #'llm-tool-name baseline)))
    (should-not (member "edit_lines" (mapcar #'llm-tool-name baseline)))
    (should (member "edit_lines" (mapcar #'llm-tool-name numbered)))
    (should-not (member "edit_file" (mapcar #'llm-tool-name numbered)))
    (let* ((tool
            (seq-find
             (lambda (item)
               (string= (llm-tool-name item) "read_file"))
             numbered))
           (function (llm-tool-function tool))
           (file (make-temp-file "ellama-eval-numbered-read-")))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "one\ntwo\n"))
            (should
             (equal
              (json-parse-string (funcall function file "text"))
              "1 | one\n2 | two")))
        (when (file-exists-p file)
          (delete-file file))))))

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

(provide 'test-ellama-eval)

;;; test-ellama-eval.el ends here
