;;; test-ellama-community-prompts.el --- Community prompts tests -*- lexical-binding: t; package-lint-main-file: "../ellama.el"; -*-

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

;; Community prompt collection tests.

;;; Code:

(require 'cl-lib)
(require 'ert)

(defconst ellama-community-prompts-test-root
  (expand-file-name
   ".."
   (file-name-directory (or load-file-name buffer-file-name)))
  "Project root directory for community prompts tests.")

(load-file
 (expand-file-name
  "ellama-community-prompts.el"
  ellama-community-prompts-test-root))

(ert-deftest test-ellama-community-prompts-parse-csv-line-basic ()
  (should (equal (ellama-community-prompts-parse-csv-line
                  "writer,Write a poem,FALSE")
                 '("writer" "Write a poem" "FALSE"))))

(ert-deftest test-ellama-community-prompts-parse-csv-line-quoted-comma ()
  (should (equal (ellama-community-prompts-parse-csv-line
                  "writer,\"Prompt, with comma\",FALSE")
                 '("writer" "Prompt, with comma" "FALSE"))))

(ert-deftest test-ellama-community-prompts-parse-csv-line-escaped-quote ()
  (should (equal (ellama-community-prompts-parse-csv-line
                  "writer,\"He said \"\"Hi\"\"\",TRUE")
                 '("writer" "He said \"Hi\"" "TRUE"))))

(ert-deftest test-ellama-community-prompts-convert-to-plist ()
  (should (equal (ellama-community-prompts-convert-to-plist
                  '("dev" "Use tests" "TRUE"))
                 '(:act "dev" :prompt "Use tests" :for-devs t)))
  (should (equal (ellama-community-prompts-convert-to-plist
                  '("all" "General" "FALSE"))
                 '(:act "all" :prompt "General" :for-devs nil))))

(ert-deftest test-ellama-community-prompts-ensure-file-skip-download ()
  (let ((ellama-community-prompts-file
         (make-temp-file "ellama-community-prompts-existing-"))
        (plz-called nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'plz)
                     (lambda (&rest _args)
                       (setq plz-called t))))
            (ellama-community-prompts-ensure-file))
          (should-not plz-called))
      (when (file-exists-p ellama-community-prompts-file)
        (delete-file ellama-community-prompts-file)))))

(ert-deftest test-ellama-community-prompts-ensure-file-download ()
  (let* ((root-dir (make-temp-file "ellama-community-prompts-root-" t))
         (target-dir (expand-file-name "nested/path" root-dir))
         (ellama-community-prompts-file
          (expand-file-name "community-prompts.csv" target-dir))
         (ellama-community-prompts-url "https://example.invalid/prompts.csv")
         (downloaded (make-temp-file "ellama-community-prompts-downloaded-"))
         (called nil))
    (unwind-protect
        (progn
          (with-temp-file downloaded
            (insert "act,prompt,for_dev\nwriter,Prompt,FALSE\n"))
          (cl-letf (((symbol-function 'plz)
                     (lambda (method url &rest args)
                       (setq called t)
                       (should (eq method 'get))
                       (should (equal url ellama-community-prompts-url))
                       (should (eq (plist-get args :as) 'file))
                       (funcall (plist-get args :then) downloaded)
                       t)))
            (ellama-community-prompts-ensure-file))
          (should called)
          (should (file-exists-p ellama-community-prompts-file))
          (should (equal (with-temp-buffer
                           (insert-file-contents ellama-community-prompts-file)
                           (buffer-string))
                         "act,prompt,for_dev\nwriter,Prompt,FALSE\n")))
      (when (file-exists-p ellama-community-prompts-file)
        (delete-file ellama-community-prompts-file))
      (when (file-exists-p root-dir)
        (delete-directory root-dir t))
      (when (file-exists-p downloaded)
        (delete-file downloaded)))))

(ert-deftest test-ellama-community-prompts-ensure-loads-and-caches ()
  (let ((ellama-community-prompts-collection nil)
        (csv-file (make-temp-file "ellama-community-prompts-csv-")))
    (unwind-protect
        (progn
          (with-temp-file csv-file
            (insert
             "act,prompt,for_dev\n"
             "dev,\"Prompt, with comma\",TRUE\n"
             "all,General,FALSE\n"))
          (let ((ellama-community-prompts-file csv-file))
            (let ((first (ellama-community-prompts-ensure)))
              (should (equal first
                             '((:act "dev"
                                :prompt "Prompt, with comma"
                                :for-devs t)
                               (:act "all"
                                :prompt "General"
                                :for-devs nil))))
              (with-temp-file csv-file
                (insert
                 "act,prompt,for_dev\n"
                 "changed,Changed,FALSE\n"))
              (let ((second (ellama-community-prompts-ensure)))
                (should (eq first second))
                (should (= 2 (length second)))))))
      (when (file-exists-p csv-file)
        (delete-file csv-file)))))

(ert-deftest test-ellama-community-prompts-select-blueprint ()
  (let ((called-args nil))
    (cl-letf (((symbol-function 'ellama-blueprint-select)
               (lambda (args)
                 (setq called-args args))))
      (ellama-community-prompts-select-blueprint)
      (should (equal called-args '(:source community))))))

(provide 'test-ellama-community-prompts)
;;; test-ellama-community-prompts.el ends here.
