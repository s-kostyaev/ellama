;;; test-ellama-blueprint.el --- Ellama blueprint tests -*- lexical-binding: t; package-lint-main-file: "../ellama.el"; -*-

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

;; Ellama blueprint tests.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'ellama-blueprint)

(defun ellama-test--blueprint-index-by-act (blueprints)
  "Return BLUEPRINTS indexed by :act."
  (let ((index (make-hash-table :test #'equal)))
    (dolist (blueprint blueprints)
      (puthash (plist-get blueprint :act) blueprint index))
    index))

(ert-deftest test-ellama-blueprint-get-local-dir ()
  (let ((ellama-blueprint-local-dir "my-blueprints"))
    (cl-letf (((symbol-function 'ellama-tools-project-root-tool)
               (lambda () "/tmp/project-root")))
      (should (equal (ellama-blueprint-get-local-dir)
                     "/tmp/project-root/my-blueprints")))))

(ert-deftest test-ellama-blueprint-find-files-filters-extensions ()
  (let* ((root (make-temp-file "ellama-blueprint-find-" t))
         (nested (expand-file-name "nested" root))
         (first (expand-file-name "a.ellama-blueprint" root))
         (second (expand-file-name "b.blueprint" nested))
         (ignored (expand-file-name "c.txt" nested)))
    (unwind-protect
        (progn
          (make-directory nested t)
          (with-temp-file first (insert "A"))
          (with-temp-file second (insert "B"))
          (with-temp-file ignored (insert "ignored"))
          (should (equal (sort (mapcar #'file-name-nondirectory
                                       (ellama-blueprint-find-files root))
                               #'string<)
                         '("a.ellama-blueprint" "b.blueprint"))))
      (when (file-exists-p root)
        (delete-directory root t)))))

(ert-deftest test-ellama-blueprint-find-files-missing-dir ()
  (should-not
   (ellama-blueprint-find-files
    "/tmp/ellama-blueprint-dir-does-not-exist-12345")))

(ert-deftest test-ellama-blueprint-read-file ()
  (let ((file (make-temp-file "ellama-blueprint-read-")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "hello"))
          (should (equal (ellama-blueprint-read-file file) "hello"))
          (delete-file file)
          (should-not (ellama-blueprint-read-file file)))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest test-ellama-blueprint-load-from-files-global-and-local ()
  (let* ((global-dir (make-temp-file "ellama-blueprint-global-" t))
         (local-dir (make-temp-file "ellama-blueprint-local-" t))
         (global-file (expand-file-name "global.ellama-blueprint" global-dir))
         (local-file (expand-file-name "local.blueprint" local-dir)))
    (unwind-protect
        (progn
          (with-temp-file global-file
            (insert "  Global prompt  \n"))
          (with-temp-file local-file
            (insert "\nLocal prompt\n"))
          (let* ((ellama-blueprint-global-dir global-dir)
                 (loaded
                  (cl-letf (((symbol-function 'ellama-blueprint-get-local-dir)
                             (lambda () local-dir)))
                    (ellama-blueprint-load-from-files)))
                 (index (ellama-test--blueprint-index-by-act loaded))
                 (global (gethash "global" index))
                 (local (gethash "local" index)))
            (should (= (length loaded) 2))
            (should (equal (plist-get global :prompt) "Global prompt"))
            (should (equal (plist-get local :prompt) "Local prompt"))
            (should (equal (plist-get global :file) global-file))
            (should (equal (plist-get local :file) local-file))))
      (when (file-exists-p global-dir)
        (delete-directory global-dir t))
      (when (file-exists-p local-dir)
        (delete-directory local-dir t)))))

(ert-deftest test-ellama-blueprint-get-all-sources-dedupes-by-act ()
  (let ((ellama-blueprints '((:act "shared" :prompt "user")
                             (:act "user-only" :prompt "user-only"))))
    (cl-letf (((symbol-function 'ellama-blueprint-load-from-files)
               (lambda ()
                 '((:act "shared" :prompt "file")
                   (:act "file-only" :prompt "file-only"))))
              ((symbol-function 'ellama-community-prompts-ensure)
               (lambda ()
                 '((:act "shared" :prompt "community")
                   (:act "community-only" :prompt "community-only")))))
      (let* ((all (ellama-blueprint-get-all-sources))
             (acts (mapcar (lambda (blueprint)
                             (plist-get blueprint :act))
                           all))
             (index (ellama-test--blueprint-index-by-act all)))
        (should (equal acts
                       '("shared" "file-only" "user-only"
                         "community-only")))
        (should (equal (plist-get (gethash "shared" index) :prompt)
                       "file"))))))

(ert-deftest test-ellama-blueprint-get-variable-list-dedupes ()
  (with-temp-buffer
    (insert "Hello {name}, id={id}. Bye {name} from {user_name}.")
    (should (equal (sort (ellama-blueprint-get-variable-list) #'string<)
                   '("id" "name" "user_name")))))

(ert-deftest test-ellama-blueprint-set-variable-replaces-all-occurrences ()
  (with-temp-buffer
    (insert "Hi {name}. Bye {name}.")
    (ellama-blueprint-set-variable "name" "Ada")
    (should (equal (buffer-string) "Hi Ada. Bye Ada."))))

(ert-deftest test-ellama-blueprint-fill-variables-prompts-once-per-variable ()
  (with-temp-buffer
    (insert "{name} and {name} are a {role}.")
    (let ((prompts '()))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (prompt &rest _)
                   (push prompt prompts)
                   (cond
                    ((string-match-p "{name}" prompt) "Ada")
                    ((string-match-p "{role}" prompt) "engineer")
                    (t "unknown")))))
        (ellama-blueprint-fill-variables))
      (should (equal (buffer-string)
                     "Ada and Ada are a engineer."))
      (should (= 1 (length (seq-filter
                            (lambda (prompt)
                              (string-match-p "{name}" prompt))
                            prompts))))
      (should (= 1 (length (seq-filter
                            (lambda (prompt)
                              (string-match-p "{role}" prompt))
                            prompts)))))))

(ert-deftest test-ellama-blueprint-run-fills-variables-and-sends-buffer ()
  (let ((ellama-blueprints '((:act "welcome"
                                   :prompt "Hello {name} from {city}.")))
        (sent nil))
    (cl-letf (((symbol-function 'ellama-community-prompts-ensure)
               (lambda () nil))
              ((symbol-function 'ellama-send-buffer-to-new-chat)
               (lambda ()
                 (setq sent (buffer-string)))))
      (ellama-blueprint-run "welcome" '(:name "Ada" :city "Paris"))
      (should (equal sent "Hello Ada from Paris.")))))

(ert-deftest
    test-ellama-blueprint-run-prefers-user-over-community-on-duplicate ()
  (let ((ellama-blueprints '((:act "shared" :prompt "user prompt")))
        (sent nil))
    (cl-letf (((symbol-function 'ellama-community-prompts-ensure)
               (lambda ()
                 '((:act "shared" :prompt "community prompt"))))
              ((symbol-function 'ellama-send-buffer-to-new-chat)
               (lambda ()
                 (setq sent (buffer-string)))))
      (ellama-blueprint-run "shared")
      (should (equal sent "user prompt")))))

(ert-deftest test-ellama-blueprint-select-filters-by-for-devs ()
  (let ((ellama-blueprint-buffer "*ellama-blueprint-select-test*")
        (seen-acts nil)
        (fill-called nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'ellama-blueprint-get-all-sources)
                     (lambda ()
                       '((:act "dev" :prompt "Dev prompt" :for-devs t)
                         (:act "general" :prompt "General" :for-devs nil))))
                    ((symbol-function 'completing-read)
                     (lambda (_prompt acts &rest _)
                       (setq seen-acts acts)
                       "dev"))
                    ((symbol-function 'switch-to-buffer)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'ellama-blueprint-fill-variables)
                     (lambda ()
                       (setq fill-called t))))
            (ellama-blueprint-select '(:for-devs t))
            (with-current-buffer (get-buffer ellama-blueprint-buffer)
              (should (equal (buffer-string) "Dev prompt"))
              (should (eq major-mode 'ellama-blueprint-mode))))
          (should (equal seen-acts '("dev")))
          (should fill-called))
      (when (get-buffer ellama-blueprint-buffer)
        (kill-buffer ellama-blueprint-buffer)))))

(ert-deftest test-ellama-blueprint-select-files-source ()
  (let ((ellama-blueprint-buffer "*ellama-blueprint-select-files-test*")
        (fill-called nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'ellama-blueprint-load-from-files)
                     (lambda ()
                       '((:act "from-file" :prompt "File prompt"))))
                    ((symbol-function 'completing-read)
                     (lambda (&rest _args) "from-file"))
                    ((symbol-function 'switch-to-buffer)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'ellama-blueprint-fill-variables)
                     (lambda ()
                       (setq fill-called t))))
            (ellama-blueprint-select '(:source files))
            (with-current-buffer (get-buffer ellama-blueprint-buffer)
              (should (equal (buffer-string) "File prompt"))
              (should (eq major-mode 'ellama-blueprint-mode))))
          (should fill-called))
      (when (get-buffer ellama-blueprint-buffer)
        (kill-buffer ellama-blueprint-buffer)))))

(ert-deftest test-ellama-blueprint-create-replaces-existing-blueprint ()
  (with-temp-buffer
    (insert "New prompt")
    (let ((ellama-blueprints
           '((:act "alpha" :prompt "Old prompt" :for-devs nil)
             (:act "beta" :prompt "Beta prompt" :for-devs nil)))
          (saved-values '()))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _args) "alpha"))
                ((symbol-function 'y-or-n-p)
                 (lambda (&rest _args) t))
                ((symbol-function 'customize-save-variable)
                 (lambda (_symbol value)
                   (push value saved-values))))
        (ellama-blueprint-create))
      (should (= (length ellama-blueprints) 2))
      (should (equal (mapcar (lambda (blueprint)
                               (plist-get blueprint :act))
                             ellama-blueprints)
                     '("beta" "alpha")))
      (let ((alpha (cl-find-if (lambda (blueprint)
                                 (equal (plist-get blueprint :act) "alpha"))
                               ellama-blueprints)))
        (should (equal (plist-get alpha :prompt) "New prompt"))
        (should (eq (plist-get alpha :for-devs) t)))
      (should saved-values))))

(ert-deftest test-ellama-blueprint-remove-found-updates-and-saves ()
  (let ((ellama-blueprints '((:act "alpha" :prompt "A")
                             (:act "beta" :prompt "B")))
        (saved nil))
    (cl-letf (((symbol-function 'customize-save-variable)
               (lambda (_symbol value)
                 (setq saved value))))
      (ellama-blueprint-remove "alpha")
      (should (equal ellama-blueprints '((:act "beta" :prompt "B"))))
      (should (equal saved ellama-blueprints)))))

(ert-deftest test-ellama-blueprint-remove-missing-does-not-save ()
  (let ((ellama-blueprints '((:act "alpha" :prompt "A")))
        (saved nil))
    (cl-letf (((symbol-function 'customize-save-variable)
               (lambda (&rest _args)
                 (setq saved t))))
      (ellama-blueprint-remove "missing")
      (should (equal ellama-blueprints '((:act "alpha" :prompt "A"))))
      (should-not saved))))

(provide 'test-ellama-blueprint)

;;; test-ellama-blueprint.el ends here
