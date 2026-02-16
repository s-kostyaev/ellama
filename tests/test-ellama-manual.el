;;; test-ellama-manual.el --- Ellama manual tests -*- lexical-binding: t; package-lint-main-file: "../ellama.el"; -*-

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
;; Ellama manual tests.
;;

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'ellama-manual)
(require 'ox-texinfo)

(defvar org-export-with-broken-links)

(defun ellama-test--with-manual-project (ellama-content readme-content fn)
  "Call FN in temporary project with ELLAMA-CONTENT and README-CONTENT."
  (let* ((root (make-temp-file "ellama-manual-" t))
         (ellama-file (expand-file-name "ellama.el" root))
         (readme-file (expand-file-name "README.org" root)))
    (unwind-protect
        (progn
          (with-temp-file ellama-file
            (insert ellama-content))
          (with-temp-file readme-file
            (insert readme-content))
          (funcall fn root))
      (when (file-exists-p root)
        (delete-directory root t)))))

(ert-deftest test-ellama-manual-export-includes-version-and-readme ()
  (let (export-content)
    (ellama-test--with-manual-project
     ";; Version: 9.9.1\n"
     "Manual body."
     (lambda (root)
       (cl-letf (((symbol-function 'project-current)
                  (lambda (&rest _) :project))
                 ((symbol-function 'project-root)
                  (lambda (_project) root))
                 ((symbol-function 'org-export-to-file)
                  (lambda (&rest _args)
                    (setq export-content (buffer-string)))))
         (ellama-manual-export))))
    (should (string-match-p
             (regexp-quote "#+MACRO: version 9.9.1")
             export-content))
    (should (string-match-p
             (regexp-quote "Manual body.")
             export-content))))

(ert-deftest test-ellama-manual-export-removes-badge-svg-links ()
  (let (export-content)
    (ellama-test--with-manual-project
     ";; Version: 1.0.0\n"
     (concat "[[http://example][file:badge.svg]]\n"
             "Visible text")
     (lambda (root)
       (cl-letf (((symbol-function 'project-current)
                  (lambda (&rest _) :project))
                 ((symbol-function 'project-root)
                  (lambda (_project) root))
                 ((symbol-function 'org-export-to-file)
                  (lambda (&rest _args)
                    (setq export-content (buffer-string)))))
         (ellama-manual-export))))
    (should-not (string-match-p (regexp-quote "badge.svg") export-content))
    (should (string-match-p (regexp-quote "Visible text") export-content))))

(ert-deftest test-ellama-manual-export-removes-gif-image-links ()
  (let (export-content)
    (ellama-test--with-manual-project
     ";; Version: 1.0.0\n"
     (concat "[[file:imgs/demo.gif]]\n"
             "Visible text")
     (lambda (root)
       (cl-letf (((symbol-function 'project-current)
                  (lambda (&rest _) :project))
                 ((symbol-function 'project-root)
                  (lambda (_project) root))
                 ((symbol-function 'org-export-to-file)
                  (lambda (&rest _args)
                    (setq export-content (buffer-string)))))
         (ellama-manual-export))))
    (should-not (string-match-p (regexp-quote "demo.gif") export-content))
    (should (string-match-p (regexp-quote "Visible text") export-content))))

(ert-deftest test-ellama-manual-export-calls-org-export-to-file-correctly ()
  (let (export-args)
    (ellama-test--with-manual-project
     ";; Version: 1.0.0\n"
     "Body"
     (lambda (root)
       (cl-letf (((symbol-function 'project-current)
                  (lambda (&rest _) :project))
                 ((symbol-function 'project-root)
                  (lambda (_project) root))
                 ((symbol-function 'org-export-to-file)
                  (lambda (&rest args)
                    (setq export-args args))))
         (ellama-manual-export))))
    (should (equal (nth 0 export-args) 'texinfo))
    (should (equal (nth 1 export-args) "ellama.texi"))
    (should-not (nth 2 export-args))
    (should-not (nth 3 export-args))
    (should-not (nth 4 export-args))
    (should-not (nth 5 export-args))
    (should-not (nth 6 export-args))
    (should (eq (nth 7 export-args) #'org-texinfo-compile))))

(ert-deftest test-ellama-manual-export-binds-broken-links-locally ()
  (let ((org-export-with-broken-links nil)
        export-binding)
    (ellama-test--with-manual-project
     ";; Version: 1.0.0\n"
     "Body"
     (lambda (root)
       (cl-letf (((symbol-function 'project-current)
                  (lambda (&rest _) :project))
                 ((symbol-function 'project-root)
                  (lambda (_project) root))
                 ((symbol-function 'org-export-to-file)
                  (lambda (&rest _args)
                    (setq export-binding org-export-with-broken-links))))
         (ellama-manual-export))))
    (should export-binding)
    (should-not org-export-with-broken-links)))

(ert-deftest test-ellama-manual-export-errors-when-version-missing ()
  (ellama-test--with-manual-project
   ";; No version header\n"
   "Body"
   (lambda (root)
     (cl-letf (((symbol-function 'project-current)
                (lambda (&rest _) :project))
               ((symbol-function 'project-root)
                (lambda (_project) root))
               ((symbol-function 'org-export-to-file)
                (lambda (&rest _args)
                  nil)))
       (should-error (ellama-manual-export) :type 'search-failed)))))

(provide 'test-ellama-manual)
;;; test-ellama-manual.el ends here.
