;;; test-ellama.el --- Ellama tests -*- lexical-binding: t -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.

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
;; Ellama tests.
;;

;;; Code:

(require 'cl-lib)
(require 'ellama)
(require 'ert)

(defvar test-ellama-license-filename
  (expand-file-name "LICENSE" (locate-dominating-file "." ".git")))

(ert-deftest test-ellama-context-element-format-buffer-markdown ()
  (let ((element (ellama-context-element-buffer :name "*scratch*")))
    (should (equal "```emacs-lisp\n(display-buffer \"*scratch*\")\n```\n"
                   (ellama-context-element-format element 'markdown-mode)))))

(ert-deftest test-ellama-context-element-format-buffer-org-mode ()
  (let ((element (ellama-context-element-buffer :name "*scratch*")))
    (should (equal "[[elisp:(display-buffer \"*scratch*\")][*scratch*]]"
                   (ellama-context-element-format element 'org-mode)))))

(ert-deftest test-ellama-context-element-format-file-markdown ()
  (let ((element (ellama-context-element-file :name "LICENSE")))
    (should (equal "[LICENSE](<LICENSE>)"
                   (ellama-context-element-format element 'markdown-mode)))))

(ert-deftest test-ellama-context-element-format-file-org-mode ()
  (let ((element (ellama-context-element-file :name "LICENSE")))
    (should (equal "[[file:LICENSE][LICENSE]]"
                   (ellama-context-element-format element 'org-mode)))))

(ert-deftest test-ellama-context-element-format-info-node-markdown ()
  (let ((element (ellama-context-element-info-node :name "(dir)Top")))
    (should (equal "```emacs-lisp\n(info \"(dir)Top\")\n```\n"
                   (ellama-context-element-format element 'markdown-mode)))))

(ert-deftest test-ellama-context-element-format-info-node-org-mode ()
  (let ((element (ellama-context-element-info-node :name "(dir)Top")))
    (should (equal "[[(dir)Top][(dir)Top]]"
                   (ellama-context-element-format element 'org-mode)))))

(ert-deftest test-ellama-context-element-format-text-markdown ()
  (let ((element (ellama-context-element-text :content "123")))
    (should (equal "123" (ellama-context-element-format element 'markdown-mode)))))

(ert-deftest test-ellama-context-element-format-text-org-mode ()
  (let ((element (ellama-context-element-text :content "123")))
    (should (equal "123" (ellama-context-element-format element 'org-mode)))))

(ert-deftest test-ellama-context-element-extract-buffer ()
  (with-temp-buffer
    (insert "123")
    (let ((element (ellama-context-element-buffer :name (buffer-name))))
      (should (equal "123" (ellama-context-element-extract element))))))

(ert-deftest test-ellama-context-element-extract-file ()
  (let ((element (ellama-context-element-file :name test-ellama-license-filename)))
    (should (string-match "GNU GENERAL PUBLIC LICENSE"
                          (ellama-context-element-extract element)))))

(ert-deftest test-ellama-context-element-extract-info-node ()
  (let ((element (ellama-context-element-info-node :name "(dir)Top")))
    (should (string-match "This" (ellama-context-element-extract element)))))

(ert-deftest test-ellama-context-element-extract-text ()
  (let ((element (ellama-context-element-text :content "123")))
    (should (string-match "123" (ellama-context-element-extract element)))))

(provide 'test-ellama)

;;; test-ellama.el ends here
