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

(ert-deftest test-ellama--code-filter ()
  (should (equal "" (ellama--code-filter "")))
  (should (equal "(hello)" (ellama--code-filter "(hello)")))
  (should (equal "(hello)\n" (ellama--code-filter "```lisp\n(hello)\n```"))))

(ert-deftest test-ellama-code-improve ()
  (let ((original "(hello)\n")
        (improved "```lisp\n(hello)\n```"))
    (with-temp-buffer
      (insert original)
      (cl-letf (((symbol-function 'llm-chat-streaming)
                 (lambda (_provider prompt partial-callback response-callback _error-callback)
                   (should (string-match original (llm-chat-prompt-to-text prompt)))
                   (cl-loop for i from 0 to (- (length improved) 1)
                            do (funcall partial-callback (substring improved 0 i)))
                   (funcall response-callback improved))))
        (ellama-code-improve)
        (should (equal original (buffer-string)))))))

(ert-deftest test-ellama-context-element-format-buffer-markdown ()
  (let ((element (ellama-context-element-buffer :name "*scratch*")))
    (should (equal "```emacs-lisp\n(display-buffer \"*scratch*\")\n```\n"
                   (ellama-context-element-format element 'markdown-mode)))))

(ert-deftest test-ellama-context-element-format-buffer-org-mode ()
  (let ((element (ellama-context-element-buffer :name "*scratch*")))
    (should (equal "[[elisp:(display-buffer \"*scratch*\")][*scratch*]]"
                   (ellama-context-element-format element 'org-mode)))))

(ert-deftest test-ellama-context-element-format-file-markdown ()
  (let ((element (ellama-context-element-file :name "test.el")))
    (should (equal "[test.el](<test.el>)"
                   (ellama-context-element-format element 'markdown-mode)))))

(ert-deftest test-ellama-context-element-format-file-org-mode ()
  (let ((element (ellama-context-element-file :name "test.el")))
    (should (equal "[[file:test.el][test.el]]"
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

(provide 'test-ellama)

;;; test-ellama.el ends here
