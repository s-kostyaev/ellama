;;; test-ellama-context.el --- Ellama context tests -*- lexical-binding: t; package-lint-main-file: "../ellama.el"; -*-

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
;; Ellama context tests.
;;

;;; Code:

(require 'ellama-context)
(require 'ert)

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

(ert-deftest test-ellama-context-element-format-webpage-quote-disabled-markdown ()
  (let ((element (ellama-context-element-webpage-quote :name "test name" :url "https://example.com/" :content "1\n\n2"))
	(ellama-show-quotes nil))
    (should (string-match "\\[test name\\](https://example.com/):\n```emacs-lisp\n(display-buffer \"\\*ellama-quote-.+\\*\")\n```\n" (ellama-context-element-format element 'markdown-mode)))))

(ert-deftest test-ellama-context-element-format-webpage-quote-enabled-markdown ()
  (let ((element (ellama-context-element-webpage-quote :name "test name" :url "https://example.com/" :content "1\n\n2"))
	(ellama-show-quotes t))
    (should (equal "[test name](https://example.com/):
> 1
> 
> 2

"
		   (ellama-context-element-format element 'markdown-mode)))))

(ert-deftest test-ellama-context-element-format-webpage-quote-disabled-org-mode ()
  (let ((element (ellama-context-element-webpage-quote :name "test name" :url "https://example.com/" :content "1\n\n2"))
	(ellama-show-quotes nil))
    (should (string-match "\\[\\[https://example.com/\\]\\[test name\\]\\] \\[\\[elisp:(display-buffer \"\\*ellama-quote-.+\\*\")\\]\\[show\\]\\]" (ellama-context-element-format element 'org-mode)))))

(ert-deftest test-ellama-context-element-format-webpage-quote-enabled-org-mode ()
  (let ((element (ellama-context-element-webpage-quote :name "test name" :url "https://example.com/" :content "1\n\n* 2"))
	(ellama-show-quotes t))
    (should (equal "[[https://example.com/][test name]]:
#+BEGIN_QUOTE
1

 * 2
#+END_QUOTE
"
		   (ellama-context-element-format element 'org-mode)))))

(ert-deftest test-ellama-context-element-format-info-node-quote-disabled-markdown ()
  (let ((element (ellama-context-element-info-node-quote :name "(emacs)Top" :content "1\n\n2"))
	(ellama-show-quotes nil))
    (should (string-match "```emacs-lisp\n(info \"(emacs)Top\")\n```\nshow:\n```emacs-lisp\n(display-buffer \"\\*ellama-quote-.+\\*\")\n```\n" (ellama-context-element-format element 'markdown-mode)))))

(ert-deftest test-ellama-context-element-format-info-node-quote-enabled-markdown ()
  (let ((element (ellama-context-element-info-node-quote :name "(emacs)Top" :content "1\n\n2"))
	(ellama-show-quotes t))
    (should (equal "```emacs-lisp\n(info \"(emacs)Top\")\n```\n> 1\n> \n> 2\n\n"
		   (ellama-context-element-format element 'markdown-mode)))))

(ert-deftest test-ellama-context-element-format-info-node-quote-disabled-org-mode ()
  (let ((element (ellama-context-element-info-node-quote :name "(emacs)Top" :content "1\n\n2"))
	(ellama-show-quotes nil))
    (should (string-match "\\[\\[(emacs)Top\\]\\[(emacs)Top\\]\\] \\[\\[elisp:(display-buffer \"\\*ellama-quote-.+\\*\")\\]\\[show\\]\\]" (ellama-context-element-format element 'org-mode)))))

(ert-deftest test-ellama-context-element-format-info-node-quote-enabled-org-mode ()
  (let ((element (ellama-context-element-info-node-quote :name "(emacs)Top" :content "1\n\n* 2"))
	(ellama-show-quotes t))
    (should (equal "[[(emacs)Top][(emacs)Top]]:\n#+BEGIN_QUOTE\n1\n\n * 2\n#+END_QUOTE\n"
		   (ellama-context-element-format element 'org-mode)))))

(ert-deftest test-ellama-context-element-format-file-quote-disabled-markdown ()
  (let ((element (ellama-context-element-file-quote :path "/tmp/test.txt" :content "1\n\n2"))
	(ellama-show-quotes nil))
    (should (string-match "\\[/tmp/test.txt\\](/tmp/test.txt):\n```emacs-lisp\n(display-buffer \"\\*ellama-quote-.+\\*\")" (ellama-context-element-format element 'markdown-mode)))))

(ert-deftest test-ellama-context-element-format-file-quote-enabled-markdown ()
  (let ((element (ellama-context-element-file-quote :path "/tmp/test.txt" :content "1\n\n2"))
	(ellama-show-quotes t))
    (should (equal "[/tmp/test.txt](/tmp/test.txt):
> 1
> 
> 2

"
		   (ellama-context-element-format element 'markdown-mode)))))

(ert-deftest test-ellama-context-element-format-file-quote-disabled-org-mode ()
  (let ((element (ellama-context-element-file-quote :path "/tmp/test.txt" :content "1\n\n2"))
	(ellama-show-quotes nil))
    (should (string-match "\\[\\[/tmp/test.txt\\]\\[/tmp/test.txt\\]\\] \\[\\[elisp:(display-buffer \"\\*ellama-quote-.+\\*\")\\]\\[show\\]\\]" (ellama-context-element-format element 'org-mode)))))

(ert-deftest test-ellama-context-element-format-file-quote-enabled-org-mode ()
  (let ((element (ellama-context-element-file-quote :path "/tmp/test.txt" :content "1\n\n* 2"))
	(ellama-show-quotes t))
    (should (equal "[[/tmp/test.txt][/tmp/test.txt]]:
#+BEGIN_QUOTE
1

 * 2
#+END_QUOTE
"
		   (ellama-context-element-format element 'org-mode)))))

(ert-deftest test-ellama-context-element-extract-buffer ()
  (with-temp-buffer
    (insert "123")
    (let ((element (ellama-context-element-buffer :name (buffer-name))))
      (should (equal "123" (ellama-context-element-extract element))))))

(ert-deftest test-ellama-context-element-extract-file ()
  (let* ((filename (expand-file-name "LICENSE" (locate-dominating-file "." ".git")))
         (element (ellama-context-element-file :name filename)))
    (should (string-match "GNU GENERAL PUBLIC LICENSE"
                          (ellama-context-element-extract element)))))

(ert-deftest test-ellama-context-element-extract-info-node ()
  (let ((element (ellama-context-element-info-node :name "(dir)Top")))
    (should (string-match "This" (ellama-context-element-extract element)))))

(ert-deftest test-ellama-context-element-extract-text ()
  (let ((element (ellama-context-element-text :content "123")))
    (should (string-match "123" (ellama-context-element-extract element)))))

(ert-deftest test-ellama-context-element-extract-webpage-quote ()
  (let ((element (ellama-context-element-webpage-quote :content "123")))
    (should (equal "123" (ellama-context-element-extract element)))))

(ert-deftest test-ellama-context-element-extract-info-node-quote ()
  (let ((element (ellama-context-element-info-node-quote :content "123")))
    (should (equal "123" (ellama-context-element-extract element)))))

(ert-deftest test-ellama-context-element-extract-file-quote ()
  (let ((element (ellama-context-element-file-quote :content "123")))
    (should (equal "123" (ellama-context-element-extract element)))))

(ert-deftest test-ellama-context-element-display-buffer ()
  (with-temp-buffer
    (let ((element (ellama-context-element-buffer :name (buffer-name))))
      (should (equal (buffer-name) (ellama-context-element-display element))))))

(ert-deftest test-ellama-context-element-display-file ()
  (let* ((filename (expand-file-name "LICENSE" (locate-dominating-file "." ".git")))
         (element (ellama-context-element-file :name filename)))
    (should (equal (file-name-nondirectory filename) (ellama-context-element-display element)))))

(ert-deftest test-ellama-context-element-display-info-node ()
  (let ((element (ellama-context-element-info-node :name "(dir)Top")))
    (should (equal "(info \"(dir)Top\")" (ellama-context-element-display element)))))

(ert-deftest test-ellama-context-element-display-text ()
  (let ((element (ellama-context-element-text :content "123")))
    (should (equal "\"123...\"" (ellama-context-element-display element)))))

(ert-deftest test-ellama-context-element-display-webpage-quote ()
  (let ((element (ellama-context-element-webpage-quote :name "Example" :url "http://example.com" :content "123")))
    (should (equal "Example" (ellama-context-element-display element)))))

(ert-deftest test-ellama-context-element-display-info-node-quote ()
  (let ((element (ellama-context-element-info-node-quote :name "Example" :content "123")))
    (should (equal "(info \"Example\")" (ellama-context-element-display element)))))

(ert-deftest test-ellama-context-element-display-file-quote ()
  (let ((element (ellama-context-element-file-quote :path "/path/to/file" :content "123")))
    (should (equal "file" (ellama-context-element-display element)))))

(ert-deftest test-ellama-context-element-extract-buffer-quote ()
  (with-temp-buffer
    (insert "123")
    (let ((element (ellama-context-element-buffer-quote :name (buffer-name) :content "123")))
      (should (equal "123" (ellama-context-element-extract element))))))

(ert-deftest test-ellama-context-element-display-buffer-quote ()
  (with-temp-buffer
    (let ((element (ellama-context-element-buffer-quote :name (buffer-name) :content "123")))
      (should (equal (buffer-name) (ellama-context-element-display element))))))

(ert-deftest test-ellama-context-prompt-with-context-clears-ephemeral ()
  (let ((ellama-context-global
	 (list (ellama-context-element-text :content "global")))
	(ellama-context-ephemeral
	 (list (ellama-context-element-text :content "ephemeral"))))
    (should (equal (ellama-context-prompt-with-context "Prompt")
		   "Context:\nglobal\nephemeral\n\nPrompt"))
    (should (null ellama-context-ephemeral))
    (should (equal (mapcar #'ellama-context-element-extract
			   ellama-context-global)
		   '("global")))))

(provide 'test-ellama-context)

;;; test-ellama-context.el ends here
