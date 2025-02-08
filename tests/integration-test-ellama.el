;;; integration-test-ellama.el --- Ellama integration tests -*- lexical-binding: t; package-lint-main-file: "../ellama.el"; -*-

;; Copyright (C) 2023-2025  Free Software Foundation, Inc.

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

(require 'ellama)
(require 'ert)

(ert-deftest ellama-extract-sting-list-test ()
  "Check if `ellama-extract-string-list' works correctly."
  (should (equal-including-properties
	   (ellama-extract-string-list
	    "fruits"
	    "Here is the fruits: apple, banana, dragon fruit. I like it.")
	   '("apple" "banana" "dragon fruit"))))

(ert-deftest ellama-semantic-similar-test ()
  "Check if `ellama-semantic-similar-p' works correctly."
  (should (equal-including-properties
	   (let ((res))
	     (dolist (el '("How many r's in strawberry?"
			   "How many times letter r appears in word strawberry?"
			   "How many r's in strawberry?"
			   "How many times letter e appears in word strawberry?"))
	       (cl-pushnew el res :test #'ellama-semantic-similar-p))
	     (reverse res))
	   '("How many r's in strawberry?"
	     "How many times letter e appears in word strawberry?"))))

(provide 'integration-test-ellama)

;;; integration-test-ellama.el ends here
