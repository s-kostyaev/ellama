;;; ellama-manual.el --- Working with ellama info manual -*- lexical-binding: t; package-lint-main-file: "ellama.el"; -*-

;; Copyright (C) 2023-2025  Free Software Foundation, Inc.

;; Author: Sergey Kostyaev <sskostyaev@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

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
;; Ellama is a tool for interacting with large language models from Emacs.
;; It allows you to ask questions and receive responses from the
;; LLMs.  Ellama can perform various tasks such as translation, code
;; review, summarization, enhancing grammar/spelling or wording and
;; more through the Emacs interface.  Ellama natively supports streaming
;; output, making it effortless to use with your preferred text editor.
;;

;;; Code:

;;;###autoload
(defun ellama-manual-export ()
  "Create info manual from readme."
  (interactive)
  (declare-function org-export-to-file "ox")
  (declare-function org-texinfo-compile "ox-texinfo")
  (declare-function project-root "project")
  (declare-function project-current "project")
  (require 'ox-texinfo)
  (let* ((org-export-with-broken-links t))
    (with-current-buffer (find-file-noselect
			  (file-name-concat
			   (project-root (project-current))
			   "README.org"))
      (org-export-to-file
	  'texinfo "ellama.texi"
	nil nil nil nil nil
	#'org-texinfo-compile))))
;;; ellama-manual.el ends here.
