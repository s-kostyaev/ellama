;;; ellama-manual.el --- Working with ellama info manual -*- lexical-binding: t; package-lint-main-file: "ellama.el"; -*-

;; Copyright (C) 2023-2026  Free Software Foundation, Inc.

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

(defvar org-export-with-broken-links)

;;;###autoload
(defun ellama-manual-export ()
  "Create info manual from readme."
  (interactive)
  (declare-function org-export-to-file "ox")
  (declare-function org-texinfo-compile "ox-texinfo")
  (declare-function project-root "project")
  (declare-function project-current "project")
  (require 'ox-texinfo)
  (let* ((org-export-with-broken-links t)
         (version (with-current-buffer (find-file-noselect
                                        (file-name-concat
                                         (project-root (project-current))
                                         "ellama.el"))
                    (save-excursion
                      (goto-char (point-min))
                      (re-search-forward ";; Version: \\([0-9\\.]+\\)")
                      (match-string 1))))
         (buf (find-file-noselect
               (file-name-concat
                (project-root (project-current))
                "README.org")))
         (content (with-current-buffer buf
                    (buffer-string))))
    (with-temp-buffer
      (org-mode)
      (insert
       (format "#+TITLE: Ellama manual
#+SUBTITLE: for version {{{version}}}
#+AUTHOR: Sergey Kostyaev

#+OPTIONS: ':t toc:t author:t compact-itemx:t nodetailmenu:t
#+LANGUAGE: en

#+MACRO: version %s

#+TEXINFO_FILENAME: ellama.info
#+TEXINFO_HEADER: @paragraphindent none

#+TEXINFO_DIR_CATEGORY: Emacs misc features
#+TEXINFO_DIR_TITLE: Ellama: (ellama)
#+TEXINFO_DIR_NAME: Ellama
#+TEXINFO_DIR_DESC: Tool for interaction with large language models.
#+TEXINFO_PRINTED_TITLE: Ellama manual

#+texinfo: @insertcopying

* COPYING
:PROPERTIES:
:COPYING: t
:END:

Copyright (C) 2023-2026  Free Software Foundation, Inc.

#+begin_quote
Permission is granted to copy, distribute and/or modify this document
under the terms of the GNU Free Documentation License, Version 1.3 or
any later version published by the Free Software Foundation; with no
Invariant Sections, with the Front-Cover Texts being “A GNU Manual,” and
with the Back-Cover Texts as in (a) below.  A copy of the license is
included in the section entitled “GNU Free Documentation License.”

(a) The FSF’s Back-Cover Text is: “You have the freedom to copy and
modify this GNU manual.”
#+end_quote

"
               version))
      (insert content)
      ;; remove badges
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[.+?\\]\\[.+?\\.svg\\]\\]\\n?" nil t)
        (replace-match ""))
      ;; remove images
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[.+?\\.gif\\]\\]\\n?" nil t)
        (replace-match ""))
      (org-export-to-file
       'texinfo "ellama.texi"
       nil nil nil nil nil
       #'org-texinfo-compile))))

(provide 'ellama-manual)
;;; ellama-manual.el ends here.
