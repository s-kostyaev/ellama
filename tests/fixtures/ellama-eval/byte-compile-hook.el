;;; byte-compile-hook.el --- Eval edit hook -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Byte-compile the file identified by ELLAMA_FILE_NAME and exit unsuccessfully
;; when compilation reports warnings or errors.

;;; Code:

(require 'bytecomp)
(require 'seq)
(require 'subr-x)

(setq byte-compile-error-on-warn t
      byte-compile-dest-file-function (lambda (_file) null-device))

(condition-case err
    (unless (byte-compile-file (getenv "ELLAMA_FILE_NAME"))
      (princ "Hook validation failed: byte compilation reported errors.\n")
      (kill-emacs 1))
  (error
   (princ (format "Hook validation failed: %s\n"
                  (error-message-string err)))
   (kill-emacs 1)))

;;; byte-compile-hook.el ends here
