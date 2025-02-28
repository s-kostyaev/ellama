;;; ellama-community-prompts.el --- Community prompt collection -*- lexical-binding: t; package-lint-main-file: "ellama.el"; -*-

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
(require 'plz)
(require 'ellama)

(defcustom ellama-community-prompts-url "https://raw.githubusercontent.com/f/awesome-chatgpt-prompts/main/prompts.csv"
  "The URL of the community prompts collection."
  :type 'string
  :group 'ellama)

(defcustom ellama-community-prompts-file (expand-file-name
					  "community-prompts.csv"
					  (file-name-concat
					   user-emacs-directory
					   "ellama"))
  "Path to the CSV file containing community prompts.
This file is expected to be located inside an `ellama' subdirectory
within your `user-emacs-directory'."
  :type 'file
  :group 'ellama)

(defun ellama-community-prompts-ensure-file ()
  "Ensure that the community prompt collection file is downloaded.
Downloads the file from `ellama-community-prompts-url` if it does
not already exist."
  (unless (file-exists-p ellama-community-prompts-file)
    (let* ((directory (file-name-directory ellama-community-prompts-file))
           (response (plz 'get ellama-community-prompts-url
                       :as 'file
                       :then (lambda (filename)
                               (rename-file filename ellama-community-prompts-file t))
                       :else (lambda (error)
                               (message "Failed to download community prompts: %s" error)))))
      (when (and response (not (file-directory-p directory)))
        (make-directory directory t))
      (when response
        (message "Community prompts file downloaded successfully.")))))

(defun ellama-community-prompts-parse-csv-line (line)
  "Parse a single CSV LINE into a list of fields, handling quotes.
LINE is the string to be parsed."
  (let ((i 0)
        (len (length line)))
    (cl-loop
     with fields = '()
     with current-field = ""
     with inside-quotes = nil
     while (< i len)
     do (let ((char (aref line i)))
          (cond
           ;; Opening quote (start of field)
           ((and (eq char ?\") (not inside-quotes))
	    (setq inside-quotes t)
	    (cl-incf i))
           ;; Closing quote (end of field or escaped quote)
           ((and (eq char ?\") inside-quotes)
	    (if (and (< (1+ i) len) (eq (aref line (1+ i)) ?\"))
                (progn  ; Escaped quote: add single quote, skip next character
                  (setq current-field (concat current-field "\""))
                  (cl-incf i 2))
	      (setq inside-quotes nil)  ; End of quoted field
	      (cl-incf i)))
           ;; Comma separator (outside quotes)
           ((and (eq char ?,) (not inside-quotes))
	    (push current-field fields)
	    (setq current-field "")
	    (cl-incf i))
           ;; Regular character
           (t
	    (setq current-field (concat current-field (string char)))
	    (cl-incf i))))
     ;; Add the last field after loop ends
     finally return (nreverse (cons current-field fields)))))

(defun ellama-community-prompts-convert-to-plist (parsed-line)
  "Convert PARSED-LINE to plist.
PARSED-LINE is expected to be a list with three elements: :act,
:prompt, and :for-devs."
  (let ((act (cl-first parsed-line))
	(prompt (cl-second parsed-line))
	(for-devs (string= "TRUE" (cl-third parsed-line))))
    `(:act ,act :prompt ,prompt :for-devs ,for-devs)))

(defvar ellama-community-prompts-collection nil
  "Community prompts collection.")

(defun ellama-community-prompts-ensure ()
  "Ensure that the community prompt collection are loaded and available.
This function ensures that the file specified by `ellama-community-prompts-file'
is read and parsed, and the resulting collection of prompts is stored in
`ellama-community-prompts-collection'. If the collection is already populated,
this function does nothing.

Returns the collection of community prompts."
  (ellama-community-prompts-ensure-file)
  (unless ellama-community-prompts-collection
    (setq ellama-community-prompts-collection
	  (let ((buf (find-file-noselect ellama-community-prompts-file)))
	    (with-current-buffer buf
	      (mapcar (lambda (line)
			(ellama-community-prompts-convert-to-plist
			 (ellama-community-prompts-parse-csv-line
			  line)))
		      (cdr (string-lines
			    (buffer-substring-no-properties
			     (point-min) (point-max)))))))))
  ellama-community-prompts-collection)

;;;###autoload
(defun ellama-community-prompts-select-blueprint ()
  "Select a prompt from the community prompt collection.
The user is prompted to choose a role, and then a
corresponding prompt is inserted into a blueprint buffer."
  (interactive)
  (ellama-blueprint-select '(:source community)))

(provide 'ellama-community-prompts)
;;; ellama-community-prompts.el ends here.
