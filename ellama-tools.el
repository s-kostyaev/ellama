;;; ellama-tools.el --- Working with tools -*- lexical-binding: t; package-lint-main-file: "ellama.el"; -*-

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
(require 'project)
(require 'json)
(require 'llm)

(defcustom ellama-tools-allow-all nil
  "Allow `ellama' using all the tools without user confirmation.
Dangerous.  Use at your own risk."
  :type 'boolean
  :group 'ellama)

(defcustom ellama-tools-allowed nil
  "List of allowed `ellama' tools.
Tools from this list will work without user confirmation."
  :type '(repeat function)
  :group 'ellama)

(defcustom ellama-tools-argument-max-length 50
  "Max length of function argument in the confirmation prompt."
  :type 'integer
  :group 'ellama)

(defvar ellama-tools-available nil
  "Alist containing all registered tools.")

(defvar ellama-tools-enabled nil
  "List of tools that have been enabled.")

(defvar-local ellama-tools-confirm-allowed (make-hash-table)
  "Contains hash table of allowed functions.
Key is a function name symbol.  Value is a boolean t.")

(defun ellama-tools-confirm (function &rest args)
  "Ask user for confirmation before calling FUNCTION with ARGS.
Generates prompt automatically.  User can approve once (y), approve
for all future calls (a), or forbid (n).  Returns the result of
FUNCTION if approved, \"Forbidden by the user\" otherwise."
  (let ((function-name (if (symbolp function)
                           (symbol-name function)
                         "anonymous-function"))
        (confirmation (gethash function ellama-tools-confirm-allowed nil)))
    (cond
     ;; If user has approved all calls, just execute the function
     ((or confirmation
          ellama-tools-allow-all
          (cl-find function ellama-tools-allowed))
      (let ((result (apply function args)))
        (if (stringp result)
            result
          (json-encode result))))
     ;; Otherwise, ask for confirmation
     (t
      ;; Generate prompt with truncated string arguments
      (let* ((args-display
              (mapcar (lambda (arg)
                        (cond
                         ((stringp arg)
                          (string-truncate-left
                           arg
                           ellama-tools-argument-max-length))
                         (t
                          (format "%S" arg))))
                      args))
             (prompt (format "Allow calling %s with arguments: %s?"
                             function-name
                             (mapconcat #'identity args-display ", ")))
             (answer (read-char-choice
                      (format "%s (y)es, (a)lways, (n)o: " prompt)
                      '(?y ?a ?n)))
             (result (cond
                      ;; Yes - execute function once
                      ((eq answer ?y)
                       (apply function args))
                      ;; Always - remember approval and execute function
                      ((eq answer ?a)
                       (puthash function t ellama-tools-confirm-allowed)
                       (apply function args))
                      ;; No - return nil
                      ((eq answer ?n)
                       "Forbidden by the user"))))
        (if (stringp result)
            result
          (json-encode result)))))))

(defun ellama-tools-wrap-with-confirm (tool-plist)
  "Wrap a tool's function with automatic confirmation.
TOOL-PLIST is a property list in the format expected by `llm-make-tool'.
Returns a new tool definition with the :function wrapped."
  (let* ((func (plist-get tool-plist :function))
         (args (plist-get tool-plist :args))
         (wrapped-args
          (mapcar
           (lambda (arg)
             (let*
                 ((type (plist-get tool-plist :type))
                  (wrapped-type (if (symbolp type)
                                    type
                                  (intern type))))
               (plist-put arg :type wrapped-type)))
           args))
         (wrapped-func (lambda (&rest args)
                         (apply #'ellama-tools-confirm func args))))
    ;; Return a new plist with the wrapped function
    (setq tool-plist (plist-put tool-plist :function wrapped-func))
    (plist-put tool-plist :args wrapped-args)))

(defun ellama-tools-define-tool (tool-plist)
  "Define a new ellama tool with automatic confirmation wrapping.
TOOL-PLIST is a property list in the format expected by `llm-make-tool'."
  (add-to-list
   'ellama-tools-available
   (apply 'llm-make-tool (ellama-tools-wrap-with-confirm tool-plist))))

(defun ellama-tools-enable-by-name-tool (name)
  "Add to `ellama-tools-enabled' each tool that matches NAME."
  (let* ((tool-name name)
         (tool (seq-find (lambda (tool) (string= tool-name (llm-tool-name tool)))
                         ellama-tools-available)))
    (add-to-list 'ellama-tools-enabled tool)
    nil))

;;;###autoload
(defun ellama-tools-enable-by-name (&optional name)
  "Add to `ellama-tools-enabled' each tool that matches NAME."
  (interactive)
  (let ((tool-name (or name
                       (completing-read
                        "Tool to enable: "
                        (cl-remove-if
                         (lambda (tname)
                           (cl-find-if
                            (lambda (tool)
                              (string= tname (llm-tool-name tool)))
                            ellama-tools-enabled))
                         (mapcar (lambda (tool) (llm-tool-name tool)) ellama-tools-available))))))
    (ellama-tools-enable-by-name-tool tool-name)))

(ellama-tools-define-tool
 '(:function
   ellama-tools-enable-by-name-tool
   :name
   "enable_tool"
   :args
   ((:name
     "name"
     :type
     string
     :description
     "Name of the tool to enable."))
   :description
   "Enable each tool that matches NAME. You need to reply to the user before using newly enabled tool."))

(defun ellama-tools-disable-by-name-tool (name)
  "Remove from `ellama-tools-enabled' each tool that matches NAME."
  (let* ((tool (seq-find (lambda (tool) (string= name (llm-tool-name tool)))
                         ellama-tools-enabled)))
    (setq ellama-tools-enabled (seq-remove (lambda (enabled-tool) (eq enabled-tool tool))
                                           ellama-tools-enabled))))

;;;###autoload
(defun ellama-tools-disable-by-name (&optional name)
  "Remove from `ellama-tools-enabled' each tool that matches NAME."
  (interactive)
  (let* ((tool-name (or name
                        (completing-read
                         "Tool to disable: "
                         (mapcar (lambda (tool) (llm-tool-name tool)) ellama-tools-enabled)))))
    (ellama-tools-disable-by-name-tool tool-name)))

(ellama-tools-define-tool
 '(:function
   ellama-tools-disable-by-name-tool
   :name
   "disable_tool"
   :args
   ((:name
     "name"
     :type
     string
     :description
     "Name of the tool to disable."))
   :description
   "Disable each tool that matches NAME."))

;;;###autoload
(defun ellama-tools-enable-all ()
  "Enable all available tools."
  (interactive)
  (setq ellama-tools-enabled ellama-tools-available))

;;;###autoload
(defun ellama-tools-disable-all ()
  "Disable all enabled tools."
  (interactive)
  (setq ellama-tools-enabled nil))

(defun ellama-tools-read-file-tool (path)
  "Read the file located at the specified PATH."
  (json-encode (if (not (file-exists-p path))
                   (format "File %s doesn't exists." path)
                 (with-temp-buffer
                   (insert-file-contents-literally path)
                   (buffer-string)))))

(ellama-tools-define-tool
 '(:function
   ellama-tools-read-file-tool
   :name
   "read_file"
   :args
   ((:name
     "path"
     :type
     string
     :description
     "Path to the file."))
   :description
   "Read the file located at the specified PATH."))

(defun ellama-tools-write-file-tool (path content)
  "Write CONTENT to the file located at the specified PATH."
  (with-temp-buffer
    (insert content)
    (setq buffer-file-name path)
    (save-buffer)))

(ellama-tools-define-tool
 '(:function
   ellama-tools-write-file-tool
   :name
   "write_file"
   :args
   ((:name
     "path"
     :type
     string
     :description
     "Path to the file.")
    (:name
     "content"
     :type
     string
     :description
     "Content to write to the file."))
   :description
   "Write CONTENT to the file located at the specified PATH."))

(defun ellama-tools-append-file-tool (path content)
  "Append CONTENT to the file located at the specified PATH."
  (with-current-buffer (find-file-noselect path)
    (goto-char (point-max))
    (insert content)
    (save-buffer)))

(ellama-tools-define-tool
 '(:function
   ellama-tools-append-file-tool
   :name
   "append_file"
   :args
   ((:name
     "path"
     :type
     string
     :description
     "Path to the file.")
    (:name
     "content"
     :type
     string
     :description
     "Content to append to the file."))
   :description
   "Append CONTENT to the file located at the specified PATH."))

(defun ellama-tools-prepend-file-tool (path content)
  "Prepend CONTENT to the file located at the specified PATH."
  (with-current-buffer (find-file-noselect path)
    (goto-char (point-min))
    (insert content)
    (save-buffer)))

(ellama-tools-define-tool
 '(:function
   ellama-tools-prepend-file-tool
   :name
   "prepend_file"
   :args
   ((:name
     "path"
     :type
     string
     :description
     "Path to the file.")
    (:name
     "content"
     :type
     string
     :description
     "Content to prepend to the file."))
   :description
   "Prepend CONTENT to the file located at the specified PATH."))

(defun ellama-tools-directory-tree-tool (dir &optional depth)
  "Return a string representing the directory tree under DIR.
DEPTH is the current recursion depth, used internally."
  (if (not (file-exists-p dir))
      (format "Directory %s doesn't exists" dir)
    (let ((indent (make-string (* (or depth 0) 2) ? ))
          (tree ""))
      (dolist (f (sort (cl-remove-if
                        (lambda (f)
                          (string-prefix-p "." f))
                        (directory-files dir))
                       #'string-lessp))
        (let* ((full   (expand-file-name f dir))
               (name   (file-name-nondirectory f))
               (type   (if (file-directory-p full) "|-" "`-"))
               (line   (concat indent type name "\n")))
          (setq tree (concat tree line))
          (when (file-directory-p full)
            (setq tree (concat tree
                               (ellama-tools-directory-tree-tool full (+ (or depth 0) 1)))))))
      tree)))

(ellama-tools-define-tool
 '(:function
   ellama-tools-directory-tree-tool
   :name
   "directory_tree"
   :args
   ((:name
     "dir"
     :type
     string
     :description
     "Directory path to generate tree for."))
   :description
   "Return a string representing the directory tree under DIR."))

(defun ellama-tools-move-file-tool (path newpath)
  "Move the file from the specified PATH to the NEWPATH."
  (if (and (file-exists-p path)
           (not (file-exists-p newpath)))
      (progn
        (rename-file path newpath))
    (error "Cannot move file: source file does not exist or destination already exists")))

(ellama-tools-define-tool
 '(:function
   ellama-tools-move-file-tool
   :name
   "move_file"
   :args
   ((:name
     "path"
     :type
     string
     :description
     "Current path of the file.")
    (:name
     "newpath"
     :type
     string
     :description
     "New path for the file."))
   :description
   "Move the file from the specified PATH to the NEWPATH."))

(defun ellama-tools-edit-file-tool (path oldcontent newcontent)
  "Edit file located at PATH.
Replace OLDCONTENT with NEWCONTENT."
  (let ((content (with-temp-buffer
                   (insert-file-contents-literally path)
                   (buffer-string)))
        (coding-system-for-write 'raw-text))
    (when (string-match (regexp-quote oldcontent) content)
      (with-temp-buffer
        (insert content)
        (goto-char (match-beginning 0))
        (delete-region (1+ (match-beginning 0)) (1+ (match-end 0)))
        (forward-char)
        (insert newcontent)
        (write-region (point-min) (point-max) path)))))

(ellama-tools-define-tool
 '(:function
   ellama-tools-edit-file-tool
   :name
   "edit_file"
   :args
   ((:name
     "path"
     :type
     string
     :description
     "Path to the file.")
    (:name
     "oldcontent"
     :type
     string
     :description
     "Old content to be replaced.")
    (:name
     "newcontent"
     :type
     string
     :description
     "New content to replace with."))
   :description
   "Edit file located at PATH. Replace OLDCONTENT with NEWCONTENT."))

(defun ellama-tools-shell-command-tool (cmd)
  "Execute shell command CMD."
  (shell-command-to-string cmd))

(ellama-tools-define-tool
 '(:function
   ellama-tools-shell-command-tool
   :name
   "shell_command"
   :args
   ((:name
     "cmd"
     :type
     string
     :description
     "Shell command to execute."))
   :description
   "Execute shell command CMD."))

(defun ellama-tools-grep-tool (dir search-string)
  "Grep SEARCH-STRING in DIR files."
  (let ((default-directory dir))
    (json-encode
     (shell-command-to-string
      (format "find . -type f -exec grep --color=never -nH -e %s \\{\\} +" (shell-quote-argument search-string))))))

(ellama-tools-define-tool
 '(:function
   ellama-tools-grep-tool
   :name
   "grep"
   :args
   ((:name
     "dir"
     :type
     string
     :description
     "Directory to search in.")
    (:name
     "search_string"
     :type
     string
     :description
     "String to search for."))
   :description
   "Grep SEARCH-STRING in directory files."))

(defun ellama-tools-grep-in-file-tool (search-string file)
  "Grep SEARCH-STRING in FILE."
  (json-encode
   (with-output-to-string
     (call-process
      "grep"
      nil standard-output nil
      "--color=never" "-nh" search-string (file-truename file)))))

(ellama-tools-define-tool
 '(:function
   ellama-tools-grep-in-file-tool
   :name "grep_in_file"
   :args ((:name "search_string" :type string :description "String to search for.")
          (:name "file" :type file :description "File to search in."))
   :description "Grep SEARCH-STRING in FILE."))

(defun ellama-tools-list-tool ()
  "List all available tools."
  (json-encode (mapcar
                (lambda (tool)
                  `(("name" . ,(llm-tool-name tool))
                    ("description" . ,(llm-tool-description tool))))
                ellama-tools-available)))

(ellama-tools-define-tool
 '(:function
   ellama-tools-list-tool
   :name
   "list_tools"
   :args
   nil
   :description
   "List all available tools."))

(defun ellama-tools-search-tool (search-string)
  "Search available tools that matches SEARCH-STRING."
  (json-encode
   (cl-remove-if-not
    (lambda (item)
      (or (string-match-p search-string (alist-get "name" item nil nil 'string=))
          (string-match-p search-string (alist-get "description" item nil nil 'string=))))
    (mapcar
     (lambda (tool)
       `(("name" . ,(llm-tool-name tool))
         ("description" . ,(llm-tool-description tool))))
     ellama-tools-available))))

(ellama-tools-define-tool
 '(:function
   ellama-tools-search-tool
   :name
   "search_tools"
   :args
   ((:name
     "search-string"
     :type
     string
     :description
     "String to search for in tool names or descriptions."))
   :description
   "Search available tools that matches SEARCH-STRING."))

(defun ellama-tools-today-tool ()
  "Return current date."
  (format-time-string "%Y-%m-%d"))

(ellama-tools-define-tool
 '(:function
   ellama-tools-today-tool
   :name
   "today"
   :args
   nil
   :description
   "Return current date."))

(defun ellama-tools-now-tool ()
  "Return current date, time and timezone."
  (format-time-string "%Y-%m-%d %H:%M:%S %Z"))

(ellama-tools-define-tool
 '(:function
   ellama-tools-now-tool
   :name
   "now"
   :args
   nil
   :description
   "Return current date, time and timezone."))

(defun ellama-tools-project-root-tool ()
  "Return current project root directory."
  (when (project-current)
    (project-root (project-current))))

(ellama-tools-define-tool
 '(:function
   ellama-tools-project-root-tool
   :name
   "project_root"
   :args
   nil
   :description
   "Return current project root directory."))

(defun ellama-tools-ask-user-tool (question answer-variant-list)
  "Ask user a QUESTION to receive a clarification.
ANSWER-VARIANT-LIST is a list of possible answer variants."
  (completing-read (concat question " ") (seq--into-list answer-variant-list)))

(ellama-tools-define-tool
 '(:function
   ellama-tools-ask-user-tool
   :name
   "ask_user"
   :args
   ((:name
     "question"
     :type
     string
     :description
     "Question to ask user for clarification.")
    (:name
     "answer_variant_list"
     :type array
     :description
     "List of possible answer variants."
     :items (:type string)))
   :description
   "Ask user a QUESTION to receive a clarification.
ANSWER-VARIANT-LIST is a list of possible answer variants."))

(defun ellama-tools-count-lines-tool (path)
  "Count lines in file located at PATH."
  (with-current-buffer (find-file-noselect path)
    (count-lines (point-min) (point-max))))

(ellama-tools-define-tool
 '(:function
   ellama-tools-count-lines-tool
   :name
   "count_lines"
   :args
   ((:name
     "path"
     :type
     string
     :description
     "Path to the file to count lines in."))
   :description
   "Count lines in file located at PATH."))

(defun ellama-tools-lines-range-tool (path from to)
  "Return content of file located at PATH lines in range FROM TO."
  (json-encode (with-current-buffer (find-file-noselect path)
                 (save-excursion
                   (let ((start (progn
                                  (goto-char (point-min))
                                  (forward-line (1- from))
                                  (beginning-of-line)
                                  (point)))
                         (end (progn
                                (goto-char (point-min))
                                (forward-line (1- to))
                                (end-of-line)
                                (point))))
                     (buffer-substring-no-properties start end))))))

(ellama-tools-define-tool
 '(:function
   ellama-tools-lines-range-tool
   :name
   "lines_range"
   :args
   ((:name
     "path"
     :type
     string
     :description
     "Path to the file to get lines from.")
    (:name
     "from"
     :type
     number
     :description
     "Starting line number.")
    (:name
     "to"
     :type
     number
     :description
     "Ending line number."))
   :description
   "Return content of file located at PATH lines in range FROM TO."))

(defun ellama-tools-apply-patch-tool (path patch)
  "Apply PATCH to file located at PATH.
PATCH is a string containing the patch data.
Returns the output of the patch command or an error message."
  (cond ((not path)
         "path is required")
        ((not (file-exists-p path))
         (format "file %s doesn't exists" path))
        ((not patch)
         "patch is required")
        (t
         (let* ((dir (file-name-directory (file-truename path)))
                (tmp (make-temp-file "ellama-patch-"))
                (patch-file (file-truename (concat tmp ".patch"))))
           (unwind-protect
               (progn
                 (with-temp-buffer
                   (insert patch)
                   (write-region (point-min) (point-max) patch-file))
                 (with-output-to-string
                   (call-process
                    "patch"
                    nil standard-output nil
                    "-p0" "-d" dir "-i" patch-file)))
             (when (file-exists-p patch-file)
               (delete-file patch-file)))))))

(ellama-tools-define-tool
 '(:function
   ellama-tools-apply-patch-tool
   :name
   "apply_patch"
   :args
   ((:name
     "path"
     :type
     string
     :description
     "Path to the file to apply patch to.")
    (:name
     "patch"
     :type
     string
     :description
     "Patch data to apply."))
   :description
   "Apply a patch to the file at PATH."))

(provide 'ellama-tools)
;;; ellama-tools.el ends here
