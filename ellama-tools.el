;;; ellama-tools.el --- Working with tools -*- lexical-binding: t; package-lint-main-file: "ellama.el"; -*-

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
(require 'project)

(defvar ellama-tools-available nil
  "Alist containing all registered tools.")

(defvar ellama-tools-enabled nil
  "List of tools that have been enabled.")

(defvar-local ellama-tools-confirm-allowed (make-hash-table)
  "Contains hash table of allowed functions.
Key is a function name symbol.  Value is a boolean t.")

(defun ellama-tools-confirm (prompt function &optional args)
  "Ask user for confirmation before calling FUNCTION with ARGS.
PROMPT is the message to display to the user.  FUNCTION is the function
to call if confirmed.  ARGS are the arguments to pass to FUNCTION.  User
can approve once (y), approve for all future calls (a), or forbid (n).
Returns the result of FUNCTION if approved, \"Forbidden by the user\"
otherwise."
  (let ((confirmation (gethash function ellama-tools-confirm-allowed nil)))
    (cond
     ;; If user has approved all calls, just execute the function
     ((when confirmation
        (if args
            (apply function args)
          (funcall function))))
     ;; Otherwise, ask for confirmation
     (t
      (let ((answer (read-char-choice
                     (format "%s (y)es, (a)lways, (n)o: " prompt)
                     '(?y ?a ?n))))
        (cond
         ;; Yes - execute function once
         ((eq answer ?y)
          (if args
              (apply function args)
            (funcall function)))
         ;; Always - remember approval and execute function
         ((eq answer ?a)
          (puthash function t ellama-tools-confirm-allowed)
          (if args
              (apply function args)
            (funcall function)))
         ;; No - return nil
         ((eq answer ?n)
          "Forbidden by the user")))))))

(defun ellama-tools--enable-by-name (name)
  "Add to `ellama-tools-enabled' each tool that matches NAME."
  (let* ((tool-name name)
         (tool (seq-find (lambda (tool) (string= tool-name (llm-tool-name tool)))
                         ellama-tools-available)))
    (add-to-list 'ellama-tools-enabled tool)))

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
    (ellama-tools--enable-by-name tool-name)))

(defun ellama-tools-enable-by-name-tool (name)
  "Add to `ellama-tools-enabled' each tool that matches NAME."
  (ellama-tools-confirm
   (format "Allow enabling tool %s?" name)
   'ellama-tools--enable-by-name
   (list name)))

(add-to-list
 'ellama-tools-available
 (llm-make-tool :function
                'ellama-tools-enable-by-name-tool
                :name
                "enable_tool"
                :args
                (list '(:name
                        "name"
                        :type
                        string
                        :description
                        "Name of the tool to enable."))
                :description
                "Enable each tool that matches NAME."))

(defun ellama-tools--disable-by-name (name)
  "Remove from `ellama-tools-enabled' each tool that matches NAME."
  (let* ((tool (seq-find (lambda (tool) (string= name (llm-tool-name tool)))
                         ellama-tools-enabled)))
    (setq ellama-tools-enabled (seq-remove (lambda (enabled-tool) (eq enabled-tool tool))
                                           ellama-tools-enabled))))

(defun ellama-tools-disable-by-name (&optional name)
  "Remove from `ellama-tools-enabled' each tool that matches NAME."
  (interactive)
  (let* ((tool-name (or name
                        (completing-read
                         "Tool to disable: "
                         (mapcar (lambda (tool) (llm-tool-name tool)) ellama-tools-enabled)))))
    (ellama-tools--disable-by-name tool-name)))

(defun ellama-tools-disable-by-name-tool (name)
  "Remove from `ellama-tools-enabled' each tool that matches NAME."
  (ellama-tools-confirm
   (format "Allow disabling tool %s?" name)
   'ellama-tools--disable-by-name
   (list name)))

(add-to-list
 'ellama-tools-available
 (llm-make-tool :function
                'ellama-tools-disable-by-name-tool
                :name
                "disable_tool"
                :args
                (list '(:name
                        "name"
                        :type
                        string
                        :description
                        "Name of the tool to disable."))
                :description
                "Disable each tool that matches NAME."))

(defun ellama-tools-enable-all ()
  "Enable all available tools."
  (interactive)
  (setq ellama-tools-enabled ellama-tools-available))

(defun ellama-tools-disable-all ()
  "Disable all enabled tools."
  (interactive)
  (setq ellama-tools-enabled nil))

(defun ellama-tools--read-file (path)
  "Read the file located at the specified PATH."
  (with-temp-buffer
    (insert-file-contents-literally path)
    (buffer-string)))

(defun ellama-tools-read-file (path)
  "Read the file located at the specified PATH."
  (ellama-tools-confirm
   (format "Allow reading file %s?" path)
   'ellama-tools--read-file
   (list path)))

(add-to-list
 'ellama-tools-available
 (llm-make-tool :function
                'ellama-tools-read-file
                :name
                "read_file"
                :args
                (list '(:name
                        "path"
                        :type
                        string
                        :description
                        "Path to the file."))
                :description
                "Read the file located at the specified PATH."))

(defun ellama-tools--write-file (path content)
  "Write CONTENT to the file located at the specified PATH."
  (with-temp-buffer
    (insert content)
    (setq buffer-file-name path)
    (save-buffer)))

(defun ellama-tools-write-file (path content)
  "Write CONTENT to the file located at the specified PATH."
  (ellama-tools-confirm
   (format "Allow writing file %s?" path)
   'ellama-tools--write-file
   (list path content)))

(add-to-list
 'ellama-tools-available
 (llm-make-tool :function
                'ellama-tools-write-file
                :name
                "write_file"
                :args
                (list '(:name
                        "path"
                        :type
                        string
                        :description
                        "Path to the file.")
                      '(:name
                        "content"
                        :type
                        string
                        :description
                        "Content to write to the file."))
                :description
                "Write CONTENT to the file located at the specified PATH."))

(defun ellama-tools--directory-tree (dir &optional depth)
  "Return a string representing the directory tree under DIR.
DEPTH is the current recursion depth, used internally."
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
                             (ellama-tools--directory-tree full (+ (or depth 0) 1)))))))
    tree))

(defun ellama-tools-directory-tree (dir)
  "Return a string representing the directory tree under DIR."
  (ellama-tools-confirm
   (format "Allow LLM to see %s directory tree?" dir)
   'ellama-tools--directory-tree
   (list dir nil)))

(add-to-list
 'ellama-tools-available
 (llm-make-tool :function
                'ellama-tools-directory-tree
                :name
                "directory_tree"
                :args
                (list '(:name
                        "dir"
                        :type
                        string
                        :description
                        "Directory path to generate tree for."))
                :description
                "Return a string representing the directory tree under DIR."))

(defun ellama-tools--move-file (path newpath)
  "Move the file from the specified PATH to the NEWPATH."
  (if (and (file-exists-p path)
           (not (file-exists-p newpath)))
      (progn
        (rename-file path newpath))
    (error "Cannot move file: source file does not exist or destination already exists")))

(defun ellama-tools-move-file (path newpath)
  "Move the file from the specified PATH to the NEWPATH."
  (ellama-tools-confirm
   (format "Allow moving file %s to %s?" path newpath)
   'ellama-tools--move-file
   (list path newpath)))

(add-to-list
 'ellama-tools-available
 (llm-make-tool :function
                'ellama-tools-move-file
                :name
                "move_file"
                :args
                (list '(:name
                        "path"
                        :type
                        string
                        :description
                        "Current path of the file.")
                      '(:name
                        "newpath"
                        :type
                        string
                        :description
                        "New path for the file."))
                :description
                "Move the file from the specified PATH to the NEWPATH."))

(defun ellama-tools--edit-file (path oldcontent newcontent)
  "Edit file located at PATH.
Replace OLDCONTENT with NEWCONTENT."
  (let ((content (with-temp-buffer
                   (insert-file-contents-literally path)
                   (buffer-string))))
    (when (string-match oldcontent content)
      (with-temp-buffer
        (insert content)
        (goto-char (match-beginning 0))
        (delete-region (match-beginning 0) (match-end 0))
        (insert newcontent)
        (write-region (point-min) (point-max) path)))))

(defun ellama-tools-edit-file (path oldcontent newcontent)
  "Edit file located at PATH.
Replace OLDCONTENT with NEWCONTENT."
  (ellama-tools-confirm
   (format "Allow editing file %s?" path)
   'ellama-tools--edit-file
   (list path oldcontent newcontent)))

(add-to-list
 'ellama-tools-available
 (llm-make-tool :function
                'ellama-tools-edit-file
                :name
                "edit_file"
                :args
                (list '(:name
                        "path"
                        :type
                        string
                        :description
                        "Path to the file.")
                      '(:name
                        "oldcontent"
                        :type
                        string
                        :description
                        "Old content to be replaced.")
                      '(:name
                        "newcontent"
                        :type
                        string
                        :description
                        "New content to replace with."))
                :description
                "Edit file located at PATH. Replace OLDCONTENT with NEWCONTENT."))

(defun ellama-tools--shell-command (cmd)
  "Execute shell command CMD."
  (shell-command-to-string cmd))

(defun ellama-tools-shell-command (cmd)
  "Execute shell command CMD."
  (ellama-tools-confirm
   (format "Allow executing shell command %s?" cmd)
   'ellama-tools--shell-command
   (list cmd)))

(add-to-list
 'ellama-tools-available
 (llm-make-tool :function
                'ellama-tools-shell-command
                :name
                "shell_command"
                :args
                (list '(:name
                        "cmd"
                        :type
                        string
                        :description
                        "Shell command to execute."))
                :description
                "Execute shell command CMD."))

(defun ellama-tools--grep (search-string)
  "Grep SEARCH-STRING in directory files."
  (shell-command-to-string (format "find . -type f -exec grep --color=never -nh -e %s \\{\\} +" search-string)))

(defun ellama-tools-grep (search-string)
  "Grep SEARCH-STRING in directory files."
  (ellama-tools-confirm
   (format "Allow grepping for %s in directory files?" search-string)
   'ellama-tools--grep
   (list search-string)))

(add-to-list
 'ellama-tools-available
 (llm-make-tool :function
                'ellama-tools-grep
                :name
                "grep"
                :args
                (list '(:name
                        "search-string"
                        :type
                        string
                        :description
                        "String to search for."))
                :description
                "Grep SEARCH-STRING in directory files."))

(defun ellama-tools--list ()
  "List all available tools."
  (mapcar
   (lambda (tool)
     `(("name" . ,(llm-tool-name tool))
       ("description" . ,(llm-tool-description tool))))
   ellama-tools-available))

(defun ellama-tools-list ()
  "List all available tools."
  (ellama-tools-confirm
   "Allow LLM to see available tools?"
   'ellama-tools--list))

(add-to-list
 'ellama-tools-available
 (llm-make-tool :function
                'ellama-tools-list
                :name
                "list_tools"
                :args
                nil
                :description
                "List all available tools."))

(defun ellama-tools--search (search-string)
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

(defun ellama-tools-search (search-string)
  "Search available tools that matches SEARCH-STRING."
  (ellama-tools-confirm
   (format "Allow searching tools with pattern %s?" search-string)
   'ellama-tools--search
   (list search-string)))

(add-to-list
 'ellama-tools-available
 (llm-make-tool :function
                'ellama-tools-search
                :name
                "search_tools"
                :args
                (list '(:name
                        "search-string"
                        :type
                        string
                        :description
                        "String to search for in tool names or descriptions."))
                :description
                "Search available tools that matches SEARCH-STRING."))

(defun ellama-tools--today ()
  "Return current date."
  (format-time-string "%Y-%m-%d"))

(defun ellama-tools-today ()
  "Return current date."
  (ellama-tools-confirm
   "Allow reading current date?"
   'ellama-tools--today))

(add-to-list
'ellama-tools-available
(llm-make-tool :function
               'ellama-tools-today
               :name
               "today"
               :args
               nil
               :description
               "Return current date."))

(defun ellama-tools--now ()
  "Return current date, time and timezone."
  (format-time-string "%Y-%m-%d %H:%M:%S %Z"))

(defun ellama-tools-now ()
  "Return current date, time and timezone."
  (ellama-tools-confirm
   "Allow reading current date, time and timezone?"
   'ellama-tools--now))

(add-to-list
 'ellama-tools-available
 (llm-make-tool :function
                'ellama-tools-now
                :name
                "now"
                :args
                nil
                :description
                "Return current date, time and timezone."))

(defun ellama-tools--project-root ()
  "Return current project root directory."
  (when (project-current)
    (project-root (project-current))))

(defun ellama-tools-project-root ()
  "Return current project root directory."
  (ellama-tools-confirm
   "Allow LLM to know the project root directory?"
   'ellama-tools--project-root))

(add-to-list
 'ellama-tools-available
 (llm-make-tool :function
                'ellama-tools-project-root
                :name
                "project_root"
                :args
                nil
                :description
                "Return current project root directory."))

(provide 'ellama-tools)
;;; ellama-tools.el ends here
