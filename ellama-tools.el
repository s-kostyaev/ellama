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

(declare-function llm-standard-provider-p "llm-provider-utils" (provider))
(declare-function ellama-new-session "ellama"
                  (provider prompt &optional ephemeral))
(declare-function ellama-session-extra "ellama" (session))
(declare-function ellama-session-id "ellama" (session))
(declare-function ellama-stream "ellama" (prompt &rest args))

(defvar ellama-provider)
(defvar ellama-coding-provider)
(defvar ellama--current-session)
(defvar ellama--current-session-id)

(defvar ellama-tools-available nil
  "Alist containing all registered tools.")

(defvar ellama-tools-enabled nil
  "List of tools that have been enabled.")

(defun ellama-tools--set-session-extra (session extra)
  "Set SESSION EXTRA."
  (with-no-warnings
    (setf (ellama-session-extra session) extra)))

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

(defcustom ellama-tools-subagent-default-max-steps 30
  "Default maximum number of auto-continue steps for a sub-agent."
  :type 'integer
  :group 'ellama)

(defcustom ellama-tools-subagent-continue-prompt "Task not marked complete. Continue working. If you are done, YOU MUST use the `report_result` tool."
  "Prompt sent to sub-agent to keep the loop going."
  :type 'string
  :group 'ellama)

(defcustom ellama-tools-subagent-roles
  '(("general"
     :system "You are a helpful general assistant."
     :tools :all)

    ("explorer"
     :system "Explore, inspect, and report findings. Do not modify files."
     :tools ("read_file" "directory_tree" "grep" "grep_in_file"
	     "count_lines" "lines_range" "project_root" "shell_command"))

    ("coder"
     :system "You are an expert software developer. Make precise changes."
     :tools ("read_file" "write_file" "edit_file" "append_file" "prepend_file"
	     "move_file" "apply_patch" "grep" "grep_in_file" "project_root"
	     "directory_tree" "count_lines" "lines_range" "shell_command")
     :provider 'ellama-coding-provider)

    ("bash"
     :system "You are a bash scripting expert."
     :tools ("shell_command")
     :provider 'ellama-coding-provider))

  "Subagent roles with provider, system prompt and allowed tools."
  :type '(alist :key-type string :value-type plist)
  :group 'ellama)

(defun ellama-tools--for-role (role)
  "Resolve tools allowed for ROLE."
  (let* ((cfg (cdr (assoc role ellama-tools-subagent-roles)))
	 (tools (plist-get cfg :tools)))
    (cond
     ((eq tools :all)
      (cl-remove-if
       (lambda (tool) (string= (llm-tool-name tool) "task"))
       ellama-tools-available))
     ((listp tools)
      (cl-remove-if-not
       (lambda (tool) (member (llm-tool-name tool) tools))
       ellama-tools-available))
     (t
      nil))))

(defun ellama-tools--provider-for-role (role)
  "Resolve provider for ROLE."
  (let* ((cfg (cdr (assoc role ellama-tools-subagent-roles)))
         (provider (plist-get cfg :provider)))
    (if (not provider)
        ellama-provider
      (while (not (llm-standard-provider-p provider))
        (setq provider (eval provider)))
      provider)))

(defvar-local ellama-tools-confirm-allowed (make-hash-table)
  "Contains hash table of allowed functions.
Key is a function name symbol.  Value is a boolean t.")

(defun ellama-tools--confirm-call (function function-name &rest args)
  "Ask for confirmation before calling FUNCTION named FUNCTION-NAME.
ARGS are passed to FUNCTION.
Generates prompt automatically.  User can approve once (y), approve
for all future calls (a), forbid (n), or view the details in a
buffer (v) before deciding.  Returns the result of FUNCTION if
approved, \"Forbidden by the user\" otherwise."
  (let ((confirmation (gethash function ellama-tools-confirm-allowed nil)))
    (cond
     ;; If user has approved all calls, just execute the function
     ((or confirmation
	  ellama-tools-allow-all
	  (cl-find function ellama-tools-allowed))
      (let* ((result (apply function args))
             (result-str (if (stringp result)
                             result
                           (when result
                             (json-encode result))))
             (cb (and args
                      (functionp (car args))
                      (car args))))
        (if (and cb result-str)
            (funcall cb result-str)
          (or result-str "done"))))
     ;; Otherwise, ask for confirmation
     (t
      ;; Generate prompt with truncated string arguments
      (save-window-excursion
	(let* ((args-display
		(mapcar (lambda (arg)
			  (cond
			   ((stringp arg)
			    (string-truncate-left
			     arg
			     ellama-tools-argument-max-length))
			   (t
			    (format "%S" arg))))
			(cl-remove-if (lambda (arg) (functionp arg)) args)))
	       (prompt (format "Allow calling %s with arguments: %s?"
			       function-name
			       (mapconcat #'identity args-display ", ")))
	       result)
          (while
              (let ((answer (read-char-choice
                             (format "%s (y)es, (a)lways, (n)o, (r)eply, (v)iew: " prompt)
                             '(?y ?a ?n ?r ?v))))
                (cond
                 ;; View - show buffer with full details
                 ((eq answer ?v)
                  (let* ((buf (get-buffer-create "*Ellama Confirmation*"))
                         (args-full
                          (mapcar (lambda (arg)
                                    (cond
                                     ((stringp arg)
                                      arg)
                                     (t
                                      (format "%S" arg))))
                                  (cl-remove-if (lambda (arg) (functionp arg)) args))))
                    (with-current-buffer buf
                      (erase-buffer)
                      (insert (propertize "Ellama Function Call Confirmation\n"
                                          'face '(:weight bold :height 1.2)))
                      (insert "\n")
                      (insert (format "Function: %s\n\n" function-name))
                      (insert "Arguments:\n")
                      (dolist (arg args-full)
                        (insert (format "  - %s\n" arg))))
                    (display-buffer buf))
                  t) ;; Try again.
                 ;; Yes - execute function once
                 ((eq answer ?y)
                  (setq result (apply function args))
                  nil) ;; Done.
                 ;; Always - remember approval and execute function
                 ((eq answer ?a)
                  (puthash function t ellama-tools-confirm-allowed)
                  (setq result (apply function args))
                  nil) ;; done
                 ;; No - return nil
                 ((eq answer ?n)
                  (setq result "Forbidden by the user")
                  nil) ;; Done.
                 ;; Reply - custom response
                 ((eq answer ?r)
                  (setq result (read-string "Answer to the agent: "))
                  nil))))
          (let ((result-str (if (stringp result)
                                result
                              (when result
                                (json-encode result))))
                (cb (and args
                         (functionp (car args))
                         (car args))))
            (if (and cb result-str)
                (funcall cb result-str)
              (or result-str "done")))))))))

(defun ellama-tools-confirm (function &rest args)
  "Ask user for confirmation before calling FUNCTION with ARGS."
  (apply #'ellama-tools--confirm-call
	 function
	 (if (symbolp function)
	     (symbol-name function)
	   "anonymous-function")
	 args))

(defun ellama-tools-confirm-with-name (function name &rest args)
  "Ask user for confirmation before calling FUNCTION with ARGS.
NAME is fallback label used when FUNCTION has no symbol name."
  (apply #'ellama-tools--confirm-call
	 function
	 (if (symbolp function)
	     (symbol-name function)
	   (cond
	    ((stringp name) name)
	    ((symbolp name) (symbol-name name))
	    (t "anonymous-function")))
	 args))

(defun ellama-tools--make-confirm-wrapper (func name)
  "Make confirmation wrapper for FUNC.
NAME is fallback label used when FUNC has no symbol name."
  (if (symbolp func)
      (lambda (&rest args)
	(apply #'ellama-tools-confirm func args))
    (lambda (&rest args)
      (apply #'ellama-tools-confirm-with-name func name args))))

(defun ellama-tools-wrap-with-confirm (tool-plist)
  "Wrap a tool's function with automatic confirmation.
TOOL-PLIST is a property list in the format expected by `llm-make-tool'.
Returns a new tool definition with the :function wrapped."
  (let* ((func (plist-get tool-plist :function))
         (name (plist-get tool-plist :name))
         (args (plist-get tool-plist :args))
         (wrapped-args
          (mapcar
           (lambda (arg)
             (let* ((type (plist-get arg :type))
                    (wrapped-type
                     (if (symbolp type)
                         type
                       (and type (intern type)))))
               (plist-put arg :type wrapped-type)))
           args))
         (wrapped-func (ellama-tools--make-confirm-wrapper func name)))
    ;; Return a new plist with the wrapped function
    (setq tool-plist (plist-put tool-plist :function wrapped-func))
    (plist-put tool-plist :args wrapped-args)))

(defun ellama-tools-define-tool (tool-plist)
  "Define a new ellama tool with automatic confirmation wrapping.
TOOL-PLIST is a property list in the format expected by `llm-make-tool'."
  (add-to-list
   'ellama-tools-available
   (apply 'llm-make-tool (ellama-tools-wrap-with-confirm tool-plist))
   nil (lambda (a b)
         (string= (llm-tool-name a)
                  (llm-tool-name b)))))

(defun ellama-tools-enable-by-name-tool (name)
  "Add to `ellama-tools-enabled' each tool that matches NAME."
  (let* ((tool-name name)
         (tool (seq-find (lambda (tool) (string= tool-name (llm-tool-name tool)))
                         ellama-tools-available)))
    (when tool
      (add-to-list 'ellama-tools-enabled tool))
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

(defun ellama-tools--string-has-raw-bytes-p (string)
  "Return non-nil when STRING contain binary-like chars.
Treat Emacs raw-byte chars and NUL bytes as binary-like."
  (let ((idx 0)
        (len (length string))
        found)
    (while (and (not found) (< idx len))
      (when (or (> (aref string idx) #x10FFFF)
                (= (aref string idx) 0))
        (setq found t))
      (setq idx (1+ idx)))
    found))

(defun ellama-tools--sanitize-tool-text-output (text label)
  "Return TEXT or a warning when TEXT is binary-like.
LABEL is used to identify the source in the warning."
  (if (ellama-tools--string-has-raw-bytes-p text)
      (concat label
              " appears to contain binary data.  Reading binary data as "
              "text is a bad idea for this tool.")
    text))

(defun ellama-tools-read-file-tool (file-name)
  "Read the file FILE-NAME."
  (json-encode (if (not (file-exists-p file-name))
                   (format "File %s doesn't exists." file-name)
                 (let ((content (with-temp-buffer
                                  (insert-file-contents file-name)
                                  (buffer-string))))
                   (ellama-tools--sanitize-tool-text-output
                    content
                    (format "File %s" file-name))))))

(ellama-tools-define-tool
 '(:function
   ellama-tools-read-file-tool
   :name
   "read_file"
   :args
   ((:name
     "file_name"
     :type
     string
     :description
     "File name."))
   :description
   "Read the file FILE_NAME."))

(defun ellama-tools-write-file-tool (file-name content)
  "Write CONTENT to the file FILE-NAME."
  (write-region content nil file-name nil 'silent))

(ellama-tools-define-tool
 '(:function
   ellama-tools-write-file-tool
   :name
   "write_file"
   :args
   ((:name
     "file_name"
     :type
     string
     :description
     "Name of the file.")
    (:name
     "content"
     :type
     string
     :description
     "Content to write to the file."))
   :description
   "Write CONTENT to the file FILE_NAME."))

(defun ellama-tools-append-file-tool (file-name content)
  "Append CONTENT to the file FILE-NAME."
  (with-current-buffer (find-file-noselect file-name)
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
     "file_name"
     :type
     string
     :description
     "Name of the file.")
    (:name
     "content"
     :type
     string
     :description
     "Content to append to the file."))
   :description
   "Append CONTENT to the file FILE-NAME."))

(defun ellama-tools-prepend-file-tool (file-name content)
  "Prepend CONTENT to the file FILE-NAME."
  (with-current-buffer (find-file-noselect file-name)
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
     "file_name"
     :type
     string
     :description
     "Name of the file.")
    (:name
     "content"
     :type
     string
     :description
     "Content to prepend to the file."))
   :description
   "Prepend CONTENT to the file FILE_NAME."))

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

(defun ellama-tools-move-file-tool (file-name new-file-name)
  "Move the file from the specified FILE-NAME to the NEW-FILE-NAME."
  (if (and (file-exists-p file-name)
           (not (file-exists-p new-file-name)))
      (progn
        (rename-file file-name new-file-name))
    (error "Cannot move file: source file does not exist or destination already exists")))

(ellama-tools-define-tool
 '(:function
   ellama-tools-move-file-tool
   :name
   "move_file"
   :args
   ((:name
     "file_name"
     :type
     string
     :description
     "Current name of the file.")
    (:name
     "new_file_name"
     :type
     string
     :description
     "New name of the file."))
   :description
   "Move the file from the specified FILE_NAME to the NEW_FILE_NAME."))

(defun ellama-tools-edit-file-tool (file-name oldcontent newcontent)
  "Edit file FILE-NAME.
Replace OLDCONTENT with NEWCONTENT."
  (let ((content (with-temp-buffer
                   (insert-file-contents-literally file-name)
                   (buffer-string)))
        (coding-system-for-write 'raw-text))
    (when (string-match (regexp-quote oldcontent) content)
      (with-temp-buffer
        (insert (replace-match newcontent t t content))
        (write-region (point-min) (point-max) file-name)))))

(ellama-tools-define-tool
 '(:function
   ellama-tools-edit-file-tool
   :name
   "edit_file"
   :args
   ((:name
     "file_name"
     :type
     string
     :description
     "Name of the file.")
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
   "Edit file FILE_NAME. Replace OLDCONTENT with NEWCONTENT."))

(defun ellama-tools-shell-command-tool (callback cmd)
  "Execute shell command CMD.
CALLBACK – function called once with the result string."
  (condition-case err
      (let ((buf (get-buffer-create
		  (concat (make-temp-name " *ellama shell command") "*"))))
	(set-process-sentinel
	 (start-process
	  "*ellama-shell-command*" buf shell-file-name shell-command-switch cmd)
	 (lambda (process _)
	   (when (not (process-live-p process))
	     (let* ((raw-output
		     ;; trim trailing newline to reduce noisy tool output
		     (string-trim-right
		      (with-current-buffer buf (buffer-string))
		      "\n"))
		    (output
		     (ellama-tools--sanitize-tool-text-output
		      raw-output
		      "Command output"))
		    (exit-code (process-exit-status process))
		    (result
		     (cond
		      ((and (string= output "") (zerop exit-code))
		       "Command completed successfully with no output.")
		      ((string= output "")
		       (format "Command failed with exit code %d and no output."
			       exit-code))
		      ((zerop exit-code)
		       output)
		      (t
		       (format "Command failed with exit code %d.\n%s"
			       exit-code output)))))
	       (funcall callback result)
	       (kill-buffer buf))))))
    (error
     (funcall callback
	      (format "Failed to start shell command: %s"
		      (error-message-string err)))))
  ;; async tool should always return nil
  ;; to work properly with the llm library
  nil)

(ellama-tools-define-tool
 '(:function
   ellama-tools-shell-command-tool
   :name
   "shell_command"
   :async t
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
  (completing-read (concat question " ")
                   (if (stringp answer-variant-list)
                       (seq--into-list (json-parse-string answer-variant-list))
                     (seq--into-list answer-variant-list))))

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

(defun ellama-tools-count-lines-tool (file-name)
  "Count lines in file FILE-NAME."
  (with-current-buffer (find-file-noselect file-name)
    (count-lines (point-min) (point-max))))

(ellama-tools-define-tool
 '(:function
   ellama-tools-count-lines-tool
   :name
   "count_lines"
   :args
   ((:name
     "file_name"
     :type
     string
     :description
     "Name of the file to count lines in."))
   :description
   "Count lines in file FILE_NAME."))

(defun ellama-tools-lines-range-tool (file-name from to)
  "Return content of file FILE-NAME lines in range FROM TO."
  (json-encode (with-current-buffer (find-file-noselect file-name)
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
                     (ellama-tools--sanitize-tool-text-output
                      (buffer-substring-no-properties start end)
                      (format "File %s" file-name)))))))

(ellama-tools-define-tool
 '(:function
   ellama-tools-lines-range-tool
   :name
   "lines_range"
   :args
   ((:name
     "file_name"
     :type
     string
     :description
     "Name of the file to get lines from.")
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
   "Return content of file FILE_NAME lines in range FROM TO."))

(defun ellama-tools-apply-patch-tool (file-name patch)
  "Apply PATCH to file FILE-NAME.
PATCH is a string containing the patch data.
Returns the output of the patch command or an error message."
  (cond ((not file-name)
         "file-name is required")
        ((not (file-exists-p file-name))
         (format "file %s doesn't exists" file-name))
        ((not patch)
         "patch is required")
        (t
         (let* ((dir (file-name-directory (file-truename file-name)))
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
     "file_name"
     :type
     string
     :description
     "Name of the file to apply patch to.")
    (:name
     "patch"
     :type
     string
     :description
     "Patch data to apply."))
   :description
   "Apply a patch to the file FILE_NAME."))

(defun ellama-tools--make-report-result-tool (callback session)
  "Make report_result tool dynamically for SESSION.
CALLBACK will be used to report result asyncronously."
  `(:function
    (lambda (result)
      (let* ((extra (ellama-session-extra ,session))
             (done (plist-get extra :task-completed)))
        (unless done
          (ellama-tools--set-session-extra
           ,session
           (plist-put extra :task-completed t))
          (funcall ,callback result)))
      "Result received. Task completed.")
    :name "report_result"
    :description "Report final result and terminate the task."
    :args ((:name "result" :type string))))

(defun ellama--subagent-loop-handler (_text)
  "Internal subagent loop handler."
  (let* ((session ellama--current-session)
         (extra (ellama-session-extra session))
         (done (plist-get extra :task-completed))
         (steps (or (plist-get extra :step-count) 0))
         (max (or (plist-get extra :max-steps)
                  ellama-tools-subagent-default-max-steps))
         (callback (plist-get extra :result-callback)))
    (cond
     (done
      (message "Subagent finished."))
     ((>= steps max)
      (ellama-tools--set-session-extra
       session
       (plist-put extra :task-completed t))
      (funcall callback (format "Max steps (%d) reached." max)))
     (t
      (ellama-tools--set-session-extra
       session
       (plist-put extra :step-count (1+ steps)))
      (ellama-stream
       ellama-tools-subagent-continue-prompt
       :session session
       :on-done #'ellama--subagent-loop-handler)))))

(defun ellama-tools-task-tool (callback description &optional role)
  "Delegate DESCRIPTION to a sub-agent asynchronously.

CALLBACK   – function called once with the result string.
ROLE       – role key from `ellama-tools-subagent-roles'."
  (let* ((parent-id ellama--current-session-id)

         ;; ---- role resolution (safe fallback) ----
         (role-key (if (assoc role ellama-tools-subagent-roles)
                       role
                     "general"))

         (provider (ellama-tools--provider-for-role role-key))
         (role-cfg   (cdr (assoc role-key ellama-tools-subagent-roles)))
         (system-msg (plist-get role-cfg :system))

         (steps-limit ellama-tools-subagent-default-max-steps)

         ;; ---- create ephemeral worker session ----
         (worker (ellama-new-session provider description t))

         ;; ---- resolve tools for role ----
         (role-tools (ellama-tools--for-role role-key))

         ;; ---- dynamic report_result tool ----
         (report-tool
          (apply #'llm-make-tool
                 (ellama-tools--make-report-result-tool callback worker)))

         ;; IMPORTANT: report tool must be first (termination tool priority)
         (all-tools (cons report-tool role-tools)))

    ;; ============================================================
    ;; Initialize session state (single source of truth)
    ;; ============================================================

    (ellama-tools--set-session-extra
     worker
     (list
      :parent-session parent-id
      :role role-key
      :tools all-tools
      :result-callback callback
      :task-completed nil
      :step-count 0
      :max-steps steps-limit))

    ;; ============================================================
    ;; Start the agent loop
    ;; ============================================================

    (ellama-stream
     description
     :session worker
     :on-done #'ellama--subagent-loop-handler
     :tools all-tools
     :system
     (format
      "%s\n\nINSTRUCTIONS:\n\
Work step-by-step. Use tools when needed.\n\
When the task is COMPLETE you MUST call `report_result` exactly once."
      system-msg))

    ;; ============================================================
    ;; Immediate response to parent LLM (async contract)
    ;; ============================================================

    (message "Subtask started (session %s, role %s). Waiting for result via callback."
             (ellama-session-id worker)
             role-key)
    nil))

(ellama-tools-define-tool
 `(:function ellama-tools-task-tool
             :name "task"
             :async t
             :description "Delegate a task to a sub-agent."
             :args ((:name "description" :type string)
                    (:name "role" :type string
                           :enum ,(seq--into-vector (mapcar #'car ellama-tools-subagent-roles))))))

(provide 'ellama-tools)
;;; ellama-tools.el ends here
