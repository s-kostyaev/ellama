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
(require 'ellama-tools-dlp)

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

(defcustom ellama-tools-use-srt nil
  "Run shell-based tools via `srt'.
When non-nil, `shell_command', `grep' and `grep_in_file' run inside the
configured sandbox runtime.
Non-shell file tools also apply local filesystem checks derived from the same
`srt' settings file (`denyRead', `allowWrite', `denyWrite') to reduce policy
drift.  Missing `srt', missing settings, or malformed settings signal a
`user-error' (fail closed)."
  :type 'boolean
  :group 'ellama)

(defcustom ellama-tools-srt-program "srt"
  "Sandbox runtime executable used when `ellama-tools-use-srt' is non-nil."
  :type 'string
  :group 'ellama)

(defcustom ellama-tools-srt-args nil
  "Extra arguments passed to `srt' before the wrapped command.
`--settings'/`-s' in this list also select the config used by local non-shell
filesystem checks.  If not provided, `~/.srt-settings.json' is used."
  :type '(repeat string)
  :group 'ellama)

(defvar ellama-tools--srt-policy-cache nil
  "Cached parsed `srt' filesystem policy.
Plist with keys `:path', `:mtime' and `:policy'.")

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
             "move_file" "grep" "grep_in_file" "project_root" "directory_tree"
             "count_lines" "lines_range" "shell_command")
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
approved, \"Forbidden by the user\" otherwise.
For async tools (callback as first argument), pass string results to the
callback and return nil."
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
        (if cb
            (progn
              (when result-str
                (funcall cb result-str))
              nil)
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
            (if cb
                (progn
                  (when result-str
                    (funcall cb result-str))
                  nil)
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

(defun ellama-tools--dlp-make-scan-context
    (direction tool-name &optional arg-name)
  "Build DLP scan context for DIRECTION, TOOL-NAME and ARG-NAME."
  (ellama-tools-dlp--make-scan-context
   :direction direction
   :tool-name tool-name
   :arg-name arg-name
   :payload-length 0
   :truncated nil))

(defun ellama-tools--dlp-handle-output-string (tool-name text)
  "Scan output TEXT for TOOL-NAME and return filtered result."
  (let* ((scan (ellama-tools-dlp--scan-text
                text
                (ellama-tools--dlp-make-scan-context 'output tool-name)))
         (verdict (plist-get scan :verdict))
         (action (plist-get verdict :action)))
    (pcase action
      ('allow
       text)
      ('warn
       (pcase ellama-tools-dlp-output-warn-behavior
         ('allow
          text)
         ('block
          (or (plist-get verdict :message)
              (format "DLP blocked output for tool %s" tool-name)))
         (_
          (pcase (ellama-tools--dlp-output-warn-choice
                  tool-name (plist-get verdict :message))
            ('allow
             text)
            ('redact
             (ellama-tools--dlp-redact-output-from-scan
              scan text tool-name))
            (_
             (format "DLP warning denied output for tool %s" tool-name))))))
      ('block
       (or (plist-get verdict :message)
           (format "DLP blocked output for tool %s" tool-name)))
      ('redact
       (or (plist-get verdict :redacted-text)
           (plist-get verdict :message)
           (format "DLP blocked output for tool %s" tool-name)))
      (_
       text))))

(defconst ellama-tools--dlp-input-max-walk-depth 24
  "Maximum nested depth traversed when scanning structured tool inputs.")

(defconst ellama-tools--dlp-input-max-walk-nodes 2000
  "Maximum composite/string nodes traversed in one structured input scan.")

(defun ellama-tools--dlp-arg-path-key-name (key)
  "Return path segment name for KEY, or nil when KEY is unsupported."
  (cond
   ((keywordp key)
    (substring (symbol-name key) 1))
   ((symbolp key)
    (symbol-name key))
   ((stringp key)
    key)
   ((numberp key)
    (format "%s" key))
   (t
    nil)))

(defun ellama-tools--dlp-arg-path-append (path child)
  "Return PATH with CHILD segment appended.
CHILD may be a string key segment or an integer index."
  (cond
   ((integerp child)
    (format "%s[%d]" path child))
   ((and (stringp child) (> (length child) 0))
    (format "%s.%s" path child))
   ((stringp child)
    (format "%s.?" path))
   (t
    path)))

(defun ellama-tools--dlp-proper-list-length (value)
  "Return proper list length for VALUE, or nil for dotted/circular lists."
  (and (listp value)
       (condition-case nil
           (length value)
         (error nil))))

(defun ellama-tools--dlp-plist-like-p (value)
  "Return non-nil when VALUE look like a plist with symbol keys."
  (let ((len (ellama-tools--dlp-proper-list-length value))
        (rest value)
        ok)
    (setq ok (and len (zerop (% len 2))))
    (while (and ok rest)
      (let ((key (car rest)))
        (unless (and (symbolp key) key)
          (setq ok nil)))
      (setq rest (cddr rest)))
    ok))

(defun ellama-tools--dlp-alist-like-p (value)
  "Return non-nil when VALUE look like an alist."
  (let ((len (ellama-tools--dlp-proper-list-length value))
        (ok t))
    (when len
      (dolist (entry value)
        (let ((key (and (consp entry) (car entry))))
          (unless (and (consp entry)
                       (or (symbolp key)
                           (stringp key)
                           (numberp key)))
            (setq ok nil)))))
    (and len ok)))

(defun ellama-tools--dlp-alist-entry-value (entry)
  "Return logical value payload for alist ENTRY."
  (if (and (consp (cdr entry))
           (null (cddr entry)))
      (cadr entry)
    (cdr entry)))

(defun ellama-tools--dlp-walk-strings-1
    (node path callback seen node-count depth)
  "Walk NODE and call CALLBACK for string leave at PATH.
SEEN tracks composite objects to avoid cycles.  NODE-COUNT is a one-slot
vector used as a mutable traversal counter.  DEPTH is current nesting depth."
  (when (and (<= depth ellama-tools--dlp-input-max-walk-depth)
             (< (aref node-count 0) ellama-tools--dlp-input-max-walk-nodes))
    (aset node-count 0 (1+ (aref node-count 0)))
    (cond
     ((stringp node)
      (funcall callback node path))
     ((consp node)
      (unless (gethash node seen)
        (puthash node t seen)
        (cond
         ((ellama-tools--dlp-plist-like-p node)
          (let ((rest node)
                (pair-index 0))
            (while rest
              (let* ((key-name
                      (or (ellama-tools--dlp-arg-path-key-name (car rest))
                          (format "key%d" pair-index)))
                     (child-path
                      (ellama-tools--dlp-arg-path-append path key-name)))
                (ellama-tools--dlp-walk-strings-1
                 (cadr rest) child-path callback seen node-count (1+ depth)))
              (setq rest (cddr rest))
              (setq pair-index (1+ pair-index)))))
         ((ellama-tools--dlp-alist-like-p node)
          (let ((entry-index 0))
            (dolist (entry node)
              (let* ((key-name
                      (or (ellama-tools--dlp-arg-path-key-name (car entry))
                          (format "item%d" entry-index)))
                     (child-path
                      (ellama-tools--dlp-arg-path-append path key-name)))
                (ellama-tools--dlp-walk-strings-1
                 (ellama-tools--dlp-alist-entry-value entry)
                 child-path callback seen node-count (1+ depth)))
              (setq entry-index (1+ entry-index)))))
         ((ellama-tools--dlp-proper-list-length node)
          (let ((index 0))
            (dolist (item node)
              (ellama-tools--dlp-walk-strings-1
               item
               (ellama-tools--dlp-arg-path-append path index)
               callback seen node-count (1+ depth))
              (setq index (1+ index)))))
         (t
          (ellama-tools--dlp-walk-strings-1
           (car node)
           (ellama-tools--dlp-arg-path-append path "car")
           callback seen node-count (1+ depth))
          (ellama-tools--dlp-walk-strings-1
           (cdr node)
           (ellama-tools--dlp-arg-path-append path "cdr")
           callback seen node-count (1+ depth))))))
     ((vectorp node)
      (unless (gethash node seen)
        (puthash node t seen)
        (dotimes (index (length node))
          (ellama-tools--dlp-walk-strings-1
           (aref node index)
           (ellama-tools--dlp-arg-path-append path index)
           callback seen node-count (1+ depth)))))
     ((hash-table-p node)
      (unless (gethash node seen)
        (puthash node t seen)
        (let ((entry-index 0))
          (maphash
           (lambda (key item)
             (let* ((key-name
                     (or (ellama-tools--dlp-arg-path-key-name key)
                         (format "key%d" entry-index)))
                    (child-path
                     (ellama-tools--dlp-arg-path-append path key-name)))
               (ellama-tools--dlp-walk-strings-1
                item child-path callback seen node-count (1+ depth))
               (setq entry-index (1+ entry-index))))
           node))))
     (t
      nil))))

(defun ellama-tools--dlp-walk-strings (value root-path callback)
  "Call CALLBACK for each string leaf in VALUE using ROOT-PATH.
CALLBACK receives `(TEXT PATH)'.  Traverse lists, vectors and hash tables."
  (ellama-tools--dlp-walk-strings-1
   value root-path callback
   (make-hash-table :test 'eq)
   (vector 0)
   0))

(defun ellama-tools--dlp-scan-input-value (tool-name arg-name value)
  "Scan VALUE for TOOL-NAME under ARG-NAME and return a decision plist."
  (let (warn-message)
    (catch 'done
      (ellama-tools--dlp-walk-strings
       value arg-name
       (lambda (text path)
         (let* ((scan (ellama-tools-dlp--scan-text
                       text
                       (ellama-tools--dlp-make-scan-context
                        'input tool-name path)))
                (verdict (plist-get scan :verdict))
                (action (plist-get verdict :action))
                (message (plist-get verdict :message)))
           (cond
            ((eq action 'block)
             (throw 'done
                    (list :action 'block
                          :message (or message
                                       (format "DLP blocked input for %s"
                                               tool-name)))))
            ((and (eq action 'warn) (not warn-message))
             (setq warn-message
                   (or message
                       (format "DLP warned on input for tool %s"
                               tool-name))))))))
      (if warn-message
          (list :action 'warn :message warn-message)
        (list :action 'allow)))))

(defun ellama-tools--dlp-input-decision (tool-plist call-args)
  "Scan TOOL-PLIST CALL-ARGS and return decision plist.
Return plist with keys `:action' and optional `:message'."
  (let* ((tool-name (plist-get tool-plist :name))
         (async (plist-get tool-plist :async))
         (declared-args (plist-get tool-plist :args))
         (values (if (and async call-args (functionp (car call-args)))
                     (cdr call-args)
                   call-args))
         (specs declared-args)
         (index 0)
         warn-message)
    (catch 'done
      (while (and values specs)
        (let* ((value (car values))
               (spec (car specs))
               (raw-name (or (plist-get spec :name)
                             (format "arg%d" (1+ index))))
               (arg-name (if (symbolp raw-name)
                             (symbol-name raw-name)
                           raw-name)))
          (let* ((arg-decision
                  (ellama-tools--dlp-scan-input-value tool-name arg-name value))
                 (action (plist-get arg-decision :action))
                 (message (plist-get arg-decision :message)))
            (cond
             ((eq action 'block)
              (throw 'done arg-decision))
             ((and (eq action 'warn) (not warn-message))
              (setq warn-message message)))))
        (setq values (cdr values))
        (setq specs (cdr specs))
        (setq index (1+ index)))
      (if warn-message
          (list :action 'warn :message warn-message)
        (list :action 'allow)))))

(defun ellama-tools--dlp-confirm-warn (tool-name message &optional subject)
  "Ask explicit confirmation for DLP `warn' on TOOL-NAME with MESSAGE.
SUBJECT describe what is being allowed."
  (eq (read-char-choice
       (format "%s. Proceed with %s %s? (y/n): "
               (or message "DLP warning")
               (or subject "tool")
               tool-name)
       '(?y ?n))
      ?y))

(defun ellama-tools--dlp-output-warn-choice (tool-name message)
  "Return output warn choice for TOOL-NAME with MESSAGE.
Return one of symbols `allow', `redact' or `block'."
  (pcase (read-char-choice
          (format "%s. Output from tool %s: (a)llow, (r)edact, (b)lock: "
                  (or message "DLP warning")
                  tool-name)
          '(?a ?r ?b ?y ?n))
    ((or ?a ?y) 'allow)
    (?r 'redact)
    (_ 'block)))

(defun ellama-tools--dlp-redact-output-from-scan (scan text tool-name)
  "Return best-effort redaction for warn SCAN on TEXT from TOOL-NAME."
  (let ((verdict (plist-get scan :verdict))
        (findings (plist-get scan :findings)))
    (condition-case nil
        (if findings
            (ellama-tools-dlp--apply-redaction text findings)
          (or (plist-get verdict :message)
              (format "DLP blocked output for tool %s" tool-name)))
      (error
       (or (plist-get verdict :message)
           (format "DLP blocked output for tool %s" tool-name))))))

(defun ellama-tools--dlp-wrap-output-callback (tool-name callback)
  "Wrap CALLBACK to apply DLP output filtering for TOOL-NAME."
  (lambda (result)
    (funcall callback
             (if (stringp result)
                 (ellama-tools--dlp-handle-output-string tool-name result)
               result))))

(defun ellama-tools--dlp-return-message (async args message)
  "Return MESSAGE while preserving async callback conventions.
When ASYNC and ARGS start with a callback, send MESSAGE to the callback and
return nil."
  (let ((cb (and async args (functionp (car args)) (car args))))
    (if cb
        (progn
          (funcall cb message)
          nil)
      message)))

(defun ellama-tools--make-dlp-wrapper (func tool-plist)
  "Make DLP wrapper for FUNC using TOOL-PLIST metadata."
  (let ((tool-name (plist-get tool-plist :name))
        (async (plist-get tool-plist :async)))
    (lambda (&rest args)
      (if (not ellama-tools-dlp-enabled)
          (apply func args)
        (let* ((decision (ellama-tools--dlp-input-decision tool-plist args))
               (action (plist-get decision :action))
               (message (plist-get decision :message)))
          (pcase action
            ('block
             (ellama-tools--dlp-return-message
              async args
              (or message
                  (format "DLP blocked input for tool %s" tool-name))))
            ('warn
             (if (not (ellama-tools--dlp-confirm-warn tool-name message))
                 (ellama-tools--dlp-return-message
                  async args
                  (format "DLP warning denied tool execution for %s" tool-name))
               (let* ((wrapped-args
                       (if (and async args (functionp (car args)))
                           (cons (ellama-tools--dlp-wrap-output-callback
                                  tool-name (car args))
                                 (cdr args))
                         args))
                      (result (apply func wrapped-args)))
                 (if (and (not async) (stringp result))
                     (ellama-tools--dlp-handle-output-string tool-name result)
                   result))))
            (_
             (let* ((wrapped-args
                     (if (and async args (functionp (car args)))
                         (cons (ellama-tools--dlp-wrap-output-callback
                                tool-name (car args))
                               (cdr args))
                       args))
                    (result (apply func wrapped-args)))
               (if (and (not async) (stringp result))
                   (ellama-tools--dlp-handle-output-string tool-name result)
                 result)))))))))

(defun ellama-tools-wrap-with-confirm (tool-plist)
  "Wrap a tool's function with automatic confirmation and DLP.
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
         (confirm-func (ellama-tools--make-confirm-wrapper func name))
         (wrapped-func
          (ellama-tools--make-dlp-wrapper confirm-func tool-plist)))
    ;; Return a new plist with the wrapped function
    (setq tool-plist (plist-put tool-plist :function wrapped-func))
    (plist-put tool-plist :args wrapped-args)))

(defun ellama-tools--tool-name= (tool name)
  "Return non-nil when TOOL name equals NAME."
  (string= name (llm-tool-name tool)))

(defun ellama-tools--remove-tool-by-name (tools name)
  "Return TOOLS list without entries named NAME."
  (seq-remove (lambda (tool)
                (ellama-tools--tool-name= tool name))
              tools))

(defun ellama-tools-define-tool (tool-plist)
  "Define or replace an ellama tool with automatic confirmation wrapping.
TOOL-PLIST is a property list in the format expected by `llm-make-tool'."
  (let* ((wrapped-tool
          (apply 'llm-make-tool (ellama-tools-wrap-with-confirm tool-plist)))
         (name (llm-tool-name wrapped-tool))
         (enabled-p (cl-some (lambda (tool)
                               (ellama-tools--tool-name= tool name))
                             ellama-tools-enabled)))
    (setq ellama-tools-available
          (cons wrapped-tool
                (ellama-tools--remove-tool-by-name
                 ellama-tools-available name)))
    (setq ellama-tools-enabled
          (if enabled-p
              (cons wrapped-tool
                    (ellama-tools--remove-tool-by-name
                     ellama-tools-enabled name))
            (ellama-tools--remove-tool-by-name
             ellama-tools-enabled name)))))

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

(defun ellama-tools--srt-policy-clear-cache ()
  "Clear cached parsed `srt' policy."
  (setq ellama-tools--srt-policy-cache nil))

(defun ellama-tools--srt-settings-file ()
  "Return resolved `srt' settings file path."
  (let ((args ellama-tools-srt-args)
        settings)
    (while args
      (let ((arg (pop args)))
        (cond
         ((member arg '("--settings" "-s"))
          (unless args
            (user-error "Missing value after `%s' in `ellama-tools-srt-args'"
                        arg))
          (setq settings (pop args)))
         ((string-prefix-p "--settings=" arg)
          (setq settings (substring arg (length "--settings=")))))))
    (expand-file-name (or settings "~/.srt-settings.json"))))

(defun ellama-tools--srt-file-mtime (path)
  "Return modification time for PATH as a float."
  (unless (file-exists-p path)
    (user-error "Missing srt settings file: %s" path))
  (float-time
   (file-attribute-modification-time
    (file-attributes path 'integer))))

(defun ellama-tools--srt-array-to-list (value)
  "Convert JSON array VALUE to a list or return nil."
  (cond
   ((null value) nil)
   ((vectorp value) (append value nil))
   ((listp value) value)
   (t value)))

(defun ellama-tools--srt-string-list (value key config-path)
  "Return VALUE as a list of strings for KEY from CONFIG-PATH.
Signal `user-error' when VALUE has an invalid shape."
  (setq value (ellama-tools--srt-array-to-list value))
  (unless (or (null value) (listp value))
    (user-error "Malformed srt config %s: `filesystem.%s' must be a list"
                config-path key))
  (dolist (item value)
    (unless (stringp item)
      (user-error "Malformed srt config %s: `filesystem.%s' must contain strings"
                  config-path key)))
  value)

(defun ellama-tools--srt-policy-load (config-path)
  "Read and parse `srt' settings from CONFIG-PATH."
  (unless (file-exists-p config-path)
    (user-error "Missing srt settings file: %s" config-path))
  (let* ((json (condition-case err
                   (with-temp-buffer
                     (insert-file-contents-literally config-path)
                     (json-parse-buffer :object-type 'alist
                                        :array-type 'array
                                        :null-object :json-null
                                        :false-object :json-false))
                 (json-parse-error
                  (user-error "Invalid srt JSON config %s: %s"
                              config-path
                              (error-message-string err)))
                 (file-error
                  (user-error "Cannot read srt settings file %s: %s"
                              config-path
                              (error-message-string err)))))
         (filesystem-pair (and (listp json) (assq 'filesystem json)))
         (filesystem (cdr filesystem-pair)))
    (unless (listp json)
      (user-error "Malformed srt config %s: top-level JSON object is required"
                  config-path))
    (when filesystem-pair
      (unless (and (listp filesystem)
                   (not (eq filesystem :json-null)))
        (user-error "Malformed srt config %s: `filesystem' must be an object"
                    config-path)))
    (list :config-path config-path
          :deny-read
          (if filesystem-pair
              (ellama-tools--srt-string-list
               (alist-get 'denyRead filesystem) "denyRead" config-path)
            nil)
          :allow-write
          (if filesystem-pair
              (ellama-tools--srt-string-list
               (alist-get 'allowWrite filesystem) "allowWrite" config-path)
            nil)
          :deny-write
          (if filesystem-pair
              (ellama-tools--srt-string-list
               (alist-get 'denyWrite filesystem) "denyWrite" config-path)
            nil))))

(defun ellama-tools--srt-policy-current ()
  "Return current parsed `srt' filesystem policy, using a small cache."
  (let* ((config-path (ellama-tools--srt-settings-file))
         (mtime (ellama-tools--srt-file-mtime config-path))
         (cache ellama-tools--srt-policy-cache))
    (if (and cache
             (equal (plist-get cache :path) config-path)
             (equal (plist-get cache :mtime) mtime))
        (plist-get cache :policy)
      (let ((policy (ellama-tools--srt-policy-load config-path)))
        (setq ellama-tools--srt-policy-cache
              (list :path config-path :mtime mtime :policy policy))
        policy))))

(defun ellama-tools--srt-strip-trailing-slashes (path)
  "Return PATH without trailing slashes, except for root."
  (if (string-match-p "\\`/+\\'" path)
      "/"
    (replace-regexp-in-string "/+\\'" "" path)))

(defun ellama-tools--srt-truename-if-possible (path)
  "Return `file-truename' for PATH when possible, else nil."
  (condition-case nil
      (file-truename path)
    (file-error nil)))

(defun ellama-tools--srt-path-exists-p (path)
  "Return non-nil when PATH exists or is a symlink."
  (let ((expanded (expand-file-name path)))
    (or (file-exists-p expanded)
        (file-symlink-p expanded))))

(defun ellama-tools--srt-normalize-literal-path (path)
  "Return normalized literal PATH for policy comparisons."
  (ellama-tools--srt-strip-trailing-slashes
   (or (ellama-tools--srt-truename-if-possible path)
       (expand-file-name path))))

(defun ellama-tools--srt-normalize-rule-literal-path (path)
  "Return normalized literal rule PATH for policy comparisons.
On macOS, keep the lexical path for exact symlink rules to match observed
`srt' behavior for `denyRead' symlink-path entries."
  (let* ((expanded (expand-file-name path))
         (probe (ellama-tools--srt-strip-trailing-slashes expanded)))
    (ellama-tools--srt-strip-trailing-slashes
     (if (and (eq system-type 'darwin)
              (file-symlink-p probe))
         expanded
       (or (ellama-tools--srt-truename-if-possible expanded)
           expanded)))))

(defun ellama-tools--srt-nearest-existing-dir (dir)
  "Return nearest existing ancestor directory for DIR."
  (let ((current (expand-file-name dir)))
    (while (and current (not (file-directory-p current)))
      (let* ((trimmed (directory-file-name current))
             (parent (file-name-directory trimmed)))
        (setq current
              (unless (or (null parent)
                          (equal (expand-file-name parent)
                                 (expand-file-name current)))
                parent))))
    current))

(defun ellama-tools--srt-normalize-nonexisting-target-path (path)
  "Return normalized PATH for a non-existing write target.
Preserve missing intermediate path segments after resolving the nearest
existing ancestor with `file-truename'."
  (let* ((expanded (expand-file-name path))
         (anchor (ellama-tools--srt-nearest-existing-dir
                  (file-name-directory expanded))))
    (if (not anchor)
        (ellama-tools--srt-strip-trailing-slashes expanded)
      (let* ((anchor-expanded (file-name-as-directory
                               (expand-file-name anchor)))
             (anchor-normalized (file-name-as-directory
                                 (ellama-tools--srt-strip-trailing-slashes
                                  (or (ellama-tools--srt-truename-if-possible
                                       anchor)
                                      anchor-expanded))))
             (suffix (file-relative-name expanded anchor-expanded)))
        (ellama-tools--srt-strip-trailing-slashes
         (expand-file-name suffix anchor-normalized))))))

(defun ellama-tools--srt-normalize-target-path (path op)
  "Return normalized target PATH for OP policy check."
  (let ((expanded (expand-file-name path)))
    (if (or (memq op '(read list))
            (file-exists-p expanded)
            (file-directory-p expanded))
        (ellama-tools--srt-normalize-literal-path expanded)
      (ellama-tools--srt-normalize-nonexisting-target-path expanded))))

(defun ellama-tools--srt-path-has-glob-p (path)
  "Return non-nil when PATH look like a glob pattern."
  (string-match-p "[*?\\[]" path))

(defun ellama-tools--srt-platform-glob-support-p ()
  "Return non-nil when local matcher should treat patterns as globs."
  t)

(defun ellama-tools--srt-glob-pattern-candidates (rule)
  "Return glob pattern candidates for RULE.
On macOS, a path may be referenced via `/var' while `file-truename'
resolves to `/private/var'.  Add a candidate with a normalized
existing directory prefix when possible."
  (let* ((expanded (expand-file-name rule))
         (pattern (if (string-suffix-p "/" rule)
                      (concat expanded "*")
                    expanded))
         (candidates (list pattern))
         (dir (file-name-directory pattern))
         (tail (file-name-nondirectory pattern)))
    (when (and dir (not (ellama-tools--srt-path-has-glob-p dir)))
      (let ((true-dir (ellama-tools--srt-truename-if-possible dir)))
        (when true-dir
          (let ((alt (concat (file-name-as-directory
                              (ellama-tools--srt-strip-trailing-slashes
                               true-dir))
                             tail)))
            (unless (member alt candidates)
              (push alt candidates))))))
    (nreverse candidates)))

(defun ellama-tools--srt-dir-prefix-match-p (dir target)
  "Return non-nil when TARGET is DIR or inside DIR."
  (let ((dir (ellama-tools--srt-strip-trailing-slashes dir))
        (target (ellama-tools--srt-strip-trailing-slashes target)))
    (or (string= dir target)
        (string-prefix-p (concat dir "/") target))))

(defun ellama-tools--srt-rule-match-p (target rule)
  "Return non-nil when normalized TARGET matches filesystem RULE."
  (let* ((raw rule)
         (globp (and (ellama-tools--srt-platform-glob-support-p)
                     (ellama-tools--srt-path-has-glob-p raw)))
         (dir-marked-p (string-suffix-p "/" raw))
         (expanded (expand-file-name raw))
         (dirp (or dir-marked-p
                   (and (not globp) (file-directory-p expanded)))))
    (cond
     (globp
      (catch 'matched
        (dolist (pattern (ellama-tools--srt-glob-pattern-candidates raw))
          (let ((regex (condition-case err
                           (wildcard-to-regexp pattern)
                         (invalid-regexp
                          (user-error "Unsupported srt filesystem pattern `%s': %s"
                                      raw
                                      (error-message-string err))))))
            (when (string-match-p regex target)
              (throw 'matched t))))
        nil))
     (dirp
      (ellama-tools--srt-dir-prefix-match-p
       (ellama-tools--srt-normalize-rule-literal-path expanded)
       target))
     (t
      (string=
       (ellama-tools--srt-normalize-rule-literal-path expanded)
       target)))))

(defun ellama-tools--srt-rule-match-any-p (target rules)
  "Return non-nil when normalized TARGET matches one of RULES."
  (catch 'matched
    (dolist (rule rules)
      (when (ellama-tools--srt-rule-match-p target rule)
        (throw 'matched t)))
    nil))

(defun ellama-tools--srt-literal-file-rule-p (rule)
  "Return non-nil when RULE look like a literal file path rule."
  (let* ((globp (ellama-tools--srt-path-has-glob-p rule))
         (expanded (expand-file-name rule)))
    (and (not globp)
         (not (string-suffix-p "/" rule))
         (not (file-directory-p expanded)))))

(defun ellama-tools--srt-deny-write-match-any-p (target rules target-exists)
  "Return non-nil when deny-write RULES should block TARGET.
When TARGET-EXISTS is nil, skip exact literal file deny rules to match
observed `srt' behavior on macOS for new file creation under an allowed
directory."
  (catch 'matched
    (dolist (rule rules)
      (when (and (ellama-tools--srt-rule-match-p target rule)
                 (or target-exists
                     ;; Current observed parity:
                     ;; macOS may allow creation despite exact file denyWrite.
                     ;; Linux denies it.
                     (not (and (eq system-type 'darwin)
                               (ellama-tools--srt-literal-file-rule-p rule)))))
        (throw 'matched t)))
    nil))

(defun ellama-tools--srt-check-access (path op)
  "Return nil when PATH is allowed for OP, else a deny reason string."
  (let* ((policy (ellama-tools--srt-policy-current))
         (target-exists (ellama-tools--srt-path-exists-p path))
         (target (ellama-tools--srt-normalize-target-path path op))
         (deny-read (plist-get policy :deny-read))
         (allow-write (plist-get policy :allow-write))
         (deny-write (plist-get policy :deny-write)))
    (pcase op
      ((or 'read 'list)
       (when (ellama-tools--srt-rule-match-any-p target deny-read)
         "Denied by `filesystem.denyRead'"))
      ('write
       (cond
        ((ellama-tools--srt-deny-write-match-any-p
          target deny-write target-exists)
         "Denied by `filesystem.denyWrite'")
        ((not (ellama-tools--srt-rule-match-any-p target allow-write))
         "Denied because write access is not allowed by `filesystem.allowWrite'")
        (t nil)))
      (_
       (error "Unsupported srt access operation: %S" op)))))

(defun ellama-tools--tool-check-file-access (path op)
  "Check local `srt' filesystem policy for PATH and OP.
Return error message on denial when `ellama-tools-use-srt' is non-nil."
  (when ellama-tools-use-srt
    (let ((reason (ellama-tools--srt-check-access path op)))
      (when reason
        (let* ((policy (ellama-tools--srt-policy-current))
               (config-path (plist-get policy :config-path))
               (target (ellama-tools--srt-normalize-target-path path op)))
          (format "srt policy denied %s access to %s (target %s) using %s: %s"
                  op path target config-path reason))))))

(defun ellama-tools--command-argv (program &rest args)
  "Return argv for PROGRAM and ARGS.
Wrap command with `srt' when `ellama-tools-use-srt' is non-nil."
  (if (not ellama-tools-use-srt)
      (cons program args)
    (let ((srt-path (executable-find ellama-tools-srt-program)))
      (unless srt-path
        (user-error
         (concat
          "Cannot find `srt' executable `%s'.  Install sandbox-runtime "
          "or disable `ellama-tools-use-srt'")
         ellama-tools-srt-program))
      (append (list srt-path) ellama-tools-srt-args (cons program args)))))

(defun ellama-tools--call-command-to-string (program &rest args)
  "Run PROGRAM with ARGS and return stdout as a string."
  (let ((argv (apply #'ellama-tools--command-argv program args)))
    (with-temp-buffer
      (apply #'call-process (car argv) nil t nil (cdr argv))
      (buffer-string))))

(defun ellama-tools-read-file-tool (file-name)
  "Read the file FILE-NAME."
  (or (ellama-tools--tool-check-file-access file-name 'read)
      (json-encode (if (not (file-exists-p file-name))
                       (format "File %s doesn't exists." file-name)
                     (let ((content (with-temp-buffer
                                      (insert-file-contents file-name)
                                      (buffer-string))))
                       (ellama-tools--sanitize-tool-text-output
                        content
                        (format "File %s" file-name)))))))

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
  (or (ellama-tools--tool-check-file-access file-name 'write)
      (write-region content nil file-name nil 'silent)))

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
  (or (ellama-tools--tool-check-file-access file-name 'write)
      (with-current-buffer (find-file-noselect file-name)
        (goto-char (point-max))
        (insert content)
        (save-buffer))))

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
  (or (ellama-tools--tool-check-file-access file-name 'write)
      (with-current-buffer (find-file-noselect file-name)
        (goto-char (point-min))
        (insert content)
        (save-buffer))))

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
  (or (ellama-tools--tool-check-file-access dir 'list)
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
          tree))))

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
  (or (ellama-tools--tool-check-file-access file-name 'read)
      (ellama-tools--tool-check-file-access file-name 'write)
      (ellama-tools--tool-check-file-access new-file-name 'write)
      (if (and (file-exists-p file-name)
               (not (file-exists-p new-file-name)))
          (progn
            (rename-file file-name new-file-name))
        (error "Cannot move file: source file does not exist or destination already exists"))))

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
  (or (ellama-tools--tool-check-file-access file-name 'read)
      (ellama-tools--tool-check-file-access file-name 'write)
      (let ((content (with-temp-buffer
                       (insert-file-contents-literally file-name)
                       (buffer-string)))
            (coding-system-for-write 'raw-text))
        (when (string-match (regexp-quote oldcontent) content)
          (with-temp-buffer
            (insert (replace-match newcontent t t content))
            (write-region (point-min) (point-max) file-name))))))

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
  (let ((argv (ellama-tools--command-argv
               shell-file-name shell-command-switch cmd)))
    (condition-case err
        (let ((buf (get-buffer-create
                    (concat (make-temp-name " *ellama shell command") "*"))))
          (set-process-sentinel
           (apply #'start-process
                  "*ellama-shell-command*" buf
                  (car argv) (cdr argv))
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
                        (error-message-string err))))))
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
     (string-trim-right
      (ellama-tools--call-command-to-string
       "find" "." "-type" "f" "-exec"
       "grep" "--color=never" "-nH" "-e" search-string "{}" "+")
      "\n"))))

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
   (ellama-tools--call-command-to-string
    "grep" "--color=never" "-nh" search-string (file-truename file))))

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
  (or (ellama-tools--tool-check-file-access file-name 'read)
      (with-current-buffer (find-file-noselect file-name)
        (count-lines (point-min) (point-max)))))

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
  (or (ellama-tools--tool-check-file-access file-name 'read)
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
                          (format "File %s" file-name))))))))

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
