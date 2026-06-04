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
(require 'cl-lib)
(require 'project)
(require 'json)
(require 'llm)
(require 'seq)
(require 'subr-x)
(require 'ellama-tools-dlp)

(declare-function llm-standard-provider-p "llm-provider-utils" (provider))
(declare-function ellama-new-session "ellama"
                  (provider prompt &optional ephemeral))
(declare-function ellama-session-p "ellama" (session))
(declare-function ellama-session-extra "ellama" (session))
(declare-function ellama-session-id "ellama" (session))
(declare-function ellama-session-provider "ellama" (session))
(declare-function ellama-get-session-buffer "ellama" (id))
(declare-function ellama-get-nick-prefix-for-mode "ellama" ())
(declare-function ellama--fill-long-lines "ellama" (string))
(declare-function ellama--scroll "ellama" (&optional buffer point))
(declare-function ellama-image-file-p "ellama" (file-name))
(declare-function ellama--image-mime-type "ellama" (file-name))
(declare-function ellama--file-size "ellama" (file-name))
(declare-function ellama--provider-supports-image-input-p "ellama" (provider))
(declare-function ellama--session-add-pending-tool-media
                  "ellama" (session file-name))
(declare-function ellama-stream "ellama" (prompt &rest args))

(defvar ellama-provider)
(defvar ellama-coding-provider)
(defvar ellama-assistant-nick)
(defvar ellama-user-nick)
(defvar ellama--current-session)
(defvar ellama--current-session-id)

(defvar ellama-tools-available nil
  "Alist containing all registered tools.")

(defvar ellama-tools-enabled nil
  "List of tools that have been enabled.")
(defcustom ellama-tools-read-before-write-enabled t
  "Require a session-local read before overwriting existing files.
When non-nil, full-file mutating tools refuse their first attempt to change an
existing file that has not been read in the current Ellama tool session.  The
next attempt is allowed so deliberate overwrites remain possible."
  :type 'boolean
  :group 'ellama)

(defvar ellama-tools--current-session nil
  "Current Ellama session used while executing tools.")

(defun ellama-tools--set-session-extra (session extra)
  "Set SESSION EXTRA."
  (let ((offset (cl-struct-slot-offset 'ellama-session 'extra)))
    (aset session offset extra)))

(defun ellama-tools--session-extra-with (session &rest pairs)
  "Return SESSION extra plist with PAIRS applied.
PAIRS is a flat plist of keys and values."
  (let ((extra (if (plistp (ellama-session-extra session))
                   (copy-sequence (ellama-session-extra session))
                 nil)))
    (while pairs
      (setq extra (plist-put extra (pop pairs) (pop pairs))))
    extra))

(defun ellama-tools--active-session ()
  "Return the active Ellama session for tool bookkeeping."
  (cond
   ((and (boundp 'ellama--current-session)
         (ellama-session-p ellama--current-session))
    ellama--current-session)
   ((ellama-session-p ellama-tools--current-session)
    ellama-tools--current-session)))

(defun ellama-tools--existing-file-key (file-name)
  "Return canonical key for existing FILE-NAME."
  (when (and (file-exists-p file-name)
             (not (file-directory-p file-name)))
    (file-truename file-name)))

(defun ellama-tools--session-list-extra (session key)
  "Return list value for SESSION extra KEY."
  (let ((value (and (ellama-session-p session)
                    (plist-get (ellama-session-extra session) key))))
    (and (listp value) value)))

(defun ellama-tools--session-note-list-extra (session key value)
  "Add VALUE to SESSION extra list KEY."
  (when (and (ellama-session-p session)
             value
             (not (member value (ellama-tools--session-list-extra
                                 session key))))
    (ellama-tools--set-session-extra
     session
     (ellama-tools--session-extra-with
      session key
      (cons value (ellama-tools--session-list-extra session key))))))

(defun ellama-tools--mark-file-read (file-name)
  "Mark existing FILE-NAME as read in the active tool session."
  (when-let* ((session (ellama-tools--active-session))
              (file-key (ellama-tools--existing-file-key file-name)))
    (ellama-tools--session-note-list-extra
     session :read-before-write-read-files file-key)))

(defun ellama-tools--file-read-p (file-name)
  "Return non-nil when FILE-NAME was read in the active tool session."
  (when-let* ((session (ellama-tools--active-session))
              (file-key (ellama-tools--existing-file-key file-name)))
    (member file-key
            (ellama-tools--session-list-extra
             session :read-before-write-read-files))))

(defun ellama-tools--read-before-write-warning-recorded-p (file-name)
  "Return non-nil when FILE-NAME already triggered read-before-write guard."
  (when-let* ((session (ellama-tools--active-session))
              (file-key (ellama-tools--existing-file-key file-name)))
    (member file-key
            (ellama-tools--session-list-extra
             session :read-before-write-warned-files))))

(defun ellama-tools--record-read-before-write-warning (file-name)
  "Record that FILE-NAME triggered read-before-write guard."
  (when-let* ((session (ellama-tools--active-session))
              (file-key (ellama-tools--existing-file-key file-name)))
    (ellama-tools--session-note-list-extra
     session :read-before-write-warned-files file-key)))

(defun ellama-tools--read-before-write-check (operation file-name)
  "Return a read-before-write refusal for OPERATION on FILE-NAME, or nil."
  (when (and ellama-tools-read-before-write-enabled
             (ellama-tools--active-session)
             (ellama-tools--existing-file-key file-name)
             (not (ellama-tools--file-read-p file-name))
             (not (ellama-tools--read-before-write-warning-recorded-p
                   file-name)))
    (ellama-tools--record-read-before-write-warning file-name)
    (format
     (concat
      "%s refused: existing file %s has not been read in this Ellama "
      "tool session. Use `read_file` or `lines_range` to inspect it, "
      "then retry. A second attempt is allowed if this overwrite is "
      "intentional.")
     operation file-name)))

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

(defcustom ellama-tools-read-file-default-mode 'auto
  "Default mode for the `read_file' tool.
Use `auto' to read text files as text and supported image files as media.
Use `text' to force text reading.
Use `image' to force image handling."
  :type '(choice (const auto)
                 (const text)
                 (const image))
  :group 'ellama)

(defcustom ellama-tools-dlp-safe-read-file-regexps
  (let* ((base-file (or load-file-name buffer-file-name default-directory))
         (base-dir (file-name-directory (expand-file-name base-file)))
         (safe-dir (condition-case nil
                       (file-truename base-dir)
                     (file-error base-dir))))
    (list (concat "\\`"
                  (regexp-quote safe-dir)
                  "ellama\\(?:-[[:alnum:]-]+\\)?\\.el\\'")))
  "Regexps matching file names whose read outputs skip DLP scans.
Only output scans from file-reading tools are skipped.  Input scans and
outputs from all other tools still use DLP."
  :type '(repeat regexp)
  :group 'ellama-tools-dlp)

(defcustom ellama-tools-shell-command-default-timeout 5
  "Default timeout in seconds for the `shell_command' tool."
  :type 'number
  :group 'ellama)

(defcustom ellama-tools-balanced-edit-enabled t
  "Validate code syntax before tools write file contents.
When non-nil, mutating file tools reject edits whose resulting buffer has
unbalanced delimiters or invalid syntax in configured major modes."
  :type 'boolean
  :group 'ellama)

(defcustom ellama-tools-balanced-edit-modes '(prog-mode conf-mode)
  "Major modes where mutating file tools validate resulting buffers.
Validation is applied when the visited file buffer derives from one of these
modes.  Keep text-oriented modes out of this list to avoid rejecting ordinary
prose that happens to contain unmatched delimiter characters."
  :type '(repeat symbol)
  :group 'ellama)

(defcustom ellama-tools-edit-before-shell-commands nil
  "Project-local shell commands to run before edit tools mutate files.
Each entry must be a plist with a required `:command' string.  When
`:show-output' is non-nil, successful hook output is returned to the agent.
Failing hooks are always returned to the agent and block the edit."
  :type '(repeat (plist :options ((:command string)
                                  (:show-output boolean)
                                  (:name string))))
  :group 'ellama)

(defcustom ellama-tools-edit-after-shell-commands nil
  "Project-local shell commands to run after edit tools mutate files.
Each entry must be a plist with a required `:command' string.  When
`:show-output' is non-nil, successful hook output is returned to the agent.
Failing hooks are always returned to the agent and do not roll back the edit."
  :type '(repeat (plist :options ((:command string)
                                  (:show-output boolean)
                                  (:name string))))
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

(defcustom ellama-tools-subagent-loop-detection-enabled t
  "Detect repeated identical tool calls in sub-agent sessions.
When non-nil, sub-agent tools return recovery guidance on repeated identical
calls and terminate the subtask if the same consecutive call chain continues."
  :type 'boolean
  :group 'ellama)

(defcustom ellama-tools-subagent-loop-detection-repeated-threshold 2
  "Number of consecutive identical sub-agent tool calls that triggers recovery.
Values below 2 are treated as 2 so the first call is never considered a loop."
  :type 'integer
  :group 'ellama)

(defcustom ellama-tools-subagent-loop-detection-max-traces 50
  "Maximum recent sub-agent tool calls retained for loop detection."
  :type 'integer
  :group 'ellama)

(defcustom ellama-tools-agent-default-max-steps 40
  "Default maximum number of auto-continue steps for plan-and-act agents."
  :type 'integer
  :group 'ellama)

(defcustom ellama-tools-task-template-dirs
  (list (locate-user-emacs-file "ellama/task-templates"))
  "Directories used to resolve relative task template names.
The `task' tool can render a template before spawning a sub-agent.  When
`template_base' is omitted in the tool call, relative template names are
searched in these directories."
  :type '(repeat directory)
  :group 'ellama)

(defcustom ellama-tools-task-template-allow-absolute-paths nil
  "Allow `task' template names to be absolute file names.
When nil, absolute template names are rejected.  Relative template names are
always resolved under either the tool call's `template_base' argument or
`ellama-tools-task-template-dirs'."
  :type 'boolean
  :group 'ellama)

(defcustom ellama-tools-output-line-budget-enabled t
  "Enable per-tool output line-budget truncation.
The guard applies before output is sent back to the LLM."
  :type 'boolean
  :group 'ellama)

(defcustom ellama-tools-output-line-budget-max-lines 200
  "Maximum line count allowed per tool-output payload."
  :type 'integer
  :group 'ellama)

(defcustom ellama-tools-output-line-budget-max-line-length 4000
  "Maximum character count allowed for one output line."
  :type 'integer
  :group 'ellama)

(defcustom ellama-tools-output-line-budget-save-overflow-file t
  "Save full overflowing output to a temp file when source is unknown."
  :type 'boolean
  :group 'ellama)

(defconst ellama-tools--output-sections-property
  'ellama-tools-output-sections
  "Text property storing independently processed output sections.")

(defcustom ellama-tools-subagent-continue-prompt "Task not marked complete. Continue working. If you are done, YOU MUST use the `report_result` tool."
  "Prompt sent to sub-agent to keep the loop going."
  :type 'string
  :group 'ellama)

(defcustom ellama-tools-agent-planning-prompt
  "Create a concise checklist plan for the user's task before acting.
Inspect context with tools if needed. When the plan is ready, either call the
`agent_submit_plan` tool with the full checklist, or include this block exactly
if tool calls are not available:

BEGIN_ELLAMA_AGENT_STATE
phase: acting
plan:
- [ ] First concrete step
- [ ] Second concrete step
END_ELLAMA_AGENT_STATE"
  "Prompt sent while the plan-and-act loop is planning."
  :type 'string
  :group 'ellama)

(defcustom ellama-tools-agent-continue-prompt
  "Continue the plan-and-act loop. Work on the first pending checklist item.
After each meaningful change, call `agent_update_plan` or include an updated
BEGIN_ELLAMA_AGENT_STATE block. When all items are complete, call
`agent_report_result` or set phase: done with a result."
  "Prompt sent to continue a plan-and-act loop."
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

(defconst ellama-tools--call-log-buffer-name "*Ellama Tool Call Logs*"
  "Name of the buffer with tool call confirmation logs.")

(defun ellama-tools--log-call (status function-name args)
  "Append tool call log entry with STATUS for FUNCTION-NAME and ARGS."
  (let ((buf (get-buffer-create ellama-tools--call-log-buffer-name))
        (args-display
         (mapcar (lambda (arg) (format "%S" arg))
                 (cl-remove-if (lambda (arg) (functionp arg)) args))))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert (format-time-string "[%Y-%m-%d %H:%M:%S] "))
      (insert (format "%s %s" status function-name))
      (when args-display
        (insert (format " %s" (mapconcat #'identity args-display ", "))))
      (insert "\n"))))

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
      (ellama-tools--log-call "autoaccepted" function-name args)
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
               result
               decision)
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
                  (setq decision "accepted")
                  (setq result (apply function args))
                  nil) ;; Done.
                 ;; Always - remember approval and execute function
                 ((eq answer ?a)
                  (setq decision "accepted")
                  (puthash function t ellama-tools-confirm-allowed)
                  (setq result (apply function args))
                  nil) ;; done
                 ;; No - return nil
                 ((eq answer ?n)
                  (setq decision "rejected")
                  (setq result "Forbidden by the user")
                  nil) ;; Done.
                 ;; Reply - custom response
                 ((eq answer ?r)
                  (setq decision "rejected")
                  (setq result (read-string "Answer to the agent: "))
                  nil))))
          (when decision
            (ellama-tools--log-call decision function-name args))
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
    (direction tool-name &optional arg-name tool-metadata)
  "Build DLP scan context for DIRECTION, TOOL-NAME and ARG-NAME.
TOOL-METADATA may include `:tool-origin', `:server-id' and
`:tool-identity' values."
  (let ((tool-origin (plist-get tool-metadata :tool-origin))
        (server-id (plist-get tool-metadata :server-id))
        (tool-identity (plist-get tool-metadata :tool-identity)))
    (ellama-tools-dlp--make-scan-context
     :direction direction
     :tool-name tool-name
     :arg-name arg-name
     :payload-length 0
     :truncated nil
     :tool-origin tool-origin
     :server-id server-id
     :tool-identity tool-identity)))

(defun ellama-tools--tool-category-name (tool-plist)
  "Return normalized category name from TOOL-PLIST, or nil."
  (let ((category (plist-get tool-plist :category)))
    (cond
     ((stringp category) category)
     ((symbolp category) (symbol-name category))
     (t nil))))

(defun ellama-tools--tool-scan-metadata (tool-plist)
  "Return DLP scan metadata plist for TOOL-PLIST."
  (let* ((tool-name (plist-get tool-plist :name))
         (category (ellama-tools--tool-category-name tool-plist)))
    (if (and (stringp category)
             (string-prefix-p "mcp-" category))
        (list :tool-origin 'mcp
              :server-id category
              :tool-identity (format "%s/%s" category tool-name))
      (list :tool-origin 'builtin
            :tool-identity tool-name))))

(defun ellama-tools--tool-call-values (async call-args)
  "Return positional CALL-ARGS, excluding callback when ASYNC."
  (if (and async call-args (functionp (car call-args)))
      (cdr call-args)
    call-args))

(defun ellama-tools--output-source-info (kind path)
  "Return output source info plist for KIND and PATH."
  (when (and (stringp path) (> (length path) 0))
    (list :kind kind
          :path (expand-file-name path))))

(defun ellama-tools--canonical-file-name-for-dlp (file-name)
  "Return canonical FILE-NAME for DLP matching, or nil."
  (when (and (stringp file-name) (not (string-empty-p file-name)))
    (let ((expanded (expand-file-name file-name)))
      (condition-case nil
          (file-truename expanded)
        (file-error expanded)))))

(defun ellama-tools--dlp-safe-read-file-p (file-name)
  "Return non-nil when FILE-NAME is safe for read-output DLP scans."
  (when-let* ((path (ellama-tools--canonical-file-name-for-dlp file-name)))
    (seq-some
     (lambda (regexp)
       (and (stringp regexp)
            (string-match-p regexp path)))
     ellama-tools-dlp-safe-read-file-regexps)))

(defun ellama-tools--tool-output-source-info (tool-name values)
  "Return source info plist for TOOL-NAME using VALUES."
  (pcase tool-name
    ((or "read_file" "lines_range" "count_lines")
     (ellama-tools--output-source-info 'file (car values)))
    ("grep_in_file"
     (ellama-tools--output-source-info 'file (nth 1 values)))
    ((or "grep" "directory_tree")
     (ellama-tools--output-source-info 'directory (car values)))
    (_
     nil)))

(defun ellama-tools--tool-output-context (tool-plist call-args)
  "Build output context plist from TOOL-PLIST and CALL-ARGS."
  (let* ((tool-name (plist-get tool-plist :name))
         (async (plist-get tool-plist :async))
         (values (ellama-tools--tool-call-values async call-args))
         (source-info (ellama-tools--tool-output-source-info
                       tool-name values)))
    (list :source-info source-info
          :skip-dlp-output (and (eq (plist-get source-info :kind) 'file)
                                (ellama-tools--dlp-safe-read-file-p
                                 (plist-get source-info :path))))))

(defun ellama-tools--parse-json-string-value (text)
  "Return decoded JSON string from TEXT, or nil when decode fails."
  (condition-case nil
      (let ((decoded (json-parse-string
                      text
                      :object-type 'alist
                      :array-type 'list
                      :null-object nil
                      :false-object nil)))
        (when (stringp decoded)
          decoded))
    (error nil)))

(defun ellama-tools--line-budget-truncate
    (text max-lines max-line-length)
  "Return line-budget truncation plist for TEXT.
MAX-LINES limits how many lines are kept.
MAX-LINE-LENGTH limits one line width in characters."
  (let* ((lines (split-string text "\n" nil))
         (total-lines (length lines))
         (kept 0)
         (long-lines 0)
         (truncated-lines nil)
         (kept-lines nil))
    (dolist (line lines)
      (when (< kept max-lines)
        (if (> (length line) max-line-length)
            (let* ((suffix " ...[line truncated]")
                   (available (- max-line-length (length suffix)))
                   (prefix-len (if (> available 0) available 0))
                   (prefix (if (> prefix-len 0)
                               (substring line 0
                                          (min (length line) prefix-len))
                             ""))
                   (line*
                    (if (> max-line-length (length suffix))
                        (concat prefix suffix)
                      (substring suffix 0 max-line-length))))
              (push line* kept-lines)
              (setq long-lines (1+ long-lines))
              (setq truncated-lines t))
          (push line kept-lines))
        (setq kept (1+ kept))))
    (let ((dropped (max 0 (- total-lines kept))))
      (list :text (string-join (nreverse kept-lines) "\n")
            :truncated (or (> dropped 0) truncated-lines)
            :total-lines total-lines
            :dropped-lines dropped
            :long-lines long-lines))))

(defun ellama-tools--save-overflow-output-file (tool-name text)
  "Save overflowing TEXT for TOOL-NAME to a temp file and return its path."
  (let* ((safe-tool (replace-regexp-in-string
                     "[^[:alnum:]_-]" "-"
                     (or tool-name "tool")))
         (path (make-temp-file
                (format "ellama-%s-output-" safe-tool)
                nil
                ".txt")))
    (condition-case nil
        (progn
          (write-region text nil path nil 'silent)
          path)
      (error nil))))

(defun ellama-tools--output-truncation-next-range (truncation)
  "Return next line range plist for TRUNCATION."
  (let* ((total-lines (plist-get truncation :total-lines))
         (max-lines (max 0 ellama-tools-output-line-budget-max-lines))
         (window (max 1 max-lines))
         (from (if (> max-lines 0) (1+ max-lines) 1))
         (to (min total-lines (+ from window -1))))
    (when (<= from total-lines)
      (list :from from :to to))))

(defun ellama-tools--output-truncation-range-hint (path truncation)
  "Return concrete `lines_range' hint for PATH and TRUNCATION."
  (when-let* ((range (ellama-tools--output-truncation-next-range truncation)))
    (format
     "Next suggested tool call: `lines_range` with file_name=%S, from=%d, to=%d.\n"
     path
     (plist-get range :from)
     (plist-get range :to))))

(defun ellama-tools--output-truncation-notice
    (tool-name truncation source-info saved-path)
  "Return tool output truncation notice string.
TOOL-NAME identifies the tool.
TRUNCATION is a line-budget result plist.
SOURCE-INFO describes source path and kind when known.
SAVED-PATH is optional path to full saved output."
  (let* ((total-lines (plist-get truncation :total-lines))
         (dropped-lines (plist-get truncation :dropped-lines))
         (long-lines (plist-get truncation :long-lines))
         (snippet (plist-get truncation :text))
         (source-kind (plist-get source-info :kind))
         (source-path (plist-get source-info :path)))
    (concat
     "[ELLAMA OUTPUT TRUNCATED]\n"
     (format "Tool `%s` output exceeded line budget and was truncated.\n"
             tool-name)
     (format "Original lines: %d.  Kept up to %d lines.\n"
             total-lines
             (max 0 ellama-tools-output-line-budget-max-lines))
     (when (> dropped-lines 0)
       (format "Dropped lines: %d.\n" dropped-lines))
     (when (> long-lines 0)
       (format
        (concat "Truncated long lines: %d (max %d chars per line).\n")
        long-lines
        (max 1 ellama-tools-output-line-budget-max-line-length)))
     (cond
      ((and source-path (eq source-kind 'file))
       (format
        (concat
         "Source file: %s\n"
         "%s"
         "Use `grep_in_file` with file=%S and a specific search_string "
         "to locate relevant lines.\n")
        source-path
        (or (ellama-tools--output-truncation-range-hint
             source-path truncation)
            "")
        source-path))
      ((and source-path (eq source-kind 'directory))
       (format
        (concat
         "Source directory: %s\n"
         "Next suggested tool call: `grep` with dir=%S and a specific "
         "search_string, then `read_file` or `lines_range` on the target file.\n")
        source-path source-path))
      (saved-path
       (format
        (concat
         "Full output saved to: %s\n"
         "%s"
         "Use `grep_in_file` with file=%S and a specific search_string "
         "to search the saved output.\n")
        saved-path
        (or (ellama-tools--output-truncation-range-hint
             saved-path truncation)
            "")
        saved-path))
      (t
       (concat
        "Full output file was not saved.\n"
        "Next suggested action: rerun the tool with narrower arguments "
        "or use `grep` with a specific search_string.\n")))
     "\n--- BEGIN TRUNCATED OUTPUT ---\n"
     snippet
     "\n--- END TRUNCATED OUTPUT ---")))

(defun ellama-tools--apply-output-line-budget
    (tool-name text &optional output-context)
  "Apply per-output line budget for TOOL-NAME TEXT.
OUTPUT-CONTEXT may include source metadata under `:source-info'."
  (if (or (not ellama-tools-output-line-budget-enabled)
          (not (stringp text)))
      text
    (let* ((max-lines (max 0 ellama-tools-output-line-budget-max-lines))
           (max-line-length
            (max 1 ellama-tools-output-line-budget-max-line-length))
           (source-info (plist-get output-context :source-info))
           (text* (or (ellama-tools--parse-json-string-value text) text))
           (truncation (ellama-tools--line-budget-truncate
                        text* max-lines max-line-length)))
      (if (not (plist-get truncation :truncated))
          text
        (let* ((save-p (and ellama-tools-output-line-budget-save-overflow-file
                            (null source-info)))
               (saved-path (and save-p
                                (ellama-tools--save-overflow-output-file
                                 tool-name text*))))
          (when (fboundp 'ellama-tools-dlp--record-incident)
            (ellama-tools-dlp--record-incident
             (list :type 'output-budget-truncation
                   :tool-name tool-name
                   :action 'truncate
                   :line-count (plist-get truncation :total-lines)
                   :dropped-lines (plist-get truncation :dropped-lines)
                   :long-lines (plist-get truncation :long-lines)
                   :saved-path saved-path)))
          (ellama-tools--output-truncation-notice
           tool-name truncation source-info saved-path))))))

(defun ellama-tools--dlp-handle-output-string
    (tool-name text &optional tool-metadata)
  "Scan output TEXT for TOOL-NAME and return filtered result.
TOOL-METADATA may provide identity fields for scan context."
  (let* ((scan (ellama-tools-dlp--scan-text
                text
                (ellama-tools--dlp-make-scan-context
                 'output tool-name nil tool-metadata)))
         (verdict (plist-get scan :verdict))
         (findings (plist-get scan :findings))
         (action (plist-get verdict :action)))
    (pcase action
      ('allow
       text)
      ((or 'warn 'warn-strong)
       (pcase ellama-tools-dlp-output-warn-behavior
         ('allow
          text)
         ('block
          (or (plist-get verdict :message)
              (format "DLP blocked output for tool %s" tool-name)))
         (_
          (pcase (ellama-tools--dlp-output-warn-choice
                  tool-name (plist-get verdict :message) text findings)
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

(defun ellama-tools--skip-output-dlp-p (output-context)
  "Return non-nil when OUTPUT-CONTEXT should disable output DLP."
  (plist-get output-context :skip-dlp-output))

(defun ellama-tools--postprocess-output-string
    (tool-name text &optional output-context tool-metadata)
  "Apply output guard and DLP filtering for TOOL-NAME TEXT.
Use OUTPUT-CONTEXT to control budget notices and overflow metadata.
TOOL-METADATA may provide tool identity details for DLP scans."
  (let ((dlp-filtered (if (and ellama-tools-dlp-enabled
                               (not (ellama-tools--skip-output-dlp-p
                                     output-context)))
                          (ellama-tools--dlp-handle-output-string
                           tool-name text tool-metadata)
                        text)))
    (ellama-tools--apply-output-line-budget
     tool-name dlp-filtered output-context)))

(defun ellama-tools--make-output-section
    (kind text &optional scan-output output-context)
  "Return output section plist with KIND and TEXT.
SCAN-OUTPUT controls whether DLP scan is applied to the section.
OUTPUT-CONTEXT controls line-budget source notices."
  (list :kind kind
        :text text
        :scan-output scan-output
        :output-context output-context))

(defun ellama-tools--sectioned-output (sections)
  "Return a string containing independently processed SECTIONS.
SECTIONS are stored as a text property for the output wrapper."
  (let* ((texts (delq nil
                      (mapcar (lambda (section)
                                (let ((text (plist-get section :text)))
                                  (and (stringp text)
                                       (not (string-empty-p text))
                                       text)))
                              sections)))
         (text (string-join texts "\n\n")))
    (when (> (length text) 0)
      (put-text-property
       0 (length text) ellama-tools--output-sections-property sections text))
    text))

(defun ellama-tools--output-sections (text)
  "Return output sections stored on TEXT, or nil."
  (and (stringp text)
       (> (length text) 0)
       (get-text-property 0 ellama-tools--output-sections-property text)))

(defun ellama-tools--postprocess-output-section
    (tool-name section &optional tool-metadata)
  "Apply output guards to SECTION for TOOL-NAME.
TOOL-METADATA may provide tool identity details for DLP scans."
  (let* ((kind (or (plist-get section :kind) 'section))
         (text (or (plist-get section :text) ""))
         (scan-output (plist-get section :scan-output))
         (output-context (plist-get section :output-context))
         (section-tool-name (format "%s/%s" tool-name kind))
         (dlp-filtered
          (if (and scan-output
                   ellama-tools-dlp-enabled
                   (not (ellama-tools--skip-output-dlp-p output-context)))
              (ellama-tools--dlp-handle-output-string
               section-tool-name text tool-metadata)
            text)))
    (ellama-tools--apply-output-line-budget
     section-tool-name dlp-filtered output-context)))

(defun ellama-tools--postprocess-output-sections
    (tool-name sections &optional tool-metadata)
  "Apply output guards to SECTIONS for TOOL-NAME independently."
  (string-join
   (mapcar (lambda (section)
             (ellama-tools--postprocess-output-section
              tool-name section tool-metadata))
           sections)
   "\n\n"))

(defun ellama-tools--postprocess-output-result
    (tool-name result &optional output-context tool-metadata)
  "Apply output guards to RESULT for TOOL-NAME.
Sectioned strings are processed section by section.  Plain strings keep the
historical single-budget behavior."
  (if-let* ((sections (ellama-tools--output-sections result)))
      (ellama-tools--postprocess-output-sections
       tool-name sections tool-metadata)
    (ellama-tools--postprocess-output-string
     tool-name result output-context tool-metadata)))

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

(defun ellama-tools--dlp-scan-input-value
    (tool-name arg-name value &optional tool-metadata)
  "Scan VALUE for TOOL-NAME under ARG-NAME and return a decision plist."
  (let (warn-message warn-strong-message)
    (catch 'done
      (ellama-tools--dlp-walk-strings
       value arg-name
       (lambda (text path)
         (let* ((scan (ellama-tools-dlp--scan-text
                       text
                       (ellama-tools--dlp-make-scan-context
                        'input tool-name path tool-metadata)))
                (verdict (plist-get scan :verdict))
                (action (plist-get verdict :action))
                (message (plist-get verdict :message)))
           (cond
            ((eq action 'block)
             (throw 'done
                    (list :action 'block
                          :message (or message
                                       (format "DLP blocked input for %s"
                                               tool-name))
                          :audit-sink-failure
                          (plist-get verdict :audit-sink-failure))))
            ((and (eq action 'warn-strong)
                  (not warn-strong-message))
             (setq warn-strong-message
                   (or message
                       (format "DLP warned strongly on input for tool %s"
                               tool-name))))
            ((and (eq action 'warn) (not warn-message))
             (setq warn-message
                   (or message
                       (format "DLP warned on input for tool %s"
                               tool-name))))))))
      (cond
       (warn-strong-message
        (list :action 'warn-strong :message warn-strong-message))
       (warn-message
        (list :action 'warn :message warn-message))
       (t
        (list :action 'allow))))))

(defun ellama-tools--dlp-input-decision (tool-plist call-args)
  "Scan TOOL-PLIST CALL-ARGS and return decision plist.
Return plist with keys `:action' and optional `:message'."
  (let* ((tool-name (plist-get tool-plist :name))
         (async (plist-get tool-plist :async))
         (declared-args (plist-get tool-plist :args))
         (tool-metadata (ellama-tools--tool-scan-metadata tool-plist))
         (values (if (and async call-args (functionp (car call-args)))
                     (cdr call-args)
                   call-args))
         (specs declared-args)
         (index 0)
         warn-message
         warn-strong-message)
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
                  (ellama-tools--dlp-scan-input-value
                   tool-name arg-name value tool-metadata))
                 (action (plist-get arg-decision :action))
                 (message (plist-get arg-decision :message)))
            (cond
             ((eq action 'block)
              (throw 'done arg-decision))
             ((and (eq action 'warn-strong) (not warn-strong-message))
              (setq warn-strong-message message))
             ((and (eq action 'warn) (not warn-message))
              (setq warn-message message)))))
        (setq values (cdr values))
        (setq specs (cdr specs))
        (setq index (1+ index)))
      (if warn-strong-message
          (list :action 'warn-strong :message warn-strong-message)
        (if warn-message
            (list :action 'warn :message warn-message)
          (list :action 'allow))))))

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

(defun ellama-tools--dlp-blocked-noninteractive-message (tool-name message)
  "Return noninteractive block message for TOOL-NAME with MESSAGE."
  (format
   (concat
    "%s. Interactive typed confirmation is required for irreversible "
    "actions on tool %s")
   (or message "DLP blocked irreversible input")
   tool-name))

(defun ellama-tools--dlp-confirm-warn-strong (tool-name message)
  "Ask typed confirmation for irreversible warning on TOOL-NAME.
MESSAGE is a user-facing warning text."
  (if (not ellama-tools-irreversible-require-typed-confirm)
      (ellama-tools--dlp-confirm-warn tool-name message "irreversible action")
    (let ((typed (read-string
                  (format
                   (concat
                    "%s. Type \"%s\" to proceed with irreversible action "
                    "for tool %s: ")
                   (or message "DLP warn-strong input")
                   ellama-tools-irreversible-typed-confirm-phrase
                   tool-name))))
      (string= typed ellama-tools-irreversible-typed-confirm-phrase))))

(defun ellama-tools--dlp-confirm-audit-sink-failure (tool-name message)
  "Ask explicit confirmation for audit sink failure on TOOL-NAME with MESSAGE."
  (eq (read-char-choice
       (format
        (concat
         "%s. Audit sink write failed for irreversible action on tool %s. "
         "Proceed without durable audit logging? (y/n): ")
        (or message "DLP blocked irreversible input")
        tool-name)
       '(?y ?n))
      ?y))

(defun ellama-tools--dlp-highlight-findings (start text findings)
  "Highlight FINDINGS in TEXT inserted at START."
  (let ((text-length (length text)))
    (dolist (finding findings)
      (let ((span-start (plist-get finding :match-start))
            (span-end (plist-get finding :match-end)))
        (when (and (integerp span-start)
                   (integerp span-end)
                   (<= 0 span-start)
                   (<= span-start span-end)
                   (<= span-end text-length))
          (add-face-text-property (+ start span-start)
                                  (+ start span-end)
                                  'match
                                  t))))))

(defun ellama-tools--dlp-view-output-warning
    (tool-name message text findings)
  "Display output DLP warning details for TOOL-NAME with MESSAGE.
Render TEXT and highlight FINDINGS spans when available."
  (let ((buf (get-buffer-create "*Ellama DLP Warning*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Ellama DLP Output Warning\n"
                            'face '(:weight bold :height 1.2)))
        (insert "\n")
        (insert (format "Tool: %s\n" tool-name))
        (insert (format "Warning: %s\n\n" (or message "DLP warning")))
        (insert "Output:\n")
        (if (stringp text)
            (let ((text-start (point)))
              (insert text)
              (insert "\n")
              (ellama-tools--dlp-highlight-findings
               text-start text findings))
          (insert "  Output content is unavailable.\n")))
      (goto-char (point-min))
      (view-mode 1))
    (display-buffer buf)))

(defun ellama-tools--dlp-output-warn-choice
    (tool-name message &optional text findings)
  "Return output warn choice for TOOL-NAME with MESSAGE.
When TEXT and FINDINGS are available, allow viewing highlighted output.
Return one of symbols `allow', `redact' or `block'."
  (save-window-excursion
    (catch 'done
      (while t
        (pcase (read-char-choice
                (format
                 "%s. Output from tool %s: (a)llow, (r)edact, (b)lock, (v)iew: "
                 (or message "DLP warning")
                 tool-name)
                '(?a ?r ?b ?y ?n ?v))
          ((or ?a ?y)
           (throw 'done 'allow))
          (?r
           (throw 'done 'redact))
          (?v
           (ellama-tools--dlp-view-output-warning
            tool-name message text findings))
          (_
           (throw 'done 'block)))))))

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

(defun ellama-tools--dlp-wrap-output-callback
    (tool-name callback &optional output-context tool-metadata)
  "Wrap CALLBACK to apply output filtering for TOOL-NAME."
  (lambda (result)
    (funcall callback
             (if (stringp result)
                 (ellama-tools--postprocess-output-result
                  tool-name result output-context tool-metadata)
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
        (async (plist-get tool-plist :async))
        (tool-metadata (ellama-tools--tool-scan-metadata tool-plist)))
    (lambda (&rest args)
      (let* ((output-context
              (ellama-tools--tool-output-context tool-plist args))
             (wrapped-args
              (if (and async args (functionp (car args)))
                  (cons (ellama-tools--dlp-wrap-output-callback
                         tool-name (car args) output-context tool-metadata)
                        (cdr args))
                args)))
        (if (not ellama-tools-dlp-enabled)
            (let ((result (apply func wrapped-args)))
              (if (and (not async) (stringp result))
                  (ellama-tools--postprocess-output-result
                   tool-name result output-context tool-metadata)
                result))
          (let* ((decision (ellama-tools--dlp-input-decision
                            tool-plist args))
                 (action (plist-get decision :action))
                 (message (plist-get decision :message))
                 (audit-sink-failure
                  (plist-get decision :audit-sink-failure)))
            (pcase action
              ('block
               (if audit-sink-failure
                   (if noninteractive
                       (ellama-tools--dlp-return-message
                        async args
                        (or message
                            (format "DLP blocked input for tool %s"
                                    tool-name)))
                     (if (not (ellama-tools--dlp-confirm-audit-sink-failure
                               tool-name message))
                         (ellama-tools--dlp-return-message
                          async args
                          (format "DLP blocked input for tool %s"
                                  tool-name))
                       (let ((result (apply func wrapped-args)))
                         (if (and (not async) (stringp result))
                             (ellama-tools--postprocess-output-result
                              tool-name result output-context tool-metadata)
                           result))))
                 (ellama-tools--dlp-return-message
                  async args
                  (or message
                      (format "DLP blocked input for tool %s" tool-name)))))
              ('warn
               (if (not (ellama-tools--dlp-confirm-warn tool-name message))
                   (ellama-tools--dlp-return-message
                    async args
                    (format "DLP warning denied tool execution for %s"
                            tool-name))
                 (let ((result (apply func wrapped-args)))
                   (if (and (not async) (stringp result))
                       (ellama-tools--postprocess-output-result
                        tool-name result output-context tool-metadata)
                     result))))
              ('warn-strong
               (if noninteractive
                   (ellama-tools--dlp-return-message
                    async args
                    (ellama-tools--dlp-blocked-noninteractive-message
                     tool-name message))
                 (if (not (ellama-tools--dlp-confirm-warn-strong
                           tool-name message))
                     (ellama-tools--dlp-return-message
                      async args
                      (format "DLP warning denied tool execution for %s"
                              tool-name))
                   (let ((result (apply func wrapped-args)))
                     (if (and (not async) (stringp result))
                         (ellama-tools--postprocess-output-result
                          tool-name result output-context tool-metadata)
                       result)))))
              (_
               (let ((result (apply func wrapped-args)))
                 (if (and (not async) (stringp result))
                     (ellama-tools--postprocess-output-result
                      tool-name result output-context tool-metadata)
                   result))))))))))

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

(defun ellama-tools--shell-quote-command (program args)
  "Return shell command string for PROGRAM with ARGS."
  (mapconcat #'shell-quote-argument (cons program args) " "))

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
      (append
       (list srt-path)
       ellama-tools-srt-args
       (list "-c" (ellama-tools--shell-quote-command program args))))))

(defun ellama-tools--process-environment-with-cat-pager (&optional env)
  "Return ENV with shell command pagers forced to cat."
  (append
   '("PAGER=cat" "GIT_PAGER=cat")
   (seq-remove
    (lambda (entry)
      (or (string-prefix-p "PAGER=" entry)
          (string-prefix-p "GIT_PAGER=" entry)))
    (copy-sequence (or env process-environment)))))

(defun ellama-tools--call-command-to-string (program &rest args)
  "Run PROGRAM with ARGS and return stdout as a string."
  (cdr (apply #'ellama-tools--call-command program args)))

(defun ellama-tools--call-command (program &rest args)
  "Run PROGRAM with ARGS and return cons of exit status and stdout."
  (let ((argv (apply #'ellama-tools--command-argv program args))
        (process-environment
         (ellama-tools--process-environment-with-cat-pager)))
    (with-temp-buffer
      (let ((status (apply #'call-process (car argv) nil t nil (cdr argv))))
        (cons status (buffer-string))))))

(defun ellama-tools--call-command-with-timeout (timeout program &rest args)
  "Run PROGRAM with ARGS and return cons of exit status and stdout.
Terminate the process and return `timeout' as status after TIMEOUT seconds."
  (let ((argv (apply #'ellama-tools--command-argv program args))
        (process-environment
         (ellama-tools--process-environment-with-cat-pager)))
    (with-temp-buffer
      (let* ((process (apply #'start-process
                             "*ellama-command*" (current-buffer)
                             (car argv) (cdr argv)))
             (deadline (+ (float-time) timeout))
             timed-out)
        (set-process-sentinel process (lambda (_process _event) nil))
        (while (and (process-live-p process)
                    (< (float-time) deadline))
          (accept-process-output
           process
           (max 0.0 (min 0.05 (- deadline (float-time))))))
        (when (process-live-p process)
          (setq timed-out t)
          (delete-process process))
        (cons (if timed-out
                  'timeout
                (process-exit-status process))
              (buffer-string))))))

(defun ellama-tools--command-failure-output (program status output)
  "Return diagnostic text for PROGRAM failure with STATUS and OUTPUT."
  (let ((trimmed (string-trim-right output "\n")))
    (if (string-empty-p trimmed)
        (format "%s failed with exit status %s." program status)
      (format "%s failed with exit status %s:\n%s"
              program status trimmed))))

(defun ellama-tools--command-timeout-output (program timeout output)
  "Return diagnostic text for PROGRAM timeout after TIMEOUT with OUTPUT."
  (let ((trimmed (string-trim-right output "\n")))
    (if (string-empty-p trimmed)
        (format "%s timed out after %s seconds." program timeout)
      (format "%s timed out after %s seconds:\n%s"
              program timeout trimmed))))

(defun ellama-tools--shell-command-timeout (timeout)
  "Return shell command TIMEOUT or the configured default."
  (if (and (numberp timeout) (> timeout 0))
      timeout
    ellama-tools-shell-command-default-timeout))

(defun ellama-tools--grep-output (result no-matches-message &optional timeout)
  "Return grep RESULT output or NO-MATCHES-MESSAGE.
TIMEOUT is the timeout in seconds used when RESULT reports a timeout."
  (let ((status (car result))
        (output (string-trim-right (cdr result) "\n")))
    (cond
     ((and (integerp status) (zerop status)) output)
     ((eq status 'timeout)
      (ellama-tools--command-timeout-output "grep" timeout output))
     ((and (integerp status)
           (= status 1)
           (string-empty-p output))
      no-matches-message)
     (t (ellama-tools--command-failure-output "grep" status output)))))

(defun ellama-tools--read-file-mode (mode)
  "Return normalized read file MODE."
  (let ((mode (or mode ellama-tools-read-file-default-mode)))
    (cond
     ((symbolp mode) mode)
     ((stringp mode) (intern (downcase mode)))
     (t 'auto))))

(defun ellama-tools--binary-file-p (file-name)
  "Return non-nil when FILE-NAME appears to be binary."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file-name nil 0 4096)
    (goto-char (point-min))
    (search-forward (string 0) nil t)))

(defun ellama-tools--read-file-as-text (file-name)
  "Read FILE-NAME as text for tool output."
  (if (ellama-tools--binary-file-p file-name)
      (format "File %s appears to be binary; text reading was not performed."
              file-name)
    (let ((content (with-temp-buffer
                     (insert-file-contents file-name)
                     (buffer-string))))
      (ellama-tools--sanitize-tool-text-output
       content
       (format "File %s" file-name)))))

(defun ellama-tools--read-image-file-tool (file-name)
  "Queue image FILE-NAME for model input and return textual tool output."
  (cond
   ((string= (downcase (or (file-name-extension file-name) "")) "svg")
    (format "SVG image input is not supported yet: %s" file-name))
   ((not (ellama-image-file-p file-name))
    (format "File %s is not a supported image file." file-name))
   ((not (ellama-session-p
          (or ellama--current-session ellama-tools--current-session)))
    "Cannot attach image file: no active Ellama session.")
   ((not (ellama--provider-supports-image-input-p
          (ellama-session-provider
           (or ellama--current-session ellama-tools--current-session))))
    (format "Provider %s does not support image input."
            (llm-name
             (ellama-session-provider
              (or ellama--current-session ellama-tools--current-session)))))
   (t
    (ellama--session-add-pending-tool-media
     (or ellama--current-session ellama-tools--current-session)
     file-name)
    (let* ((expanded (expand-file-name file-name))
           (link (if (provided-mode-derived-p major-mode 'org-mode)
                     (format "[[file:%s][%s]]" expanded file-name)
                   (format "[%s](file://%s)" file-name expanded))))
      (format "Image file queued for model input: %s (%s, %d bytes)."
              link
              (ellama--image-mime-type file-name)
              (ellama--file-size file-name))))))

(defun ellama-tools-read-file-tool (file-name &optional mode)
  "Read the file FILE-NAME.
MODE can be `auto', `text' or `image'."
  (or (ellama-tools--tool-check-file-access file-name 'read)
      (let ((result
             (cond
              ((not (file-exists-p file-name))
               (format "File %s does not exist." file-name))
              ((file-directory-p file-name)
               (format "%s is a directory, not a file." file-name))
              (t
               (pcase (ellama-tools--read-file-mode mode)
                 ('auto
                  (prog1
                      (if (ellama-image-file-p file-name)
                          (ellama-tools--read-image-file-tool file-name)
                        (ellama-tools--read-file-as-text file-name))
                    (ellama-tools--mark-file-read file-name)))
                 ('text
                  (prog1
                      (ellama-tools--read-file-as-text file-name)
                    (ellama-tools--mark-file-read file-name)))
                 ('image
                  (prog1
                      (ellama-tools--read-image-file-tool file-name)
                    (ellama-tools--mark-file-read file-name)))
                 (_
                  (format
                   "Unsupported read_file mode %S. Use auto, text or image."
                   mode)))))))
        (json-encode result))))

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
     "File name.")
    (:name
     "mode"
     :type
     string
     :optional
     t
     :enum
     ["auto" "text" "image"]
     :description
     "Read mode: auto, text, or image."))
   :description
   "Read the file FILE_NAME."))

(defun ellama-tools--file-parent-directory-error-message (operation file-name)
  "Return parent directory error for OPERATION on FILE-NAME."
  (let ((dir (file-name-directory (expand-file-name file-name))))
    (when (and dir
               (not (file-directory-p dir)))
      (format "Cannot %s %s: parent directory does not exist: %s."
              operation
              file-name
              dir))))

(defun ellama-tools--line-count ()
  "Return line count for the current buffer."
  (save-excursion
    (goto-char (point-max))
    (line-number-at-pos)))

(defun ellama-tools--diagnostic-location (pos)
  "Return location plist for POS in the current buffer."
  (save-excursion
    (goto-char (max (point-min) (min pos (point-max))))
    (list :pos (point)
          :line (line-number-at-pos)
          :column (current-column))))

(defun ellama-tools--syntax-diagnostic (check err)
  "Return diagnostic for CHECK and ERR at point."
  (append
   (list :check check
         :error (error-message-string err))
   (ellama-tools--diagnostic-location (point))))

(defun ellama-tools--remove-unexpected-closers-in-buffer ()
  "Remove unexpected closing delimiters from the current buffer.
Returns the number of characters removed, or nil if none found."
  (when (fboundp 'syntax-propertize)
    (syntax-propertize (point-max)))
  (let ((unexpected-pos))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((pos (point))
               (syntax (syntax-after pos))
               (class (and syntax (syntax-class syntax)))
               (state (syntax-ppss pos)))
          (when (and (eq class 5)
                     (not (nth 3 state))
                     (not (nth 4 state)))
            (unless (nth 1 state)
              (push pos unexpected-pos))))
        (forward-char 1)))
    (let ((count (length unexpected-pos)))
      (when (and count (cl-plusp count))
        (dolist (pos (sort unexpected-pos #'<))
          (goto-char pos)
          (delete-char 1))
        count))))

(defun ellama-tools--try-fix-unexpected-closers (text file-name)
  "Try to fix TEXT by removing unexpected closers in FILE-NAME context.
Returns a plist with :text (fixed or original), :auto-fixed (t if fixed),
and :validation (re-validation result of returned text)."
  (let ((buffer (find-file-noselect file-name)))
    (with-current-buffer buffer
      (let ((original (buffer-string))
            (modified (buffer-modified-p))
            (point-pos (point))
            (buffer-undo-list t)
            (inhibit-modification-hooks t)
            (inhibit-read-only t))
        (unwind-protect
            (progn
              (erase-buffer)
              (insert text)
              (let ((removed (ellama-tools--remove-unexpected-closers-in-buffer)))
                (if (and removed (cl-plusp removed))
                    (let* ((fixed-text (buffer-string))
                           (re-validation (ellama-tools--syntax-validation-current-buffer)))
                      (if (plist-get re-validation :valid)
                          (list :text fixed-text
                                :auto-fixed t
                                :validation re-validation)
                        (let ((original-validation (ellama-tools--syntax-validation-current-buffer)))
                          (erase-buffer)
                          (insert text)
                          (list :text text
                                :auto-fixed nil
                                :validation original-validation))))
                  (let ((validation (ellama-tools--syntax-validation-current-buffer)))
                    (list :text text
                          :auto-fixed nil
                          :validation validation)))))
          (erase-buffer)
          (insert original)
          (goto-char (max (point-min) (min point-pos (point-max))))
          (set-buffer-modified-p modified))))))

(defun ellama-tools--check-parens-diagnostic ()
  "Return `check-parens' diagnostic for the current buffer."
  (condition-case err
      (let ((inhibit-message t))
        (check-parens)
        nil)
    (error
     (ellama-tools--syntax-diagnostic "check-parens" err))))

(defun ellama-tools--elisp-reader-diagnostic ()
  "Return Elisp reader diagnostic for the current buffer."
  (when (derived-mode-p 'emacs-lisp-mode 'lisp-interaction-mode)
    (save-excursion
      (goto-char (point-min))
      (catch 'done
        (while t
          (condition-case err
              (read (current-buffer))
            (end-of-file
             (throw 'done nil))
            (error
             (throw
              'done
              (ellama-tools--syntax-diagnostic "elisp-reader" err)))))))))

(defun ellama-tools--char-location (char pos)
  "Return plist for delimiter CHAR at POS."
  (append (list :char char)
          (ellama-tools--diagnostic-location pos)))

(defun ellama-tools--delimiter-balance-current-buffer ()
  "Return delimiter balance for the current buffer syntax table."
  (let (opens unexpected)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((pos (point))
               (char (char-after))
               (syntax (syntax-after pos))
               (class (and syntax (syntax-class syntax)))
               (state (syntax-ppss pos)))
          (unless (or (nth 3 state)
                      (nth 4 state))
            (pcase class
              (4
               (push (ellama-tools--char-location char pos) opens))
              (5
               (let ((expected (matching-paren char)))
                 (if (and opens
                          expected
                          (= expected (plist-get (car opens) :char)))
                     (pop opens)
                   (push (ellama-tools--char-location char pos)
                         unexpected)))))))
        (forward-char 1)))
    (list :opens opens :unexpected (nreverse unexpected))))

(defun ellama-tools--format-delimiter-location (entry)
  "Return formatted delimiter balance ENTRY."
  (format "%c at line %d, column %d"
          (plist-get entry :char)
          (plist-get entry :line)
          (plist-get entry :column)))

(defun ellama-tools--format-missing-closer (entry)
  "Return formatted missing closer description for ENTRY."
  (let* ((open (plist-get entry :char))
         (close (matching-paren open)))
    (format "%s for %s"
            (if close (format "%c" close) "matching closer")
            (ellama-tools--format-delimiter-location entry))))

(defun ellama-tools--format-delimiter-balance (balance)
  "Return human-readable delimiter BALANCE."
  (let ((opens (plist-get balance :opens))
        (unexpected (plist-get balance :unexpected)))
    (concat
     "Delimiter balance:\n"
     (if opens
         (concat
          "- Missing closers: "
          (string-join
           (mapcar #'ellama-tools--format-missing-closer
                   (seq-take opens 5))
           "; ")
          "\n")
       "- Missing closers: none\n")
     (if unexpected
         (concat
          "- Unexpected closers: "
          (string-join
           (mapcar #'ellama-tools--format-delimiter-location
                   (seq-take unexpected 5))
           "; ")
          "\n")
       "- Unexpected closers: none\n"))))

(defun ellama-tools--nearby-text (pos)
  "Return nearby text around POS in the current buffer."
  (save-excursion
    (let* ((safe-pos (max (point-min) (min pos (point-max))))
           (line (progn
                   (goto-char safe-pos)
                   (line-number-at-pos)))
           (column (current-column))
           (last-line (ellama-tools--line-count))
           (from (max 1 (- line 2)))
           (to (min last-line (+ line 2)))
           (width (length (number-to-string to)))
           rows)
      (dotimes (offset (1+ (- to from)))
        (let ((current (+ from offset)))
          (goto-char (point-min))
          (forward-line (1- current))
          (push
           (format (format "%%%dd | %%s" width)
                   current
                   (buffer-substring-no-properties
                    (line-beginning-position)
                    (line-end-position)))
           rows)
          (when (= current line)
            (push
             (format "%s | %s^"
                     (make-string width ? )
                     (make-string column ? ))
             rows))))
      (concat "Nearby text:\n"
              (string-join (nreverse rows) "\n")))))

(defun ellama-tools--balanced-edit-current-mode-p ()
  "Return non-nil when the current mode should be syntax checked."
  (and ellama-tools-balanced-edit-enabled
       (cl-some (lambda (mode) (derived-mode-p mode))
                ellama-tools-balanced-edit-modes)))

(defun ellama-tools--syntax-validation-current-buffer ()
  "Return syntax validation result for the current buffer."
  (if (not (ellama-tools--balanced-edit-current-mode-p))
      (list :valid t :checked nil :mode major-mode)
    (when (fboundp 'syntax-propertize)
      (syntax-propertize (point-max)))
    (if-let* ((diagnostic (or (ellama-tools--check-parens-diagnostic)
                              (ellama-tools--elisp-reader-diagnostic))))
        (append (list :valid nil
                      :checked t
                      :mode major-mode
                      :balance
                      (ellama-tools--delimiter-balance-current-buffer)
                      :nearby
                      (ellama-tools--nearby-text
                       (plist-get diagnostic :pos)))
                diagnostic)
      (list :valid t :checked t :mode major-mode))))

(defun ellama-tools--validate-text-in-file-buffer (file-name text)
  "Validate TEXT using the existing Emacs buffer setup for FILE-NAME."
  (let ((buffer (find-file-noselect file-name)))
    (with-current-buffer buffer
      (let ((original (buffer-string))
            (modified (buffer-modified-p))
            (point-pos (point))
            (buffer-undo-list t)
            (inhibit-modification-hooks t)
            (inhibit-read-only t))
        (unwind-protect
            (progn
              (erase-buffer)
              (insert text)
              (ellama-tools--syntax-validation-current-buffer))
          (erase-buffer)
          (insert original)
          (goto-char (max (point-min) (min point-pos (point-max))))
          (set-buffer-modified-p modified))))))

(defun ellama-tools--validation-status (validation)
  "Return short status string for VALIDATION."
  (cond
   ((null validation)
    "not available")
   ((not (plist-get validation :checked))
    (format "not checked in %s" (plist-get validation :mode)))
   ((plist-get validation :valid)
    "balanced")
   (t
    (format "unbalanced (%s at line %d, column %d: %s)"
            (plist-get validation :check)
            (plist-get validation :line)
            (plist-get validation :column)
            (plist-get validation :error)))))

(defun ellama-tools--format-balanced-edit-rejection
    (operation file-name candidate-validation original-validation
               old-validation new-validation)
  "Return OPERATION rejection for FILE-NAME and VALIDATION results.
CANDIDATE-VALIDATION, ORIGINAL-VALIDATION, OLD-VALIDATION and
NEW-VALIDATION are syntax validation results."
  (let* ((balance (plist-get candidate-validation :balance))
         (missing-closers (plist-get balance :opens))
         (has-missing (and missing-closers (seq-first missing-closers)))
         (instruction
          (if has-missing
              (format
               "You have unclosed delimiters. You need to add %s.
%s"
               (string-join
                (mapcar (lambda (entry)
                          (ellama-tools--format-missing-closer entry))
                        (seq-take missing-closers 5)))
               (format
                (concat
                 "Retry the operation. Keep the requested semantic change, "
                 "but add the missing closers so that delimiters are balanced in %s.")
                (plist-get candidate-validation :mode)))
            (format
             (concat
              "Retry the operation. Keep the requested semantic change, "
              "but provide text whose delimiters are balanced in %s.")
             (plist-get candidate-validation :mode)))))
    (format
     (concat
      "%s rejected: resulting buffer has unbalanced delimiters or "
      "invalid syntax.\n\n"
      "File: %s\n"
      "Mode: %s\n"
      "Check: %s\n"
      "Error: %s\n"
      "Position: line %d, column %d\n\n"
      "%s\n\n"
      "Original file status: %s\n"
      "Old fragment standalone status: %s\n"
      "New fragment standalone status: %s\n\n"
      "%s\n\n"
      "Instruction: %s")
     operation
     file-name
     (plist-get candidate-validation :mode)
     (plist-get candidate-validation :check)
     (plist-get candidate-validation :error)
     (plist-get candidate-validation :line)
     (plist-get candidate-validation :column)
     (plist-get candidate-validation :nearby)
     (ellama-tools--validation-status original-validation)
     (ellama-tools--validation-status old-validation)
     (ellama-tools--validation-status new-validation)
     (ellama-tools--format-delimiter-balance balance)
     instruction)))

(defun ellama-tools--balanced-edit-check-candidate
    (operation file-name candidate &optional original old-fragment new-fragment)
  "Return validation decision for OPERATION writing CANDIDATE to FILE-NAME.
ORIGINAL, OLD-FRAGMENT and NEW-FRAGMENT provide optional diagnostic context.
Decision plist contains :checked, :valid, :rejection, :auto-fixed,
and :fixed-text fields."
  (let* ((candidate-validation
          (ellama-tools--validate-text-in-file-buffer file-name candidate))
         (checked-p (plist-get candidate-validation :checked))
         (valid-p (plist-get candidate-validation :valid))
         (original-validation
          (when original
            (ellama-tools--validate-text-in-file-buffer file-name original)))
         (old-validation
          (when old-fragment
            (ellama-tools--validate-text-in-file-buffer file-name
                                                        old-fragment)))
         (new-validation
          (when new-fragment
            (ellama-tools--validate-text-in-file-buffer file-name
                                                        new-fragment)))
         (fix-result nil)
         (auto-fixed nil)
         (fixed-text nil))
    (when (and checked-p (not valid-p))
      (setq fix-result
            (ellama-tools--try-fix-unexpected-closers candidate file-name))
      (setq auto-fixed (plist-get fix-result :auto-fixed))
      (setq fixed-text (plist-get fix-result :text))
      (setq valid-p (plist-get (plist-get fix-result :validation) :valid))
      (when auto-fixed
        (setq candidate-validation (plist-get fix-result :validation))))
    (list
     :checked checked-p
     :valid valid-p
     :auto-fixed auto-fixed
     :fixed-text (and auto-fixed fixed-text)
     :rejection
     (when (and checked-p (not valid-p))
       (ellama-tools--format-balanced-edit-rejection
        operation file-name candidate-validation
        original-validation old-validation new-validation)))))

(defun ellama-tools--balanced-edit-success-suffix (decision)
  "Return success message suffix for validation DECISION."
  (if (plist-get decision :auto-fixed)
      " after syntax validation (auto-fixed unexpected closers)"
    (if (plist-get decision :checked)
        " after syntax validation"
      "")))

(defun ellama-tools--edit-shell-hooks-buffer (file-name)
  "Return buffer used to read edit shell hooks for FILE-NAME."
  (find-file-noselect file-name))

(defun ellama-tools--edit-shell-hooks-for-file (file-name phase)
  "Return configured edit shell hooks for FILE-NAME and PHASE."
  (with-current-buffer (ellama-tools--edit-shell-hooks-buffer file-name)
    (if (eq phase 'before)
        ellama-tools-edit-before-shell-commands
      ellama-tools-edit-after-shell-commands)))

(defun ellama-tools--edit-project-root (file-name)
  "Return project root used for edit shell hooks on FILE-NAME."
  (let* ((expanded (expand-file-name file-name))
         (dir (or (file-name-directory expanded) default-directory))
         (default-directory (file-name-as-directory dir)))
    (if-let* ((project (project-current nil)))
        (project-root project)
      default-directory)))

(defun ellama-tools--edit-shell-hook-normalize (spec)
  "Return normalized edit shell hook SPEC."
  (cond
   ((stringp spec)
    (list :command spec :show-output nil))
   ((and (plistp spec)
         (stringp (plist-get spec :command)))
    spec)
   (t
    (list :error
          (format "Invalid edit shell hook configuration: %S" spec)))))

(defun ellama-tools--edit-shell-hook-env
    (phase file-name operation tool-name root spec)
  "Return process environment for edit shell hook SPEC.
PHASE is `before' or `after'.  FILE-NAME, OPERATION, TOOL-NAME and ROOT
describe the edit."
  (append
   (list
    (format "ELLAMA_HOOK_PHASE=%s" phase)
    (format "ELLAMA_EDIT_OPERATION=%s" operation)
    (format "ELLAMA_TOOL_NAME=%s" tool-name)
    (format "ELLAMA_FILE_NAME=%s" (expand-file-name file-name))
    (format "ELLAMA_PROJECT_ROOT=%s" root)
    (format "ELLAMA_HOOK_NAME=%s" (or (plist-get spec :name) "")))
   (ellama-tools--process-environment-with-cat-pager)))

(defun ellama-tools--edit-shell-hook-result
    (phase command name status output show-output)
  "Return edit shell hook result plist.
PHASE, COMMAND, NAME, STATUS, OUTPUT and SHOW-OUTPUT describe one hook run."
  (list :phase phase
        :command command
        :name name
        :status status
        :output output
        :show-output show-output))

(defun ellama-tools--run-edit-shell-hook
    (phase file-name operation tool-name spec callback)
  "Run edit shell hook SPEC asynchronously and call CALLBACK with result.
PHASE, FILE-NAME, OPERATION and TOOL-NAME describe the edit."
  (let* ((hook (ellama-tools--edit-shell-hook-normalize spec))
         (command (plist-get hook :command))
         (error-message (plist-get hook :error))
         (name (plist-get hook :name))
         (show-output (plist-get hook :show-output))
         (root (ellama-tools--edit-project-root file-name)))
    (if error-message
        (funcall callback
                 (ellama-tools--edit-shell-hook-result
                  phase nil nil 1 error-message t))
      (let* ((default-directory (file-name-as-directory root))
             (process-environment
              (ellama-tools--edit-shell-hook-env
               phase file-name operation tool-name root hook))
             (argv (ellama-tools--command-argv
                    shell-file-name
                    shell-command-switch
                    (concat "exec 2>&1; " command)))
             (buffer (generate-new-buffer " *ellama-edit-hook*"))
             process)
        (condition-case err
            (progn
              (setq process
                    (make-process
                     :name "ellama-edit-hook"
                     :buffer buffer
                     :command argv
                     :noquery t
                     :sentinel
                     (lambda (proc _event)
                       (when (memq (process-status proc) '(exit signal))
                         (let* ((status (process-exit-status proc))
                                (output
                                 (if (buffer-live-p buffer)
                                     (with-current-buffer buffer
                                       (string-trim-right
                                        (buffer-string) "\n"))
                                   "")))
                           (when (buffer-live-p buffer)
                             (kill-buffer buffer))
                           (funcall
                            callback
                            (ellama-tools--edit-shell-hook-result
                             phase command name status output
                             show-output)))))))
              (set-process-query-on-exit-flag process nil))
          (error
           (when (buffer-live-p buffer)
             (kill-buffer buffer))
           (funcall
            callback
            (ellama-tools--edit-shell-hook-result
             phase command name 1
             (format "Failed to run edit shell hook: %s"
                     (error-message-string err))
             show-output))))))))

(defun ellama-tools--edit-shell-hook-failed-p (result)
  "Return non-nil when edit shell hook RESULT fail."
  (not (equal (plist-get result :status) 0)))

(defun ellama-tools--format-edit-shell-hook-result (result)
  "Return user-facing text for edit shell hook RESULT."
  (let* ((phase (if (eq (plist-get result :phase) 'before)
                    "Before"
                  "After"))
         (name (plist-get result :name))
         (command (plist-get result :command))
         (status (plist-get result :status))
         (output (or (plist-get result :output) ""))
         (failed-p (ellama-tools--edit-shell-hook-failed-p result))
         (subject (if (and (stringp name) (not (string-empty-p name)))
                      (format "%s edit hook `%s`" phase name)
                    (format "%s edit hook" phase))))
    (string-join
     (delq nil
           (list
            (if failed-p
                (format "%s failed with exit status %s:" subject status)
              (format "%s completed:" subject))
            (when command
              (format "Command: %s" command))
            (unless (string-empty-p output)
              output)))
     "\n\n")))

(defun ellama-tools--edit-shell-hook-visible-p (result)
  "Return non-nil when edit shell hook RESULT should be returned."
  (or (ellama-tools--edit-shell-hook-failed-p result)
      (and (plist-get result :show-output)
           (not (string-empty-p (or (plist-get result :output) ""))))))

(defun ellama-tools--edit-shell-hook-section (_tool-name result)
  "Return output section for edit shell hook RESULT."
  (ellama-tools--make-output-section
   (if (eq (plist-get result :phase) 'before)
       'edit-before-hook
     'edit-after-hook)
   (ellama-tools--format-edit-shell-hook-result result)
   nil))

(defun ellama-tools--run-edit-shell-hooks
    (phase file-name operation tool-name callback)
  "Run edit shell hooks asynchronously and call CALLBACK with result.
PHASE, FILE-NAME, OPERATION and TOOL-NAME describe the edit.  Before hooks
stop on the first failure.  After hooks run all commands."
  (let ((hooks (ellama-tools--edit-shell-hooks-for-file file-name phase)))
    (cl-labels
        ((run
           (remaining sections failed)
           (if (not remaining)
               (funcall callback
                        (list :failed failed
                              :sections (nreverse sections)))
             (ellama-tools--run-edit-shell-hook
              phase file-name operation tool-name (car remaining)
              (lambda (result)
                (let ((next-sections sections)
                      (next-failed failed))
                  (when (ellama-tools--edit-shell-hook-visible-p result)
                    (push (ellama-tools--edit-shell-hook-section
                           tool-name result)
                          next-sections))
                  (when (ellama-tools--edit-shell-hook-failed-p result)
                    (setq next-failed t))
                  (if (and next-failed (eq phase 'before))
                      (funcall callback
                               (list :failed t
                                     :sections (nreverse next-sections)))
                    (run (cdr remaining)
                         next-sections
                         next-failed))))))))
      (run hooks nil nil))))

(defun ellama-tools--refresh-file-buffer-after-hooks (file-name)
  "Refresh unmodified visiting buffer for FILE-NAME after shell hooks."
  (let ((expanded (expand-file-name file-name)))
    (when-let* ((buffer (get-file-buffer expanded)))
      (with-current-buffer buffer
        (when (and (not (buffer-modified-p))
                   (file-exists-p expanded))
          (revert-buffer t t t))))))

(defun ellama-tools--edit-output
    (main-message before-sections after-sections)
  "Return edit output with MAIN-MESSAGE and hook sections.
BEFORE-SECTIONS and AFTER-SECTIONS are visible shell hook output sections."
  (let ((sections
         (append
          before-sections
          (list (ellama-tools--make-output-section
                 'tool-result main-message t))
          after-sections)))
    (if (or before-sections after-sections)
        (ellama-tools--sectioned-output sections)
      main-message)))

(defun ellama-tools--run-edit-with-shell-hooks
    (tool-name operation file-name edit-fn success-message callback)
  "Run edit shell hooks asynchronously and call CALLBACK with result.
TOOL-NAME, OPERATION, FILE-NAME, EDIT-FN and SUCCESS-MESSAGE describe the
edit.  CALLBACK receives the final tool output after all relevant hooks finish."
  (ellama-tools--run-edit-shell-hooks
   'before file-name operation tool-name
   (lambda (before)
     (let ((before-sections (plist-get before :sections)))
       (if (plist-get before :failed)
           (funcall callback
                    (ellama-tools--sectioned-output before-sections))
         (funcall edit-fn)
         (ellama-tools--run-edit-shell-hooks
          'after file-name operation tool-name
          (lambda (after)
            (let ((after-sections (plist-get after :sections)))
              (ellama-tools--refresh-file-buffer-after-hooks file-name)
              (funcall
               callback
               (ellama-tools--edit-output
                success-message before-sections after-sections))))))))))

(defun ellama-tools--current-file-content (file-name)
  "Return current FILE-NAME content, or an empty string when missing.
Prefer an existing visiting buffer so append and prepend preserve unsaved
buffer contents, matching their historical behavior."
  (if-let* ((buffer (get-file-buffer (expand-file-name file-name))))
      (with-current-buffer buffer
        (buffer-string))
    (if (file-exists-p file-name)
        (with-temp-buffer
          (insert-file-contents-literally file-name)
          (buffer-string))
      "")))

(defun ellama-tools-write-file-tool (callback file-name content)
  "Write CONTENT to FILE-NAME and call CALLBACK with the result."
  (let ((result
         (or (ellama-tools--tool-check-file-access file-name 'write)
             (ellama-tools--file-parent-directory-error-message
              "write" file-name)
             (when (file-directory-p file-name)
               (format "Cannot write %s: path is a directory." file-name))
             (ellama-tools--read-before-write-check "Write" file-name)
             (condition-case err
                 (let* ((original (when (file-exists-p file-name)
                                    (ellama-tools--current-file-content
                                     file-name)))
                        (decision
                         (ellama-tools--balanced-edit-check-candidate
                          "Write" file-name content original nil content))
                        (rejection (plist-get decision :rejection))
                        (auto-fixed (plist-get decision :auto-fixed))
                        (fixed-text (plist-get decision :fixed-text))
                        (text-to-write (if (and auto-fixed fixed-text)
                                           fixed-text
                                         content)))
                   (if rejection
                       rejection
                     (ellama-tools--run-edit-with-shell-hooks
                      "write_file" "write" file-name
                      (lambda ()
                        (ellama-tools--write-file-buffer-content
                         file-name text-to-write))
                      (format "Wrote %d characters to %s%s."
                              (length content)
                              file-name
                              (ellama-tools--balanced-edit-success-suffix
                               decision))
                      callback)
                     nil))
               (file-error
                (format "Cannot write %s: %s"
                        file-name
                        (error-message-string err)))))))
    (when result
      (funcall callback result)))
  nil)

(ellama-tools-define-tool
 '(:function
   ellama-tools-write-file-tool
   :name
   "write_file"
   :async
   t
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

(defun ellama-tools-append-file-tool (callback file-name content)
  "Append CONTENT to FILE-NAME and call CALLBACK with the result."
  (let ((result
         (or (ellama-tools--tool-check-file-access file-name 'write)
             (ellama-tools--file-parent-directory-error-message
              "append to" file-name)
             (when (file-directory-p file-name)
               (format "Cannot append to %s: path is a directory." file-name))
             (ellama-tools--read-before-write-check "Append" file-name)
             (condition-case err
                 (let* ((original
                         (ellama-tools--current-file-content file-name))
                        (candidate (concat original content))
                        (decision
                         (ellama-tools--balanced-edit-check-candidate
                          "Append" file-name candidate original nil content))
                        (rejection (plist-get decision :rejection))
                        (auto-fixed (plist-get decision :auto-fixed))
                        (fixed-text (plist-get decision :fixed-text))
                        (text-to-write (if (and auto-fixed fixed-text)
                                           fixed-text
                                         candidate)))
                   (if rejection
                       rejection
                     (ellama-tools--run-edit-with-shell-hooks
                      "append_file" "append" file-name
                      (lambda ()
                        (ellama-tools--write-file-buffer-content
                         file-name text-to-write))
                      (format "Appended %d characters to %s%s."
                              (length content)
                              file-name
                              (ellama-tools--balanced-edit-success-suffix
                               decision))
                      callback)
                     nil))
               (file-error
                (format "Cannot append to %s: %s"
                        file-name
                        (error-message-string err)))))))
    (when result
      (funcall callback result)))
  nil)

(ellama-tools-define-tool
 '(:function
   ellama-tools-append-file-tool
   :name
   "append_file"
   :async
   t
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

(defun ellama-tools-prepend-file-tool (callback file-name content)
  "Prepend CONTENT to FILE-NAME and call CALLBACK with the result."
  (let ((result
         (or (ellama-tools--tool-check-file-access file-name 'write)
             (ellama-tools--file-parent-directory-error-message
              "prepend to" file-name)
             (when (file-directory-p file-name)
               (format "Cannot prepend to %s: path is a directory."
                       file-name))
             (ellama-tools--read-before-write-check "Prepend" file-name)
             (condition-case err
                 (let* ((original
                         (ellama-tools--current-file-content file-name))
                        (candidate (concat content original))
                        (decision
                         (ellama-tools--balanced-edit-check-candidate
                          "Prepend" file-name candidate original nil content))
                        (rejection (plist-get decision :rejection))
                        (auto-fixed (plist-get decision :auto-fixed))
                        (fixed-text (plist-get decision :fixed-text))
                        (text-to-write (if (and auto-fixed fixed-text)
                                           fixed-text
                                         candidate)))
                   (if rejection
                       rejection
                     (ellama-tools--run-edit-with-shell-hooks
                      "prepend_file" "prepend" file-name
                      (lambda ()
                        (ellama-tools--write-file-buffer-content
                         file-name text-to-write))
                      (format "Prepended %d characters to %s%s."
                              (length content)
                              file-name
                              (ellama-tools--balanced-edit-success-suffix
                               decision))
                      callback)
                     nil))
               (file-error
                (format "Cannot prepend to %s: %s"
                        file-name
                        (error-message-string err)))))))
    (when result
      (funcall callback result)))
  nil)

(ellama-tools-define-tool
 '(:function
   ellama-tools-prepend-file-tool
   :name
   "prepend_file"
   :async
   t
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

(defun ellama-tools--directory-tree-timeout-output (timeout output)
  "Return diagnostic text for `directory_tree' timeout after TIMEOUT.
OUTPUT is the partial directory tree collected before the timeout."
  (if (string-empty-p output)
      (format "directory_tree timed out after %s seconds." timeout)
    (format "directory_tree timed out after %s seconds.\n%s"
            timeout output)))

(defun ellama-tools--directory-tree (dir depth deadline)
  "Return directory tree under DIR at DEPTH until DEADLINE."
  (let* ((indent (make-string (* (or depth 0) 2) ? ))
         (entries
          (sort (cl-remove-if
                 (lambda (f)
                   (string-prefix-p "." f))
                 (directory-files dir))
                #'string-lessp))
         (single-entry-p (= (length entries) 1))
         (tree "")
         timed-out)
    (catch 'done
      (dolist (f entries)
        (when (>= (float-time) deadline)
          (setq timed-out t)
          (throw 'done nil))
        (let* ((full   (expand-file-name f dir))
               (name   (file-name-nondirectory f))
               (type   (if (file-directory-p full) "|-" "`-"))
               (line   (concat indent
                               (unless single-entry-p type)
                               name "\n")))
          (setq tree (concat tree line))
          (when (file-directory-p full)
            (let ((child (ellama-tools--directory-tree
                          full
                          (+ (or depth 0) 1)
                          deadline)))
              (setq tree (concat tree (car child)))
              (when (cdr child)
                (setq timed-out t)
                (throw 'done nil)))))))
    (cons tree timed-out)))

(defun ellama-tools-directory-tree-tool (dir &optional timeout)
  "Return a string representing the directory tree under DIR.
TIMEOUT is the optional command timeout in seconds."
  (or (ellama-tools--tool-check-file-access dir 'list)
      (if (not (file-exists-p dir))
          (format "Directory %s does not exist." dir)
        (if (not (file-directory-p dir))
            (format "%s is not a directory." dir)
          (let* ((timeout (ellama-tools--shell-command-timeout timeout))
                 (result (ellama-tools--directory-tree
                          dir
                          0
                          (+ (float-time) timeout)))
                 (tree (car result)))
            (if (cdr result)
                (ellama-tools--directory-tree-timeout-output timeout tree)
              tree))))))

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
     "Directory path to generate tree for.")
    (:name
     "timeout"
     :type
     number
     :optional
     t
     :description
     "Command timeout in seconds. Defaults to 5."))
   :description
   "Return a string representing the directory tree under DIR."))

(defun ellama-tools-move-file-tool (file-name new-file-name)
  "Move the file from the specified FILE-NAME to the NEW-FILE-NAME."
  (or (ellama-tools--tool-check-file-access file-name 'read)
      (ellama-tools--tool-check-file-access file-name 'write)
      (ellama-tools--tool-check-file-access new-file-name 'write)
      (ellama-tools--read-before-write-check "Move" file-name)
      (cond
       ((not (file-exists-p file-name))
        (format "Cannot move file: source file does not exist: %s."
                file-name))
       ((file-exists-p new-file-name)
        (format "Cannot move file: destination already exists: %s."
                new-file-name))
       (t
        (condition-case err
            (progn
              (rename-file file-name new-file-name)
              (format "Moved %s to %s." file-name new-file-name))
          (file-error
           (format "Cannot move %s to %s: %s"
                   file-name
                   new-file-name
                   (error-message-string err))))))))

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

(defun ellama-tools--edit-file-old-content-not-found-message (file-name)
  "Return edit failure message for missing old content in FILE-NAME."
  (format
   (concat
    "No replacement made: old content was not found in %s. "
    "Ensure oldcontent matches the file text exactly, including actual "
    "newline characters rather than escaped \\n sequences.")
   file-name))

(defun ellama-tools--write-file-buffer-content (file-name content)
  "Write CONTENT to FILE-NAME and update any visiting buffer."
  (let ((buffer (find-file-noselect file-name)))
    (with-current-buffer buffer
      (let ((coding-system-for-write 'raw-text)
            (buffer-undo-list t)
            (inhibit-read-only t))
        (erase-buffer)
        (insert content)
        (save-buffer)))))

(defun ellama-tools-edit-file-tool (callback file-name oldcontent newcontent)
  "Edit FILE-NAME and call CALLBACK with the result.
Replace OLDCONTENT with NEWCONTENT."
  (let ((result
         (or (ellama-tools--tool-check-file-access file-name 'read)
             (ellama-tools--tool-check-file-access file-name 'write)
             (let ((content (with-temp-buffer
                              (insert-file-contents-literally file-name)
                              (buffer-string))))
               (ellama-tools--mark-file-read file-name)
               (if (not (string-match (regexp-quote oldcontent) content))
                   (ellama-tools--edit-file-old-content-not-found-message
                    file-name)
                 (let* ((candidate (replace-match newcontent t t content))
                        (decision
                         (ellama-tools--balanced-edit-check-candidate
                          "Edit" file-name candidate content oldcontent
                          newcontent))
                        (rejection (plist-get decision :rejection))
                        (auto-fixed (plist-get decision :auto-fixed))
                        (fixed-text (plist-get decision :fixed-text))
                        (text-to-write (if (and auto-fixed fixed-text)
                                           fixed-text
                                         candidate)))
                   (if rejection
                       rejection
                     (ellama-tools--run-edit-with-shell-hooks
                      "edit_file" "edit" file-name
                      (lambda ()
                        (ellama-tools--write-file-buffer-content
                         file-name text-to-write))
                      (format "Edited %s%s."
                              file-name
                              (ellama-tools--balanced-edit-success-suffix
                               decision))
                      callback)
                     nil)))))))
    (when result
      (funcall callback result)))
  nil)

(ellama-tools-define-tool
 '(:function
   ellama-tools-edit-file-tool
   :name
   "edit_file"
   :async
   t
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

(defun ellama-tools-shell-command-tool (callback cmd &optional timeout)
  "Execute shell command CMD.
CALLBACK – function called once with the result string.
TIMEOUT is the optional command timeout in seconds."
  (let ((argv (ellama-tools--command-argv
               shell-file-name shell-command-switch cmd))
        (timeout (ellama-tools--shell-command-timeout timeout))
        (process-environment
         (ellama-tools--process-environment-with-cat-pager)))
    (condition-case err
        (let ((buf (get-buffer-create
                    (concat (make-temp-name " *ellama shell command") "*")))
              process timer done timed-out)
          (cl-labels
              ((command-output ()
                 ;; trim trailing newline to reduce noisy tool output
                 (ellama-tools--sanitize-tool-text-output
                  (string-trim-right
                   (with-current-buffer buf (buffer-string))
                   "\n")
                  "Command output"))
               (timeout-result ()
                 (let ((output (command-output)))
                   (if (string-empty-p output)
                       (format "Command timed out after %s seconds."
                               timeout)
                     (format "Command timed out after %s seconds.\n%s"
                             timeout output))))
               (finish (result)
                 (unless done
                   (setq done t)
                   (when timer
                     (cancel-timer timer))
                   (unwind-protect
                       (funcall callback result)
                     (when (buffer-live-p buf)
                       (kill-buffer buf)))))
               (finish-process (proc)
                 (if timed-out
                     (finish (timeout-result))
                   (let* ((output (command-output))
                          (exit-code (process-exit-status proc))
                          (result
                           (cond
                            ((and (string= output "") (zerop exit-code))
                             "Command completed successfully with no output.")
                            ((string= output "")
                             (format
                              "Command failed with exit code %d and no output."
                              exit-code))
                            ((zerop exit-code)
                             output)
                            (t
                             (format "Command failed with exit code %d.\n%s"
                                     exit-code output)))))
                     (finish result)))))
            (setq process
                  (apply #'start-process
                         "*ellama-shell-command*" buf
                         (car argv) (cdr argv)))
            (set-process-sentinel
             process
             (lambda (proc _)
               (when (not (process-live-p proc))
                 (finish-process proc))))
            (setq timer
                  (run-at-time
                   timeout nil
                   (lambda ()
                     (when (and process (process-live-p process))
                       (setq timed-out t)
                       (let ((result (timeout-result)))
                         (delete-process process)
                         (finish result))))))))
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
     "Shell command to execute.")
    (:name
     "timeout"
     :type
     number
     :optional
     t
     :description
     "Command timeout in seconds. Defaults to 5."))
   :description
   "Execute shell command CMD."))

(defun ellama-tools--grep-case-args (case-sensitive)
  "Return grep arguments for CASE-SENSITIVE matching."
  (unless case-sensitive
    '("-i")))

(defun ellama-tools--grep-search-args (case-sensitive)
  "Return grep arguments for literal CASE-SENSITIVE matching."
  (append
   (ellama-tools--grep-case-args case-sensitive)
   '("-F")))

(defun ellama-tools-grep-tool (dir search-string &optional case-sensitive timeout)
  "Grep SEARCH-STRING in DIR files.
Match case-insensitively unless CASE-SENSITIVE is non-nil.
TIMEOUT is the optional command timeout in seconds."
  (let ((search-dir (expand-file-name dir))
        (timeout (ellama-tools--shell-command-timeout timeout)))
    (json-encode
     (cond
      ((not (file-exists-p search-dir))
       (format "Directory %s does not exist." search-dir))
      ((not (file-directory-p search-dir))
       (format "%s is not a directory." search-dir))
      (t
       (let ((default-directory (file-name-as-directory search-dir)))
         (ellama-tools--grep-output
          (apply
           #'ellama-tools--call-command-with-timeout
           (append
            (list timeout
                  "find" "." "-type" "f" "-exec" "grep" "--color=never")
            (ellama-tools--grep-search-args case-sensitive)
            (list "-nH" "-e" search-string "{}" "+")))
          (format "No matches for %S in %s."
                  search-string
                  search-dir)
          timeout)))))))

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
     "String to search for.")
    (:name
     "case_sensitive"
     :type
     boolean
     :optional
     t
     :description
     "When non-nil, match case sensitively. Defaults to false.")
    (:name
     "timeout"
     :type
     number
     :optional
     t
     :description
     "Command timeout in seconds. Defaults to 5."))
   :description
   "Grep SEARCH-STRING in directory files. Case-insensitive by default."))

(defun ellama-tools-grep-in-file-tool (search-string file
                                                     &optional case-sensitive)
  "Grep SEARCH-STRING in FILE.
Match case-insensitively unless CASE-SENSITIVE is non-nil."
  (or (ellama-tools--tool-check-file-access file 'read)
      (json-encode
       (cond
        ((not (file-exists-p file))
         (format "File %s does not exist." file))
        ((file-directory-p file)
         (format "%s is a directory, not a file." file))
        (t
         (let ((truename (file-truename file)))
           (ellama-tools--grep-output
            (apply
             #'ellama-tools--call-command
             (append
              (list "grep" "--color=never")
              (ellama-tools--grep-search-args case-sensitive)
              (list "-nh" "-e" search-string truename)))
            (format "No matches for %S in %s."
                    search-string truename))))))))

(ellama-tools-define-tool
 '(:function
   ellama-tools-grep-in-file-tool
   :name "grep_in_file"
   :args ((:name "search_string" :type string
                 :description "String to search for.")
          (:name "file" :type string :description "File to search in.")
          (:name "case_sensitive" :type boolean :optional t
                 :description
                 "When non-nil, match case sensitively. Defaults to false."))
   :description "Grep SEARCH-STRING in FILE. Case-insensitive by default."))

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
  (if-let* ((project (project-current nil)))
      (project-root project)
    (expand-file-name default-directory)))

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
      (if (not (file-exists-p file-name))
          (format "File %s does not exist." file-name)
        (if (file-directory-p file-name)
            (format "%s is a directory, not a file." file-name)
          (with-current-buffer (find-file-noselect file-name)
            (count-lines (point-min) (point-max)))))))

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
      (json-encode
       (cond
        ((not (file-exists-p file-name))
         (format "File %s does not exist." file-name))
        ((file-directory-p file-name)
         (format "%s is a directory, not a file." file-name))
        ((> from to)
         (format "Invalid line range for %s: from (%s) is greater than to (%s)."
                 file-name
                 from
                 to))
        ((< from 1)
         (format "Invalid line range for %s: from (%s) must be at least 1."
                 file-name
                 from))
        (t
         (with-current-buffer (find-file-noselect file-name)
           (save-excursion
             (let ((line-count (count-lines (point-min) (point-max))))
               (if (> to line-count)
                   (format
                    "Invalid line range for %s: to (%s) exceeds line count (%s)."
                    file-name
                    to
                    line-count)
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
                   (prog1
                       (ellama-tools--sanitize-tool-text-output
                        (buffer-substring-no-properties start end)
                        (format "File %s" file-name))
                     (ellama-tools--mark-file-read file-name))))))))))))

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

(defun ellama-tools--task-template-placeholder-names (template)
  "Return placeholder names used by TEMPLATE."
  (let ((start 0)
        names)
    (while (string-match "{\\([[:alnum:]_-]+\\)}" template start)
      (push (match-string 1 template) names)
      (setq start (match-end 0)))
    (sort (delete-dups names) #'string<)))

(defun ellama-tools--task-template-argument-name (key)
  "Return string name for template argument KEY."
  (cond
   ((stringp key)
    key)
   ((keywordp key)
    (substring (symbol-name key) 1))
   ((symbolp key)
    (symbol-name key))
   (t
    (format "%s" key))))

(defun ellama-tools--task-template-arguments-alist (arguments)
  "Return normalized alist from template ARGUMENTS."
  (cond
   ((null arguments)
    nil)
   ((hash-table-p arguments)
    (let (result)
      (maphash
       (lambda (key value)
         (push (cons (ellama-tools--task-template-argument-name key)
                     (format "%s" value))
               result))
       arguments)
      result))
   ((and (listp arguments)
         (cl-every
          (lambda (item)
            (and (consp item)
                 (not (keywordp (car item)))))
          arguments))
    (mapcar
     (lambda (item)
       (cons (ellama-tools--task-template-argument-name (car item))
             (format "%s" (cdr item))))
     arguments))
   ((and (listp arguments)
         (zerop (mod (length arguments) 2)))
    (let (result)
      (while arguments
        (push (cons (ellama-tools--task-template-argument-name
                     (pop arguments))
                    (format "%s" (pop arguments)))
              result))
      result))
   (t
    (error "Task template arguments must be an object"))))

(defun ellama-tools--task-template-bullet-list (items)
  "Return ITEMS formatted as a bullet list."
  (if items
      (mapconcat (lambda (item) (format "- %s" item)) items "\n")
    "- none"))

(defun ellama-tools--task-template-example (template template-base role names)
  "Return example task call for TEMPLATE, TEMPLATE-BASE, ROLE and NAMES."
  (format
   "{\n  \"template\": %S,%s\n  \"arguments\": {%s\n  },\n  \"role\": %S\n}"
   template
   (if template-base
       (format "\n  \"template_base\": %S," template-base)
     "")
   (if names
       (concat
        "\n"
        (mapconcat
         (lambda (name)
           (format "    %S: \"...\"" name))
         names
         ",\n"))
     "")
   (or role "general")))

(defun ellama-tools--task-template-validation-error
    (template template-base role required supplied missing unused)
  "Return task template validation message.
TEMPLATE, TEMPLATE-BASE and ROLE describe the failed call.  REQUIRED,
SUPPLIED, MISSING and UNUSED are lists of argument names."
  (concat
   "Task template validation failed. No subagent was started.\n\n"
   (format "Template: %s\n" template)
   (when template-base
     (format "Template base: %s\n" template-base))
   "\nRequired arguments:\n"
   (ellama-tools--task-template-bullet-list required)
   "\n\nSupplied arguments:\n"
   (ellama-tools--task-template-bullet-list supplied)
   "\n\nMissing arguments:\n"
   (ellama-tools--task-template-bullet-list missing)
   (when unused
     (concat
      "\n\nUnused arguments:\n"
      (ellama-tools--task-template-bullet-list unused)
      "\n\nThese were ignored. Use only the required argument names above."))
   "\n\nRetry the task call using this shape:\n"
   (ellama-tools--task-template-example
    template template-base role required)))

(defun ellama-tools--task-template-error (message)
  "Return task template error MESSAGE."
  (format "Task template error. No subagent was started.\n\n%s" message))

(defun ellama-tools--task-template-readable-file (file base)
  "Return FILE when it is a readable regular file under BASE."
  (cond
   ((not (file-directory-p base))
    (error "%s" (ellama-tools--task-template-error
                 (format "Template base is not a directory: %s"
                         base))))
   ((not (file-exists-p file))
    (error "%s" (ellama-tools--task-template-error
                 (format "Template does not exist: %s"
                         file))))
   (t
    (let* ((base-name (file-truename (file-name-as-directory base)))
           (file-name (file-truename file)))
      (cond
       ((not (file-in-directory-p file-name base-name))
        (error "%s" (ellama-tools--task-template-error
                     (format "Template path escapes base directory: %s"
                             file))))
       ((not (file-regular-p file-name))
        (error "%s" (ellama-tools--task-template-error
                     (format "Template is not a regular file: %s"
                             file))))
       ((not (file-readable-p file-name))
        (error "%s" (ellama-tools--task-template-error
                     (format "Template is not readable: %s"
                             file))))
       (t
        file-name))))))

(defun ellama-tools--resolve-task-template (template template-base)
  "Resolve TEMPLATE using TEMPLATE-BASE or configured template directories."
  (cond
   ((or (null template) (string= template ""))
    (error "%s" (ellama-tools--task-template-error
                 "Template name is empty")))
   ((file-name-absolute-p template)
    (if (not ellama-tools-task-template-allow-absolute-paths)
        (error "%s" (ellama-tools--task-template-error
                     "Absolute template paths are disabled.  Use a relative \
template with template_base"))
      (let ((dir (file-name-directory template)))
        (ellama-tools--task-template-readable-file template dir))))
   (template-base
    (ellama-tools--task-template-readable-file
     (expand-file-name template template-base)
     template-base))
   (t
    (let ((dirs ellama-tools-task-template-dirs)
          resolved)
      (while (and dirs (not resolved))
        (let* ((base (expand-file-name (car dirs)))
               (file (expand-file-name template base)))
          (when (file-exists-p file)
            (setq resolved
                  (ellama-tools--task-template-readable-file file base))))
        (setq dirs (cdr dirs)))
      (or resolved
          (error "%s" (ellama-tools--task-template-error
                       (format "Template was not found in configured \
template directories: %s"
                               template))))))))

(defun ellama-tools--render-task-template
    (template-text template template-base role arguments)
  "Render TEMPLATE-TEXT with ARGUMENTS.
TEMPLATE, TEMPLATE-BASE and ROLE are used for validation hints."
  (let* ((required (ellama-tools--task-template-placeholder-names
                    template-text))
         (alist (ellama-tools--task-template-arguments-alist arguments))
         (supplied (sort (delete-dups (mapcar #'car alist)) #'string<))
         (missing (cl-set-difference required supplied :test #'string=))
         (unused (cl-set-difference supplied required :test #'string=))
         (rendered template-text))
    (when missing
      (error "%s" (ellama-tools--task-template-validation-error
                   template template-base role required supplied
                   (sort missing #'string<) (sort unused #'string<))))
    (dolist (item alist)
      (setq rendered
            (replace-regexp-in-string
             (regexp-quote (format "{%s}" (car item)))
             (cdr item)
             rendered t t)))
    rendered))

(defun ellama-tools--task-description
    (description template template-base role arguments)
  "Return task DESCRIPTION or render TEMPLATE.
TEMPLATE-BASE, ROLE and ARGUMENTS are used for template rendering and hints."
  (if (and template (not (string= template "")))
      (let* ((file-name (ellama-tools--resolve-task-template
                         template template-base))
             (template-text
              (with-temp-buffer
                (insert-file-contents file-name)
                (buffer-string))))
        (ellama-tools--render-task-template
         template-text template template-base role arguments))
    (or description
        (error "%s" (ellama-tools--task-template-error
                     "Either description or template is required")))))

(defconst ellama-tools--agent-state-begin "BEGIN_ELLAMA_AGENT_STATE"
  "Marker starting a fallback plan-and-act state block.")

(defconst ellama-tools--agent-state-end "END_ELLAMA_AGENT_STATE"
  "Marker ending a fallback plan-and-act state block.")

(defun ellama-tools--agent-state (session)
  "Return plan-and-act state from SESSION."
  (when-let* (((ellama-session-p session))
              (extra (ellama-session-extra session)))
    (plist-get extra :agent-loop)))

(defun ellama-tools--agent-put-state (session state)
  "Store plan-and-act STATE in SESSION."
  (ellama-tools--set-session-extra
   session
   (ellama-tools--session-extra-with session :agent-loop state)))

(defun ellama-tools--agent-state-put (state key value)
  "Return STATE with KEY set to VALUE."
  (plist-put (copy-sequence state) key value))

(defun ellama-tools--agent-session-buffer (session &optional buffer)
  "Return live chat BUFFER for SESSION."
  (or (and (buffer-live-p buffer) buffer)
      (when-let* (((ellama-session-p session))
                  (extra (ellama-session-extra session))
                  (uid (plist-get extra :uid))
                  (session-buffer (ellama-get-session-buffer uid)))
        (and (buffer-live-p session-buffer) session-buffer))
      (when-let* (((ellama-session-p session))
                  (session-buffer
                   (ellama-get-session-buffer (ellama-session-id session))))
        (and (buffer-live-p session-buffer) session-buffer))))

(defun ellama-tools--agent-step-status (step)
  "Return STEP status."
  (or (plist-get step :status) 'pending))

(defun ellama-tools--agent-step-done-p (step)
  "Return non-nil when STEP is done."
  (eq (ellama-tools--agent-step-status step) 'done))

(defun ellama-tools--agent-next-step (state)
  "Return first pending step in STATE."
  (seq-find (lambda (step)
              (not (ellama-tools--agent-step-done-p step)))
            (plist-get state :plan)))

(defun ellama-tools--agent-plan-complete-p (state)
  "Return non-nil when STATE plan is fully done."
  (let ((plan (plist-get state :plan)))
    (and plan (seq-every-p #'ellama-tools--agent-step-done-p plan))))

(defun ellama-tools--agent-clean-step-title (line)
  "Return checklist step title parsed from LINE."
  (let ((title (string-trim line)))
    (setq title
          (replace-regexp-in-string
           "\\`[-+*][[:space:]]+\\(?:\\[[ Xx-]\\][[:space:]]+\\)?"
           "" title))
    (setq title
          (replace-regexp-in-string
           "\\`[0-9]+[.)][[:space:]]+\\(?:\\[[ Xx-]\\][[:space:]]+\\)?"
           "" title))
    (string-trim title)))

(defun ellama-tools--agent-status-from-line (line)
  "Return checklist status parsed from LINE."
  (cond
   ((string-match-p "\\[[Xx]\\]" line) 'done)
   ((string-match-p "\\[-\\]" line) 'in-progress)
   (t 'pending)))

(defun ellama-tools--agent-parse-plan (text)
  "Return checklist entries parsed from TEXT."
  (let ((id 0)
        steps)
    (dolist (line (split-string (or text "") "\n"))
      (let ((trimmed (string-trim line)))
        (when (string-match-p
               "\\`\\(?:[-+*][[:space:]]+\\|[0-9]+[.)][[:space:]]+\\)"
               trimmed)
          (let ((title (ellama-tools--agent-clean-step-title trimmed)))
            (unless (string-empty-p title)
              (push (list :id (cl-incf id)
                          :title title
                          :status (ellama-tools--agent-status-from-line
                                   trimmed))
                    steps))))))
    (nreverse steps)))

(defun ellama-tools--agent-state-block (text)
  "Return fallback state block from TEXT, or nil."
  (when (and (stringp text)
             (string-match
              (concat (regexp-quote ellama-tools--agent-state-begin)
                      "\\(?:.\\|\n\\)*?"
                      (regexp-quote ellama-tools--agent-state-end))
              text))
    (match-string 0 text)))

(defun ellama-tools--agent-parse-phase (text)
  "Return plan-and-act phase parsed from TEXT."
  (when (and (stringp text)
             (string-match
              "^phase:[[:space:]]*\\([[:alpha:]-]+\\)[[:space:]]*$"
              text))
    (intern (match-string 1 text))))

(defun ellama-tools--agent-parse-field (field text)
  "Return FIELD value parsed from TEXT."
  (when (and (stringp text)
             (string-match
              (format "^%s:[[:space:]]*\\(.+\\)[[:space:]]*$"
                      (regexp-quote field))
              text))
    (string-trim (match-string 1 text))))

(defun ellama-tools--agent-state-from-text (text)
  "Return plan-and-act state update parsed from TEXT."
  (when-let* ((block (ellama-tools--agent-state-block text)))
    (let ((plan (ellama-tools--agent-parse-plan block))
          (phase (ellama-tools--agent-parse-phase block))
          (result (ellama-tools--agent-parse-field "result" block))
          (blocked (ellama-tools--agent-parse-field "blocked" block)))
      (when (or plan phase result blocked)
        (list :phase phase
              :plan plan
              :result result
              :blocked blocked)))))

(defun ellama-tools--agent-merge-state-update (state update)
  "Return STATE merged with parsed UPDATE."
  (let ((state (copy-sequence state)))
    (when-let ((phase (plist-get update :phase)))
      (setq state (plist-put state :phase phase)))
    (when-let ((plan (plist-get update :plan)))
      (setq state (plist-put state :plan plan)))
    (when-let ((result (plist-get update :result)))
      (setq state (plist-put state :result result)))
    (when-let ((blocked (plist-get update :blocked)))
      (setq state (plist-put state :blocked blocked)
            state (plist-put state :phase 'blocked)))
    (when (eq (plist-get state :phase) 'done)
      (setq state (plist-put state :completed t)))
    state))

(defun ellama-tools--agent-checkbox (status)
  "Return Org checkbox string for STATUS."
  (pcase status
    ('done "[X]")
    ((or 'in-progress 'blocked) "[-]")
    (_ "[ ]")))

(defun ellama-tools--agent-render-plan (state)
  "Return Org text rendering plan in STATE."
  (mapconcat
   (lambda (step)
     (format "- %s %s"
             (ellama-tools--agent-checkbox
              (ellama-tools--agent-step-status step))
             (plist-get step :title)))
   (plist-get state :plan)
   "\n"))

(defun ellama-tools--agent-insert-note (buffer title body)
  "Insert visible plan-and-act note with TITLE and BODY into BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-max))
        (unless (bobp)
          (unless (bolp)
            (insert "\n"))
          (unless (save-excursion
                    (forward-line -1)
                    (looking-at-p "[[:space:]]*$"))
            (insert "\n")))
        (insert (ellama-get-nick-prefix-for-mode)
                " Ellama Agent " title ":\n"
                (or body "")
                "\n\n")
        (ellama--scroll buffer (point))))))

(defun ellama-tools--agent-insert-state (session buffer title)
  "Insert SESSION plan-and-act state into BUFFER with TITLE."
  (when-let* ((state (ellama-tools--agent-state session)))
    (let ((plan (ellama-tools--agent-render-plan state))
          (phase (plist-get state :phase))
          (status (plist-get state :status))
          (result (plist-get state :result))
          (blocked (plist-get state :blocked)))
      (ellama-tools--agent-insert-note
       (ellama-tools--agent-session-buffer session buffer)
       title
       (string-join
        (delq nil
              (list (format "Phase: %s" phase)
                    (when status (format "Status: %s" status))
                    (unless (string-empty-p plan) plan)
                    (when blocked (format "Blocked: %s" blocked))
                    (when result (format "Result: %s" result))))
        "\n")))))

(defun ellama-tools--agent-controller-tools ()
  "Return plan-and-act controller tools."
  (list
   (llm-make-tool
    :function #'ellama-tools-agent-submit-plan-tool
    :name "agent_submit_plan"
    :description "Submit the checklist plan before acting."
    :args '((:name "plan" :type string
                   :description "Org-style checklist or numbered plan.")))
   (llm-make-tool
    :function #'ellama-tools-agent-update-plan-tool
    :name "agent_update_plan"
    :description "Update checklist status after making progress."
    :args '((:name "plan" :type string
                   :description "Full updated Org-style checklist.")
            (:name "status" :type string
                   :description "Short status summary.")))
   (llm-make-tool
    :function #'ellama-tools-agent-report-result-tool
    :name "agent_report_result"
    :description "Report final result and terminate the plan-and-act loop."
    :args '((:name "result" :type string)))))

(defun ellama-tools--agent-tool-name-p (name)
  "Return non-nil when NAME is a plan-and-act controller tool."
  (member name '("agent_submit_plan"
                 "agent_update_plan"
                 "agent_report_result")))

(defun ellama-tools--agent-combine-tools (tools)
  "Return TOOLS plus plan-and-act controller tools."
  (let ((base (seq-remove
               (lambda (tool)
                 (ellama-tools--agent-tool-name-p (llm-tool-name tool)))
               tools)))
    (append (ellama-tools--agent-controller-tools) base)))

(defun ellama-tools--agent-restore-tools (session)
  "Restore SESSION tools saved before plan-and-act started."
  (let* ((state (ellama-tools--agent-state session))
         (had-tools (plist-get state :had-original-tools))
         (tools (plist-get state :original-tools)))
    (ellama-tools--set-session-extra
     session
     (ellama-tools--session-extra-with
      session :tools (and had-tools tools)))))

(defun ellama-tools--agent-append-user-header (buffer)
  "Append user heading to BUFFER after a finished plan-and-act loop."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((header
             (concat (ellama-get-nick-prefix-for-mode)
                     " " ellama-user-nick ":")))
        (unless (save-excursion
                  (goto-char (point-max))
                  (skip-chars-backward " \t\n")
                  (beginning-of-line)
                  (looking-at-p (regexp-quote header)))
          (save-excursion
            (goto-char (point-max))
            (unless (bobp)
              (unless (bolp)
                (insert "\n"))
              (unless (save-excursion
                        (forward-line -1)
                        (looking-at-p "[[:space:]]*$"))
                (insert "\n")))
            (insert header "\n")))))))

(defun ellama-tools--agent-finish (session buffer)
  "Restore SESSION tools and append a user heading in BUFFER."
  (ellama-tools--agent-restore-tools session)
  (ellama-tools--agent-append-user-header
   (ellama-tools--agent-session-buffer session buffer)))

(defun ellama-tools-agent-active-p (session)
  "Return non-nil when SESSION has a running plan-and-act loop."
  (when-let* ((state (ellama-tools--agent-state session)))
    (and (not (plist-get state :completed))
         (not (memq (plist-get state :phase) '(done blocked))))))

(defun ellama-tools--agent-active-session ()
  "Return active session for plan-and-act controller tools."
  (or (ellama-tools--active-session)
      ellama--current-session))

(defun ellama-tools-agent-submit-plan-tool (plan)
  "Submit plan-and-act PLAN."
  (let* ((session (ellama-tools--agent-active-session))
         (state (ellama-tools--agent-state session))
         (steps (ellama-tools--agent-parse-plan plan)))
    (unless (and (ellama-session-p session) state)
      (error "No active plan-and-act session"))
    (unless steps
      (error "Plan must contain at least one checklist or numbered item"))
    (setq state (ellama-tools--agent-state-put state :plan steps))
    (setq state (ellama-tools--agent-state-put state :phase 'acting))
    (ellama-tools--agent-put-state session state)
    (ellama-tools--agent-insert-state session nil "Plan")
    "Plan accepted. Continue with the first pending item."))

(defun ellama-tools-agent-update-plan-tool (plan &optional status)
  "Update current plan-and-act PLAN with optional STATUS."
  (let* ((session (ellama-tools--agent-active-session))
         (state (ellama-tools--agent-state session))
         (steps (ellama-tools--agent-parse-plan plan)))
    (unless (and (ellama-session-p session) state)
      (error "No active plan-and-act session"))
    (unless steps
      (error "Plan update must contain at least one checklist or numbered item"))
    (setq state (ellama-tools--agent-state-put state :plan steps))
    (setq state (ellama-tools--agent-state-put state :phase 'acting))
    (when status
      (setq state (ellama-tools--agent-state-put state :status status)))
    (ellama-tools--agent-put-state session state)
    (ellama-tools--agent-insert-state session nil "Status")
    "Plan updated."))

(defun ellama-tools-agent-report-result-tool (result)
  "Report final plan-and-act RESULT."
  (let* ((session (ellama-tools--agent-active-session))
         (state (ellama-tools--agent-state session)))
    (unless (and (ellama-session-p session) state)
      (error "No active plan-and-act session"))
    (setq state (ellama-tools--agent-state-put state :phase 'done))
    (setq state (ellama-tools--agent-state-put state :completed t))
    (setq state (ellama-tools--agent-state-put state :result result))
    (ellama-tools--agent-put-state session state)
    (ellama-tools--agent-insert-state session nil "Done")
    (ellama-tools--agent-finish session nil)
    "Result received. Plan-and-act loop completed."))

(defun ellama-tools--agent-system-message (system)
  "Return plan-and-act system message appended to SYSTEM."
  (format
   "%s\n\nPLAN AND ACT INSTRUCTIONS:\n\
You are running inside Ellama's plan-and-act controller. The controller gives \
you three extra tools for this session:

- `agent_submit_plan`: call this once after you have a concrete checklist. Do \
not start making changes before submitting a plan unless you need read-only \
inspection to craft it.
- `agent_update_plan`: call this after meaningful progress, especially when a \
checklist item moves to in-progress or done.
- `agent_report_result`: call this exactly once when the task is complete.

Do not ask the user unless you are truly blocked. Execute the checklist step by \
step and keep it current. If the provider or model cannot use tools, emit \
exactly one BEGIN_ELLAMA_AGENT_STATE block with phase, plan, and optional \
result/blocked fields instead."
   (or system "")))

(defun ellama-tools--agent-prompt (state)
  "Return controller prompt for plan-and-act STATE."
  (let ((plan (ellama-tools--agent-render-plan state))
        (next (ellama-tools--agent-next-step state)))
    (string-join
     (delq nil
           (list
            (if (plist-get state :plan)
                ellama-tools-agent-continue-prompt
              ellama-tools-agent-planning-prompt)
            (unless (string-empty-p plan)
              (concat "Current plan state:\n" plan))
            (when next
              (format "Next pending item: %s" (plist-get next :title)))))
     "\n\n")))

(defun ellama-tools--insert-agent-prompt (buffer prompt)
  "Insert controller PROMPT into BUFFER and return insertion point."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      (unless (bobp)
        (unless (bolp)
          (insert "\n"))
        (unless (save-excursion
                  (forward-line -1)
                  (looking-at-p "[[:space:]]*$"))
          (insert "\n")))
      (insert (ellama-get-nick-prefix-for-mode)
              " Agent controller:\n"
              (ellama--fill-long-lines prompt)
              "\n\n"
              (ellama-get-nick-prefix-for-mode)
              " " ellama-assistant-nick ":\n")
      (ellama--scroll buffer (point))
      (point))))

(defun ellama-tools--agent-apply-text-update (session text)
  "Apply fallback state update parsed from TEXT to SESSION."
  (when-let* ((state (ellama-tools--agent-state session))
              (update (ellama-tools--agent-state-from-text text)))
    (let ((merged (ellama-tools--agent-merge-state-update state update)))
      (ellama-tools--agent-put-state session merged)
      merged)))

(defun ellama-tools--agent-loop-handler (text &optional session buffer system error-cb)
  "Continue plan-and-act SESSION loop after TEXT in BUFFER with SYSTEM message.
ERROR-CB is called on non-tool LLM errors to track consecutive failures."
  (let* ((session (or session ellama--current-session))
         (parsed (and (ellama-session-p session)
                      (ellama-tools--agent-apply-text-update session text)))
         (state (and (ellama-session-p session)
                     (ellama-tools--agent-state session)))
         (buffer (ellama-tools--agent-session-buffer session buffer))
         (done (plist-get state :completed))
         (phase (plist-get state :phase))
         (steps (or (plist-get state :step-count) 0))
         (max (or (plist-get state :max-steps)
                  ellama-tools-agent-default-max-steps))
         (tools (and (ellama-session-p session)
                     (plist-get (ellama-session-extra session) :tools)))
         (system (or system (plist-get state :system))))
    (when parsed
      (ellama-tools--agent-insert-state
       session buffer (if (plist-get parsed :plan) "Plan" "Status")))
    (when (and (stringp text)
               (plist-get state :consecutive-error-count))
      (setq state
            (ellama-tools--agent-state-put
             state :consecutive-error-count 0))
      (ellama-tools--agent-put-state session state))
    (cond
     ((not (ellama-session-p session))
      (message "Plan-and-act session is not available."))
     (done
      (message "Plan-and-act loop finished.")
      (ellama-tools--agent-finish session buffer))
     ((eq phase 'done)
      (setq state (ellama-tools--agent-state-put state :completed t))
      (ellama-tools--agent-put-state session state)
      (ellama-tools--agent-insert-state session buffer "Done")
      (ellama-tools--agent-finish session buffer))
     ((eq phase 'blocked)
      (ellama-tools--agent-insert-state session buffer "Blocked")
      (ellama-tools--agent-finish session buffer))
     ((>= steps max)
      (setq state (ellama-tools--agent-state-put state :phase 'blocked))
      (setq state (ellama-tools--agent-state-put
                   state :blocked (format "Max steps (%d) reached." max)))
      (ellama-tools--agent-put-state session state)
      (ellama-tools--agent-insert-state session buffer "Blocked")
      (ellama-tools--agent-finish session buffer))
     ((ellama-tools--agent-plan-complete-p state)
      (setq state (ellama-tools--agent-state-put state :phase 'done))
      (setq state (ellama-tools--agent-state-put state :completed t))
      (ellama-tools--agent-put-state session state)
      (ellama-tools--agent-insert-state session buffer "Done")
      (ellama-tools--agent-finish session buffer))
     (t
      (setq state (ellama-tools--agent-state-put state :step-count (1+ steps)))
      (ellama-tools--agent-put-state session state)
      (let* ((prompt (ellama-tools--agent-prompt state))
             (point (when (buffer-live-p buffer)
                      (ellama-tools--insert-agent-prompt buffer prompt))))
        (apply
         #'ellama-stream
         prompt
         (append
          (list :buffer buffer
                :session session
                :tools tools
                :system system
                :on-error error-cb
                :on-done
                (ellama-tools--make-agent-loop-handler
                 session buffer system))
          (when point
            (list :point point)))))))))

(defun ellama-tools--make-agent-loop-handler (session &optional buffer system)
  "Return plan-and-act loop handler for SESSION, BUFFER and SYSTEM."
  (let ((session session))
    (lambda (text)
      (ellama-tools--agent-loop-handler
       text session buffer system
       (ellama-tools--make-agent-error-callback session)))))

(declare-function ellama--session-compact "ellama" (session &rest args))
(declare-function ellama--session-extra-get "ellama" (session key))

(defun ellama-tools--agent-error-message (err)
  "Return human-readable message for agent loop ERR."
  (cond
   ((stringp err) err)
   ((and (consp err) (symbolp (car err)))
    (error-message-string err))
   (t (format "%s" err))))

(defun ellama-tools--continue-agent-after-error (session)
  "Continue plan-and-act SESSION after an LLM error."
  (ellama-tools--agent-loop-handler
   nil session nil nil
   (ellama-tools--make-agent-error-callback session)))

(defun ellama-tools--compact-and-continue-agent (session)
  "Compact plan-and-act SESSION and continue the loop."
  (let (continued)
    (cl-labels
        ((continue ()
           (unless continued
             (setq continued t)
             (if (ellama--session-extra-get
                  session :auto-compact-last-error)
                 (let* ((error (ellama--session-extra-get
                                session :auto-compact-last-error))
                        (state (ellama-tools--agent-state session))
                        (buffer (ellama-tools--agent-session-buffer
                                 session nil)))
                   (message "Agent loop stopped after compaction error: %s"
                            error)
                   (when state
                     (setq state
                           (ellama-tools--agent-state-put
                            state :phase 'blocked))
                     (setq state
                           (ellama-tools--agent-state-put
                            state :blocked
                            (format "Context compaction failed: %s" error)))
                     (ellama-tools--agent-put-state session state)
                     (ellama-tools--agent-insert-state
                      session buffer "Blocked"))
                   (ellama-tools--agent-finish session buffer))
               (ellama-tools--continue-agent-after-error session)))))
      (unless (ellama--session-compact
               session :automatic t :on-done #'continue)
        (when (ellama--session-extra-get
               session :auto-compact-last-error)
          (continue))))))

(defun ellama-tools--make-agent-error-callback (session)
  "Return error callback for plan-and-act loop of SESSION.
Increments consecutive-error-count; if it reaches 2, compacts the session
and restarts the loop."
  (let ((session session))
    (lambda (err)
      (let* ((extra (ellama-session-extra session))
             (state (plist-get extra :agent-loop))
             (message (ellama-tools--agent-error-message err)))
        (unless (and state (plist-get state :completed))
          (let* ((count (or (plist-get state :consecutive-error-count) 0))
                 (new-count (1+ count)))
            (message "Agent loop error: %s (consecutive: %d)"
                     message new-count)
            (if (>= new-count 2)
                (progn
                  (message "Compacting session after repeated errors...")
                  (setq state
                        (ellama-tools--agent-state-put
                         state :consecutive-error-count new-count))
                  (ellama-tools--agent-put-state session state)
                  (ellama-tools--compact-and-continue-agent session))
              (setq state
                    (ellama-tools--agent-state-put
                     state :consecutive-error-count new-count))
              (ellama-tools--agent-put-state session state)
              (ellama-tools--continue-agent-after-error session))))))))

(defun ellama-tools-start-plan-and-act
    (session buffer prompt &optional system tools max-steps)
  "Initialize plan-and-act state for SESSION in BUFFER.
PROMPT is the original user task.  SYSTEM is the base system message.
TOOLS is the tool list to extend with controller tools.  MAX-STEPS limits
automatic continuations."
  (unless (ellama-session-p session)
    (error "No session for plan-and-act loop"))
  (let* ((extra (ellama-session-extra session))
         (had-tools (and (plistp extra) (plist-member extra :tools)))
         (original-tools (and (plistp extra) (plist-get extra :tools)))
         (combined-tools
          (ellama-tools--agent-combine-tools
           (or tools original-tools ellama-tools-enabled)))
         (agent-system (ellama-tools--agent-system-message system))
         (state (list :phase 'planning
                      :plan nil
                      :task prompt
                      :step-count 0
                      :consecutive-error-count 0
                      :max-steps (or max-steps
                                     ellama-tools-agent-default-max-steps)
                      :completed nil
                      :system agent-system
                      :had-original-tools had-tools
                      :original-tools original-tools)))
    (ellama-tools--set-session-extra
     session
     (ellama-tools--session-extra-with
      session
      :agent-loop state
      :tools combined-tools))
    (ellama-tools--agent-insert-note
     buffer "Status" "Planning started.")
    (list :system agent-system
          :tools combined-tools
          :on-error (ellama-tools--make-agent-error-callback session)
          :on-done (ellama-tools--make-agent-loop-handler
                    session buffer agent-system))))

(defun ellama-tools-resume-plan-and-act
    (session buffer &optional system tools)
  "Return stream args needed to resume SESSION plan-and-act loop in BUFFER.
SYSTEM and TOOLS can override the stored runtime values."
  (unless (ellama-tools-agent-active-p session)
    (error "No active plan-and-act loop"))
  (let* ((state (ellama-tools--agent-state session))
         (agent-system (or system
                           (plist-get state :system)
                           (ellama-tools--agent-system-message nil)))
         (combined-tools
          (ellama-tools--agent-combine-tools
           (or tools
               (plist-get (ellama-session-extra session) :tools)
               ellama-tools-enabled))))
    (setq state (ellama-tools--agent-state-put state :system agent-system))
    (ellama-tools--agent-put-state session state)
    (ellama-tools--set-session-extra
     session
     (ellama-tools--session-extra-with session :tools combined-tools))
    (list :system agent-system
          :tools combined-tools
          :on-error (ellama-tools--make-agent-error-callback session)
          :on-done (ellama-tools--make-agent-loop-handler
                    session buffer agent-system))))

(defun ellama-tools--insert-subagent-prompt (buffer description)
  "Insert sub-agent DESCRIPTION into BUFFER as a main-agent turn.
Return insertion point for sub-agent response."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      (let ((prefix (ellama-get-nick-prefix-for-mode)))
        (unless (bobp)
          (unless (bolp)
            (insert "\n"))
          (unless (save-excursion
                    (forward-line -1)
                    (looking-at-p "[[:space:]]*$"))
            (insert "\n")))
        (insert prefix " Main agent:\n"
                (ellama--fill-long-lines description) "\n\n"
                prefix " " ellama-assistant-nick ":\n")
        (ellama--scroll buffer (point))
        (point)))))

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

(defun ellama-tools--subagent-loop-state ()
  "Return a fresh sub-agent tool loop state plist."
  (list :tool-history nil
        :loop-detected nil
        :loop-reason ""
        :loop-recovery-attempted nil
        :loop-recovery-count 0
        :loop-recovery-reason ""))

(defun ellama-tools--subagent-complete (session result)
  "Complete sub-agent SESSION with RESULT when it is still active."
  (when-let* (((ellama-session-p session))
              (extra (ellama-session-extra session))
              ((not (plist-get extra :task-completed))))
    (ellama-tools--set-session-extra
     session
     (plist-put (copy-sequence extra) :task-completed t))
    (when-let* ((callback (plist-get extra :result-callback))
                ((functionp callback)))
      (funcall callback result))))

(defun ellama-tools--subagent-loop-recovery-message
    (name args result repeat-count)
  "Return recovery guidance for repeated tool NAME with ARGS.
RESULT is the latest tool result.  REPEAT-COUNT is the repeated call count."
  (concat
   (format
    "LOOP RECOVERY: `%s` was called %d times with identical arguments.\n"
    name repeat-count)
   (format "Do not call `%s` again with these exact arguments: %S\n"
           name args)
   (format "Latest tool result:\n%s\n\n" result)
   (if (equal name "edit_file")
       "Next action: read the exact current file text with `read_file` \
or `lines_range`, copy the exact oldcontent from that output, then call \
`edit_file` with different oldcontent.  If you cannot identify an exact \
replacement, call `report_result` with the blocker."
     "Next action: choose a different tool call or different arguments \
based on the latest result.  If no different useful action exists, call \
`report_result` with the blocker.")))

(defun ellama-tools--subagent-trace-tool-call
    (session name args result)
  "Record sub-agent tool call NAME with ARGS and RESULT for SESSION.
Return recovery guidance or loop termination text when it should replace
RESULT for the agent."
  (when (and ellama-tools-subagent-loop-detection-enabled
             (ellama-session-p session))
    (let* ((extra (ellama-session-extra session))
           (loop-state (or (plist-get extra :tool-loop-state)
                           (ellama-tools--subagent-loop-state)))
           (tool-history (or (plist-get loop-state :tool-history) (list)))
           (threshold
            (max 2 ellama-tools-subagent-loop-detection-repeated-threshold))
           replacement)
      (unless (plist-get extra :task-completed)
        (let* ((repeated
                (seq-take-while
                 (lambda (history-entry)
                   (and (equal (plist-get history-entry :name) name)
                        (equal (plist-get history-entry :args) args)))
                 tool-history))
               (repeat-count (1+ (length repeated))))
          (when (>= repeat-count threshold)
            (let* ((recovery-count
                    (or (plist-get loop-state :loop-recovery-count) 0))
                   (loop-reason
                    (format
                     "Loop detected: tool %s called %d times with identical args"
                     name repeat-count)))
              (if (= repeat-count threshold)
                  (progn
                    (setq replacement
                          (ellama-tools--subagent-loop-recovery-message
                           name args result repeat-count))
                    (setq loop-state
                          (plist-put loop-state
                                     :loop-recovery-attempted t))
                    (setq loop-state
                          (plist-put loop-state
                                     :loop-recovery-count
                                     (1+ recovery-count)))
                    (setq loop-state
                          (plist-put loop-state
                                     :loop-recovery-reason loop-reason)))
                (setq loop-state
                      (plist-put loop-state :loop-detected t))
                (setq loop-state
                      (plist-put loop-state :loop-reason loop-reason))
                (setq replacement loop-reason)
                (ellama-tools--subagent-complete session loop-reason)))))
        (push (list :name name :args args) tool-history)
        (let ((max-traces ellama-tools-subagent-loop-detection-max-traces))
          (when (> (length tool-history) max-traces)
            (setf (nthcdr max-traces tool-history) nil)))
        (setq loop-state (plist-put loop-state :tool-history tool-history))
        (ellama-tools--set-session-extra
         session
         (ellama-tools--session-extra-with
          session :tool-loop-state loop-state)))
      replacement)))

(defun ellama-tools--subagent-tool-error-result (name err)
  "Return a tool result for sub-agent tool NAME error ERR."
  (format
   "Tool `%s` failed: %s\n\nNext action: choose a different tool call or \
different arguments.  If no different useful action exists, call \
`report_result` with the blocker."
   name
   (error-message-string err)))

(defun ellama-tools--wrap-subagent-tool (tool session)
  "Return TOOL wrapped with sub-agent loop detection for SESSION."
  (let* ((wrapped-tool (copy-sequence tool))
         (name (llm-tool-name tool))
         (function (llm-tool-function tool))
         (async (llm-tool-async tool)))
    (setf
     (llm-tool-function wrapped-tool)
     (lambda (&rest args)
       (if (and async args (functionp (car args)))
           (let ((callback (car args))
                 (call-args (cdr args)))
             (condition-case err
                 (apply
                  function
                  (lambda (result)
                    (funcall
                     callback
                     (or (ellama-tools--subagent-trace-tool-call
                          session name call-args result)
                         result)))
                  call-args)
               (error
                (let ((result
                       (ellama-tools--subagent-tool-error-result name err)))
                  (funcall
                   callback
                   (or (ellama-tools--subagent-trace-tool-call
                        session name call-args result)
                       result))
                  nil))))
         (let ((result
                (condition-case err
                    (apply function args)
                  (error
                   (ellama-tools--subagent-tool-error-result name err)))))
           (or (ellama-tools--subagent-trace-tool-call
                session name args result)
               result)))))
    wrapped-tool))

(defun ellama-tools--wrap-subagent-tools (tools session)
  "Return TOOLS wrapped with sub-agent loop detection for SESSION."
  (mapcar (lambda (tool)
            (ellama-tools--wrap-subagent-tool tool session))
          tools))

(defun ellama-tools--subagent-system-message (system-msg)
  "Return subagent system message from SYSTEM-MSG."
  (format
   "%s\n\nINSTRUCTIONS:\n\
Work step-by-step. Use tools when needed.\n\
When the task is COMPLETE you MUST call `report_result` exactly once."
   (or system-msg "")))

(defun ellama-tools--subagent-buffer (session &optional buffer)
  "Return live subagent BUFFER for SESSION."
  (or (when-let* (((ellama-session-p session))
                  (extra (ellama-session-extra session))
                  (uid (plist-get extra :uid))
                  (session-buffer (ellama-get-session-buffer uid)))
        (and (buffer-live-p session-buffer) session-buffer))
      (when-let* (((ellama-session-p session))
                  (session-buffer
                   (ellama-get-session-buffer (ellama-session-id session))))
        (and (buffer-live-p session-buffer) session-buffer))
      (and (buffer-live-p buffer) buffer)))

(defun ellama--subagent-loop-handler (text &optional session buffer system error-cb)
  "Continue subagent SESSION loop after TEXT in BUFFER with SYSTEM message.
ERROR-CB is called on non-tool LLM errors to track consecutive failures."
  (let* ((session (or session ellama--current-session))
         (extra (and (ellama-session-p session)
                     (ellama-session-extra session)))
         (done (plist-get extra :task-completed))
         (steps (or (plist-get extra :step-count) 0))
         (max (or (plist-get extra :max-steps)
                  ellama-tools-subagent-default-max-steps))
         (callback (plist-get extra :result-callback))
         (tools (plist-get extra :tools))
         (system (or system (plist-get extra :system)))
         (buffer (ellama-tools--subagent-buffer session buffer)))
    (when (and (stringp text)
               (plist-get extra :consecutive-error-count))
      (setq extra (plist-put extra :consecutive-error-count 0))
      (ellama-tools--set-session-extra session extra))
    (cond
     ((not (ellama-session-p session))
      (message "Subagent session is not available."))
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
      (let ((point (when (buffer-live-p buffer)
                     (ellama-tools--insert-subagent-prompt
                      buffer ellama-tools-subagent-continue-prompt))))
        (apply
         #'ellama-stream
         ellama-tools-subagent-continue-prompt
         (append
          (list :buffer buffer
                :session session
                :tools tools
                :system system
                :on-error error-cb
                :on-done
                (ellama-tools--make-subagent-loop-handler
                 session buffer system))
          (when point
            (list :point point)))))))))

(defun ellama-tools--make-subagent-loop-handler
    (session &optional buffer system)
  "Return subagent loop handler for SESSION, BUFFER and SYSTEM."
  (let ((session session))
    (lambda (text)
      (ellama--subagent-loop-handler
       text session buffer system
       (ellama-tools--make-subagent-error-callback session)))))

(defun ellama-tools--continue-subagent-after-error (session)
  "Continue subagent SESSION after an LLM error."
  (ellama--subagent-loop-handler
   nil session nil nil
   (ellama-tools--make-subagent-error-callback session)))

(defun ellama-tools--compact-and-continue-subagent (session)
  "Compact subagent SESSION and continue the loop."
  (let (continued)
    (cl-labels
        ((continue ()
           (unless continued
             (setq continued t)
             (if (ellama--session-extra-get
                  session :auto-compact-last-error)
                 (message "Subagent stopped after compaction error: %s"
                          (ellama--session-extra-get
                           session :auto-compact-last-error))
               (ellama-tools--continue-subagent-after-error session)))))
      (unless (ellama--session-compact
               session :automatic t :on-done #'continue)
        (when (ellama--session-extra-get
               session :auto-compact-last-error)
          (continue))))))

(defun ellama-tools--make-subagent-error-callback (session)
  "Return error callback for subagent loop of SESSION.
Increments consecutive-error-count; if it reaches 2, compacts the session
and restarts the loop."
  (let ((session session))
    (lambda (err)
      (let* ((extra (ellama-session-extra session))
             (count (or (plist-get extra :consecutive-error-count) 0))
             (new-count (1+ count))
             (message (ellama-tools--agent-error-message err)))
        (message "Subagent error: %s (consecutive: %d)"
                 message new-count)
        (if (>= new-count 2)
            (progn
              (message "Compacting subagent session after repeated errors...")
              (ellama-tools--set-session-extra
               session (plist-put extra :consecutive-error-count new-count))
              (ellama-tools--compact-and-continue-subagent session))
          (ellama-tools--set-session-extra
           session (plist-put extra :consecutive-error-count new-count))
          (ellama-tools--continue-subagent-after-error session))))))

(defun ellama-tools-task-tool
    (callback &optional description role template template-base arguments)
  "Delegate DESCRIPTION or rendered TEMPLATE to a sub-agent asynchronously.

CALLBACK   – function called once with the result string.
ROLE       – role key from `ellama-tools-subagent-roles'.
TEMPLATE   – optional template name or relative path.
TEMPLATE-BASE – optional base directory for relative TEMPLATE.
ARGUMENTS  – object with template substitution values."
  (let ((role-key (if (assoc role ellama-tools-subagent-roles)
                      role
                    "general")))
    (condition-case err
        (let* ((description
                (ellama-tools--task-description
                 description template template-base role-key arguments))
               (parent-id ellama--current-session-id)

               (provider (ellama-tools--provider-for-role role-key))
               (role-cfg   (cdr (assoc role-key ellama-tools-subagent-roles)))
               (system-msg (plist-get role-cfg :system))
               (subagent-system
                (ellama-tools--subagent-system-message system-msg))

               (steps-limit ellama-tools-subagent-default-max-steps)

               ;; ---- create ephemeral worker session ----
               (worker (ellama-new-session provider description t))
               (worker-buffer (ellama-get-session-buffer
                               (ellama-session-id worker)))
               (worker-point (ellama-tools--insert-subagent-prompt
                              worker-buffer description))

               ;; ---- resolve tools for role ----
               (role-tools (ellama-tools--for-role role-key))
               (subagent-tools
                (ellama-tools--wrap-subagent-tools role-tools worker))

               ;; ---- dynamic report_result tool ----
               (report-tool
                (apply #'llm-make-tool
                       (ellama-tools--make-report-result-tool
                        callback worker)))

               ;; IMPORTANT: report tool must be first (termination tool priority)
               (all-tools (cons report-tool subagent-tools)))

          ;; ============================================================
          ;; Initialize session state (single source of truth)
          ;; ============================================================

          (ellama-tools--set-session-extra
           worker
           (ellama-tools--session-extra-with
            worker
            :parent-session parent-id
            :role role-key
            :tools all-tools
            :result-callback callback
            :task-completed nil
            :step-count 0
            :consecutive-error-count 0
            :max-steps steps-limit
            :tool-loop-state (ellama-tools--subagent-loop-state)
            :system subagent-system))

          ;; ============================================================
          ;; Start the agent loop
          ;; ============================================================

          (ellama-stream
           description
           :buffer worker-buffer
           :point worker-point
           :session worker
           :on-error (ellama-tools--make-subagent-error-callback worker)
           :on-done (ellama-tools--make-subagent-loop-handler
                     worker worker-buffer subagent-system)
           :tools all-tools
           :system subagent-system)

          ;; ============================================================
          ;; Immediate response to parent LLM (async contract)
          ;; ============================================================

          (message "Subtask started (session %s, role %s). Waiting for result via callback."
                   (ellama-session-id worker)
                   role-key)
          nil)
      (error
       (funcall callback (error-message-string err))
       nil))))

(ellama-tools-define-tool
 `(:function ellama-tools-task-tool
             :name "task"
             :async t
             :description "Delegate a task to a sub-agent.
Use either a free-form description or a template with arguments.  When using
a template, pass arguments as an object whose keys exactly match template
placeholders.  If validation fails, retry using the returned hint."
             :args ((:name "description" :type string
                           :description "Free-form task prompt. Optional when template is provided.")
                    (:name "role" :type string
                           :enum ,(seq--into-vector (mapcar #'car ellama-tools-subagent-roles)))
                    (:name "template" :type string
                           :description "Template name or relative path.")
                    (:name "template_base" :type string
                           :description "Base directory for resolving a relative template path.")
                    (:name "arguments" :type object
                           :description "Template substitution values keyed by placeholder name."))))

(provide 'ellama-tools)
;;; ellama-tools.el ends here
