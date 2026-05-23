;;; ellama-eval.el --- Tool-use evaluation harness -*- lexical-binding: t; package-lint-main-file: "ellama.el"; -*-

;; Copyright (C) 2023-2026  Free Software Foundation, Inc.

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
;; Experimental evaluation harness for comparing tool profiles with agents.
;;

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'ellama)
(require 'ellama-tools)

(defgroup ellama-eval nil
  "Evaluation harness for Ellama agent and tool experiments."
  :group 'ellama)

(defcustom ellama-eval-timeout-seconds 180
  "Maximum time to wait for one evaluation case."
  :type 'integer
  :group 'ellama-eval)

(defcustom ellama-eval-keep-workspaces nil
  "Keep temporary evaluation workspaces after each run."
  :type 'boolean
  :group 'ellama-eval)

(defcustom ellama-eval-default-max-steps 12
  "Default sub-agent step limit for evaluation cases."
  :type 'integer
  :group 'ellama-eval)

(cl-defstruct ellama-eval--run-state
  "Mutable state for one active evaluation run."
  status result trace worker started-at workspace case profile callback
  timeout-timer)

(defvar ellama-eval--active-run nil
  "Current evaluation run state.
Only one evaluation run is supported at a time.")

(defvar ellama-eval-last-results nil
  "Results from the most recent interactive evaluation suite.")

(defconst ellama-eval--summary-buffer-name "*Ellama Eval Summary*"
  "Name of the interactive evaluation summary buffer.")

(defconst ellama-eval--base-tool-names
  '("read_file" "lines_range" "grep" "grep_in_file"
    "directory_tree" "project_root")
  "Tool names included in every hypothesis profile.")

(defconst ellama-eval-hypothesis-profiles
  '(baseline numbered-read line-edit numbered-line-edit balanced-edit)
  "Profiles used by hypothesis suites.")

(defconst ellama-eval--coder-system
  "You are a careful coding agent. Inspect the project, make the requested \
change, then report the final result."
  "System prompt for edit-oriented evaluation cases.")

(defconst ellama-eval--explorer-system
  "You are a careful code explorer. Inspect the project and answer the \
question precisely without changing files."
  "System prompt for read-oriented evaluation cases.")

(defconst ellama-eval-hypothesis-cases
  `((:id "edit-replace-function-body"
         :suite edit
         :system ,ellama-eval--coder-system
         :prompt
         "Update `ellama-eval-score-value` so it clamp VALUE to LIMIT with `min`, \
then return the clamped value multiplied by WEIGHT."
         :files
         (("sample.el" . "(defun ellama-eval-score-value (value limit weight)\n  \"Return VALUE scaled by WEIGHT, clamped to LIMIT.\"\n  (* value weight))\n"))
         :expected-files
         (("sample.el" . "(defun ellama-eval-score-value (value limit weight)\n  \"Return VALUE scaled by WEIGHT, clamped to LIMIT.\"\n  (* (min value limit) weight))\n"))
         :ignore-docstrings t
         :file-regexps
         (("sample.el" . ("\"[^\"]*VALUE[^\"]*\""))
          ("sample.el" . ("\"[^\"]*LIMIT[^\"]*\""))
          ("sample.el" . ("\"[^\"]*WEIGHT[^\"]*\""))))
    (:id "edit-update-target-branch"
         :suite edit
         :system ,ellama-eval--coder-system
         :prompt
         "In `ellama-eval-route-status`, only the `stale` branch should return \
`retry`. Leave the `failed` branch unchanged."
         :files
         (("router.el" . "(defun ellama-eval-route-status (status)\n  (cond\n   ((eq status 'ready) 'run)\n   ((eq status 'stale) 'skip)\n   ((eq status 'failed) 'skip)\n   (t 'ignore)))\n"))
         :expected-files
         (("router.el" . "(defun ellama-eval-route-status (status)\n  (cond\n   ((eq status 'ready) 'run)\n   ((eq status 'stale) 'retry)\n   ((eq status 'failed) 'skip)\n   (t 'ignore)))\n")))
    (:id "edit-remove-obsolete-binding"
         :suite edit
         :system ,ellama-eval--coder-system
         :prompt
         "Remove the obsolete local variable `legacy-mode` from \
`ellama-eval-build-request` and keep the returned plist behavior the same."
         :files
         (("request.el" . "(defun ellama-eval-build-request (payload kind)\n  (let ((legacy-mode nil)\n        (request-id (format \"%s-%s\" kind payload)))\n    (list :id request-id :payload payload)))\n"))
         :expected-files
         (("request.el" . "(defun ellama-eval-build-request (payload kind)\n  (let ((request-id (format \"%s-%s\" kind payload)))\n    (list :id request-id :payload payload)))\n")))
    (:id "edit-expand-guard-clause"
         :suite edit
         :system ,ellama-eval--coder-system
         :prompt
         "Update `ellama-eval-guarded-join` so it reject both nil ITEMS and \
non-list ITEMS before calling `string-join`."
         :files
         (("strings.el" . "(defun ellama-eval-guarded-join (items)\n  (if (null items)\n      \"\"\n    (string-join items \", \")))\n"))
         :expected-files
         (("strings.el" . "(defun ellama-eval-guarded-join (items)\n  (if (or (null items) (not (listp items)))\n      \"\"\n    (string-join items \", \")))\n")))
    (:id "edit-nested-plist-construction"
         :suite edit
         :system ,ellama-eval--coder-system
         :prompt
         "Update `ellama-eval-normalize-event` so the returned plist include \
`:kind` from PAYLOAD between `:id` and `:name`. Keep all guards unchanged."
         :files
         (("event.el" . "(defun ellama-eval-normalize-event (event)\n  (let ((payload (plist-get event :payload)))\n    (when (and payload\n               (listp payload))\n      (let ((meta (plist-get payload :meta)))\n        (when (and meta\n                   (plist-get meta :enabled))\n          (list :id (plist-get event :id)\n                :name (plist-get payload :name)))))))\n"))
         :expected-files
         (("event.el" . "(defun ellama-eval-normalize-event (event)\n  (let ((payload (plist-get event :payload)))\n    (when (and payload\n               (listp payload))\n      (let ((meta (plist-get payload :meta)))\n        (when (and meta\n                   (plist-get meta :enabled))\n          (list :id (plist-get event :id)\n                :kind (plist-get payload :kind)\n                :name (plist-get payload :name)))))))\n")))
    (:id "edit-nested-branch-plists"
         :suite edit
         :system ,ellama-eval--coder-system
         :prompt
         "Update `ellama-eval-route-request` so accepted Bearer requests return \
`:source 'bearer` and accepted public requests return `:source 'public`. Keep \
the rejected and invalid branches unchanged."
         :files
         (("request-router.el" . "(defun ellama-eval-route-request (request)\n  (let* ((headers (plist-get request :headers))\n         (auth (alist-get \"authorization\" headers nil nil #'string=)))\n    (cond\n     ((and auth\n           (string-prefix-p \"Bearer \" auth))\n      (let ((token (substring auth 7)))\n        (if (string-empty-p token)\n            (list :status 'invalid\n                  :reason \"empty token\")\n          (list :status 'accepted\n                :token token))))\n     ((plist-get request :public)\n      (list :status 'accepted\n            :token nil))\n     (t\n      (list :status 'rejected\n            :reason \"missing token\")))))\n"))
         :expected-files
         (("request-router.el" . "(defun ellama-eval-route-request (request)\n  (let* ((headers (plist-get request :headers))\n         (auth (alist-get \"authorization\" headers nil nil #'string=)))\n    (cond\n     ((and auth\n           (string-prefix-p \"Bearer \" auth))\n      (let ((token (substring auth 7)))\n        (if (string-empty-p token)\n            (list :status 'invalid\n                  :reason \"empty token\")\n          (list :status 'accepted\n                :source 'bearer\n                :token token))))\n     ((plist-get request :public)\n      (list :status 'accepted\n            :source 'public\n            :token nil))\n     (t\n      (list :status 'rejected\n            :reason \"missing token\")))))\n")))
    (:id "edit-nested-backquote-template"
         :suite edit
         :system ,ellama-eval--coder-system
         :prompt
         "Update `ellama-eval-build-dashboard` so each item plist include \
`:status` from ROW, defaulting to `unknown`, between `:id` and `:label`."
         :files
         (("dashboard.el" . "(defun ellama-eval-build-dashboard (rows)\n  (let ((visible (seq-filter\n                  (lambda (row)\n                    (plist-get row :visible))\n                  rows)))\n    `(:count ,(length visible)\n      :items ,(mapcar\n                (lambda (row)\n                  `(:id ,(plist-get row :id)\n                    :label ,(or (plist-get row :label) \"untitled\")))\n                visible))))\n"))
         :expected-files
         (("dashboard.el" . "(defun ellama-eval-build-dashboard (rows)\n  (let ((visible (seq-filter\n                  (lambda (row)\n                    (plist-get row :visible))\n                  rows)))\n    `(:count ,(length visible)\n      :items ,(mapcar\n                (lambda (row)\n                  `(:id ,(plist-get row :id)\n                    :status ,(or (plist-get row :status) 'unknown)\n                    :label ,(or (plist-get row :label) \"untitled\")))\n                visible))))\n")))
    (:id "edit-nested-filter-lambda"
         :suite edit
         :system ,ellama-eval--coder-system
         :prompt
         "Update `ellama-eval-active-items` so archived items are excluded in \
addition to disabled items. Keep the result mapping unchanged."
         :files
         (("items.el" . "(defun ellama-eval-active-items (items)\n  (mapcar\n   (lambda (item)\n     (let ((meta (plist-get item :meta)))\n       (list :id (plist-get item :id)\n             :label (or (plist-get meta :label)\n                        (plist-get item :fallback)))))\n   (seq-filter\n    (lambda (item)\n      (let ((flags (plist-get item :flags)))\n        (and (not (memq 'disabled flags))\n             (plist-get item :id))))\n    items)))\n"))
         :expected-files
         (("items.el" . "(defun ellama-eval-active-items (items)\n  (mapcar\n   (lambda (item)\n     (let ((meta (plist-get item :meta)))\n       (list :id (plist-get item :id)\n             :label (or (plist-get meta :label)\n                        (plist-get item :fallback)))))\n   (seq-filter\n    (lambda (item)\n      (let ((flags (plist-get item :flags)))\n        (and (not (memq 'disabled flags))\n             (not (memq 'archived flags))\n             (plist-get item :id))))\n    items)))\n")))
    (:id "edit-nested-condition-case"
         :suite edit
         :system ,ellama-eval--coder-system
         :prompt
         "Update `ellama-eval-load-record` so only the `file-error` branch \
include `:recoverable t`. Do not change the generic error branch."
         :files
         (("loader.el" . "(defun ellama-eval-load-record (reader path)\n  (condition-case err\n      (let ((record (funcall reader path)))\n        (if (and (listp record)\n                 (plist-get record :id))\n            (list :ok t\n                  :record record)\n          (list :ok nil\n                :error 'invalid-record)))\n    (file-error\n     (list :ok nil\n           :error (car err)\n           :path path))\n    (error\n     (list :ok nil\n           :error (car err)\n           :path path))))\n"))
         :expected-files
         (("loader.el" . "(defun ellama-eval-load-record (reader path)\n  (condition-case err\n      (let ((record (funcall reader path)))\n        (if (and (listp record)\n                 (plist-get record :id))\n            (list :ok t\n                  :record record)\n          (list :ok nil\n                :error 'invalid-record)))\n    (file-error\n     (list :ok nil\n           :recoverable t\n           :error (car err)\n           :path path))\n    (error\n     (list :ok nil\n           :error (car err)\n           :path path))))\n")))
    (:id "edit-nested-pcase-branch"
         :suite edit
         :system ,ellama-eval--coder-system
         :prompt
         "Update `ellama-eval-describe-message` so the event branch include \
`:source` from PAYLOAD with default `internal`. Leave the metric branch \
unchanged."
         :files
         (("message.el" . "(defun ellama-eval-describe-message (message)\n  (pcase message\n    (`(:type event :payload ,payload)\n     (let ((name (plist-get payload :name)))\n       (if name\n           (list :kind 'event\n                 :name name)\n         (list :kind 'event\n               :name \"unknown\"))))\n    (`(:type metric :payload ,payload)\n     (list :kind 'metric\n           :value (plist-get payload :value)))\n    (_\n     (list :kind 'unknown))))\n"))
         :expected-files
         (("message.el" . "(defun ellama-eval-describe-message (message)\n  (pcase message\n    (`(:type event :payload ,payload)\n     (let ((name (plist-get payload :name)))\n       (if name\n           (list :kind 'event\n                 :source (or (plist-get payload :source) 'internal)\n                 :name name)\n         (list :kind 'event\n               :source (or (plist-get payload :source) 'internal)\n               :name \"unknown\"))))\n    (`(:type metric :payload ,payload)\n     (list :kind 'metric\n           :value (plist-get payload :value)))\n    (_\n     (list :kind 'unknown))))\n")))
    (:id "edit-nested-accumulator"
         :suite edit
         :system ,ellama-eval--coder-system
         :prompt
         "Update `ellama-eval-collect-errors` so warning entries are ignored. \
Keep the order of collected non-warning errors unchanged."
         :files
         (("errors.el" . "(defun ellama-eval-collect-errors (entries)\n  (let (errors)\n    (dolist (entry entries)\n      (let ((details (plist-get entry :details)))\n        (when (and details\n                   (plist-get details :error))\n          (push (list :id (plist-get entry :id)\n                      :message (plist-get details :error))\n                errors))))\n    (nreverse errors)))\n"))
         :expected-files
         (("errors.el" . "(defun ellama-eval-collect-errors (entries)\n  (let (errors)\n    (dolist (entry entries)\n      (let ((details (plist-get entry :details)))\n        (when (and details\n                   (plist-get details :error)\n                   (not (eq (plist-get details :level) 'warning)))\n          (push (list :id (plist-get entry :id)\n                      :message (plist-get details :error))\n                errors))))\n    (nreverse errors)))\n")))
    (:id "edit-nested-state-machine"
         :suite edit
         :system ,ellama-eval--coder-system
         :prompt
         "Update `ellama-eval-next-state` so an expired active state returns \
`expired` instead of `stale`. Leave all other branches unchanged."
         :files
         (("state.el" . "(defun ellama-eval-next-state (state event)\n  (let ((kind (plist-get event :kind)))\n    (cond\n     ((eq state 'idle)\n      (if (eq kind 'start)\n          'active\n        'idle))\n     ((eq state 'active)\n      (cond\n       ((plist-get event :cancelled)\n        'cancelled)\n       ((plist-get event :expired)\n        'stale)\n       ((eq kind 'finish)\n        'done)\n       (t\n        'active)))\n     (t\n      state))))\n"))
         :expected-files
         (("state.el" . "(defun ellama-eval-next-state (state event)\n  (let ((kind (plist-get event :kind)))\n    (cond\n     ((eq state 'idle)\n      (if (eq kind 'start)\n          'active\n        'idle))\n     ((eq state 'active)\n      (cond\n       ((plist-get event :cancelled)\n        'cancelled)\n       ((plist-get event :expired)\n        'expired)\n       ((eq kind 'finish)\n        'done)\n       (t\n        'active)))\n     (t\n      state))))\n")))
    (:id "explore-locate-provider"
         :suite explore
         :system ,ellama-eval--explorer-system
         :prompt
         "Which function chooses the provider for a named role? Answer with the \
function name only."
         :files
         (("tools.el" . "(defun ellama-eval-provider-for-role (role)\n  (alist-get role ellama-eval-role-providers nil nil #'string=))\n\n(defun ellama-eval-tools-for-role (role)\n  (alist-get role ellama-eval-role-tools nil nil #'string=))\n"))
         :answer-regexps ("\\`ellama-eval-provider-for-role\\'"))
    (:id "explore-identify-read-helper"
         :suite explore
         :system ,ellama-eval--explorer-system
         :prompt
         "What helper reads the file contents before the result is sanitized? \
Answer with the helper name only."
         :files
         (("reader.el" . "(defun ellama-eval-read-file-tool (file)\n  (ellama-eval-sanitize\n   (ellama-eval-read-file-as-text file)))\n\n(defun ellama-eval-read-file-as-text (file)\n  (with-temp-buffer\n    (insert-file-contents file)\n    (buffer-string)))\n"))
         :answer-regexps ("\\`ellama-eval-read-file-as-text\\'"))
    (:id "explore-summarize-policy"
         :suite explore
         :system ,ellama-eval--explorer-system
         :prompt
         "State whether denied writes return nil or an explanatory string. Answer \
in one short sentence."
         :files
         (("policy.el" . "(defun ellama-eval-check-write (path)\n  (if (ellama-eval-path-allowed-p path)\n      nil\n    (format \"Write denied for %s\" path)))\n"))
         :answer-regexps ("string")))
  "Built-in cases for the numbered-read and line-edit hypothesis.")

(defun ellama-eval--tool-spec (name function args description)
  "Return a tool plist for NAME, FUNCTION, ARGS and DESCRIPTION."
  `(:function ,function
              :name ,name
              :args ,args
              :description ,description))

(defun ellama-eval--tool-spec-by-name (name)
  "Return the baseline tool spec identified by NAME."
  (pcase name
    ("read_file"
     (ellama-eval--tool-spec
      "read_file"
      #'ellama-tools-read-file-tool
      '((:name "file_name" :type string :description "File name.")
        (:name "mode" :type string :optional t
               :enum ["auto" "text" "image"]
               :description "Read mode: auto, text, or image."))
      "Read the file FILE_NAME."))
    ("lines_range"
     (ellama-eval--tool-spec
      "lines_range"
      #'ellama-tools-lines-range-tool
      '((:name "file_name" :type string
               :description "Name of the file to get lines from.")
        (:name "from" :type number :description "Starting line number.")
        (:name "to" :type number :description "Ending line number."))
      "Return content of file FILE_NAME lines in range FROM TO."))
    ("grep"
     (ellama-eval--tool-spec
      "grep"
      #'ellama-tools-grep-tool
      '((:name "dir" :type string :description "Directory to search in.")
        (:name "search_string" :type string
               :description "String to search for."))
      "Grep SEARCH-STRING in directory files."))
    ("grep_in_file"
     (ellama-eval--tool-spec
      "grep_in_file"
      #'ellama-tools-grep-in-file-tool
      '((:name "search_string" :type string
               :description "String to search for.")
        (:name "file" :type string :description "File to search in."))
      "Grep SEARCH-STRING in FILE."))
    ("directory_tree"
     (ellama-eval--tool-spec
      "directory_tree"
      #'ellama-tools-directory-tree-tool
      '((:name "dir" :type string
               :description "Directory path to generate tree for."))
      "Return a string representing the directory tree under DIR."))
    ("project_root"
     (ellama-eval--tool-spec
      "project_root"
      #'ellama-tools-project-root-tool
      nil
      "Return current project root directory."))
    ("edit_file"
     (ellama-eval--tool-spec
      "edit_file"
      #'ellama-tools-edit-file-tool
      '((:name "file_name" :type string :description "Name of the file.")
        (:name "oldcontent" :type string
               :description "Old content to be replaced.")
        (:name "newcontent" :type string
               :description "New content to replace with."))
      "Edit file FILE_NAME. Replace OLDCONTENT with NEWCONTENT."))
    ("edit_lines"
     (ellama-eval--tool-spec
      "edit_lines"
      #'ellama-eval-edit-lines-tool
      '((:name "file_name" :type string :description "Name of the file.")
        (:name "from" :type number :description "Starting line number.")
        (:name "to" :type number :description "Ending line number.")
        (:name "replacement" :type string
               :description "Replacement block for the line range."))
      "Replace file FILE_NAME lines FROM through TO with REPLACEMENT."))
    (_
     (error "Unknown evaluation tool %s" name))))

(defun ellama-eval--profile-numbered-read-p (profile)
  "Return non-nil when PROFILE use numbered read output."
  (memq profile '(numbered-read numbered-line-edit)))

(defun ellama-eval--profile-line-edit-p (profile)
  "Return non-nil when PROFILE use line-based editing."
  (memq profile '(line-edit numbered-line-edit)))

(defun ellama-eval--profile-balanced-edit-p (profile)
  "Return non-nil when PROFILE validate edits before applying them."
  (eq profile 'balanced-edit))

(defun ellama-eval--profile-tool-names (profile)
  "Return tool names for PROFILE."
  (unless (memq profile ellama-eval-hypothesis-profiles)
    (error "Unknown evaluation profile %S" profile))
  (append ellama-eval--base-tool-names
          (list (if (ellama-eval--profile-line-edit-p profile)
                    "edit_lines"
                  "edit_file"))))

(defun ellama-eval--replace-profile-read-specs (spec profile)
  "Return SPEC adjusted for numbered read PROFILE."
  (if (not (ellama-eval--profile-numbered-read-p profile))
      spec
    (pcase (plist-get spec :name)
      ("read_file"
       (plist-put (copy-sequence spec)
                  :function #'ellama-eval-numbered-read-file-tool))
      ("lines_range"
       (plist-put (copy-sequence spec)
                  :function #'ellama-eval-numbered-lines-range-tool))
      (_
       spec))))

(defun ellama-eval--replace-profile-edit-specs (spec profile)
  "Return SPEC adjusted for balanced edit PROFILE."
  (if (not (ellama-eval--profile-balanced-edit-p profile))
      spec
    (pcase (plist-get spec :name)
      ("edit_file"
       (plist-put (copy-sequence spec)
                  :function #'ellama-eval-balanced-edit-file-tool))
      (_
       spec))))

(defun ellama-eval--trace-tool-call (name args status result)
  "Record a tool call with NAME, ARGS, STATUS and RESULT."
  (when-let* ((state ellama-eval--active-run))
    (push (list :name name
                :args args
                :status status
                :result result
                :finished-at (float-time))
          (ellama-eval--run-state-trace state))))

(defun ellama-eval--instrument-tool-spec (spec)
  "Return SPEC with tool function wrapped for eval tracing."
  (let ((name (plist-get spec :name))
        (function (plist-get spec :function)))
    (plist-put
     (copy-sequence spec)
     :function
     (lambda (&rest args)
       (condition-case err
           (let ((result (apply function args)))
             (ellama-eval--trace-tool-call name args 'ok result)
             result)
         (error
          (ellama-eval--trace-tool-call
           name args 'error (error-message-string err))
          (signal (car err) (cdr err))))))))

(defun ellama-eval--make-profile-tools (profile)
  "Return instrumented `llm' tools for PROFILE."
  (mapcar
   (lambda (name)
     (let* ((spec (ellama-eval--tool-spec-by-name name))
            (read-spec
             (ellama-eval--replace-profile-read-specs spec profile))
            (edit-spec
             (ellama-eval--replace-profile-edit-specs read-spec profile))
            (instrumented
             (ellama-eval--instrument-tool-spec edit-spec)))
       (apply #'llm-make-tool
              (ellama-tools-wrap-with-confirm instrumented))))
   (ellama-eval--profile-tool-names profile)))

(defun ellama-eval--number-text-lines (text &optional first-line)
  "Return TEXT with line numbers starting at FIRST-LINE."
  (let ((line-number (or first-line 1))
        lines)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (not (eobp))
        (push (format "%d | %s"
                      line-number
                      (buffer-substring-no-properties
                       (line-beginning-position)
                       (line-end-position)))
              lines)
        (setq line-number (1+ line-number))
        (forward-line 1)))
    (string-join (nreverse lines) "\n")))

(defun ellama-eval-numbered-read-file-tool (file-name &optional mode)
  "Read FILE-NAME and number text output lines.
MODE follows the same contract as `ellama-tools-read-file-tool'."
  (or (ellama-tools--tool-check-file-access file-name 'read)
      (json-encode
       (if (not (file-exists-p file-name))
           (format "File %s doesn't exists." file-name)
         (pcase (ellama-tools--read-file-mode mode)
           ('auto
            (if (ellama-image-file-p file-name)
                (ellama-tools--read-image-file-tool file-name)
              (ellama-eval--number-text-lines
               (ellama-tools--read-file-as-text file-name))))
           ('text
            (ellama-eval--number-text-lines
             (ellama-tools--read-file-as-text file-name)))
           ('image
            (ellama-tools--read-image-file-tool file-name))
           (_
            (format "Unsupported read_file mode %S. Use auto, text or image."
                    mode)))))))

(defun ellama-eval-numbered-lines-range-tool (file-name from to)
  "Return numbered FILE-NAME content from line FROM through TO."
  (or (ellama-tools--tool-check-file-access file-name 'read)
      (json-encode
       (with-current-buffer (find-file-noselect file-name)
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
             (ellama-eval--number-text-lines
              (ellama-tools--sanitize-tool-text-output
               (buffer-substring-no-properties start end)
               (format "File %s" file-name))
              from)))))))

(defun ellama-eval--line-count ()
  "Return line count for the current buffer."
  (save-excursion
    (goto-char (point-max))
    (line-number-at-pos)))

(defun ellama-eval--diagnostic-location (pos)
  "Return location plist for POS in the current buffer."
  (save-excursion
    (goto-char (max (point-min) (min pos (point-max))))
    (list :pos (point)
          :line (line-number-at-pos)
          :column (current-column))))

(defun ellama-eval--syntax-diagnostic (check err)
  "Return diagnostic for CHECK and ERR at point."
  (append
   (list :check check
         :error (error-message-string err))
   (ellama-eval--diagnostic-location (point))))

(defun ellama-eval--check-parens-diagnostic ()
  "Return `check-parens' diagnostic for the current buffer."
  (condition-case err
      (let ((inhibit-message t))
        (check-parens)
        nil)
    (error
     (ellama-eval--syntax-diagnostic "check-parens" err))))

(defun ellama-eval--elisp-reader-diagnostic ()
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
              (ellama-eval--syntax-diagnostic "elisp-reader" err)))))))))

(defun ellama-eval--char-location (char pos)
  "Return plist for delimiter CHAR at POS."
  (append (list :char char)
          (ellama-eval--diagnostic-location pos)))

(defun ellama-eval--delimiter-balance-current-buffer ()
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
               (push (ellama-eval--char-location char pos) opens))
              (5
               (let ((expected (matching-paren char)))
                 (if (and opens
                          expected
                          (= expected (plist-get (car opens) :char)))
                     (pop opens)
                   (push (ellama-eval--char-location char pos)
                         unexpected)))))))
        (forward-char 1)))
    (list :opens opens :unexpected (nreverse unexpected))))

(defun ellama-eval--format-delimiter-location (entry)
  "Return formatted delimiter balance ENTRY."
  (format "%c at line %d, column %d"
          (plist-get entry :char)
          (plist-get entry :line)
          (plist-get entry :column)))

(defun ellama-eval--format-missing-closer (entry)
  "Return formatted missing closer description for ENTRY."
  (let* ((open (plist-get entry :char))
         (close (matching-paren open)))
    (format "%s for %s"
            (if close (format "%c" close) "matching closer")
            (ellama-eval--format-delimiter-location entry))))

(defun ellama-eval--format-delimiter-balance (balance)
  "Return human-readable delimiter BALANCE."
  (let ((opens (plist-get balance :opens))
        (unexpected (plist-get balance :unexpected)))
    (concat
     "Delimiter balance:\n"
     (if opens
         (concat
          "- Missing closers: "
          (string-join
           (mapcar #'ellama-eval--format-missing-closer
                   (seq-take opens 5))
           "; ")
          "\n")
       "- Missing closers: none\n")
     (if unexpected
         (concat
          "- Unexpected closers: "
          (string-join
           (mapcar #'ellama-eval--format-delimiter-location
                   (seq-take unexpected 5))
           "; ")
          "\n")
       "- Unexpected closers: none\n"))))

(defun ellama-eval--nearby-text (pos)
  "Return nearby text around POS in the current buffer."
  (save-excursion
    (let* ((safe-pos (max (point-min) (min pos (point-max))))
           (line (progn
                   (goto-char safe-pos)
                   (line-number-at-pos)))
           (column (current-column))
           (last-line (progn
                        (goto-char (point-max))
                        (line-number-at-pos)))
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

(defun ellama-eval--syntax-validation-current-buffer ()
  "Return syntax validation result for the current buffer."
  (when (fboundp 'syntax-propertize)
    (syntax-propertize (point-max)))
  (if-let* ((diagnostic (or (ellama-eval--check-parens-diagnostic)
                            (ellama-eval--elisp-reader-diagnostic))))
      (append (list :valid nil
                    :mode major-mode
                    :balance
                    (ellama-eval--delimiter-balance-current-buffer)
                    :nearby
                    (ellama-eval--nearby-text
                     (plist-get diagnostic :pos)))
              diagnostic)
    (list :valid t :mode major-mode)))

(defun ellama-eval--validate-text-in-file-buffer (file-name text)
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
              (ellama-eval--syntax-validation-current-buffer))
          (erase-buffer)
          (insert original)
          (goto-char (max (point-min) (min point-pos (point-max))))
          (set-buffer-modified-p modified))))))

(defun ellama-eval--validation-status (validation)
  "Return short status string for VALIDATION."
  (if (plist-get validation :valid)
      "balanced"
    (format "unbalanced (%s at line %d, column %d: %s)"
            (plist-get validation :check)
            (plist-get validation :line)
            (plist-get validation :column)
            (plist-get validation :error))))

(defun ellama-eval--format-edit-rejection
    (file-name candidate-validation original-validation
               old-validation new-validation)
  "Return edit rejection for FILE-NAME and VALIDATION results.
CANDIDATE-VALIDATION, ORIGINAL-VALIDATION, OLD-VALIDATION and
NEW-VALIDATION are syntax validation results."
  (format
   (concat
    "Edit rejected: resulting buffer has unbalanced delimiters or "
    "invalid syntax.\n\n"
    "File: %s\n"
    "Mode: %s\n"
    "Check: %s\n"
    "Error: %s\n"
    "Position: line %d, column %d\n\n"
    "%s\n\n"
    "Original file status: %s\n"
    "Old fragment standalone status: %s\n"
    "Replacement fragment standalone status: %s\n\n"
    "%s\n"
    "Instruction: retry the edit. Keep the requested semantic change, "
    "but return replacement text whose delimiters are balanced in %s.")
   file-name
   (plist-get candidate-validation :mode)
   (plist-get candidate-validation :check)
   (plist-get candidate-validation :error)
   (plist-get candidate-validation :line)
   (plist-get candidate-validation :column)
   (plist-get candidate-validation :nearby)
   (ellama-eval--validation-status original-validation)
   (ellama-eval--validation-status old-validation)
   (ellama-eval--validation-status new-validation)
   (ellama-eval--format-delimiter-balance
    (plist-get candidate-validation :balance))
   (plist-get candidate-validation :mode)))

(defun ellama-eval--write-file-buffer-content (file-name content)
  "Write CONTENT to FILE-NAME and its visiting buffer."
  (let ((buffer (find-file-noselect file-name)))
    (with-current-buffer buffer
      (let ((coding-system-for-write 'raw-text)
            (buffer-undo-list t)
            (inhibit-read-only t))
        (erase-buffer)
        (insert content)
        (save-buffer)))))

(defun ellama-eval-balanced-edit-file-tool
    (file-name oldcontent newcontent)
  "Replace OLDCONTENT with NEWCONTENT in FILE-NAME after validation."
  (or (ellama-tools--tool-check-file-access file-name 'read)
      (ellama-tools--tool-check-file-access file-name 'write)
      (let ((content (with-temp-buffer
                       (insert-file-contents-literally file-name)
                       (buffer-string))))
        (if (not (string-match (regexp-quote oldcontent) content))
            (format "Edit rejected: old content was not found in %s."
                    file-name)
          (let* ((candidate (replace-match newcontent t t content))
                 (candidate-validation
                  (ellama-eval--validate-text-in-file-buffer
                   file-name candidate)))
            (if (plist-get candidate-validation :valid)
                (progn
                  (ellama-eval--write-file-buffer-content
                   file-name candidate)
                  (format "Edited %s after syntax validation." file-name))
              (ellama-eval--format-edit-rejection
               file-name
               candidate-validation
               (ellama-eval--validate-text-in-file-buffer
                file-name content)
               (ellama-eval--validate-text-in-file-buffer
                file-name oldcontent)
               (ellama-eval--validate-text-in-file-buffer
                file-name newcontent))))))))

(defun ellama-eval-edit-lines-tool (file-name from to replacement)
  "Replace FILE-NAME lines FROM through TO with REPLACEMENT."
  (or (ellama-tools--tool-check-file-access file-name 'read)
      (ellama-tools--tool-check-file-access file-name 'write)
      (progn
        (unless (and (integerp from)
                     (integerp to)
                     (<= 1 from)
                     (<= from to))
          (error "Line range must use positive integers with FROM <= TO"))
        (with-temp-buffer
          (insert-file-contents-literally file-name)
          (let ((line-count (ellama-eval--line-count)))
            (when (> to line-count)
              (error "Line range %d-%d exceeds file line count %d"
                     from to line-count)))
          (goto-char (point-min))
          (forward-line (1- from))
          (let ((start (line-beginning-position))
                end
                suffix-p)
            (goto-char (point-min))
            (forward-line to)
            (setq end (point))
            (setq suffix-p (< end (point-max)))
            (delete-region start end)
            (goto-char start)
            (insert replacement)
            (when (and suffix-p
                       (not (string-empty-p replacement))
                       (not (string-suffix-p "\n" replacement)))
              (insert "\n")))
          (let ((coding-system-for-write 'raw-text))
            (write-region (point-min) (point-max) file-name nil 'silent))))
      (format "Replaced lines %d-%d in %s." from to file-name)))

(defun ellama-eval--write-case-files (workspace files)
  "Write FILES into WORKSPACE."
  (dolist (entry files)
    (let ((path (expand-file-name (car entry) workspace)))
      (make-directory (file-name-directory path) t)
      (with-temp-file path
        (insert (cdr entry))))))

(defun ellama-eval--read-file-string (file-name)
  "Return FILE-NAME content, or nil when it does not exist."
  (when (file-exists-p file-name)
    (with-temp-buffer
      (insert-file-contents-literally file-name)
      (buffer-string))))

(defun ellama-eval--strip-defun-docstrings (text)
  "Return TEXT with top-level defun docstrings replaced."
  (when text
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward "^(defun\\_>" nil t)
        (goto-char (match-beginning 0))
        (condition-case nil
            (progn
              (forward-char 1)
              (forward-sexp 1)
              (skip-chars-forward " \t\r\n")
              (forward-sexp 1)
              (skip-chars-forward " \t\r\n")
              (forward-sexp 1)
              (skip-chars-forward " \t\r\n")
              (when (eq (char-after) ?\")
                (let ((start (point)))
                  (forward-sexp 1)
                  (delete-region start (point))
                  (insert "\"<docstring>\""))))
          (scan-error
           (goto-char (line-end-position)))))
      (buffer-string))))

(defun ellama-eval--file-checks
    (workspace expected-files &optional ignore-docstrings)
  "Return file check entries for WORKSPACE and EXPECTED-FILES.
When IGNORE-DOCSTRINGS is non-nil, ignore defun docstring wording."
  (mapcar
   (lambda (entry)
     (let* ((relative (car entry))
            (expected (cdr entry))
            (actual
             (ellama-eval--read-file-string
              (expand-file-name relative workspace)))
            (expected-for-check
             (if ignore-docstrings
                 (ellama-eval--strip-defun-docstrings expected)
               expected))
            (actual-for-check
             (if ignore-docstrings
                 (ellama-eval--strip-defun-docstrings actual)
               actual)))
       (list :path relative
             :expected expected
             :actual actual
             :ignore-docstrings (and ignore-docstrings t)
             :matched (equal expected-for-check actual-for-check))))
   expected-files))

(defun ellama-eval--file-regexp-checks (workspace file-regexps)
  "Return file regexp check entries for WORKSPACE and FILE-REGEXPS."
  (apply
   #'append
   (mapcar
    (lambda (entry)
      (let* ((relative (car entry))
             (actual
              (ellama-eval--read-file-string
               (expand-file-name relative workspace))))
        (mapcar
         (lambda (regexp)
           (list :path relative
                 :regexp regexp
                 :matched (and actual (string-match-p regexp actual))))
         (cdr entry))))
    file-regexps)))

(defun ellama-eval--answer-checks (answer regexps)
  "Return regexp check entries for ANSWER and REGEXPS."
  (mapcar
   (lambda (regexp)
     (list :regexp regexp
           :matched (and answer (string-match-p regexp answer))))
   regexps))

(defun ellama-eval--checks-pass-p (entries)
  "Return non-nil when validation ENTRIES match."
  (cl-every (lambda (entry) (plist-get entry :matched)) entries))

(defun ellama-eval--tool-count (trace names)
  "Count TRACE entries whose names appear in NAMES."
  (cl-count-if
   (lambda (entry)
     (member (plist-get entry :name) names))
   trace))

(defun ellama-eval--provider-role-plist (provider system tool-names)
  "Return eval role plist for PROVIDER, SYSTEM and TOOL-NAMES."
  (append (list :system system :tools tool-names)
          (when provider (list :provider provider))))

(defun ellama-eval--result-status
    (run-status file-checks file-regexp-checks answer-checks)
  "Return final evaluation status.
RUN-STATUS is the execution status.  FILE-CHECKS, FILE-REGEXP-CHECKS
and ANSWER-CHECKS contain oracle results."
  (cond
   ((eq run-status 'timeout) 'timeout)
   ((and (ellama-eval--checks-pass-p file-checks)
         (ellama-eval--checks-pass-p file-regexp-checks)
         (ellama-eval--checks-pass-p answer-checks))
    'passed)
   (t
    'failed)))

(defun ellama-eval--cancel-timeout-timer (state)
  "Cancel timeout timer stored in STATE."
  (when-let* ((timer (ellama-eval--run-state-timeout-timer state)))
    (cancel-timer timer)
    (setf (ellama-eval--run-state-timeout-timer state) nil)))

(defun ellama-eval--cleanup-workspace (state)
  "Delete STATE workspace unless retention is enabled."
  (let ((workspace (ellama-eval--run-state-workspace state)))
    (unless (and ellama-eval-keep-workspaces
                 (file-directory-p workspace))
      (when (file-directory-p workspace)
        (delete-directory workspace t)))))

(defun ellama-eval--build-result (state run-status)
  "Build result plist for STATE and RUN-STATUS."
  (let* ((case (ellama-eval--run-state-case state))
         (workspace (ellama-eval--run-state-workspace state))
         (expected-files (plist-get case :expected-files))
         (ignore-docstrings (plist-get case :ignore-docstrings))
         (file-regexps (plist-get case :file-regexps))
         (answer-regexps (plist-get case :answer-regexps))
         (trace (nreverse (ellama-eval--run-state-trace state)))
         (file-checks
          (ellama-eval--file-checks
           workspace expected-files ignore-docstrings))
         (file-regexp-checks
          (ellama-eval--file-regexp-checks workspace file-regexps))
         (answer-checks
          (ellama-eval--answer-checks
           (ellama-eval--run-state-result state)
           answer-regexps))
         (status
          (ellama-eval--result-status
           run-status file-checks file-regexp-checks answer-checks))
         (worker (ellama-eval--run-state-worker state))
         (extra (and worker (ellama-session-extra worker)))
         (continuation-steps (or (plist-get extra :step-count) 0)))
    (list
     :case-id (plist-get case :id)
     :suite (plist-get case :suite)
     :profile (ellama-eval--run-state-profile state)
     :status status
     :success (eq status 'passed)
     :report-result (ellama-eval--run-state-result state)
     :elapsed-ms
     (round (* 1000
               (- (float-time)
                  (ellama-eval--run-state-started-at state))))
     :steps (if worker (1+ continuation-steps) continuation-steps)
     :continuation-steps continuation-steps
     :tool-call-count (length trace)
     :read-call-count
     (ellama-eval--tool-count trace '("read_file" "lines_range"))
     :edit-call-count
     (ellama-eval--tool-count trace '("edit_file" "edit_lines"))
     :tool-trace trace
     :file-checks file-checks
     :file-regexp-checks file-regexp-checks
     :answer-checks answer-checks
     :workspace (when ellama-eval-keep-workspaces workspace))))

(defun ellama-eval--finish-run (state run-status &optional task-result)
  "Finalize STATE with RUN-STATUS and optional TASK-RESULT."
  (when (eq (ellama-eval--run-state-status state) 'pending)
    (when task-result
      (setf (ellama-eval--run-state-result state) task-result))
    (setf (ellama-eval--run-state-status state) run-status)
    (ellama-eval--cancel-timeout-timer state)
    (let ((result (ellama-eval--build-result state run-status))
          (callback (ellama-eval--run-state-callback state)))
      (when (eq ellama-eval--active-run state)
        (setq ellama-eval--active-run nil))
      (ellama-eval--cleanup-workspace state)
      (when callback
        (funcall callback result))
      result)))

(defun ellama-eval--timeout-run (state)
  "Finalize STATE as timed out."
  (ellama-eval--finish-run state 'timeout))

(defun ellama-eval-start-case
    (case profile callback &optional provider)
  "Start CASE under PROFILE and call CALLBACK with its result.
PROVIDER overrides the provider used by the eval role."
  (when ellama-eval--active-run
    (error "An Ellama evaluation run is already active"))
  (let* ((workspace (make-temp-file "ellama-eval-" t))
         (files (plist-get case :files))
         (system (or (plist-get case :system)
                     ellama-eval--coder-system))
         (tool-names (ellama-eval--profile-tool-names profile))
         (tools (ellama-eval--make-profile-tools profile))
         (state
          (make-ellama-eval--run-state
           :status 'pending
           :trace nil
           :started-at (float-time)
           :workspace workspace
           :case case
           :profile profile
           :callback callback))
         (original-new-session (symbol-function 'ellama-new-session)))
    (setq ellama-eval--active-run state)
    (setf (ellama-eval--run-state-timeout-timer state)
          (run-at-time ellama-eval-timeout-seconds nil
                       #'ellama-eval--timeout-run state))
    (condition-case err
        (let ((default-directory workspace)
              (ellama-tools-allow-all t)
              (ellama-tools-allowed nil)
              (ellama-tools-confirm-allowed (make-hash-table))
              (ellama-tools-available tools)
              (ellama-tools-enabled tools)
              (ellama-tools-subagent-default-max-steps
               (or (plist-get case :max-steps)
                   ellama-eval-default-max-steps))
              (ellama-tools-subagent-roles
               `(("eval" . ,(ellama-eval--provider-role-plist
                             provider system tool-names)))))
          (ellama-eval--write-case-files workspace files)
          (cl-letf (((symbol-function 'ellama-new-session)
                     (lambda (role-provider prompt &optional ephemeral)
                       (let ((session
                              (funcall original-new-session
                                       role-provider prompt ephemeral)))
                         (setf (ellama-eval--run-state-worker state) session)
                         session))))
            (ellama-tools-task-tool
             (lambda (task-result)
               (ellama-eval--finish-run state 'completed task-result))
             (plist-get case :prompt)
             "eval"))
          state)
      (error
       (ellama-eval--cancel-timeout-timer state)
       (when (eq ellama-eval--active-run state)
         (setq ellama-eval--active-run nil))
       (ellama-eval--cleanup-workspace state)
       (signal (car err) (cdr err))))))

(defun ellama-eval-run-case (case profile &optional provider)
  "Run CASE under PROFILE with optional PROVIDER."
  (let ((result :pending))
    (ellama-eval-start-case
     case profile
     (lambda (case-result)
       (setq result case-result))
     provider)
    (while (eq result :pending)
      (accept-process-output nil 0.05))
    result))

(defun ellama-eval-run-hypothesis-suite
    (&optional provider profiles cases)
  "Run the built-in hypothesis suite.
PROVIDER overrides the provider used by the eval role.  PROFILES and CASES
allow partial runs."
  (let ((profiles (or profiles ellama-eval-hypothesis-profiles))
        (cases (or cases ellama-eval-hypothesis-cases))
        results)
    (dolist (case cases)
      (dolist (profile profiles)
        (push (ellama-eval-run-case case profile provider) results)))
    (nreverse results)))

(defun ellama-eval--hypothesis-jobs (profiles cases)
  "Return ordered hypothesis jobs for PROFILES and CASES."
  (let (jobs)
    (dolist (case cases)
      (dolist (profile profiles)
        (push (list :case case :profile profile) jobs)))
    (nreverse jobs)))

(defun ellama-eval-run-hypothesis-suite-async
    (provider profiles cases callback &optional progress-callback)
  "Run the hypothesis suite asynchronously.
PROVIDER, PROFILES and CASES mirror `ellama-eval-run-hypothesis-suite'.
Call CALLBACK with all results.  Call PROGRESS-CALLBACK with partial results,
completed count, total count and the latest result."
  (let* ((profiles (or profiles ellama-eval-hypothesis-profiles))
         (cases (or cases ellama-eval-hypothesis-cases))
         (jobs (ellama-eval--hypothesis-jobs profiles cases))
         (total (length jobs))
         (completed 0)
         results)
    (cl-labels
        ((start-next ()
           (if (null jobs)
               (funcall callback (nreverse results))
             (let* ((job (pop jobs))
                    (case (plist-get job :case))
                    (profile (plist-get job :profile)))
               (ellama-eval-start-case
                case profile
                (lambda (result)
                  (push result results)
                  (setq completed (1+ completed))
                  (when progress-callback
                    (funcall progress-callback
                             (reverse results) completed total result))
                  (run-at-time 0 nil #'start-next))
                provider)))))
      (when progress-callback
        (funcall progress-callback nil 0 total nil))
      (run-at-time 0 nil #'start-next)
      total)))

(defun ellama-eval-summarize-results (results)
  "Return aggregate summary rows for RESULTS."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (result results)
      (let* ((key (list (plist-get result :suite)
                        (plist-get result :profile)))
             (row (or (gethash key table)
                      (list :suite (car key)
                            :profile (cadr key)
                            :runs 0
                            :passed 0
                            :steps 0
                            :tool-calls 0))))
        (plist-put row :runs (1+ (plist-get row :runs)))
        (when (plist-get result :success)
          (plist-put row :passed (1+ (plist-get row :passed))))
        (plist-put row :steps
                   (+ (plist-get row :steps)
                      (plist-get result :steps)))
        (plist-put row :tool-calls
                   (+ (plist-get row :tool-calls)
                      (plist-get result :tool-call-count)))
        (puthash key row table)))
    (let (summary)
      (maphash
       (lambda (_key row)
         (let ((runs (plist-get row :runs)))
           (push (append row
                         (list :success-rate
                               (/ (float (plist-get row :passed)) runs)
                               :mean-steps
                               (/ (float (plist-get row :steps)) runs)
                               :mean-tool-calls
                               (/ (float (plist-get row :tool-calls)) runs)))
                 summary)))
       table)
      (sort summary
            (lambda (left right)
              (string<
               (format "%s/%s"
                       (plist-get left :suite)
                       (plist-get left :profile))
               (format "%s/%s"
                       (plist-get right :suite)
                       (plist-get right :profile))))))))

(defun ellama-eval--plist-p (value)
  "Return non-nil when VALUE is a property list."
  (and (consp value)
       (proper-list-p value)
       (zerop (mod (length value) 2))
       (cl-loop for (key _value) on value by #'cddr
                always (keywordp key))))

(defun ellama-eval--json-key (key)
  "Return JSON object key string for plist KEY."
  (if (keywordp key)
      (substring (symbol-name key) 1)
    (format "%s" key)))

(defun ellama-eval--json-value (value &optional key)
  "Return VALUE converted to a shape suitable for JSON encoding.
KEY is the property list key that contains VALUE."
  (cond
   ((memq key '(:success :matched))
    (if value t json-false))
   ((memq key '(:tool-trace :file-checks
                            :file-regexp-checks :answer-checks))
    (vconcat (mapcar #'ellama-eval--json-value value)))
   ((ellama-eval--plist-p value)
    (ellama-eval--json-object value))
   ((proper-list-p value)
    (vconcat (mapcar #'ellama-eval--json-value value)))
   ((and (symbolp value) (not (memq value (list t json-false))))
    (symbol-name value))
   (t value)))

(defun ellama-eval--json-object (plist)
  "Return PLIST as a JSON object hash table."
  (let ((object (make-hash-table :test #'equal)))
    (while plist
      (let ((key (pop plist))
            (value (pop plist)))
        (puthash (ellama-eval--json-key key)
                 (ellama-eval--json-value value key)
                 object)))
    object))

(defun ellama-eval--json-encode-result (result)
  "Return RESULT encoded as JSON for JSONL output."
  (json-encode (ellama-eval--json-value result)))

(defun ellama-eval-write-results-jsonl (results file-name)
  "Write RESULTS as JSON lines to FILE-NAME."
  (with-temp-file file-name
    (dolist (result results)
      (insert (ellama-eval--json-encode-result result) "\n"))))

(defun ellama-eval--provider-candidates ()
  "Return interactive provider candidates for evaluation."
  (append
   `(("default model" . ellama-provider)
     ("coding provider" . ellama-coding-provider)
     ("ollama model" . (ellama-get-ollama-local-model)))
   ellama-providers))

(defun ellama-eval--read-provider ()
  "Read evaluation provider from the minibuffer."
  (let* ((providers (ellama-eval--provider-candidates))
         (choice
          (completing-read
           "Evaluation provider: "
           (mapcar #'car providers)
           nil t nil nil "coding provider")))
    (eval (alist-get choice providers nil nil #'string=))))

(defun ellama-eval--read-suite-selection ()
  "Read suite selection from the minibuffer."
  (intern
   (completing-read
    "Evaluation suite: "
    '("all" "edit" "explore")
    nil t nil nil "all")))

(defun ellama-eval--cases-for-suite-selection (selection)
  "Return cases selected by SELECTION."
  (if (eq selection 'all)
      ellama-eval-hypothesis-cases
    (seq-filter
     (lambda (case)
       (eq (plist-get case :suite) selection))
     ellama-eval-hypothesis-cases)))

(defun ellama-eval--read-profile-selection ()
  "Read evaluation profiles from the minibuffer."
  (let* ((choices (mapcar #'symbol-name ellama-eval-hypothesis-profiles))
         (selected
          (completing-read-multiple
           "Evaluation profiles, comma separated (empty means all): "
           choices nil t)))
    (if selected
        (mapcar #'intern selected)
      ellama-eval-hypothesis-profiles)))

(defun ellama-eval--read-results-file ()
  "Read optional JSONL results file for the interactive command."
  (when (y-or-n-p "Write evaluation results to JSONL after completion? ")
    (let* ((directory default-directory)
           (default-file
            (expand-file-name "ellama-eval-results.jsonl" directory))
           (selected
            (read-file-name "Evaluation JSONL file: "
                            directory default-file nil
                            "ellama-eval-results.jsonl"))
           (expanded (expand-file-name selected directory)))
      (when (and buffer-file-name
                 (equal (file-truename expanded)
                        (file-truename buffer-file-name)))
        (user-error
         "Refusing to overwrite the current buffer file with eval JSONL"))
      expanded)))

(defun ellama-eval--insert-summary-rows (rows)
  "Insert aggregate summary ROWS into the current buffer."
  (insert
   (format "%-10s %-22s %8s %8s %10s %10s\n"
           "Suite" "Profile" "Runs" "Passed" "MeanSteps" "MeanCalls"))
  (insert (make-string 78 ?-) "\n")
  (dolist (row rows)
    (insert
     (format "%-10s %-22s %8d %8d %10.2f %10.2f\n"
             (plist-get row :suite)
             (plist-get row :profile)
             (plist-get row :runs)
             (plist-get row :passed)
             (plist-get row :mean-steps)
             (plist-get row :mean-tool-calls)))))

(defun ellama-eval--insert-result-rows (results)
  "Insert per-run RESULTS into the current buffer."
  (insert
   (format "%-30s %-22s %-10s %7s %7s %9s\n"
           "Case" "Profile" "Status" "Steps" "Calls" "Elapsed"))
  (insert (make-string 92 ?-) "\n")
  (dolist (result results)
    (insert
     (format "%-30s %-22s %-10s %7d %7d %8dms\n"
             (plist-get result :case-id)
             (plist-get result :profile)
             (plist-get result :status)
             (plist-get result :steps)
             (plist-get result :tool-call-count)
             (plist-get result :elapsed-ms)))))

(defun ellama-eval--render-summary-buffer
    (results completed total &optional results-file)
  "Render RESULTS summary with COMPLETED and TOTAL progress.
RESULTS-FILE is shown when JSONL export is configured."
  (let ((buffer (get-buffer-create ellama-eval--summary-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Ellama evaluation\n\n")
        (insert (format "Progress: %d/%d completed\n" completed total))
        (when results-file
          (insert (format "JSONL target: %s\n" results-file)))
        (insert "\n")
        (if results
            (progn
              (insert "Aggregate summary\n\n")
              (ellama-eval--insert-summary-rows
               (ellama-eval-summarize-results results))
              (insert "\nLatest results\n\n")
              (ellama-eval--insert-result-rows results))
          (insert "No completed cases yet.\n"))
        (goto-char (point-min))
        (special-mode)))
    (display-buffer buffer)
    buffer))

;;;###autoload
(defun ellama-eval-run-hypothesis-suite-interactive
    (provider suite profiles results-file)
  "Start the built-in hypothesis suite without blocking Emacs.
PROVIDER is used for eval roles.  SUITE narrows the built-in cases.  PROFILES
select experiment profiles.  RESULTS-FILE receives JSONL output when non-nil."
  (interactive
   (list (ellama-eval--read-provider)
         (ellama-eval--read-suite-selection)
         (ellama-eval--read-profile-selection)
         (ellama-eval--read-results-file)))
  (let ((cases (ellama-eval--cases-for-suite-selection suite)))
    (ellama-eval-run-hypothesis-suite-async
     provider profiles cases
     (lambda (results)
       (setq ellama-eval-last-results results)
       (when results-file
         (ellama-eval-write-results-jsonl results results-file))
       (ellama-eval--render-summary-buffer
        results (length results) (length results) results-file)
       (message "Ellama evaluation finished: %d runs." (length results)))
     (lambda (results completed total _latest)
       (ellama-eval--render-summary-buffer
        results completed total results-file)))
    (message "Ellama evaluation started.")))

(provide 'ellama-eval)

;;; ellama-eval.el ends here
