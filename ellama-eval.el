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
  status result trace worker started-at)

(defvar ellama-eval--active-run nil
  "Current evaluation run state.
Only one evaluation run is supported at a time.")

(defconst ellama-eval--base-tool-names
  '("read_file" "lines_range" "grep" "grep_in_file"
    "directory_tree" "project_root")
  "Tool names included in every hypothesis profile.")

(defconst ellama-eval-hypothesis-profiles
  '(baseline numbered-read line-edit numbered-line-edit)
  "Profiles used by the line-edit and numbered-read hypothesis suite.")

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
         (("sample.el" . "(defun ellama-eval-score-value (value limit weight)\n  \"Return VALUE scaled by WEIGHT.\"\n  (* value weight))\n"))
         :expected-files
         (("sample.el" . "(defun ellama-eval-score-value (value limit weight)\n  \"Return VALUE scaled by WEIGHT.\"\n  (* (min value limit) weight))\n")))
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
         "Update `ellama-eval-guarded-join` so it reject both nil ITEMS and empty \
ITEMS before calling `string-join`."
         :files
         (("strings.el" . "(defun ellama-eval-guarded-join (items)\n  (if (null items)\n      \"\"\n    (string-join items \", \")))\n"))
         :expected-files
         (("strings.el" . "(defun ellama-eval-guarded-join (items)\n  (if (or (null items) (equal items '()))\n      \"\"\n    (string-join items \", \")))\n")))
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
         :answer-regexps ("string" "Write denied")))
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
            (instrumented
             (ellama-eval--instrument-tool-spec read-spec)))
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

(defun ellama-eval--file-checks (workspace expected-files)
  "Return file check entries for WORKSPACE and EXPECTED-FILES."
  (mapcar
   (lambda (entry)
     (let* ((relative (car entry))
            (expected (cdr entry))
            (actual
             (ellama-eval--read-file-string
              (expand-file-name relative workspace))))
       (list :path relative
             :expected expected
             :actual actual
             :matched (equal expected actual))))
   expected-files))

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

(defun ellama-eval--run-status (state deadline)
  "Return run status from STATE and DEADLINE."
  (cond
   ((not (eq (ellama-eval--run-state-status state) 'pending))
    (ellama-eval--run-state-status state))
   ((>= (float-time) deadline)
    'timeout)
   (t
    'pending)))

(defun ellama-eval--provider-role-plist (provider system tool-names)
  "Return eval role plist for PROVIDER, SYSTEM and TOOL-NAMES."
  (append (list :system system :tools tool-names)
          (when provider (list :provider provider))))

(defun ellama-eval--result-status
    (run-status file-checks answer-checks)
  "Return final evaluation status.
RUN-STATUS is the execution status.  FILE-CHECKS and ANSWER-CHECKS contain
oracle results."
  (cond
   ((eq run-status 'timeout) 'timeout)
   ((and (ellama-eval--checks-pass-p file-checks)
         (ellama-eval--checks-pass-p answer-checks))
    'passed)
   (t
    'failed)))

(defun ellama-eval-run-case (case profile &optional provider)
  "Run CASE under PROFILE with optional PROVIDER."
  (let* ((workspace (make-temp-file "ellama-eval-" t))
         (files (plist-get case :files))
         (expected-files (plist-get case :expected-files))
         (answer-regexps (plist-get case :answer-regexps))
         (system (or (plist-get case :system)
                     ellama-eval--coder-system))
         (tool-names (ellama-eval--profile-tool-names profile))
         (tools (ellama-eval--make-profile-tools profile))
         (state
          (make-ellama-eval--run-state
           :status 'pending
           :trace nil
           :started-at (float-time)))
         (deadline (+ (float-time) ellama-eval-timeout-seconds))
         (result nil)
         (original-new-session (symbol-function 'ellama-new-session)))
    (unwind-protect
        (let ((default-directory workspace)
              (ellama-eval--active-run state)
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
               (setf (ellama-eval--run-state-result state) task-result)
               (setf (ellama-eval--run-state-status state) 'completed))
             (plist-get case :prompt)
             "eval"))
          (while (eq (ellama-eval--run-status state deadline) 'pending)
            (accept-process-output nil 0.05))
          (when (eq (ellama-eval--run-status state deadline) 'timeout)
            (setf (ellama-eval--run-state-status state) 'timeout))
          (let* ((trace (nreverse (ellama-eval--run-state-trace state)))
                 (run-status (ellama-eval--run-state-status state))
                 (file-checks
                  (ellama-eval--file-checks workspace expected-files))
                 (answer-checks
                  (ellama-eval--answer-checks
                   (ellama-eval--run-state-result state)
                   answer-regexps))
                 (status
                  (ellama-eval--result-status
                   run-status file-checks answer-checks))
                 (worker (ellama-eval--run-state-worker state))
                 (extra (and worker (ellama-session-extra worker))))
            (setq result
                  (list
                   :case-id (plist-get case :id)
                   :suite (plist-get case :suite)
                   :profile profile
                   :status status
                   :success (eq status 'passed)
                   :report-result (ellama-eval--run-state-result state)
                   :elapsed-ms
                   (round (* 1000
                             (- (float-time)
                                (ellama-eval--run-state-started-at state))))
                   :steps (or (plist-get extra :step-count) 0)
                   :tool-call-count (length trace)
                   :read-call-count
                   (ellama-eval--tool-count
                    trace '("read_file" "lines_range"))
                   :edit-call-count
                   (ellama-eval--tool-count
                    trace '("edit_file" "edit_lines"))
                   :tool-trace trace
                   :file-checks file-checks
                   :answer-checks answer-checks
                   :workspace (when ellama-eval-keep-workspaces workspace)))))
      (unless (and ellama-eval-keep-workspaces
                   (file-directory-p workspace))
        (when (file-directory-p workspace)
          (delete-directory workspace t))))
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

(defun ellama-eval-write-results-jsonl (results file-name)
  "Write RESULTS as JSON lines to FILE-NAME."
  (with-temp-file file-name
    (dolist (result results)
      (insert (json-encode result) "\n"))))

(provide 'ellama-eval)

;;; ellama-eval.el ends here
