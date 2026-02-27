;;; ellama-tools-dlp.el --- DLP settings for Ellama tools -*- lexical-binding: t; package-lint-main-file: "ellama.el"; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

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
;; DLP scaffolding for Ellama tool input/output scanning.
;;

;;; Code:

(require 'cl-lib)

(defgroup ellama-tools-dlp nil
  "DLP settings for `ellama' tools."
  :group 'ellama)

(defconst ellama-tools-dlp--sensitive-env-name-regexp
  (concat
   "\\(TOKEN\\|SECRET\\|KEY\\|PASS\\|PWD\\|AUTH\\|COOKIE\\|"
   "CRED\\|SESSION\\)")
  "Regexp fragment matching sensitive environment variable names.")

(defconst ellama-tools-dlp--default-regex-rules
  (list
   (list :id "shell-env-secret-ref"
         :pattern
         (concat
          "\\$\\(?:{\\)?[[:alpha:]_][[:alnum:]_]*"
          ellama-tools-dlp--sensitive-env-name-regexp
          "[[:alnum:]_]*\\(?:}\\)?")
         :case-fold t
         :directions '(input)
         :tools '("shell_command")
         :args '("cmd")
         :severity 'high)
   (list :id "shell-http-secret-param-ref"
         :pattern
         (concat
          "[?&][[:alnum:]_.-]*"
          "\\(?:key\\|token\\|secret\\|auth\\|password\\|session\\)"
          "[[:alnum:]_.-]*="
          "\\(?:\\$\\(?:{\\)?[[:alpha:]_][[:alnum:]_]*\\(?:}\\)?"
          "\\|[[:upper:]][[:upper:][:digit:]_]*"
          ellama-tools-dlp--sensitive-env-name-regexp
          "[[:upper:][:digit:]_]*\\)")
         :case-fold t
         :directions '(input)
         :tools '("shell_command")
         :args '("cmd")
         :severity 'high))
  "Built-in regex rules always applied by DLP.")

(defconst ellama-tools-dlp--default-policy-overrides
  (list
   (list :tool "shell_command"
         :direction 'input
         :arg "cmd"
         :action 'block))
  "Built-in policy overrides always applied by DLP.")

(defcustom ellama-tools-dlp-enabled nil
  "Enable DLP checks for `ellama' tools."
  :type 'boolean
  :group 'ellama-tools-dlp)

(defcustom ellama-tools-dlp-mode 'monitor
  "Select DLP rollout mode."
  :type '(choice (const :tag "Monitor" monitor)
                 (const :tag "Enforce" enforce))
  :group 'ellama-tools-dlp)

(defcustom ellama-tools-dlp-max-scan-size (* 5 1024 1024)
  "Set maximum payload size in bytes to scan for tool input or output."
  :type 'integer
  :group 'ellama-tools-dlp)

(defcustom ellama-tools-dlp-input-fail-open t
  "Allow tool execution when DLP input scanning fails internally."
  :type 'boolean
  :group 'ellama-tools-dlp)

(defcustom ellama-tools-dlp-output-fail-open t
  "Allow tool output passthrough when DLP output scanning fails internally."
  :type 'boolean
  :group 'ellama-tools-dlp)

(defcustom ellama-tools-dlp-scan-env-exact-secrets t
  "Enable exact-secret scanning based on environment variables."
  :type 'boolean
  :group 'ellama-tools-dlp)

(defcustom ellama-tools-dlp-redaction-placeholder-format
  "[REDACTED:RULE_ID]"
  "Set placeholder template used for DLP redaction.
Replace `RULE_ID' with the detector rule identifier when redacting."
  :type 'string
  :group 'ellama-tools-dlp)

(defcustom ellama-tools-dlp-input-default-action 'warn
  "Set default enforcement action for input findings in enforce mode."
  :type '(choice (const :tag "Allow" allow)
                 (const :tag "Warn" warn)
                 (const :tag "Block" block))
  :group 'ellama-tools-dlp)

(defcustom ellama-tools-dlp-output-default-action 'redact
  "Set default enforcement action for output findings in enforce mode."
  :type '(choice (const :tag "Allow" allow)
                 (const :tag "Warn" warn)
                 (const :tag "Block" block)
                 (const :tag "Redact" redact))
  :group 'ellama-tools-dlp)

(defcustom ellama-tools-dlp-policy-overrides nil
  "Set additional per-tool and per-arg DLP policy overrides.
Built-in baseline overrides are always applied.
Each element is a plist.  Supported keys:
`:tool' (required), `:direction', `:arg', `:action', and `:except'.
When `:except' is non-nil and the override matches, findings are ignored."
  :type '(repeat plist)
  :group 'ellama-tools-dlp)

(defcustom ellama-tools-dlp-log-targets '(memory)
  "Select DLP incident logging targets.
Supported targets are `memory' and `message'."
  :type '(set (const :tag "In-memory list" memory)
              (const :tag "Messages buffer" message))
  :group 'ellama-tools-dlp)

(defcustom ellama-tools-dlp-incident-log-max 200
  "Maximum number of DLP incidents kept in memory.
Set to 0 to disable in-memory incident retention."
  :type 'integer
  :group 'ellama-tools-dlp)

(defcustom ellama-tools-dlp-message-prefix "ellama-dlp"
  "Prefix used when emitting incidents via `message'."
  :type 'string
  :group 'ellama-tools-dlp)

(defcustom ellama-tools-dlp-regex-rules nil
  "Additional regex-based DLP detector rules.
Built-in baseline rules are always applied.
Each rule is a plist.  Supported keys:
`:id', `:pattern', `:enabled', `:case-fold', `:directions', `:tools',
`:args', and `:severity'."
  :type '(repeat plist)
  :group 'ellama-tools-dlp)

(defcustom ellama-tools-dlp-env-secret-min-length 12
  "Minimum environment value length to consider for exact-secret scanning."
  :type 'integer
  :group 'ellama-tools-dlp)

(defcustom ellama-tools-dlp-env-secret-max-length 4096
  "Maximum environment value length to consider for exact-secret scanning."
  :type 'integer
  :group 'ellama-tools-dlp)

(defcustom ellama-tools-dlp-env-secret-entropy-threshold 3.2
  "Minimum entropy score used by the default env-secret heuristic stage."
  :type 'number
  :group 'ellama-tools-dlp)

(defcustom ellama-tools-dlp-env-secret-min-score 3
  "Minimum heuristic score required to accept an env secret candidate."
  :type 'integer
  :group 'ellama-tools-dlp)

(defcustom ellama-tools-dlp-env-secret-heuristic-stages
  '(ellama-tools-dlp--env-secret-stage-length
    ellama-tools-dlp--env-secret-stage-single-line
    ellama-tools-dlp--env-secret-stage-reject-obvious-config
    ellama-tools-dlp--env-secret-stage-token-shape
    ellama-tools-dlp--env-secret-stage-known-prefix
    ellama-tools-dlp--env-secret-stage-name-signal
    ellama-tools-dlp--env-secret-stage-entropy)
  "Ordered list of heuristic stage functions for env-secret candidates.
Each function receives `(ENV-NAME ENV-VALUE)' and returns nil, an integer score,
or a plist with `:score' and/or `:reject'."
  :type '(repeat function)
  :group 'ellama-tools-dlp)

(defconst ellama-tools-dlp--zero-width-chars
  (list #x00ad #x200b #x200c #x200d #x200e #x200f #x2060 #xfeff)
  "Invisible characters removed during DLP normalization.")

(defconst ellama-tools-dlp--zero-width-regexp
  (regexp-opt (mapcar #'char-to-string ellama-tools-dlp--zero-width-chars))
  "Regexp matching zero-width/invisible characters removed in v1.")

(defvar ellama-tools-dlp--incident-log nil
  "Newest-first list of sanitized internal DLP incident plists.")

(defvar ellama-tools-dlp--regex-cache (make-hash-table :test 'equal)
  "Cache for validated regex rules.")

(defvar ellama-tools-dlp--exact-secret-cache nil
  "Cache of env-derived exact-secret variants and safe metadata.")

(defconst ellama-tools-dlp--exact-secret-rule-id "env-exact-secret"
  "Rule ID used for exact env-secret findings.")

(defun ellama-tools-dlp--bool-or-nil-p (value)
  "Return non-nil when VALUE is a boolean or nil."
  (or (null value) (eq value t)))

(defun ellama-tools-dlp--plist-key-present-p (plist key)
  "Return non-nil when PLIST contain KEY."
  (not (null (plist-member plist key))))

(defun ellama-tools-dlp--nonnegative-integer-p (value)
  "Return non-nil when VALUE is a non-negative integer."
  (and (integerp value) (>= value 0)))

(defun ellama-tools-dlp--validate-scan-context (context)
  "Signal an error when CONTEXT is not a valid scan context plist."
  (unless (listp context)
    (error "DLP scan context must be a plist"))
  (unless (memq (plist-get context :direction) '(input output))
    (error "DLP scan context :direction must be `input' or `output'"))
  (unless (stringp (plist-get context :tool-name))
    (error "DLP scan context :tool-name must be a string"))
  (when (and (ellama-tools-dlp--plist-key-present-p context :arg-name)
             (not (or (null (plist-get context :arg-name))
                      (stringp (plist-get context :arg-name)))))
    (error "DLP scan context :arg-name must be nil or a string"))
  (unless (ellama-tools-dlp--nonnegative-integer-p
           (plist-get context :payload-length))
    (error "DLP scan context :payload-length must be a non-negative integer"))
  (when (and (ellama-tools-dlp--plist-key-present-p context :truncated)
             (not (ellama-tools-dlp--bool-or-nil-p
                   (plist-get context :truncated))))
    (error "DLP scan context :truncated must be boolean or nil"))
  context)

(defun ellama-tools-dlp--scan-context-p (context)
  "Return non-nil when CONTEXT matches the DLP scan context schema."
  (condition-case nil
      (progn
        (ellama-tools-dlp--validate-scan-context context)
        t)
    (error nil)))

(cl-defun ellama-tools-dlp--make-scan-context
    (&key direction tool-name arg-name payload-length truncated)
  "Build a DLP scan context plist.
DIRECTION must contain `input'/'output'.
TOOL-NAME is a tool name.
ARG-NAME is a name of an argument.
PAYLOAD-LENGTH is a length of a payload.
TRUNCATED is a flag show that payload was truncated."
  (ellama-tools-dlp--validate-scan-context
   (list :direction direction
         :tool-name tool-name
         :arg-name arg-name
         :payload-length payload-length
         :truncated truncated)))

(defun ellama-tools-dlp--scan-context-get (context key &optional default)
  "Return KEY from scan CONTEXT, or DEFAULT when KEY is absent."
  (if (ellama-tools-dlp--plist-key-present-p context key)
      (plist-get context key)
    default))

(defun ellama-tools-dlp--validate-finding (finding)
  "Signal an error when FINDING is not a valid DLP finding plist."
  (unless (listp finding)
    (error "DLP finding must be a plist"))
  (let ((rule-id (plist-get finding :rule-id))
        (detector (plist-get finding :detector))
        (match-start (plist-get finding :match-start))
        (match-end (plist-get finding :match-end)))
    (unless (or (stringp rule-id) (symbolp rule-id))
      (error "DLP finding :rule-id must be a string or symbol"))
    (unless (memq detector '(regex exact-secret))
      (error "DLP finding :detector must be `regex' or `exact-secret'"))
    (when (and (ellama-tools-dlp--plist-key-present-p finding :severity)
               (not (or (null (plist-get finding :severity))
                        (symbolp (plist-get finding :severity))
                        (stringp (plist-get finding :severity)))))
      (error "DLP finding :severity must be nil, symbol, or string"))
    (when (and (ellama-tools-dlp--plist-key-present-p finding :match-start)
               match-start
               (not (ellama-tools-dlp--nonnegative-integer-p match-start)))
      (error "DLP finding :match-start must be a non-negative integer"))
    (when (and (ellama-tools-dlp--plist-key-present-p finding :match-end)
               match-end
               (not (ellama-tools-dlp--nonnegative-integer-p match-end)))
      (error "DLP finding :match-end must be a non-negative integer"))
    (when (or match-start match-end)
      (unless (and (integerp match-start) (integerp match-end))
        (error "DLP finding spans require both :match-start and :match-end"))
      (when (> match-start match-end)
        (error "DLP finding :match-start must not exceed :match-end"))))
  finding)

(defun ellama-tools-dlp--finding-p (finding)
  "Return non-nil when FINDING matches the DLP finding schema."
  (condition-case nil
      (progn
        (ellama-tools-dlp--validate-finding finding)
        t)
    (error nil)))

(cl-defun ellama-tools-dlp--make-finding
    (&key rule-id detector severity match-start match-end)
  "Build a DLP finding plist.
RULE-ID is an identificator of a rule.
DETECTOR is a name of a detector.
SEVERITY can be nil, a symbol or a string.
MATCH-START and MATCH-END is a match boundaries."
  (ellama-tools-dlp--validate-finding
   (list :rule-id rule-id
         :detector detector
         :severity severity
         :match-start match-start
         :match-end match-end)))

(defun ellama-tools-dlp--finding-get (finding key &optional default)
  "Return KEY from FINDING, or DEFAULT when KEY is absent."
  (if (ellama-tools-dlp--plist-key-present-p finding key)
      (plist-get finding key)
    default))

(defun ellama-tools-dlp--validate-verdict (verdict)
  "Signal an error when VERDICT is not a valid DLP verdict plist."
  (unless (listp verdict)
    (error "DLP verdict must be a plist"))
  (unless (memq (plist-get verdict :action) '(allow warn block redact))
    (error "DLP verdict :action must be allow, warn, block, or redact"))
  (when (and (ellama-tools-dlp--plist-key-present-p verdict :message)
             (not (or (null (plist-get verdict :message))
                      (stringp (plist-get verdict :message)))))
    (error "DLP verdict :message must be nil or a string"))
  (let ((findings (plist-get verdict :findings)))
    (when (and (ellama-tools-dlp--plist-key-present-p verdict :findings)
               (not (listp findings)))
      (error "DLP verdict :findings must be a list"))
    (when (listp findings)
      (dolist (finding findings)
        (ellama-tools-dlp--validate-finding finding))))
  (when (and (ellama-tools-dlp--plist-key-present-p verdict :redacted-text)
             (not (or (null (plist-get verdict :redacted-text))
                      (stringp (plist-get verdict :redacted-text)))))
    (error "DLP verdict :redacted-text must be nil or a string"))
  verdict)

(defun ellama-tools-dlp--verdict-p (verdict)
  "Return non-nil when VERDICT matches the DLP verdict schema."
  (condition-case nil
      (progn
        (ellama-tools-dlp--validate-verdict verdict)
        t)
    (error nil)))

(cl-defun ellama-tools-dlp--make-verdict
    (&key action message findings redacted-text)
  "Build a DLP verdict plist.
ACTION is an action.
MESSAGE is a string message.
FINDINGS contains current findings.
REDACTED-TEXT is a redacted text to prevent secrets leakage."
  (ellama-tools-dlp--validate-verdict
   (list :action action
         :message message
         :findings findings
         :redacted-text redacted-text)))

(defun ellama-tools-dlp--verdict-get (verdict key &optional default)
  "Return KEY from VERDICT, or DEFAULT when KEY is absent."
  (if (ellama-tools-dlp--plist-key-present-p verdict key)
      (plist-get verdict key)
    default))

(defun ellama-tools-dlp--clear-incident-log ()
  "Clear internal DLP incident log."
  (setq ellama-tools-dlp--incident-log nil))

(defun ellama-tools-dlp--incident-log ()
  "Return a copy of the internal DLP incident log."
  (copy-tree ellama-tools-dlp--incident-log))

(defun ellama-tools-dlp-recent-incidents (&optional count)
  "Return a copy of recent DLP incidents, newest first.
When COUNT is non-nil, return at most COUNT incidents."
  (let ((incidents (ellama-tools-dlp--incident-log)))
    (if (and (integerp count) (>= count 0))
        (cl-subseq incidents 0 (min count (length incidents)))
      incidents)))

(defun ellama-tools-dlp--sanitize-log-string (value)
  "Sanitize control and escape characters in string VALUE."
  (replace-regexp-in-string
   "[[:cntrl:]\177]"
   "?"
   value t t))

(defun ellama-tools-dlp--sanitize-incident-value (value)
  "Return sanitized log-safe VALUE."
  (cond
   ((stringp value)
    (ellama-tools-dlp--sanitize-log-string value))
   ;; Do not recurse into closures.  Emacs 28 can error when incident logging
   ;; later `copy-tree's sanitized events that still contain anonymous
   ;; function objects.
   ((functionp value)
    (if (byte-code-function-p value)
        'compiled-function
      'function))
   ((consp value)
    (mapcar #'ellama-tools-dlp--sanitize-incident-value value))
   ((vectorp value)
    (vconcat (mapcar #'ellama-tools-dlp--sanitize-incident-value value)))
   (t
    value)))

(defun ellama-tools-dlp--incident-summary (event)
  "Return one-line summary string for sanitized incident EVENT."
  (let* ((type (plist-get event :type))
         (direction (plist-get event :direction))
         (tool (plist-get event :tool-name))
         (arg (plist-get event :arg-name))
         (action (plist-get event :action))
         (rule-ids (plist-get event :rule-ids))
         (error-type (plist-get event :error-type)))
    (ellama-tools-dlp--sanitize-log-string
     (format "type=%s dir=%s tool=%s arg=%s action=%s rules=%s error=%s"
             type direction tool arg action rule-ids error-type))))

(defun ellama-tools-dlp--record-incident-memory (event)
  "Record sanitized DLP EVENT in memory log target."
  (let ((max (max 0 ellama-tools-dlp-incident-log-max)))
    (when (> max 0)
      (push (copy-tree event) ellama-tools-dlp--incident-log)
      (when (> (length ellama-tools-dlp--incident-log) max)
        (setcdr (nthcdr (1- max) ellama-tools-dlp--incident-log) nil)))))

(defun ellama-tools-dlp--record-incident-message (event)
  "Record sanitized DLP EVENT in `message' log target."
  (message "%s %s"
           ellama-tools-dlp-message-prefix
           (ellama-tools-dlp--incident-summary event)))

(defun ellama-tools-dlp--record-incident (event)
  "Record sanitized DLP EVENT and return it."
  (let ((sanitized (ellama-tools-dlp--sanitize-incident-value event)))
    (when (memq 'memory ellama-tools-dlp-log-targets)
      (ellama-tools-dlp--record-incident-memory sanitized))
    (when (memq 'message ellama-tools-dlp-log-targets)
      (ellama-tools-dlp--record-incident-message sanitized))
    sanitized))

(defun ellama-tools-dlp--log-exact-secret-error
    (context error-type &optional stage env-name)
  "Record sanitized exact-secret detector error incident.
CONTEXT is optional scan context.
ERROR-TYPE is a symbol describing the failure.
STAGE is optional stage function symbol.
ENV-NAME is optional environment variable name."
  (ellama-tools-dlp--record-incident
   (list :type 'exact-secret-error
         :timestamp (format-time-string "%FT%T%z")
         :direction (and context (plist-get context :direction))
         :tool-name (and context (plist-get context :tool-name))
         :arg-name (and context (plist-get context :arg-name))
         :stage stage
         :env-name env-name
         :error-type error-type)))

(defun ellama-tools-dlp--clear-regex-cache ()
  "Clear cached regex rule compilation results."
  (clrhash ellama-tools-dlp--regex-cache))

(defun ellama-tools-dlp--string-list-like-p (value)
  "Return non-nil when VALUE is a list of strings or symbols."
  (and (listp value)
       (cl-every (lambda (item)
                   (or (stringp item) (symbolp item)))
                 value)))

(defun ellama-tools-dlp--normalize-name-list (value)
  "Normalize VALUE list items to strings."
  (mapcar (lambda (item)
            (if (symbolp item)
                (symbol-name item)
              item))
          value))

(defun ellama-tools-dlp--validate-regex-rule (rule)
  "Signal an error when RULE is not a valid regex detector rule plist."
  (unless (listp rule)
    (error "DLP regex rule must be a plist"))
  (unless (or (stringp (plist-get rule :id))
              (symbolp (plist-get rule :id)))
    (error "DLP regex rule :id must be a string or symbol"))
  (unless (stringp (plist-get rule :pattern))
    (error "DLP regex rule :pattern must be a string"))
  (when (and (plist-member rule :enabled)
             (not (ellama-tools-dlp--bool-or-nil-p
                   (plist-get rule :enabled))))
    (error "DLP regex rule :enabled must be boolean or nil"))
  (when (and (plist-member rule :case-fold)
             (not (ellama-tools-dlp--bool-or-nil-p
                   (plist-get rule :case-fold))))
    (error "DLP regex rule :case-fold must be boolean or nil"))
  (when (and (plist-member rule :directions)
             (not (and (listp (plist-get rule :directions))
                       (cl-every (lambda (direction)
                                   (memq direction '(input output)))
                                 (plist-get rule :directions)))))
    (error "DLP regex rule :directions must contain `input'/'output'"))
  (when (and (plist-member rule :tools)
             (not (ellama-tools-dlp--string-list-like-p
                   (plist-get rule :tools))))
    (error "DLP regex rule :tools must be a list of strings or symbols"))
  (when (and (plist-member rule :args)
             (not (ellama-tools-dlp--string-list-like-p
                   (plist-get rule :args))))
    (error "DLP regex rule :args must be a list of strings or symbols"))
  (when (and (plist-member rule :severity)
             (not (or (null (plist-get rule :severity))
                      (stringp (plist-get rule :severity))
                      (symbolp (plist-get rule :severity)))))
    (error "DLP regex rule :severity must be nil, string, or symbol"))
  rule)

(defun ellama-tools-dlp--normalize-regex-rule (rule)
  "Return normalized regex RULE plist."
  (setq rule (copy-tree (ellama-tools-dlp--validate-regex-rule rule)))
  (when (plist-member rule :tools)
    (setq rule
          (plist-put rule :tools
                     (ellama-tools-dlp--normalize-name-list
                      (plist-get rule :tools)))))
  (when (plist-member rule :args)
    (setq rule
          (plist-put rule :args
                     (ellama-tools-dlp--normalize-name-list
                      (plist-get rule :args)))))
  rule)

(defun ellama-tools-dlp--regex-rule-cache-key (rule)
  "Build cache key for normalized regex RULE.
Include scoping fields because cached entries store the normalized rule plist."
  (copy-tree rule))

(defun ellama-tools-dlp--regex-rule-compile (rule)
  "Validate regex RULE pattern and return cached compile metadata."
  (let* ((rule* (ellama-tools-dlp--normalize-regex-rule rule))
         (key (ellama-tools-dlp--regex-rule-cache-key rule*))
         (cached (gethash key ellama-tools-dlp--regex-cache :missing)))
    (if (not (eq cached :missing))
        cached
      (let* ((case-fold-search (eq (plist-get rule* :case-fold) t))
             (entry
              (condition-case err
                  (progn
                    ;; Force regex parse now so invalid patterns fail early.
                    (string-match-p (plist-get rule* :pattern) "")
                    (list :status 'ok
                          :rule rule*))
                (invalid-regexp
                 (list :status 'error
                       :error-type 'invalid-regexp
                       :error-message (error-message-string err)
                       :rule rule*)))))
        (puthash key entry ellama-tools-dlp--regex-cache)
        entry))))

(defun ellama-tools-dlp--regex-rule-enabled-p (rule)
  "Return non-nil when regex RULE is enabled."
  (or (not (plist-member rule :enabled))
      (eq (plist-get rule :enabled) t)))

(defun ellama-tools-dlp--regex-rule-applies-p (rule context)
  "Return non-nil when regex RULE is applied to scan CONTEXT."
  (let ((direction (plist-get context :direction))
        (tool-name (plist-get context :tool-name))
        (arg-name (plist-get context :arg-name))
        (directions (plist-get rule :directions))
        (tools (plist-get rule :tools))
        (args (plist-get rule :args)))
    (and (ellama-tools-dlp--regex-rule-enabled-p rule)
         (or (null directions) (memq direction directions))
         (or (null tools) (member tool-name tools))
         (or (null args)
             (and arg-name (member arg-name args))))))

(defun ellama-tools-dlp--log-regex-error (context rule error-entry)
  "Record sanitized regex engine ERROR-ENTRY for RULE in CONTEXT."
  (ellama-tools-dlp--record-incident
   (list :type 'regex-error
         :timestamp (format-time-string "%FT%T%z")
         :direction (plist-get context :direction)
         :tool-name (plist-get context :tool-name)
         :arg-name (plist-get context :arg-name)
         :rule-id (plist-get rule :id)
         :error-type (plist-get error-entry :error-type)
         :error-message (plist-get error-entry :error-message))))

(defun ellama-tools-dlp--regex-rule-findings (text context rule)
  "Return regex findings for RULE against TEXT in CONTEXT."
  (let* ((compiled (ellama-tools-dlp--regex-rule-compile rule))
         (rule* (plist-get compiled :rule)))
    (cond
     ((not (eq (plist-get compiled :status) 'ok))
      (ellama-tools-dlp--log-regex-error context rule* compiled)
      nil)
     ((not (ellama-tools-dlp--regex-rule-applies-p rule* context))
      nil)
     (t
      (let ((case-fold-search (eq (plist-get rule* :case-fold) t))
            (pattern (plist-get rule* :pattern))
            (severity (plist-get rule* :severity))
            (rule-id (plist-get rule* :id))
            (pos 0)
            (findings nil))
        (condition-case err
            (save-match-data
              (while (and (<= pos (length text))
                          (string-match pattern text pos))
                (let ((start (match-beginning 0))
                      (end (match-end 0)))
                  (push (ellama-tools-dlp--make-finding
                         :rule-id rule-id
                         :detector 'regex
                         :severity severity
                         :match-start start
                         :match-end end)
                        findings)
                  ;; Avoid infinite loops on zero-length matches.
                  (setq pos (if (= start end) (1+ end) end)))))
          (error
           (ellama-tools-dlp--log-regex-error
            context
            rule*
            (list :error-type 'regex-runtime-error
                  :error-message (error-message-string err)))))
        (nreverse findings))))))

(defun ellama-tools-dlp--detect-regex-findings (text context &optional rules)
  "Return regex detector findings for TEXT in CONTEXT.
When RULES is nil, combine built-in and user regex rules."
  (ellama-tools-dlp--validate-scan-context context)
  (unless (stringp text)
    (error "DLP regex detection expects a string payload"))
  (let ((selected-rules (or rules
                            (append ellama-tools-dlp--default-regex-rules
                                    ellama-tools-dlp-regex-rules)))
        (all-findings nil))
    (dolist (rule selected-rules)
      (setq all-findings
            (nconc all-findings
                   (ellama-tools-dlp--regex-rule-findings text context rule))))
    all-findings))

(defun ellama-tools-dlp--shannon-entropy (text)
  "Return Shannon entropy (bits per char) for TEXT."
  (if (or (not (stringp text)) (= (length text) 0))
      0.0
    (let ((counts (make-hash-table :test 'equal))
          (len (float (length text)))
          (entropy 0.0))
      (dolist (ch (string-to-list text))
        (puthash ch (1+ (gethash ch counts 0)) counts))
      (maphash
       (lambda (_ch count)
         (let ((p (/ count len)))
           (setq entropy (+ entropy (* -1.0 p (log p 2))))))
       counts)
      entropy)))

(defun ellama-tools-dlp--env-secret-stage-length (_env-name env-value)
  "Reject ENV-VALUE outside configured length bounds."
  (let ((len (length env-value)))
    (cond
     ((< len (max 0 ellama-tools-dlp-env-secret-min-length))
      '(:reject too-short))
     ((> len (max 0 ellama-tools-dlp-env-secret-max-length))
      '(:reject too-long))
     (t
      nil))))

(defun ellama-tools-dlp--env-secret-stage-single-line (_env-name env-value)
  "Reject multiline ENV-VALUE strings."
  (when (string-match-p "[\r\n]" env-value)
    '(:reject multiline)))

(defun ellama-tools-dlp--env-secret-stage-reject-obvious-config
    (_env-name env-value)
  "Reject path/list/config-like ENV-VALUE strings."
  (cond
   ((string-match-p "\\`[[:digit:]]+\\'" env-value)
    '(:reject numeric-only))
   ((string-match-p "\\`[-[:alnum:]_]+\\'" env-value)
    (if (<= (length env-value) 16)
        '(:reject plain-word)
      nil))
   ((or (string-match-p "\\`[~/]" env-value)
        (string-match-p "\\`[A-Za-z]:[\\/]" env-value))
    '(:reject path-like))
   ((string-match-p "[\\/]" env-value)
    (if (or (string-match-p ":" env-value)
            (string-match-p "\\.[[:alpha:]][[:alnum:]]*\\'" env-value))
        '(:reject path-like)
      nil))
   ((string-match-p "," env-value)
    '(:reject list-like))
   ((string-match-p "\\`https?://" env-value)
    '(:reject url-like))
   (t
    nil)))

(defun ellama-tools-dlp--env-secret-stage-token-shape (_env-name env-value)
  "Score token-like ENV-VALUE shapes."
  (let ((score 0))
    (when (string-match-p "\\`[[:alnum:]_./+=:-]+\\'" env-value)
      (setq score (1+ score)))
    (when (string-match-p "[[:digit:]]" env-value)
      (setq score (1+ score)))
    (when (string-match-p "[[:upper:]]" env-value)
      (setq score (1+ score)))
    (when (string-match-p "[[:lower:]]" env-value)
      (setq score (1+ score)))
    (when (string-match-p "[-_=]" env-value)
      (setq score (1+ score)))
    (when (> score 0)
      (list :score score))))

(defun ellama-tools-dlp--env-secret-stage-known-prefix (_env-name env-value)
  "Score ENV-VALUE with known token prefixes."
  (cond
   ((string-match-p
     "\\`\\(sk-[[:alnum:]-]+\\|gh[pousr]_[[:alnum:]_]+\\|xox[baprs]-\\)"
     env-value)
    '(:score 4))
   ((string-match-p "\\`\\(AKIA\\|ASIA\\|AIza\\)" env-value)
    '(:score 3))
   (t
    nil)))

(defun ellama-tools-dlp--env-secret-stage-name-signal (env-name _env-value)
  "Score ENV-NAME when it look security-sensitive."
  (when (string-match-p ellama-tools-dlp--sensitive-env-name-regexp
                        (upcase env-name))
    '(:score 2)))

(defun ellama-tools-dlp--env-secret-stage-entropy (_env-name env-value)
  "Score ENV-VALUE by entropy."
  (let ((entropy (ellama-tools-dlp--shannon-entropy env-value)))
    (if (>= entropy ellama-tools-dlp-env-secret-entropy-threshold)
        (list :score 2)
      nil)))

(defun ellama-tools-dlp--env-secret-apply-stage-result (result score)
  "Apply heuristic stage RESULT to SCORE.
Return plist with keys `:score' and optional `:reject'."
  (cond
   ((null result)
    (list :score score))
   ((integerp result)
    (list :score (+ score result)))
   ((listp result)
    (list :score (+ score (or (plist-get result :score) 0))
          :reject (plist-get result :reject)))
   (t
    (list :score score :reject 'invalid-stage-result))))

(defun ellama-tools-dlp--env-secret-heuristic-evaluate (env-name env-value)
  "Evaluate env secret heuristic for ENV-NAME and ENV-VALUE."
  (let ((score 0)
        reject)
    (cl-block evaluate
      (dolist (stage ellama-tools-dlp-env-secret-heuristic-stages)
        (let ((stage-fn (if (functionp stage) stage
                          (and (symbolp stage) (fboundp stage) stage))))
          (unless stage-fn
            (setq reject 'invalid-stage)
            (ellama-tools-dlp--log-exact-secret-error
             nil 'invalid-stage stage env-name)
            (cl-return-from evaluate nil))
          (condition-case nil
              (let* ((result (funcall stage-fn env-name env-value))
                     (applied (ellama-tools-dlp--env-secret-apply-stage-result
                               result score)))
                (setq score (plist-get applied :score))
                (when (plist-get applied :reject)
                  (setq reject (plist-get applied :reject))
                  (cl-return-from evaluate nil)))
            (error
             (setq reject 'heuristic-stage-error)
             (ellama-tools-dlp--log-exact-secret-error
              nil 'heuristic-stage-error stage env-name)
             (cl-return-from evaluate nil))))))
    (list :accepted (and (null reject)
                         (>= score ellama-tools-dlp-env-secret-min-score))
          :score score
          :reject reject)))

(defun ellama-tools-dlp--parse-env-entry (entry)
  "Parse environment ENTRY and return cons `(NAME . VALUE)'."
  (when (and (stringp entry)
             (string-match "\\`\\([^=]+\\)=\\(.*\\)\\'" entry))
    (cons (match-string 1 entry)
          (match-string 2 entry))))

(defun ellama-tools-dlp--hex-encode-string (text)
  "Return lowercase hex encoding of TEXT in UTF-8."
  (mapconcat (lambda (byte) (format "%02x" byte))
             (string-to-list (encode-coding-string text 'utf-8 t))
             ""))

(defun ellama-tools-dlp--base64url-from-base64 (text &optional keep-padding)
  "Convert base64 TEXT to base64url.
When KEEP-PADDING is non-nil, keep trailing `=' padding."
  (let ((url (replace-regexp-in-string
              "/" "_"
              (replace-regexp-in-string "+" "-" text t t)
              t t)))
    (if keep-padding
        url
      (replace-regexp-in-string "=+\\'" "" url t t))))

(defun ellama-tools-dlp--exact-secret-variants-for-value (value)
  "Return precomputed exact-secret variants for VALUE.
Variants include raw, base64, base64url (padded and unpadded), and hex."
  (let ((seen (make-hash-table :test 'equal))
        (variants nil))
    (cl-labels ((add-variant (kind text)
                  (when (and (stringp text)
                             (> (length text) 0)
                             (not (gethash text seen)))
                    (puthash text t seen)
                    (push (list :kind kind :text text) variants))))
      (let* ((raw value)
             (bytes (encode-coding-string value 'utf-8 t))
             (b64 (base64-encode-string bytes t))
             (b64url (ellama-tools-dlp--base64url-from-base64 b64 nil))
             (b64url-padded (ellama-tools-dlp--base64url-from-base64 b64 t))
             (hex-lower (ellama-tools-dlp--hex-encode-string value))
             (hex-upper (upcase hex-lower)))
        (add-variant 'raw raw)
        (add-variant 'base64 b64)
        (add-variant 'base64url b64url)
        (add-variant 'base64url b64url-padded)
        (add-variant 'hex hex-lower)
        (add-variant 'hex hex-upper)))
    (nreverse variants)))

(defun ellama-tools-dlp--build-exact-secret-cache ()
  "Build env exact-secret cache from `process-environment'."
  (let ((signature (copy-sequence process-environment))
        (value-seen (make-hash-table :test 'equal))
        (candidate-meta nil)
        (variants nil))
    (if (not ellama-tools-dlp-scan-env-exact-secrets)
        (list :signature signature :variants nil :candidates nil)
      (dolist (entry process-environment)
        (let ((pair (ellama-tools-dlp--parse-env-entry entry)))
          (when pair
            (let* ((env-name (car pair))
                   (env-value (cdr pair)))
              (unless (gethash env-value value-seen)
                (let ((heuristic
                       (ellama-tools-dlp--env-secret-heuristic-evaluate
                        env-name env-value)))
                  (when (plist-get heuristic :accepted)
                    (puthash env-value t value-seen)
                    (push (list :env-name env-name
                                :score (plist-get heuristic :score))
                          candidate-meta)
                    (dolist (variant
                             (ellama-tools-dlp--exact-secret-variants-for-value
                              env-value))
                      (push (list :text (plist-get variant :text)
                                  :kind (plist-get variant :kind)
                                  :env-name env-name)
                            variants)))))))))
      (list :signature signature
            :candidates (nreverse candidate-meta)
            :variants (nreverse variants)))))

(defun ellama-tools-dlp--invalidate-exact-secret-cache ()
  "Invalidate env exact-secret cache."
  (setq ellama-tools-dlp--exact-secret-cache nil))

(defun ellama-tools-dlp--refresh-exact-secret-cache ()
  "Refresh env exact-secret cache and return it."
  (setq ellama-tools-dlp--exact-secret-cache
        (condition-case nil
            (ellama-tools-dlp--build-exact-secret-cache)
          (error
           (ellama-tools-dlp--log-exact-secret-error nil 'cache-build-error)
           (list :signature (copy-sequence process-environment)
                 :candidates nil
                 :variants nil)))))

(defun ellama-tools-dlp--exact-secret-cache-current ()
  "Return current env exact-secret cache, refreshing when environment change."
  (let ((signature (copy-sequence process-environment)))
    (if (and (listp ellama-tools-dlp--exact-secret-cache)
             (equal (plist-get ellama-tools-dlp--exact-secret-cache :signature)
                    signature))
        ellama-tools-dlp--exact-secret-cache
      (ellama-tools-dlp--refresh-exact-secret-cache))))

(defun ellama-tools-dlp--exact-secret-cache-candidates ()
  "Return safe candidate metadata from exact-secret cache."
  (copy-tree (plist-get (ellama-tools-dlp--exact-secret-cache-current)
                        :candidates)))

(defun ellama-tools-dlp--exact-secret-cache-variants ()
  "Return cached exact-secret variants.
This helper is for tests and internal inspection."
  (copy-tree (plist-get (ellama-tools-dlp--exact-secret-cache-current)
                        :variants)))

(defun ellama-tools-dlp--detect-exact-secret-findings (text context)
  "Return exact env-secret findings for TEXT in CONTEXT."
  (ellama-tools-dlp--validate-scan-context context)
  (unless (stringp text)
    (error "DLP exact-secret detection expects a string payload"))
  (let* ((cache (ellama-tools-dlp--exact-secret-cache-current))
         (variants (plist-get cache :variants))
         (findings nil))
    (dolist (variant variants)
      (let ((needle (plist-get variant :text))
            (pos 0))
        (when (and (stringp needle) (> (length needle) 0))
          (while (and (<= pos (length text))
                      (string-match (regexp-quote needle) text pos))
            (let ((start (match-beginning 0))
                  (end (match-end 0)))
              (push (ellama-tools-dlp--make-finding
                     :rule-id ellama-tools-dlp--exact-secret-rule-id
                     :detector 'exact-secret
                     :severity 'high
                     :match-start start
                     :match-end end)
                    findings)
              (setq pos (if (= start end) (1+ end) end)))))))
    (let ((seen (make-hash-table :test 'equal))
          (deduped nil))
      (dolist (finding (nreverse findings))
        (let ((key (list (plist-get finding :match-start)
                         (plist-get finding :match-end)
                         (plist-get finding :rule-id))))
          (unless (gethash key seen)
            (puthash key t seen)
            (push finding deduped))))
      (nreverse deduped))))

(defun ellama-tools-dlp--normalize-text-line-endings (text)
  "Normalize line endings in TEXT to LF."
  (replace-regexp-in-string "\r" "\n"
                            (replace-regexp-in-string "\r\n" "\n" text t t)
                            t t))

(defun ellama-tools-dlp--remove-invisible-characters (text)
  "Remove zero-width and invisible characters from TEXT."
  (replace-regexp-in-string ellama-tools-dlp--zero-width-regexp "" text t t))

(defun ellama-tools-dlp--normalize-text (text)
  "Normalize TEXT for DLP scanning.
Normalize line endings, remove invisible characters, then apply NFKC.
If NFKC normalization fails, return the pre-NFKC normalized text."
  (unless (stringp text)
    (error "DLP normalization expects a string"))
  (let ((normalized (ellama-tools-dlp--remove-invisible-characters
                     (ellama-tools-dlp--normalize-text-line-endings text))))
    (condition-case nil
        (progn
          (require 'ucs-normalize nil t)
          (if (fboundp 'ucs-normalize-NFKC-string)
              (ucs-normalize-NFKC-string normalized)
            normalized))
      (error normalized))))

(defun ellama-tools-dlp--string-prefix-bytes (text max-bytes)
  "Return longest prefix of TEXT with byte size at most MAX-BYTES."
  (unless (stringp text)
    (error "DLP truncation expects a string"))
  (unless (ellama-tools-dlp--nonnegative-integer-p max-bytes)
    (error "DLP max byte size must be a non-negative integer"))
  (if (<= (string-bytes text) max-bytes)
      text
    (let ((low 0)
          (high (length text)))
      (while (< low high)
        (let* ((mid (ceiling (+ low high) 2))
               (candidate (substring text 0 mid)))
          (if (<= (string-bytes candidate) max-bytes)
              (setq low mid)
            (setq high (1- mid)))))
      (substring text 0 low))))

(defun ellama-tools-dlp--log-truncation (context original-bytes scanned-bytes)
  "Record a truncation incident for CONTEXT.
ORIGINAL-BYTES is the original payload byte length.
SCANNED-BYTES is the truncated payload byte length."
  (ellama-tools-dlp--record-incident
   (list :type 'truncation
         :timestamp (format-time-string "%FT%T%z")
         :direction (plist-get context :direction)
         :tool-name (plist-get context :tool-name)
         :arg-name (plist-get context :arg-name)
         :payload-length original-bytes
         :scanned-length scanned-bytes
         :truncated t)))

(defun ellama-tools-dlp--truncate-payload (text context)
  "Apply scan-size limit to TEXT and return payload/CONTEXT plist.
Return plist with keys `:text' and `:context'."
  (ellama-tools-dlp--validate-scan-context context)
  (unless (stringp text)
    (error "DLP truncation expects a string payload"))
  (let* ((original-bytes (string-bytes text))
         (max-bytes (max 0 ellama-tools-dlp-max-scan-size))
         (context* (copy-tree context)))
    (setq context* (plist-put context* :payload-length original-bytes))
    (if (<= original-bytes max-bytes)
        (progn
          (setq context* (plist-put context* :truncated nil))
          (list :text text :context context*))
      (let* ((truncated (ellama-tools-dlp--string-prefix-bytes text max-bytes))
             (scanned-bytes (string-bytes truncated)))
        (setq context* (plist-put context* :truncated t))
        (ellama-tools-dlp--log-truncation context* original-bytes scanned-bytes)
        (list :text truncated :context context*)))))

(defun ellama-tools-dlp--prepare-payload (text context)
  "Prepare TEXT for scanning and return payload/CONTEXT plist.
This runs truncation before normalization and normalizes once."
  (let* ((truncated (ellama-tools-dlp--truncate-payload text context))
         (truncated-text (plist-get truncated :text))
         (truncated-context (plist-get truncated :context))
         (normalized-text (ellama-tools-dlp--normalize-text truncated-text)))
    (list :text normalized-text
          :context truncated-context)))

(defun ellama-tools-dlp--policy-name-string (value)
  "Return normalized string form for policy VALUE."
  (cond
   ((symbolp value) (symbol-name value))
   ((stringp value) value)
   (t nil)))

(defun ellama-tools-dlp--policy-direction-match-p (override context)
  "Return non-nil when policy OVERRIDE direction matches CONTEXT."
  (let ((configured (plist-get override :direction))
        (direction (plist-get context :direction)))
    (cond
     ((null configured) t)
     ((symbolp configured) (eq configured direction))
     ((listp configured) (memq direction configured))
     (t nil))))

(defun ellama-tools-dlp--policy-arg-match-p (configured-arg actual-arg)
  "Return non-nil when CONFIGURED-ARG is applied to ACTUAL-ARG path.
Match exact arg names and nested path prefixes like `arg.', `arg[0]'."
  (or (equal configured-arg actual-arg)
      (and (stringp configured-arg)
           (stringp actual-arg)
           (string-prefix-p configured-arg actual-arg)
           (> (length actual-arg) (length configured-arg))
           (memq (aref actual-arg (length configured-arg)) '(?. ?\[)))))

(defun ellama-tools-dlp--policy-override-match-p (override context)
  "Return non-nil when policy OVERRIDE is applied to scan CONTEXT."
  (let ((tool (ellama-tools-dlp--policy-name-string (plist-get override :tool)))
        (arg (ellama-tools-dlp--policy-name-string (plist-get override :arg))))
    (and tool
         (string= tool (plist-get context :tool-name))
         (ellama-tools-dlp--policy-direction-match-p override context)
         (or (null arg)
             (ellama-tools-dlp--policy-arg-match-p
              arg
              (plist-get context :arg-name))))))

(defun ellama-tools-dlp--default-action-for-direction (direction)
  "Return configured default action for DIRECTION."
  (pcase direction
    ('input ellama-tools-dlp-input-default-action)
    ('output ellama-tools-dlp-output-default-action)
    (_ 'allow)))

(defun ellama-tools-dlp--policy-exception-p (context)
  "Return non-nil when CONTEXT matches a policy exception override."
  (cl-some (lambda (override)
             (and (plist-get override :except)
                  (ellama-tools-dlp--policy-override-match-p override context)))
           (ellama-tools-dlp--effective-policy-overrides)))

(defun ellama-tools-dlp--policy-override-action (context)
  "Return last matching override action for CONTEXT, or nil."
  (let (result)
    (dolist (override (ellama-tools-dlp--effective-policy-overrides))
      (when (and (ellama-tools-dlp--policy-override-match-p override context)
                 (memq (plist-get override :action)
                       '(allow warn block redact)))
        (setq result (plist-get override :action))))
    result))

(defun ellama-tools-dlp--policy-action (context findings)
  "Return configured policy action for CONTEXT and FINDINGS.
This function ignore rollout mode and return the configured action."
  (ellama-tools-dlp--validate-scan-context context)
  (cond
   ((null findings) 'allow)
   ((ellama-tools-dlp--policy-exception-p context) 'allow)
   (t
    (or (ellama-tools-dlp--policy-override-action context)
        (ellama-tools-dlp--default-action-for-direction
         (plist-get context :direction))
        'allow))))

(defun ellama-tools-dlp--effective-policy-overrides ()
  "Return built-in plus user policy overrides."
  (append ellama-tools-dlp--default-policy-overrides
          ellama-tools-dlp-policy-overrides))

(defun ellama-tools-dlp--findings-rule-ids (findings)
  "Return sorted unique rule IDs from FINDINGS as strings."
  (let ((seen (make-hash-table :test 'equal))
        ids)
    (dolist (finding findings)
      (let ((rule-id (plist-get finding :rule-id)))
        (when rule-id
          (setq rule-id (ellama-tools-dlp--policy-name-string rule-id))
          (unless (gethash rule-id seen)
            (puthash rule-id t seen)
            (push rule-id ids)))))
    (sort ids #'string<)))

(defun ellama-tools-dlp--findings-detectors (findings)
  "Return sorted unique detector names from FINDINGS as strings."
  (let ((seen (make-hash-table :test 'equal))
        detectors)
    (dolist (finding findings)
      (let ((detector (plist-get finding :detector)))
        (when detector
          (setq detector (ellama-tools-dlp--policy-name-string detector))
          (unless (gethash detector seen)
            (puthash detector t seen)
            (push detector detectors)))))
    (sort detectors #'string<)))

(defun ellama-tools-dlp--format-safe-message (context action findings)
  "Return safe user-facing DLP message for CONTEXT ACTION and FINDINGS."
  (let* ((direction (symbol-name (plist-get context :direction)))
         (tool (plist-get context :tool-name))
         (arg (plist-get context :arg-name))
         (rule-ids (ellama-tools-dlp--findings-rule-ids findings))
         (rule-text (if rule-ids
                        (mapconcat #'identity rule-ids ",")
                      "unknown")))
    (format "DLP %s %s for tool %s%s (rules: %s)"
            action
            direction
            tool
            (if arg (format " arg %s" arg) "")
            rule-text)))

(defun ellama-tools-dlp--redaction-placeholder (rule-id)
  "Return placeholder string for RULE-ID."
  (replace-regexp-in-string
   "RULE_ID"
   (or (ellama-tools-dlp--policy-name-string rule-id) "unknown")
   ellama-tools-dlp-redaction-placeholder-format
   t t))

(defun ellama-tools-dlp--merge-redaction-spans (findings text-length)
  "Return merged redaction spans from FINDINGS for TEXT-LENGTH.
Each span is a plist with `:start', `:end', and `:rule-id'."
  (let ((spans nil))
    (dolist (finding findings)
      (let ((start (plist-get finding :match-start))
            (end (plist-get finding :match-end)))
        (unless (and (integerp start) (integerp end))
          (error "DLP redaction require match spans"))
        (when (or (< start 0) (< end 0) (> start end) (> end text-length))
          (error "DLP redaction span is out of range"))
        (push (list :start start
                    :end end
                    :rule-id (plist-get finding :rule-id))
              spans)))
    (setq spans
          (sort spans
                (lambda (a b)
                  (if (= (plist-get a :start) (plist-get b :start))
                      (< (plist-get a :end) (plist-get b :end))
                    (< (plist-get a :start) (plist-get b :start))))))
    (let (merged)
      (dolist (span spans)
        (let ((last (car merged)))
          (if (and last (<= (plist-get span :start) (plist-get last :end)))
              (progn
                (setcar merged
                        (list :start (plist-get last :start)
                              :end (max (plist-get last :end)
                                        (plist-get span :end))
                              :rule-id
                              (if (equal (plist-get last :rule-id)
                                         (plist-get span :rule-id))
                                  (plist-get last :rule-id)
                                'multiple))))
            (push span merged))))
      (nreverse merged))))

(defun ellama-tools-dlp--apply-redaction (text findings)
  "Return redacted TEXT based on FINDINGS."
  (unless (stringp text)
    (error "DLP redaction expects a string payload"))
  (let* ((spans (ellama-tools-dlp--merge-redaction-spans
                 findings (length text)))
         (cursor 0)
         (parts nil))
    (dolist (span spans)
      (push (substring text cursor (plist-get span :start)) parts)
      (push (ellama-tools-dlp--redaction-placeholder
             (plist-get span :rule-id))
            parts)
      (setq cursor (plist-get span :end)))
    (push (substring text cursor) parts)
    (apply #'concat (nreverse parts))))

(defun ellama-tools-dlp--apply-enforcement
    (text context findings configured-action)
  "Return DLP verdict for TEXT in CONTEXT with FINDINGS.
CONFIGURED-ACTION is the policy action before rollout mode adjustment."
  (let* ((mode ellama-tools-dlp-mode)
         (action (if (or (null findings) (eq mode 'monitor))
                     'allow
                   configured-action))
         (message (and findings
                       (ellama-tools-dlp--format-safe-message
                        context
                        (if (eq mode 'monitor) 'monitor action)
                        findings))))
    (condition-case nil
        (pcase action
          ('allow
           (ellama-tools-dlp--make-verdict
            :action 'allow
            :message message
            :findings findings))
          ('warn
           (ellama-tools-dlp--make-verdict
            :action 'warn
            :message message
            :findings findings))
          ('block
           (ellama-tools-dlp--make-verdict
            :action 'block
            :message message
            :findings findings))
          ('redact
           (if (or (not (eq (plist-get context :direction) 'output))
                   (plist-get context :truncated))
               (ellama-tools-dlp--make-verdict
                :action 'block
                :message (ellama-tools-dlp--format-safe-message
                          context 'block findings)
                :findings findings)
             (ellama-tools-dlp--make-verdict
              :action 'redact
              :message message
              :findings findings
              :redacted-text (ellama-tools-dlp--apply-redaction
                              text findings))))
          (_
           (ellama-tools-dlp--make-verdict
            :action 'allow
            :message message
            :findings findings)))
      (error
       ;; Redaction failure must fail closed.  Other verdict construction
       ;; errors also fail closed for safety when findings exist.
       (ellama-tools-dlp--make-verdict
        :action 'block
        :message (ellama-tools-dlp--format-safe-message context 'block findings)
        :findings findings)))))

(defun ellama-tools-dlp--log-scan-decision
    (context findings verdict configured-action)
  "Record a sanitized DLP decision incident.
CONTEXT, FINDINGS, VERDICT and CONFIGURED-ACTION will be recorded."
  (ellama-tools-dlp--record-incident
   (list :type 'scan-decision
         :timestamp (format-time-string "%FT%T%z")
         :direction (plist-get context :direction)
         :tool-name (plist-get context :tool-name)
         :arg-name (plist-get context :arg-name)
         :mode ellama-tools-dlp-mode
         :action (plist-get verdict :action)
         :configured-action configured-action
         :rule-ids (ellama-tools-dlp--findings-rule-ids findings)
         :detectors (ellama-tools-dlp--findings-detectors findings)
         :findings-count (length findings)
         :payload-length (plist-get context :payload-length)
         :truncated (plist-get context :truncated))))

(defun ellama-tools-dlp--log-scan-error (context error-type)
  "Record sanitized internal DLP scan ERROR-TYPE for CONTEXT."
  (ellama-tools-dlp--record-incident
   (list :type 'scan-error
         :timestamp (format-time-string "%FT%T%z")
         :direction (plist-get context :direction)
         :tool-name (plist-get context :tool-name)
         :arg-name (plist-get context :arg-name)
         :error-type error-type)))

(defun ellama-tools-dlp--sort-findings (findings)
  "Return FINDINGS sorted by position and rule ID."
  (sort (copy-tree findings)
        (lambda (a b)
          (let ((sa (or (plist-get a :match-start) most-positive-fixnum))
                (sb (or (plist-get b :match-start) most-positive-fixnum))
                (ea (or (plist-get a :match-end) most-positive-fixnum))
                (eb (or (plist-get b :match-end) most-positive-fixnum))
                (ra (format "%s" (plist-get a :rule-id)))
                (rb (format "%s" (plist-get b :rule-id))))
            (cond
             ((/= sa sb) (< sa sb))
             ((/= ea eb) (< ea eb))
             (t (string< ra rb)))))))

(defun ellama-tools-dlp--detect-findings (text context)
  "Return combined DLP findings for TEXT in CONTEXT."
  (let ((findings (ellama-tools-dlp--detect-regex-findings text context)))
    (condition-case nil
        (setq findings
              (nconc findings
                     (ellama-tools-dlp--detect-exact-secret-findings
                      text context)))
      (error
       (ellama-tools-dlp--log-exact-secret-error
        context 'detect-runtime-error)))
    (ellama-tools-dlp--sort-findings findings)))

(defun ellama-tools-dlp--scan-text (text context)
  "Scan TEXT in CONTEXT and return DLP result plist.
Return plist with keys `:context', `:findings', and `:verdict'."
  (ellama-tools-dlp--validate-scan-context context)
  (unless (stringp text)
    (error "DLP scan expects a string payload"))
  (if (not ellama-tools-dlp-enabled)
      (let ((context* (plist-put (copy-tree context) :payload-length
                                 (string-bytes text))))
        (list :context context*
              :findings nil
              :verdict (ellama-tools-dlp--make-verdict :action 'allow)))
    (condition-case nil
        (let* ((prepared (ellama-tools-dlp--prepare-payload text context))
               (prepared-text (plist-get prepared :text))
               (prepared-context (plist-get prepared :context))
               (findings (ellama-tools-dlp--detect-findings
                          prepared-text prepared-context))
               (configured-action
                (ellama-tools-dlp--policy-action prepared-context findings))
               (verdict (ellama-tools-dlp--apply-enforcement
                         prepared-text prepared-context findings
                         configured-action)))
          (setq verdict
                (plist-put verdict :configured-action configured-action))
          (ellama-tools-dlp--log-scan-decision
           prepared-context findings verdict configured-action)
          (list :context prepared-context
                :findings findings
                :verdict verdict))
      (error
       (ellama-tools-dlp--log-scan-error context 'internal-error)
       (let* ((direction (plist-get context :direction))
              (fail-open (if (eq direction 'input)
                             ellama-tools-dlp-input-fail-open
                           ellama-tools-dlp-output-fail-open))
              (action (if fail-open 'allow 'block))
              (message (unless fail-open
                         (format "DLP blocked %s due to internal error"
                                 (symbol-name direction)))))
         (list :context context
               :findings nil
               :verdict (ellama-tools-dlp--make-verdict
                         :action action
                         :message message)))))))

(defun ellama-tools-dlp-reset-runtime-state ()
  "Reset in-memory DLP runtime state used for testing and tuning.
Clear incident logs and detector caches."
  (interactive)
  (ellama-tools-dlp--clear-incident-log)
  (ellama-tools-dlp--clear-regex-cache)
  (ellama-tools-dlp--invalidate-exact-secret-cache)
  t)

(defun ellama-tools-dlp--increment-count (table key)
  "Increment hash TABLE counter for KEY."
  (puthash key (1+ (gethash key table 0)) table))

(defun ellama-tools-dlp--hash-counts-to-alist (table)
  "Return sorted alist view of count hash TABLE."
  (let (result)
    (maphash (lambda (key value)
               (push (cons key value) result))
             table)
    (sort result
          (lambda (a b)
            (if (= (cdr a) (cdr b))
                (string< (format "%s" (car a))
                         (format "%s" (car b)))
              (> (cdr a) (cdr b)))))))

(defun ellama-tools-dlp-incident-stats (&optional count)
  "Return aggregated incident stats for recent incidents.
When COUNT is non-nil, aggregate only the newest COUNT incidents."
  (let* ((incidents (ellama-tools-dlp-recent-incidents count))
         (by-type (make-hash-table :test 'equal))
         (by-action (make-hash-table :test 'equal))
         (by-tool (make-hash-table :test 'equal))
         (by-rule-id (make-hash-table :test 'equal))
         (truncated-count 0))
    (dolist (incident incidents)
      (let ((type (plist-get incident :type))
            (action (plist-get incident :action))
            (tool (plist-get incident :tool-name))
            (rule-ids (plist-get incident :rule-ids)))
        (when type
          (ellama-tools-dlp--increment-count by-type type))
        (when action
          (ellama-tools-dlp--increment-count by-action action))
        (when tool
          (ellama-tools-dlp--increment-count by-tool tool))
        (when (plist-get incident :truncated)
          (setq truncated-count (1+ truncated-count)))
        (when (listp rule-ids)
          (dolist (rule-id rule-ids)
            (ellama-tools-dlp--increment-count by-rule-id rule-id)))))
    (list :total (length incidents)
          :truncated-count truncated-count
          :by-type (ellama-tools-dlp--hash-counts-to-alist by-type)
          :by-action (ellama-tools-dlp--hash-counts-to-alist by-action)
          :by-tool (ellama-tools-dlp--hash-counts-to-alist by-tool)
          :by-rule-id (ellama-tools-dlp--hash-counts-to-alist by-rule-id))))

(defun ellama-tools-dlp--stats-section-lines (title rows)
  "Return formatted lines for stats section TITLE from ROWS."
  (cons (format "%s:" title)
        (if rows
            (mapcar (lambda (row)
                      (format "  %s: %d" (car row) (cdr row)))
                    rows)
          '("  (none)"))))

(defun ellama-tools-dlp-incident-stats-report (&optional count)
  "Return human-readable DLP incident stats report string.
When COUNT is non-nil, summarize only the newest COUNT incidents."
  (let* ((stats (ellama-tools-dlp-incident-stats count))
         (header (list
                  (if count
                      (format "Ellama DLP Incident Stats (recent %d)" count)
                    "Ellama DLP Incident Stats")
                  (format "Total incidents: %d" (plist-get stats :total))
                  (format "Truncated incidents: %d"
                          (plist-get stats :truncated-count))
                  ""))
         (lines (append
                 header
                 (ellama-tools-dlp--stats-section-lines
                  "By type" (plist-get stats :by-type))
                 '("")
                 (ellama-tools-dlp--stats-section-lines
                  "By action" (plist-get stats :by-action))
                 '("")
                 (ellama-tools-dlp--stats-section-lines
                  "By tool" (plist-get stats :by-tool))
                 '("")
                 (ellama-tools-dlp--stats-section-lines
                  "By rule id" (plist-get stats :by-rule-id)))))
    (concat (mapconcat #'identity lines "\n") "\n")))

(defun ellama-tools-dlp-show-incident-stats (&optional count)
  "Show DLP incident stats report in a temporary buffer.
When COUNT is non-nil, summarize only the newest COUNT incidents."
  (interactive
   (list
    (let ((value (read-number "Recent incidents (0 = all): " 0)))
      (unless (zerop value)
        value))))
  (with-output-to-temp-buffer "*Ellama DLP Incident Stats*"
    (princ (ellama-tools-dlp-incident-stats-report count))))

(provide 'ellama-tools-dlp)
;;; ellama-tools-dlp.el ends here
