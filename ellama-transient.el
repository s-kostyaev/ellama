;;; ellama-transient.el --- Transient menus for ellama -*- lexical-binding: t; package-lint-main-file: "ellama.el"; -*-

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
(require 'ellama)
(require 'transient)
(require 'ellama-context)
(eval-when-compile
  (require 'llm-ollama)
  (require 'llm-openai))

(defcustom ellama-transient-system-show-limit 45
  "Maximum length of system message to show."
  :type 'ingeger
  :group 'ellama)

(defvaralias 'ellama-transient-ollama-model-name
  'ellama-transient-model-name)
(defvar ellama-transient-model-name "")
(defvar ellama-transient-temperature 0.7)
(defvar ellama-transient-context-length 4096)
(defvar ellama-transient-host "localhost")
(defvar ellama-transient-port 11434)
(defvar ellama-transient-url nil)
(defvar ellama-transient-provider nil)
(defvar ellama--current-session-uid)

(defun ellama-transient-system-show ()
  "Show transient system message."
  (format "System message (%s)"
          (string-limit (car (string-lines ellama-global-system))
                        ellama-transient-system-show-limit)))

(transient-define-suffix ellama-transient-set-system ()
  "Set system message.
If a region is active, use the text within the region as the system message.
Otherwise, prompt the user to enter a system message."
  (interactive)
  (if (region-active-p)
      (setq ellama-global-system (buffer-substring-no-properties
                                  (region-beginning) (region-end)))
    (let* ((msg-string (read-string "Set system mesage: "))
           (msg (when (not (string-empty-p msg-string)) msg-string)))
      (setq ellama-global-system msg))))

(defun ellama-transient-set-system-from-buffer ()
  "Set system message from current buffer."
  (interactive)
  (setq ellama-global-system (buffer-substring-no-properties
                              (point-min) (point-max))))

(transient-define-suffix ellama-transient-set-model ()
  "Set model name."
  (interactive)
  (setq ellama-transient-model-name
        (ellama-transient-read-model-name ellama-transient-provider)))

(defalias 'ellama-transient-set-ollama-model
  'ellama-transient-set-model)

(transient-define-suffix ellama-transient-set-temperature ()
  "Set temperature value."
  (interactive)
  (setq ellama-transient-temperature (read-number "Enter temperature: ")))

(transient-define-suffix ellama-transient-set-context-length ()
  "Set context length."
  (interactive)
  (setq ellama-transient-context-length (read-number "Enter context length: ")))

(transient-define-suffix ellama-transient-set-host ()
  "Set host address."
  (interactive)
  (setq ellama-transient-host (read-string "Enter host: ")))

(transient-define-suffix ellama-transient-set-port ()
  "Set port number."
  (interactive)
  (setq ellama-transient-port (read-number "Enter port: ")))

(transient-define-suffix ellama-transient-set-url ()
  "Set API URL."
  (interactive)
  (setq ellama-transient-url (read-string "Enter URL: " ellama-transient-url)))

(transient-define-suffix ellama-transient-reset-model-fields ()
  "Reset model fields to provider defaults."
  (interactive)
  (setq ellama-transient-model-name nil
        ellama-transient-temperature nil
        ellama-transient-context-length nil))

(defvar ellama-provider-list '(ellama-provider
                               ellama-coding-provider
                               ellama-translation-provider
                               ellama-extraction-provider
                               ellama-summarization-provider
                               ellama-naming-provider)
  "List of providers.")

(transient-define-suffix ellama-transient-model-get-from-provider ()
  "Fill transient model from provider."
  (interactive)
  (ellama-fill-transient-model
   (eval (read
          (completing-read "Select provider: "
                           (mapcar #'prin1-to-string
                                   ellama-provider-list))))))

(transient-define-suffix ellama-transient-model-get-from-current-session ()
  "Fill transient model from current session."
  (interactive)
  (when-let ((session (ellama-get-current-session)))
    (ellama-fill-transient-model
     (ellama-session-provider session))))

(transient-define-suffix ellama-transient-set-provider ()
  "Set transient model to provider."
  (interactive)
  (let ((provider (read
                   (completing-read "Select provider: "
                                    (mapcar #'prin1-to-string
                                            ellama-provider-list)))))
    (set provider
         (ellama-construct-provider-from-transient (symbol-value provider)))
    ;; if you change `ellama-provider' you probably want new chat session
    (when (equal provider 'ellama-provider)
      (setq ellama--current-session-id nil
            ellama--current-session-uid nil))))

;;;###autoload (autoload 'ellama-select-model "ellama-transient" nil t)
(transient-define-prefix ellama-select-model ()
  "Select model."
  [["Model"
    ("f" "Load from provider" ellama-transient-model-get-from-provider
     :transient t)
    ("F" "Load from current session"
     ellama-transient-model-get-from-current-session
     :description (lambda ()
                    (format "Load from current session (%s)"
                            ellama--current-session-id))
     :transient t)
    ("m" "Set Model" ellama-transient-set-model
     :transient t
     :description ellama-transient-model-description)
    ("t" "Set Temperature" ellama-transient-set-temperature
     :transient t
     :description ellama-transient-temperature-description)
    ("c" "Set Context Length" ellama-transient-set-context-length
     :if (lambda () (ellama-transient--ollama-provider-p
                     ellama-transient-provider))
     :transient t
     :description ellama-transient-context-length-description)
    ("r" "Reset model fields" ellama-transient-reset-model-fields
     :transient t)
    ("S" "Set provider" ellama-transient-set-provider
     :transient t)
    ("s" "Set provider and quit" ellama-transient-set-provider)]
   ["Connection"
    ("h" "Set Host" ellama-transient-set-host
     :if (lambda () (ellama-transient--ollama-provider-p
                     ellama-transient-provider))
     :transient t
     :description (lambda () (if ellama-transient-host
                                 (format "Host (%s)" ellama-transient-host)
                               "Host")))
    ("p" "Set Port" ellama-transient-set-port
     :if (lambda () (ellama-transient--ollama-provider-p
                     ellama-transient-provider))
     :transient t
     :description (lambda () (if ellama-transient-port
                                 (format "Port (%s)" ellama-transient-port)
                               "Port")))
    ("u" "Set URL" ellama-transient-set-url
     :if (lambda () (ellama-transient--openai-compatible-provider-p
                     ellama-transient-provider))
     :transient t
     :description (lambda () (if ellama-transient-url
                                 (format "URL (%s)" ellama-transient-url)
                               "URL")))]
   ["Quit" ("q" "Quit" transient-quit-one)]])

(defalias 'ellama-select-ollama-model 'ellama-select-model)

(defun ellama-transient--default-value-p (value)
  "Return non-nil when VALUE means provider default."
  (or (null value)
      (and (stringp value) (string-empty-p value))))

(defun ellama-transient--field-description (name value &optional format)
  "Return description for NAME and VALUE.
FORMAT is used for non-default VALUE."
  (format "%s (%s)" name
          (if (ellama-transient--default-value-p value)
              "default"
            (if format
                (format format value)
              value))))

(defun ellama-transient-model-description ()
  "Return transient model description."
  (ellama-transient--field-description "Model" ellama-transient-model-name))

(defun ellama-transient-temperature-description ()
  "Return transient temperature description."
  (ellama-transient--field-description
   "Temperature" ellama-transient-temperature "%.2f"))

(defun ellama-transient-context-length-description ()
  "Return transient context length description."
  (ellama-transient--field-description
   "Context Length" ellama-transient-context-length "%d"))

(defun ellama-transient--ollama-provider-p (provider)
  "Return non-nil when PROVIDER is an Ollama provider."
  (declare-function llm-ollama-p "ext:llm-ollama")
  (and provider
       (fboundp 'llm-ollama-p)
       (llm-ollama-p provider)))

(defun ellama-transient--openai-compatible-provider-p (provider)
  "Return non-nil when PROVIDER is OpenAI-compatible."
  (declare-function llm-openai-compatible-p "ext:llm-openai")
  (and provider
       (fboundp 'llm-openai-compatible-p)
       (llm-openai-compatible-p provider)))

(defun ellama-transient--openai-provider-p (provider)
  "Return non-nil when PROVIDER is an OpenAI provider."
  (declare-function llm-openai-p "ext:llm-openai")
  (and provider
       (fboundp 'llm-openai-p)
       (llm-openai-p provider)))

(defun ellama-transient--alist (value)
  "Return VALUE as an alist."
  (cond
   ((vectorp value) (append value nil))
   ((listp value) value)))

(defun ellama-transient--standard-temperature (provider)
  "Return default chat temperature for PROVIDER."
  (declare-function llm-standard-chat-provider-p "ext:llm-provider-utils")
  (declare-function llm-standard-chat-provider-default-chat-temperature
                    "ext:llm-provider-utils")
  (when (and provider
             (fboundp 'llm-standard-chat-provider-p)
             (llm-standard-chat-provider-p provider))
    (llm-standard-chat-provider-default-chat-temperature provider)))

(defun ellama-transient--set-standard-temperature (provider)
  "Set PROVIDER default chat temperature from transient."
  (declare-function llm-standard-chat-provider-p "ext:llm-provider-utils")
  (declare-function llm-standard-chat-provider-default-chat-temperature
                    "ext:llm-provider-utils")
  (declare-function (setf llm-standard-chat-provider-default-chat-temperature)
                    "ext:llm-provider-utils")
  (when (and provider
             (fboundp 'llm-standard-chat-provider-p)
             (llm-standard-chat-provider-p provider))
    (setf (llm-standard-chat-provider-default-chat-temperature provider)
          ellama-transient-temperature)))

(defun ellama-transient--provider-context-length (provider)
  "Return default context length from PROVIDER."
  (declare-function llm-ollama-default-chat-non-standard-params
                    "ext:llm-ollama")
  (when (ellama-transient--ollama-provider-p provider)
    (when-let ((params (llm-ollama-default-chat-non-standard-params provider)))
      (alist-get "num_ctx" (ellama-transient--alist params)
                 nil nil #'string=))))

(defun ellama-transient--provider-model (provider)
  "Return chat model from PROVIDER."
  (declare-function llm-ollama-chat-model "ext:llm-ollama")
  (declare-function llm-openai-chat-model "ext:llm-openai")
  (declare-function llm-openai-compatible-chat-model "ext:llm-openai")
  (cond
   ((ellama-transient--ollama-provider-p provider)
    (llm-ollama-chat-model provider))
   ((ellama-transient--openai-compatible-provider-p provider)
    (llm-openai-compatible-chat-model provider))
   ((ellama-transient--openai-provider-p provider)
    (llm-openai-chat-model provider))))

(defun ellama-transient--provider-models (provider)
  "Return available chat models for PROVIDER."
  (when (and provider (member 'model-list (llm-capabilities provider)))
    (condition-case err
        (llm-models provider)
      (error
       (message "Ellama could not fetch model list: %s" err)
       nil))))

(defun ellama-transient-read-model-name (&optional provider)
  "Read model name for PROVIDER."
  (let* ((provider (or provider ellama-provider))
         (models (ellama-transient--provider-models provider))
         (default (if (ellama-transient--default-value-p
                       ellama-transient-model-name)
                      (ellama-transient--provider-model provider)
                    ellama-transient-model-name)))
    (if models
        (completing-read "Select model: " models nil nil nil nil default)
      (read-string "Enter model: " default))))

(defun ellama-transient--fill-ollama (provider)
  "Set transient Ollama fields from PROVIDER."
  (declare-function llm-ollama-host "ext:llm-ollama")
  (declare-function llm-ollama-port "ext:llm-ollama")
  (declare-function llm-ollama-chat-model "ext:llm-ollama")
  (declare-function llm-ollama-default-chat-temperature "ext:llm-ollama")
  (declare-function llm-ollama-default-chat-non-standard-params
                    "ext:llm-ollama")
  (when (ellama-transient--ollama-provider-p provider)
    (setq ellama-transient-model-name (llm-ollama-chat-model provider))
    (setq ellama-transient-temperature
          (llm-ollama-default-chat-temperature provider))
    (setq ellama-transient-host (llm-ollama-host provider))
    (setq ellama-transient-port (llm-ollama-port provider))
    (setq ellama-transient-context-length
          (ellama-transient--provider-context-length provider))))

(defun ellama-fill-transient-model (provider)
  "Set transient model fields from PROVIDER."
  (declare-function llm-openai-compatible-url "ext:llm-openai")
  (setq ellama-transient-provider provider)
  (when-let ((model (ellama-transient--provider-model provider)))
    (setq ellama-transient-model-name model))
  (setq ellama-transient-temperature
        (ellama-transient--standard-temperature provider))
  (when (ellama-transient--openai-compatible-provider-p provider)
    (setq ellama-transient-url (llm-openai-compatible-url provider)))
  (ellama-transient--fill-ollama provider))

(defalias 'ellama-fill-transient-ollama-model
  'ellama-fill-transient-model)

(defun ellama-transient--replace-param (params key value)
  "Return PARAMS with KEY set to VALUE."
  (let ((params (copy-tree (ellama-transient--alist params))))
    (if-let ((cell (assoc key params)))
        (setcdr cell value)
      (push (cons key value) params))
    params))

(defun ellama-transient--remove-param (params key)
  "Return PARAMS with KEY removed."
  (cl-remove key (copy-tree (ellama-transient--alist params))
             :key #'car :test #'equal))

(defun ellama-transient--provider-default-model (provider)
  "Return default chat model for PROVIDER type."
  (when-let ((constructor (intern-soft (format "make-%s" (type-of provider)))))
    (when (fboundp constructor)
      (condition-case nil
          (ellama-transient--provider-model (funcall constructor))
        (error nil)))))

(defun ellama-transient--effective-model (provider)
  "Return transient model or PROVIDER default model."
  (if (ellama-transient--default-value-p ellama-transient-model-name)
      (ellama-transient--provider-default-model provider)
    ellama-transient-model-name))

(defun ellama-transient--set-ollama-fields (provider)
  "Set Ollama-specific PROVIDER fields from transient."
  (declare-function llm-ollama-chat-model "ext:llm-ollama")
  (declare-function llm-ollama-host "ext:llm-ollama")
  (declare-function llm-ollama-port "ext:llm-ollama")
  (declare-function llm-ollama-default-chat-non-standard-params
                    "ext:llm-ollama")
  (declare-function (setf llm-ollama-chat-model) "ext:llm-ollama")
  (declare-function (setf llm-ollama-host) "ext:llm-ollama")
  (declare-function (setf llm-ollama-port) "ext:llm-ollama")
  (declare-function (setf llm-ollama-default-chat-non-standard-params)
                    "ext:llm-ollama")
  (setf (llm-ollama-chat-model provider)
        (ellama-transient--effective-model provider)
        (llm-ollama-host provider) ellama-transient-host
        (llm-ollama-port provider) ellama-transient-port)
  (setf (llm-ollama-default-chat-non-standard-params provider)
        (if ellama-transient-context-length
            (ellama-transient--replace-param
             (llm-ollama-default-chat-non-standard-params provider)
             "num_ctx" ellama-transient-context-length)
          (ellama-transient--remove-param
           (llm-ollama-default-chat-non-standard-params provider)
           "num_ctx")))
  provider)

(defun ellama-transient--set-openai-fields (provider)
  "Set OpenAI-compatible PROVIDER fields from transient."
  (declare-function llm-openai-chat-model "ext:llm-openai")
  (declare-function llm-openai-compatible-chat-model "ext:llm-openai")
  (declare-function llm-openai-compatible-url "ext:llm-openai")
  (declare-function (setf llm-openai-chat-model) "ext:llm-openai")
  (declare-function (setf llm-openai-compatible-chat-model) "ext:llm-openai")
  (declare-function (setf llm-openai-compatible-url) "ext:llm-openai")
  (cond
   ((ellama-transient--openai-compatible-provider-p provider)
    (setf (llm-openai-compatible-chat-model provider)
          (ellama-transient--effective-model provider))
    (when ellama-transient-url
      (setf (llm-openai-compatible-url provider) ellama-transient-url)))
   ((ellama-transient--openai-provider-p provider)
    (setf (llm-openai-chat-model provider)
          (ellama-transient--effective-model provider))))
  provider)

(defun ellama-construct-provider-from-transient (&optional base-provider)
  "Make provider from transient menu using BASE-PROVIDER."
  (declare-function make-llm-ollama "ext:llm-ollama")
  (declare-function copy-llm-ollama "ext:llm-ollama")
  (declare-function copy-llm-openai "ext:llm-openai")
  (declare-function copy-llm-openai-compatible "ext:llm-openai")
  (let* ((base-provider (or ellama-transient-provider base-provider))
         (provider
          (cond
           ((ellama-transient--ollama-provider-p base-provider)
            (copy-llm-ollama base-provider))
           ((ellama-transient--openai-compatible-provider-p base-provider)
            (copy-llm-openai-compatible base-provider))
           ((ellama-transient--openai-provider-p base-provider)
            (copy-llm-openai base-provider))
           ((not base-provider)
            (require 'llm-ollama)
            (make-llm-ollama))
           (t
            (error "Provider type does not support transient model changes")))))
    (ellama-transient--set-standard-temperature provider)
    (cond
     ((ellama-transient--ollama-provider-p provider)
      (ellama-transient--set-ollama-fields provider))
     ((or (ellama-transient--openai-compatible-provider-p provider)
          (ellama-transient--openai-provider-p provider))
      (ellama-transient--set-openai-fields provider)))
    provider))

(defun ellama-construct-ollama-provider-from-transient ()
  "Make Ollama provider from transient menu."
  (let ((ellama-transient-provider nil))
    (ellama-construct-provider-from-transient)))

(transient-define-suffix ellama-transient-code-review (&optional args)
  "Review the code.  ARGS used for transient arguments."
  (interactive (list (transient-args transient-current-command)))
  (ellama-code-review
   (transient-arg-value "--new-session" args)
   :ephemeral (transient-arg-value "--ephemeral" args)))

(defun ellama--context-summary ()
  "Return a summary of context elements as a string."
  (let ((context (append
                  ellama-context-global ellama-context-ephemeral))
        (total-chars 0)
        (summary))

    (cl-loop for element in context do
             (let* ((content
                     (ellama-context-element-extract element))
                    (chars (length content))
                    (name (ellama-context-element-display element)))
               (cl-incf total-chars chars)
               (push
                (if (ellama-context-element-quote-p element)
                    (format "%s (%d chars region)" name chars)
                  (format "%s" name))
                summary)))
    (concat "Context: " (string-join summary ", ") (format " (total %d chars)" total-chars))))


(defun ellama--transient-context ()
  "Summarise session and context for transient menus."
  (format "%s %s %s %s"
          (propertize "Session:" 'face 'ellama-key-face)
          (ellama-get-current-session-id)
          (propertize "Context: " 'face 'ellama-key-face)
          (ellama--context-summary)))

;;;###autoload (autoload 'ellama-transient-code-menu "ellama-transient" nil t)
(transient-define-prefix ellama-transient-code-menu ()
  "Code Commands."
  ["Session Options"
   :description (lambda () (ellama--transient-context))
   ("-n" "Create New Session" "--new-session")]
  ["Ephemeral sessions"
   :if (lambda () ellama-session-auto-save)
   ("-e" "Create Ephemeral Session" "--ephemeral")]
  [["Code Commands"
    ("c" "Complete" ellama-code-complete)
    ("a" "Add" ellama-code-add)
    ("e" "Edit" ellama-code-edit)
    ("i" "Improve" ellama-code-improve)
    ("r" "Review" ellama-transient-code-review)
    ("m" "Generate Commit Message" ellama-generate-commit-message)]
   ["Quit" ("q" "Quit" transient-quit-one)]])

;;;###autoload (autoload 'ellama-transient-summarize-menu "ellama-transient" nil t)
(transient-define-prefix ellama-transient-summarize-menu ()
  "Summarize Commands."
  [["Summarize Commands"
    ("s" "Summarize" ellama-summarize)
    ("w" "Summarize Webpage" ellama-summarize-webpage)
    ("k" "Summarize Killring" ellama-summarize-killring)]
   ["Quit" ("q" "Quit" transient-quit-one)]])

;;;###autoload (autoload 'ellama-transient-session-menu "ellama-transient" nil t)
(transient-define-prefix ellama-transient-session-menu ()
  "Session Commands."
  [["Session Commands"
    ("l" "Load Session" ellama-load-session)
    ("r" "Rename Session" ellama-session-rename)
    ("d" "Delete Session" ellama-session-delete)
    ("a" "Activate Session" ellama-session-switch)
    ("k" "Kill Session" ellama-session-kill)
    ("c" "Compact Current Session" ellama-session-compact-current)
    ("C" "Compact Session" ellama-session-compact)]
   ["Quit" ("q" "Quit" transient-quit-one)]])

;;;###autoload (autoload 'ellama-transient-improve-menu "ellama-transient" nil t)
(transient-define-prefix ellama-transient-improve-menu ()
  "Improve Commands."
  [["Improve Commands"
    ("w" "Improve Wording" ellama-improve-wording)
    ("g" "Improve Grammar" ellama-improve-grammar)
    ("c" "Improve Conciseness" ellama-improve-conciseness)]
   ["Quit" ("q" "Quit" transient-quit-one)]])

;;;###autoload (autoload 'ellama-transient-make-menu "ellama-transient" nil t)
(transient-define-prefix ellama-transient-make-menu ()
  "Make Commands."
  [["Make Commands"
    ("l" "Make List" ellama-make-list)
    ("t" "Make Table" ellama-make-table)
    ("f" "Make Format" ellama-make-format)]
   ["Quit" ("q" "Quit" transient-quit-one)]])

(transient-define-suffix ellama-transient-ask-line (&optional args)
  "Ask line.  ARGS used for transient arguments."
  (interactive (list (transient-args transient-current-command)))
  (ellama-ask-line
   (transient-arg-value "--new-session" args)
   :ephemeral (transient-arg-value "--ephemeral" args)))

(transient-define-suffix ellama-transient-ask-selection (&optional args)
  "Ask selection.  ARGS used for transient arguments."
  (interactive (list (transient-args transient-current-command)))
  (ellama-ask-selection
   (transient-arg-value "--new-session" args)
   :ephemeral (transient-arg-value "--ephemeral" args)))

(transient-define-suffix ellama-transient-ask-about (&optional args)
  "Ask about current buffer or region.  ARGS used for transient arguments."
  (interactive (list (transient-args transient-current-command)))
  (ellama-ask-about
   (transient-arg-value "--new-session" args)
   :ephemeral (transient-arg-value "--ephemeral" args)))

(transient-define-suffix ellama-transient-ask-image (&optional args)
  "Ask about an image.  ARGS used for transient arguments."
  (interactive (list (transient-args transient-current-command)))
  (ellama-chat
   (read-string "Ask Ellama: ")
   (transient-arg-value "--new-session" args)
   :ephemeral (transient-arg-value "--ephemeral" args)
   :images (list (read-file-name "Image: " nil nil t))))

;;;###autoload (autoload 'ellama-transient-ask-menu "ellama-transient" nil t)
(transient-define-prefix ellama-transient-ask-menu ()
  "Ask Commands."
  ["Session Options"
   :description (lambda () (ellama--transient-context))
   ("-n" "Create New Session" "--new-session")]
  ["Ephemeral sessions"
   :if (lambda () ellama-session-auto-save)
   ("-e" "Create Ephemeral Session" "--ephemeral")]
  [["Ask Commands"
    ("l" "Ask Line" ellama-transient-ask-line)
    ("s" "Ask Selection" ellama-transient-ask-selection)
    ("a" "Ask About" ellama-transient-ask-about)
    ("i" "Ask Image" ellama-transient-ask-image)]
   ["Quit" ("q" "Quit" transient-quit-one)]])

;;;###autoload (autoload 'ellama-transient-translate-menu "ellama-transient" nil t)
(transient-define-prefix ellama-transient-translate-menu ()
  "Translate Commands."
  [["Translate Commands"
    ("t" "Translate Text" ellama-translate)
    ("b" "Translate Buffer" ellama-translate-buffer)
    ("e" "Enable Translation" ellama-chat-translation-enable)
    ("d" "Disable Translation" ellama-chat-translation-disable)]
   ["Quit" ("q" "Quit" transient-quit-one)]])

(declare-function ellama-context-update-buffer "ellama-context")
(defvar ellama-context-buffer)

(transient-define-suffix ellama-transient-add-buffer (&optional args)
  "Add current buffer to context.
ARGS used for transient arguments."
  (interactive (list (transient-args transient-current-command)))
  (ellama-context-add-buffer
   (read-buffer "Buffer: ")
   (transient-arg-value "--ephemeral" args)))

(transient-define-suffix ellama-transient-add-directory (&optional args)
  "Add directory to context.
ARGS used for transient arguments."
  (interactive (list (transient-args transient-current-command)))
  (let ((directory (read-directory-name "Directory: ")))
    (ellama-context-add-directory
     directory
     (transient-arg-value "--ephemeral" args))))

(transient-define-suffix ellama-transient-add-file (&optional args)
  "Add file to context.
ARGS used for transient arguments."
  (interactive (list (transient-args transient-current-command)))
  (ellama-context-add-file (transient-arg-value "--ephemeral" args)))

(transient-define-suffix ellama-transient-add-image (&optional args)
  "Add image to context.
ARGS used for transient arguments."
  (interactive (list (transient-args transient-current-command)))
  (ellama-context-add-image
   (when (transient-arg-value "--ephemeral" args)
     'ephemeral)))

(transient-define-suffix ellama-transient-add-selection (&optional args)
  "Add current selection to context.
ARGS used for transient arguments."
  (interactive (list (transient-args transient-current-command)))
  (when (region-active-p)
    (ellama-context-add-selection (transient-arg-value "--ephemeral" args))))

(transient-define-suffix ellama-transient-add-info-node (&optional args)
  "Add Info Node to context.
ARGS used for transient arguments."
  (interactive (list (transient-args transient-current-command)))
  (let ((info-node (Info-copy-current-node-name)))
    (ellama-context-add-info-node
     info-node
     (transient-arg-value "--ephemeral" args))))

;;;###autoload (autoload 'ellama-transient-context-menu "ellama-transient" nil t)
(transient-define-prefix ellama-transient-context-menu ()
  "Context Commands."
  ["Options"
   ("-e" "Use Ephemeral Context" "--ephemeral")]
  ["Context Commands"
   :description (lambda ()
                  (ellama-context-update-buffer)
                  (format "Current context:
%s" (with-current-buffer ellama-context-buffer
      (buffer-substring (point-min) (point-max)))))
   ["Add"
    ("b" "Add Buffer" ellama-transient-add-buffer)
    ("d" "Add Directory" ellama-transient-add-directory)
    ("f" "Add File" ellama-transient-add-file)
    ("I" "Add Image" ellama-transient-add-image)
    ("s" "Add Selection" ellama-transient-add-selection)
    ("i" "Add Info Node" ellama-transient-add-info-node)]
   ["Manage"
    ("m" "Manage context" ellama-context-manage)
    ("D" "Delete element" ellama-context-element-remove-by-name)
    ("r" "Context reset" ellama-context-reset)]
   ["Quit" ("q" "Quit" transient-quit-one)]])

;;;###autoload (autoload 'ellama-transient-blueprint-menu "ellama-transient" nil t)
(transient-define-prefix ellama-transient-blueprint-menu ()
  "Blueprint Menu."
  ["Blueprint Commands"
   ["Chat"
    ("b" "Blueprint" ellama-blueprint-select)
    ("U" "User defined blueprint" ellama-blueprint-select-user-defined-blueprint)
    ("C" "Community blueprint" ellama-community-prompts-select-blueprint)]
   ["Manage"
    ("c" "Create from buffer" ellama-blueprint-create)
    ("n" "New blueprint" ellama-blueprint-new)
    ("r" "Remove blueprint" ellama-blueprint-remove)]
   ["Quit" ("q" "Quit" transient-quit-one)]])

;;;###autoload (autoload 'ellama-transient-blueprint-mode-menu "ellama-transient" nil t)
(transient-define-prefix ellama-transient-blueprint-mode-menu ()
  ["Blueprint Commands"
   ["Chat"
    ("c" "Send to chat" ellama-send-buffer-to-new-chat-then-kill)]
   ["System Message"
    ("s" "Set system and chat" ellama-blueprint-chat-with-system-kill-buffer)
    ("S" "Set system and quit" ellama-blueprint-set-system-kill-buffer)]
   ["Create"
    ("C" "Create new blueprint from buffer" ellama-blueprint-create)]
   ["Variables"
    ("v" "Fill variables" ellama-blueprint-fill-variables)]
   ["Quit"
    ("k" "Kill" ellama-kill-current-buffer)
    ("q" "Quit" transient-quit-one)]])

;;;###autoload (autoload 'ellama-transient-tools-menu "ellama-transient" nil t)
(transient-define-prefix ellama-transient-tools-menu ()
  ["Tools Commands"
   :description (lambda ()
                  (format "Enabled tools:\n%s"
                          (string-join (mapcar (lambda (tool)
                                                 (llm-tool-name tool))
                                               ellama-tools-enabled)
                                       " ")))
   ["Tools"
    ("e" "Enable tool" ellama-tools-enable-by-name
     :transient t)
    ("E" "Enable all tools" ellama-tools-enable-all
     :transient t)
    ("d" "Disable tool" ellama-tools-disable-by-name
     :transient t)
    ("D" "Disable all tools" ellama-tools-disable-all
     :transient t)]
   ["Quit"
    ("k" "Kill" ellama-kill-current-buffer)
    ("q" "Quit" transient-quit-one)]])

(transient-define-suffix ellama-transient-chat (&optional args)
  "Chat with Ellama.  ARGS used for transient arguments."
  (interactive (list (transient-args transient-current-command)))
  (ellama-chat
   (read-string "Ask Ellama: ")
   (transient-arg-value "--new-session" args)
   :ephemeral (transient-arg-value "--ephemeral" args)))

(transient-define-suffix ellama-transient-chat-with-image (&optional args)
  "Chat with Ellama about an image.  ARGS used for transient arguments."
  (interactive (list (transient-args transient-current-command)))
  (ellama-chat
   (read-string "Ask Ellama: ")
   (transient-arg-value "--new-session" args)
   :ephemeral (transient-arg-value "--ephemeral" args)
   :images (list (read-file-name "Image: " nil nil t))))

;;;###autoload (autoload 'ellama-transient-main-menu "ellama-transient" nil t)
(transient-define-prefix ellama-transient-main-menu ()
  "Main Menu."
  ["Session Options"
   :description (lambda () (ellama--transient-context))
   ("-n" "Create New Session" "--new-session")]
  ["Ephemeral sessions"
   :if (lambda () ellama-session-auto-save)
   ("-e" "Create Ephemeral Session" "--ephemeral")]
  ["Main"
   [("c" "Chat" ellama-transient-chat)
    ("i" "Chat with image" ellama-transient-chat-with-image)
    ("b" "Chat with blueprint" ellama-blueprint-select)
    ("B" "Blueprint Commands" ellama-transient-blueprint-menu)
    ("T" "Tools Commands" ellama-transient-tools-menu)]
   [("a" "Ask Commands" ellama-transient-ask-menu)
    ("C" "Code Commands" ellama-transient-code-menu)]]
  ["Text"
   [("w" "Write" ellama-write)
    ("P" "Proofread" ellama-proofread)
    ("k" "Text Complete" ellama-complete)
    ("g" "Text change" ellama-change)
    ("d" "Define word" ellama-define-word)]
   [("s" "Summarize Commands" ellama-transient-summarize-menu)
    ("i" "Improve Commands" ellama-transient-improve-menu)
    ("t" "Translate Commands" ellama-transient-translate-menu)
    ("m" "Make Commands" ellama-transient-make-menu)]]
  ["System"
   [("o" "Model" ellama-select-model)
    ("p" "Provider selection" ellama-provider-select)
    ("y" "Set system message" ellama-transient-set-system
     :transient t
     :description ellama-transient-system-show)
    ("Y" "Edit system message" ellama-blueprint-edit-system-message)]
   [("S" "Session Commands" ellama-transient-session-menu)
    ("x" "Context Commands" ellama-transient-context-menu)]]
  [["Problem solving"
    ("R" "Solve reasoning problem" ellama-solve-reasoning-problem)
    ("D" "Solve domain specific problem" ellama-solve-domain-specific-problem)]]
  [["Quit" ("q" "Quit" transient-quit-one)]]
  (interactive)
  (transient-setup 'ellama-transient-main-menu)
  (when (and (not ellama-transient-provider)
             (ellama-transient--default-value-p ellama-transient-model-name))
    (ellama-fill-transient-model ellama-provider)))

;;;###autoload (autoload 'ellama "ellama-transient" nil t)
(defalias 'ellama 'ellama-transient-main-menu)

(provide 'ellama-transient)
;;; ellama-transient.el ends here.
