;;; ellama.el --- Ollama client

;; Copyright (C) 2023 Sergey Kostyaev

;; Author: Sergey Kostyaev <sskostyaev@gmail.com>
;; URL: http://github.com/s-kostyaev/ellama
;; Keywords: help local tools
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.1.0
;; Created: 8th Oct 2023

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Ellama is ollama client for Emacs
;;

;;; Code:

(require 'json)

(defgroup ellama nil
  "Ollama client for Emacs."
  :group 'ellama)

(defcustom ellama-url "http://localhost:11434/api/generate" "Url to call ollama."
  :group 'ellama
  :type 'string)

(defcustom ellama-curl-executable (executable-find "curl") "Path to curl executable."
  :group 'ellama
  :type 'string)

(defcustom ellama-model "mistral" "Model to use ollama with."
  :group 'ellama
  :type 'string)

(defcustom ellama-buffer "*ellama*" "Default ellama buffer."
  :group 'ellama
  :type 'string)

(defcustom ellama-always-show-buffer nil "Always show ellama buffer."
  :group 'ellama
  :type 'boolean)

(defcustom ellama-user-nick "User" "User nick in logs."
  :grop 'ellama
  :type 'string)

(defcustom ellama-assistant-nick "Ellama" "Assistant nick in logs."
  :group 'ellama
  :type 'string)

(defcustom ellama-buffer-mode 'markdown-mode "Major mode for ellama logs."
  :group 'ellama
  :type 'function)

(defcustom ellama-language "Russian" "Language for ellama translation."
  :group 'ellama
  :type 'string)

(defcustom ellama-template nil "Template to use with ollama instead of default."
  :group 'ellama
  :type 'string)

(defvar ellama-context nil "Context that contains ellama conversation memory.")

(defvar ellama--unprocessed-data nil)

(defvar ellama--request nil)

(make-local-variable 'ellama-context)
(make-local-variable 'ellama--unprocessed-data)
(make-local-variable 'ellama--request)

(defun ellama--filter (proc string)
  "Filter function for ellama curl process.
Filter PROC output STRING."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark proc))
	  (when ellama--unprocessed-data
	    (setq string (concat ellama--unprocessed-data string)))
	  (condition-case nil
	      (progn
		(mapc (lambda (s)
			(when-let ((data
				    (json-parse-string s :object-type 'plist)))
			  (when-let ((context (plist-get data :context)))
			    (setq ellama-context context))
			  (when-let ((response (plist-get data :response)))
			    (insert response))))
		      (split-string string "\n" t))
		(setq ellama--unprocessed-data nil)
		(set-marker (process-mark proc) (point))
		(if moving (goto-char (process-mark proc))))
	    (error (setq ellama--unprocessed-data
			 (car (last (split-string string "\n" t)))))))))))

(defun ellama-query (prompt &rest args)
  "Query ellama for PROMPT.

ARGS contains keys for fine control.

:buffer BUFFER -- BUFFER is the buffer (or `buffer-name') to insert ollama reply
in. Default value is `ellama-buffer'.

:display BOOL -- If BOOL, show BUFFER to user.
Default value is `ellama-always-show-buffer'.

:log BOOL -- If BOOL, show conversation between user and ellama, prefixed with
nicks.

:model MODEL -- MODEL that ollama should use to generate answer. Default value
is `ellama-model'.

:memory BOOL -- If BOOL, enable conversation memory.

:system SYSTEM -- SYSTEM message for prompt MODEL. If not set, default value
inside ollama will be used. May not work for some models, see
https://github.com/jmorganca/ollama/issues/693 - :template can help you in that
case.

:temperature TEMPERATURE -- set MODEL temperature to TEMPERATURE. If not set,
 default value inside ollama will be used.

:template TEMPLATE -- TEMPLATE to use with ollama MODEL instead of ollama's
default. Default value is `ellama-template'."
  (let ((buffer (or (plist-get args :buffer) ellama-buffer))
	(display (or (plist-get args :display) ellama-always-show-buffer))
	(log (plist-get args :log))
	(model (or (plist-get args :model) ellama-model))
	(memory (plist-get args :memory))
	(system (plist-get args :system))
	(temperature (plist-get args :temperature))
	(template (or (plist-get args :template) ellama-template)))
    (when (not (bufferp buffer))
      (create-file-buffer buffer)
      (with-current-buffer buffer
	(if ellama-buffer-mode
	    (funcall ellama-buffer-mode))))
    (when display
      (display-buffer buffer))
    (when log
      (with-current-buffer buffer
	(save-excursion
	  (goto-char (point-max))
	  (insert "## " ellama-user-nick ":\n" prompt "\n\n"
		  "## " ellama-assistant-nick ":\n"))))
    (let ((sentinel (if log
			(lambda (proc event)
			  (when (string= event "finished\n")
			    (with-current-buffer (process-buffer proc)
			      (save-excursion
				(goto-char (point-max))
				(insert "\n\n")))))
		      (lambda (_ _) nil))))
      (with-current-buffer buffer
	(setq ellama--request (list :model model :prompt prompt))
	(when (and memory ellama-context)
	  (setq ellama--request (plist-put ellama--request :context ellama-context)))
	(when system
	  (setq ellama--request (plist-put ellama--request :system system)))
	(when temperature
	  (setq ellama--request (plist-put ellama--request :options
					   (list :temperature temperature))))
	(when template
	  (setq ellama--request (plist-put ellama--request :template template)))
	;; (message "request: %s" (json-encode-plist ellama--request))
	(make-process
	 :buffer buffer
	 :name "ellama"
	 :command (list
		   ellama-curl-executable
		   "-X" "POST" ellama-url "-d"
		   (json-encode-plist ellama--request))
	 :filter 'ellama--filter
	 :sentinel sentinel)))))

;;;###autoload
(defun ellama-ask ()
  "Ask ellama about something."
  (interactive)
  (let ((prompt (read-string "Ask ellama: ")))
    (ellama-query prompt :display t :log t :memory t)))

;;;###autoload
(defun ellama-ask-about ()
  "Ask ellama about selected region or current buffer."
  (interactive)
  (let ((input (read-string "Ask ellama about this text: "))
	(text (if (region-active-p)
		  (buffer-substring-no-properties (region-beginning) (region-end))
		(buffer-substring-no-properties (point-min) (point-max)))))
    (ellama-query
     (format "Regarding the following text, %s:\n%s" input text)
     :display t :log t :memory t)))

(defun ellama-instant (prompt)
  "Prompt ellama for PROMPT to reply instantly."
  (ellama-query prompt :display t :buffer (make-temp-name ellama-buffer)))

;;;###autoload
(defun ellama-translate ()
  "Ask ellama to translate selected region or word at point."
  (interactive)
  (if (region-active-p)
      (ellama-instant
       (format "Translate the following text to %s:\n%s"
	       ellama-language
	       (buffer-substring-no-properties (region-beginning) (region-end))))
    (ellama-instant
     (format "Translate %s to %s" (thing-at-point 'word) ellama-language))))

;;;###autoload
(defun ellama-define-word ()
  "Find definition of current word."
  (interactive)
  (ellama-instant (format "Define %s" (thing-at-point 'word))))

;;;###autoload
(defun ellama-summarize ()
  "Summarize selected region of current buffer."
  (interactive)
  (let ((text (if (region-active-p)
		  (buffer-substring-no-properties (region-beginning) (region-end))
		(buffer-substring-no-properties (point-min) (point-max)))))
    (ellama-instant (format "Summarize the following text:\n%s" text))))

(provide 'ellama)
;;; ellama.el ends here.
