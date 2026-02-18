;;; test-ellama-transient.el --- Ellama transient tests -*- lexical-binding: t; package-lint-main-file: "../ellama.el"; -*-

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
;; Ellama transient tests.
;;

;;; Code:

(add-to-list 'load-path default-directory)

(require 'cl-lib)
(require 'ellama)
(require 'ellama-context)
(require 'ellama-transient)
(require 'ert)
(require 'llm-ollama)

(ert-deftest test-ellama-fill-transient-ollama-model-populates-fields ()
  (let ((provider (make-llm-ollama
                   :chat-model "test-model"
                   :default-chat-temperature 0.2
                   :host "example.org"
                   :port 11000
                   :default-chat-non-standard-params
                   '(("num_ctx" . 8192))))
        (ellama-transient-ollama-model-name "")
        (ellama-transient-temperature 0.7)
        (ellama-transient-context-length 4096)
        (ellama-transient-host "localhost")
        (ellama-transient-port 11434))
    (ellama-fill-transient-ollama-model provider)
    (should (equal ellama-transient-ollama-model-name "test-model"))
    (should (= ellama-transient-temperature 0.2))
    (should (= ellama-transient-context-length 8192))
    (should (equal ellama-transient-host "example.org"))
    (should (= ellama-transient-port 11000))))

(ert-deftest test-ellama-fill-transient-ollama-model-defaults ()
  (let ((provider (make-llm-ollama
                   :chat-model "model"
                   :default-chat-temperature nil
                   :host "localhost"
                   :port 11434
                   :default-chat-non-standard-params nil))
        (ellama-transient-temperature 0.3)
        (ellama-transient-context-length 123))
    (ellama-fill-transient-ollama-model provider)
    (should (= ellama-transient-temperature 0.7))
    (should (= ellama-transient-context-length 4096))))

(ert-deftest test-ellama-fill-transient-ollama-model-noop-for-non-ollama ()
  (let ((ellama-transient-ollama-model-name "keep-model")
        (ellama-transient-temperature 0.9)
        (ellama-transient-context-length 2222)
        (ellama-transient-host "keep-host")
        (ellama-transient-port 22000))
    (ellama-fill-transient-ollama-model :not-ollama-provider)
    (should (equal ellama-transient-ollama-model-name "keep-model"))
    (should (= ellama-transient-temperature 0.9))
    (should (= ellama-transient-context-length 2222))
    (should (equal ellama-transient-host "keep-host"))
    (should (= ellama-transient-port 22000))))

(ert-deftest
    test-ellama-construct-ollama-provider-from-transient-passes-all-params ()
  (let ((ellama-transient-ollama-model-name "model-x")
        (ellama-transient-temperature 0.61)
        (ellama-transient-host "localhost")
        (ellama-transient-port 12000)
        (ellama-transient-context-length 16384))
    (let* ((provider (ellama-construct-ollama-provider-from-transient))
           (params
            (seq--into-list
             (llm-ollama-default-chat-non-standard-params provider))))
      (should (llm-ollama-p provider))
      (should (equal (llm-ollama-chat-model provider) "model-x"))
      (should (= (llm-ollama-default-chat-temperature provider) 0.61))
      (should (equal (llm-ollama-host provider) "localhost"))
      (should (= (llm-ollama-port provider) 12000))
      (should (equal params '(("num_ctx" . 16384)))))))

(ert-deftest test-ellama-transient-set-provider-updates-selected-symbol ()
  (let ((ellama-provider :old-default)
        (ellama-coding-provider :old-coding))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args)
                 "ellama-coding-provider"))
              ((symbol-function 'ellama-construct-ollama-provider-from-transient)
               (lambda ()
                 :new-provider)))
      (ellama-transient-set-provider)
      (should (eq ellama-coding-provider :new-provider))
      (should (eq ellama-provider :old-default)))))

(ert-deftest
    test-ellama-transient-set-provider-resets-session-only-for-default-provider ()
  (let ((ellama-provider :default-provider)
        (ellama-coding-provider :coding-provider)
        (ellama--current-session-id "session-1")
        (ellama--current-session-uid "uid-1")
        (providers '("ellama-provider" "ellama-coding-provider"))
        (values '(:new-default :new-coding)))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args)
                 (prog1 (car providers)
                   (setq providers (cdr providers)))))
              ((symbol-function 'ellama-construct-ollama-provider-from-transient)
               (lambda ()
                 (prog1 (car values)
                   (setq values (cdr values))))))
      (ellama-transient-set-provider)
      (should (eq ellama-provider :new-default))
      (should-not ellama--current-session-id)
      (should-not ellama--current-session-uid)
      (setq ellama--current-session-id "session-2")
      (setq ellama--current-session-uid "uid-2")
      (ellama-transient-set-provider)
      (should (eq ellama-coding-provider :new-coding))
      (should (equal ellama--current-session-id "session-2"))
      (should (equal ellama--current-session-uid "uid-2")))))

(ert-deftest
    test-ellama-transient-model-get-from-current-session-guard-and-provider-flow
    ()
  (let ((called 0))
    (cl-letf (((symbol-function 'ellama-get-current-session)
               (lambda () nil))
              ((symbol-function 'ellama-fill-transient-ollama-model)
               (lambda (&rest _args)
                 (cl-incf called))))
      (ellama-transient-model-get-from-current-session)
      (should (= called 0))))
  (let ((session (make-ellama-session :provider :session-provider))
        provided-session
        provided-provider)
    (cl-letf (((symbol-function 'ellama-get-current-session)
               (lambda ()
                 (setq provided-session session)
                 session))
              ((symbol-function 'ellama-fill-transient-ollama-model)
               (lambda (provider)
                 (setq provided-provider provider))))
      (ellama-transient-model-get-from-current-session)
      (should (eq provided-session session))
      (should (eq provided-provider :session-provider)))))

(ert-deftest test-ellama-transient-set-system-region-and-prompt-paths ()
  (let ((ellama-global-system "old"))
    (with-temp-buffer
      (insert "alpha beta")
      (goto-char (point-min))
      (set-mark (point))
      (goto-char (+ (point-min) 5))
      (activate-mark)
      (ellama-transient-set-system)
      (should (equal ellama-global-system "alpha"))))
  (let ((ellama-global-system "old"))
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _args)
                 "from prompt")))
      (ellama-transient-set-system)
      (should (equal ellama-global-system "from prompt"))))
  (let ((ellama-global-system "keep"))
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _args)
                 "")))
      (ellama-transient-set-system)
      (should-not ellama-global-system))))

(ert-deftest test-ellama-transient-system-show-uses-first-line-and-limit ()
  (let ((ellama-global-system "abcdefghij\nsecond line")
        (ellama-transient-system-show-limit 5))
    (should (equal (ellama-transient-system-show)
                   "System message (abcde)"))))

(ert-deftest test-ellama-context-summary-counts-and-marks-quoted-elements ()
  (let ((ellama-context-global '(:global))
        (ellama-context-ephemeral '(:ephemeral)))
    (cl-letf (((symbol-function 'ellama-context-element-extract)
               (lambda (element)
                 (if (eq element :global)
                     "hello"
                   "cat")))
              ((symbol-function 'ellama-context-element-display)
               (lambda (element)
                 (if (eq element :global)
                     "Global file"
                   "Selection")))
              ((symbol-function 'ellama-context-element-quote-p)
               (lambda (element)
                 (eq element :ephemeral))))
      (let ((summary (ellama--context-summary)))
        (should (string-match-p (regexp-quote "Global file") summary))
        (should (string-match-p
                 (regexp-quote "Selection (3 chars region)")
                 summary))
        (should (string-match-p
                 (regexp-quote "(total 8 chars)")
                 summary))))))

(ert-deftest test-ellama-transient-arg-forwarding ()
  (let ((args '("--new-session" "--ephemeral"))
        code-review-call
        ask-line-call
        ask-selection-call
        ask-about-call
        chat-call)
    (cl-letf (((symbol-function 'ellama-code-review)
               (lambda (new-session &rest rest)
                 (setq code-review-call (list new-session rest))))
              ((symbol-function 'ellama-ask-line)
               (lambda (new-session &rest rest)
                 (setq ask-line-call (list new-session rest))))
              ((symbol-function 'ellama-ask-selection)
               (lambda (new-session &rest rest)
                 (setq ask-selection-call (list new-session rest))))
              ((symbol-function 'ellama-ask-about)
               (lambda (new-session &rest rest)
                 (setq ask-about-call (list new-session rest))))
              ((symbol-function 'ellama-chat)
               (lambda (prompt new-session &rest rest)
                 (setq chat-call (list prompt new-session rest))))
              ((symbol-function 'read-string)
               (lambda (&rest _args)
                 "Ask me")))
      (ellama-transient-code-review args)
      (ellama-transient-ask-line args)
      (ellama-transient-ask-selection args)
      (ellama-transient-ask-about args)
      (ellama-transient-chat args))
    (should (equal code-review-call '(t (:ephemeral t))))
    (should (equal ask-line-call '(t (:ephemeral t))))
    (should (equal ask-selection-call '(t (:ephemeral t))))
    (should (equal ask-about-call '(t (:ephemeral t))))
    (should (equal chat-call '("Ask me" t (:ephemeral t))))))

(ert-deftest
    test-ellama-transient-main-menu-initializes-model-only-when-empty ()
  (let ((ellama-provider :provider)
        (ellama-transient-ollama-model-name "")
        (fill-calls 0)
        fill-provider)
    (cl-letf (((symbol-function 'transient-setup)
               (lambda (&rest _args) nil))
              ((symbol-function 'ellama-fill-transient-ollama-model)
               (lambda (provider)
                 (cl-incf fill-calls)
                 (setq fill-provider provider))))
      (ellama-transient-main-menu)
      (should (= fill-calls 1))
      (should (eq fill-provider :provider))
      (setq ellama-transient-ollama-model-name "model-already-set")
      (ellama-transient-main-menu)
      (should (= fill-calls 1)))))

(provide 'test-ellama-transient)

;;; test-ellama-transient.el ends here
