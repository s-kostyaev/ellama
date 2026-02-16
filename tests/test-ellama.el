;;; test-ellama.el --- Ellama tests -*- lexical-binding: t; package-lint-main-file: "../ellama.el"; -*-

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
;; Ellama tests.
;;

;;; Code:

(require 'cl-lib)
(require 'ellama)
(require 'ellama-context)
(require 'ellama-transient)
(require 'ert)
(require 'llm-fake)

(defconst ellama-test-root
  (expand-file-name
   ".."
   (file-name-directory (or load-file-name buffer-file-name)))
  "Project root directory for test assets.")

(defun ellama-test--fake-stream-partials (response style)
  "Return streaming partial strings for RESPONSE using STYLE."
  (let ((prev "")
        (chunks (pcase style
                  ('line (string-lines response))
                  ((or 'word-leading 'word-trailing)
                   (string-split response " "))
                  (_ (error "Unknown style %S" style)))))
    (mapcar
     (lambda (chunk)
       (setq prev
             (pcase style
               ('line (concat prev chunk))
               ('word-leading (concat prev " " chunk))
               ('word-trailing (concat prev chunk " "))))
       prev)
     chunks)))

(defun ellama-test--run-with-fake-streaming (response prompt-regexp fn
						      &optional style)
  "Run FN with fake streaming RESPONSE and assert PROMPT-REGEXP.
STYLE controls partial message shape.  Default value is `word-leading'."
  (let* ((provider
          (make-llm-fake
           :chat-action-func (lambda () response)))
         (ellama-provider provider)
         (ellama-coding-provider provider)
         (ellama-response-process-method 'streaming)
         (ellama-spinner-enabled nil)
         (partial-style (or style 'word-leading)))
    (cl-letf (((symbol-function 'llm-chat-streaming)
               (lambda (stream-provider prompt partial-callback
                        response-callback _error-callback _multi-output)
                 (should (string-match-p prompt-regexp
                                         (llm-chat-prompt-to-text prompt)))
                 (let ((response-plist (llm-chat stream-provider prompt t)))
                   (dolist (partial
                            (ellama-test--fake-stream-partials
                             (plist-get response-plist :text)
                             partial-style))
                     (funcall partial-callback `(:text ,partial)))
                   (funcall response-callback response-plist)))))
      (funcall fn))))

(ert-deftest test-ellama--code-filter ()
  (should (equal "" (ellama--code-filter "")))
  (should (equal "(hello)" (ellama--code-filter "(hello)")))
  (should (equal "(hello)\n" (ellama--code-filter "```lisp\n(hello)\n```"))))

(ert-deftest test-ellama-code-improve ()
  (let ((original "(hello)\n")
        (improved "```lisp\n(hello)\n```"))
    (with-temp-buffer
      (insert original)
      (ellama-test--run-with-fake-streaming
       improved
       original
       (lambda ()
         (ellama-code-improve))
       'line)
      (should (equal original (buffer-string))))))

(ert-deftest test-ellama-lorem-ipsum ()
  (let ((fill-column 70)
        (raw "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.")
        (expected "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do
eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad
minim veniam, quis nostrud exercitation ullamco laboris nisi ut
aliquip ex ea commodo consequat. Duis aute irure dolor in
reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla
pariatur. Excepteur sint occaecat cupidatat non proident, sunt in
culpa qui officia deserunt mollit anim id est laborum. Sed do eiusmod
tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim
veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex
ea commodo consequat. Duis aute irure dolor in reprehenderit in
voluptate velit esse cillum dolore eu fugiat nulla pariatur.")
        )
    (with-temp-buffer
      (org-mode)
      (ellama-test--run-with-fake-streaming
       raw
       "test"
       (lambda ()
         (ellama-write "test"))
       'word-leading)
      (should (equal expected (buffer-string))))))

(ert-deftest test-ellama-sieve-of-eratosthenes ()
  (let* ((fill-column 80)
	 (raw "Sure! Let's go through the **Sieve of Eratosthenes** step by step — it's an ancient and elegant algorithm used to find all **prime numbers** up to a given limit.

---

### 🔍 What is the Sieve of Eratosthenes?

The **Sieve of Eratosthenes** is a method developed by the ancient Greek mathematician Eratosthenes (circa 240 BCE) to efficiently find all **prime numbers** less than or equal to a given number (say, N).

A **prime number** is a natural number greater than 1 that has no positive divisors other than 1 and itself.

---

### 🎯 Goal

Given a number N, list all **prime numbers** from 2 to N.

---

### ✅ How Does It Work? (Step-by-Step)

Let’s say we want to find all primes up to **30**.

#### Step 1: List all numbers from 2 to N
Write down all integers from 2 to 30:

```
2  3  4  5  6  7  5  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30
```

We’ll go through them one by one.

---

#### Step 2: Start with the first prime — 2

- 2 is the first prime number.
- Eliminate all multiples of 2 (except 2 itself):  
  → 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30

Now our list looks like this (only numbers not crossed out):

```
2  3  5  7  9 11 13 15 17 19 21 23 25 27 29
```

(We removed all multiples of 2)

---

#### Step 3: Move to the next uneliminated number — 3

- 3 is the next prime.
- Eliminate all multiples of 3 (except 3 itself):  
  → 9, 15, 21, 27

Now remove those from the list:

```
2  3  5  7  11 13 17 19 23 25 29
```

(Now 9, 15, 21, 27 are gone)

---

#### Step 4: Next uneliminated number — 5

- 5 is next prime.
- Eliminate multiples of 5:  
  → 25 (since 5×5=25)

Remove 25:

```
2  3  5  7  11 13 17 19 23 29
```

---

#### Step 5: Next uneliminated number — 7

- Check if 7 has any multiples left that haven’t been removed.
- Multiples of 7: 14, 21, 28
- But 14, 21, 28 were already eliminated earlier (by 2 and 3), so no need to remove them again.

So we **stop** here.

Why? Because the next number after 7 is 11 — but **11 is greater than √30**.

---

### 🔍 Key Insight

> You only need to check numbers up to √N.

Why?
- If a number greater than √N is composite (not prime), it must have a factor less than or equal to √N.
- So if we’ve already eliminated all multiples of numbers ≤ √N, then all remaining numbers are prime.

For N = 30, √30 ≈ 5.48 → we only need to check primes up to 5.

So we only need to go through 2, 3, 5 — which we did.

---

### ✅ Final Result

All the remaining numbers are **prime**:

> **2, 3, 5, 7, 11, 13, 17, 19, 23, 29**

✅ These are all the prime numbers ≤ 30.

---

### 📝 Summary of the Algorithm

1. Create a list of consecutive integers from 2 to N.
2. Start with the first number (2).
3. Mark all multiples of that number (except the number itself) as composite (not prime).
4. Move to the next unmarked number and repeat.
5. Stop when the square of the current number exceeds N (i.e., when current number > √N).

---

### 💡 Example in Code (Python)

```python
def sieve_of_eratosthenes(n):
    if n < 2:
	return []

    # Create a boolean array \"is_prime[0..n]\" and initialize all entries as True
    is_prime = [True] * (n + 1)
    is_prime[0] = is_prime[1] = False  # 0 and 1 are not prime

    for i in range(2, int(n**0.5) + 1):
	if is_prime[i]:
	    # Mark all multiples of i (starting from i*i) as not prime
	    for j in range(i * i, n + 1, i):
		is_prime[j] = False

    # Collect all prime numbers
    primes = [i for i in range(2, n + 1) if is_prime[i]]
    return primes

# Example: Get primes up to 30
print(sieve_of_eratosthenes(30))
# Output: [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
```

---

### 🎉 Why Is It Important?

- Very efficient for finding all primes up to a limit.
- Used in number theory, cryptography, and computer science.
- Simple and intuitive — great for learning algorithms.

---

### 🚀 Fun Fact

Eratosthenes was not only a mathematician but also a geographer, astronomer, and poet. He used this sieve to find primes — and even calculated Earth’s circumference with remarkable accuracy!

---

Let me know if you'd like to see a visual version, or try it with a different number! 😊
")
	 (expected "Sure! Let's go through the *Sieve of Eratosthenes* step by step — it's an
ancient and elegant algorithm used to find all *prime numbers* up to a given
limit.

---

*** 🔍 What is the Sieve of Eratosthenes?

The *Sieve of Eratosthenes* is a method developed by the ancient Greek
mathematician Eratosthenes (circa 240 BCE) to efficiently find all *prime
numbers* less than or equal to a given number (say, N).

A *prime number* is a natural number greater than 1 that has no positive
divisors other than 1 and itself.

---

*** 🎯 Goal

Given a number N, list all *prime numbers* from 2 to N.

---

*** ✅ How Does It Work? (Step-by-Step)

Let’s say we want to find all primes up to *30*.

**** Step 1: List all numbers from 2 to N
Write down all integers from 2 to 30:
#+BEGIN_SRC
2  3  4  5  6  7  5  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27
28 29 30
#+END_SRC

We’ll go through them one by one.

---

**** Step 2: Start with the first prime — 2

- 2 is the first prime number.
- Eliminate all multiples of 2 (except 2 itself):  
  → 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30

Now our list looks like this (only numbers not crossed out):
#+BEGIN_SRC
2  3  5  7  9 11 13 15 17 19 21 23 25 27 29
#+END_SRC

(We removed all multiples of 2)

---

**** Step 3: Move to the next uneliminated number — 3

- 3 is the next prime.
- Eliminate all multiples of 3 (except 3 itself):  
  → 9, 15, 21, 27

Now remove those from the list:
#+BEGIN_SRC
2  3  5  7  11 13 17 19 23 25 29
#+END_SRC

(Now 9, 15, 21, 27 are gone)

---

**** Step 4: Next uneliminated number — 5

- 5 is next prime.
- Eliminate multiples of 5:  
  → 25 (since 5×5=25)

Remove 25:
#+BEGIN_SRC
2  3  5  7  11 13 17 19 23 29
#+END_SRC

---

**** Step 5: Next uneliminated number — 7

- Check if 7 has any multiples left that haven’t been removed.
- Multiples of 7: 14, 21, 28
- But 14, 21, 28 were already eliminated earlier (by 2 and 3), so no need to
  remove them again.

So we *stop* here.

Why? Because the next number after 7 is 11 — but *11 is greater than √30*.

---

*** 🔍 Key Insight

> You only need to check numbers up to √N.

Why?
- If a number greater than √N is composite (not prime), it must have a factor
  less than or equal to √N.
- So if we’ve already eliminated all multiples of numbers ≤ √N, then all
  remaining numbers are prime.

For N = 30, √30 ≈ 5.48 → we only need to check primes up to 5.

So we only need to go through 2, 3, 5 — which we did.

---

*** ✅ Final Result

All the remaining numbers are *prime*:

> *2, 3, 5, 7, 11, 13, 17, 19, 23, 29*

✅ These are all the prime numbers ≤ 30.

---

*** 📝 Summary of the Algorithm

1. Create a list of consecutive integers from 2 to N.
2. Start with the first number (2).
3. Mark all multiples of that number (except the number itself) as composite
(not prime).
4. Move to the next unmarked number and repeat.
5. Stop when the square of the current number exceeds N (i.e., when current
number > √N).

---

*** 💡 Example in Code (Python)
#+BEGIN_SRC python
def sieve_of_eratosthenes(n):
    if n < 2:
	return []

    # Create a boolean array \"is_prime[0..n]\" and initialize all entries as True
    is_prime = [True] * (n + 1)
    is_prime[0] = is_prime[1] = False  # 0 and 1 are not prime

    for i in range(2, int(n**0.5) + 1):
	if is_prime[i]:
	    # Mark all multiples of i (starting from i*i) as not prime
	    for j in range(i * i, n + 1, i):
		is_prime[j] = False

    # Collect all prime numbers
    primes = [i for i in range(2, n + 1) if is_prime[i]]
    return primes

# Example: Get primes up to 30
print(sieve_of_eratosthenes(30))
# Output: [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
#+END_SRC

---

*** 🎉 Why Is It Important?

- Very efficient for finding all primes up to a limit.
- Used in number theory, cryptography, and computer science.
- Simple and intuitive — great for learning algorithms.

---

*** 🚀 Fun Fact

Eratosthenes was not only a mathematician but also a geographer, astronomer, and
poet. He used this sieve to find primes — and even calculated Earth’s
circumference with remarkable accuracy!

---

Let me know if you'd like to see a visual version, or try it with a different
number! 😊")
	 )
    (with-temp-buffer
      (org-mode)
      (ellama-test--run-with-fake-streaming
       raw
       "test"
       (lambda ()
         (ellama-write "test"))
       'word-leading)
      (should (equal expected
                     (buffer-substring-no-properties (point-min)
                                                     (point-max)))))))

(ert-deftest test-ellama-duplicate-strings ()
  (let ((fill-column 80)
	(raw "Great question! Whether you should start with **\"Natural Language Processing with Transformers\"** (O’Reilly) or wait for **\"Build a Large Language Model (From Scratch)\"** depends on your **goals, background, and learning style**. Here’s a detailed comparison to help
you decide:

---

")
	(expected "Great question! Whether you should start with *\"Natural Language Processing with
Transformers\"* (O’Reilly) or wait for *\"Build a Large Language Model (From
Scratch)\"* depends on your *goals, background, and learning style*. Here’s a
detailed comparison to help you decide:

---")
	)
    (with-temp-buffer
      (org-mode)
      (ellama-test--run-with-fake-streaming
       raw
       "test"
       (lambda ()
         (ellama-write "test"))
       'word-trailing)
      (should (equal expected (buffer-string))))))

(ert-deftest test-ellama-stream-llm-fake-output-to-buffer ()
  (let* ((ellama-provider
          (make-llm-fake
           :output-to-buffer "*ellama-fake-log*"
           :chat-action-func (lambda () "Fake answer")))
         (ellama-response-process-method 'streaming)
         (ellama-spinner-enabled nil)
         (ellama-fill-paragraphs nil)
         done-text)
    (unwind-protect
        (with-temp-buffer
          (cl-letf (((symbol-function 'sleep-for)
                     (lambda (&rest _args) nil)))
            (ellama-stream "test prompt"
                           :provider ellama-provider
                           :buffer (current-buffer)
                           :on-done (lambda (text) (setq done-text text))))
          (should (equal done-text "Fake answer"))
          (should (equal (buffer-string) "Fake answer"))
          (with-current-buffer (get-buffer-create "*ellama-fake-log*")
            (let ((log (buffer-string)))
              (should (string-match-p "Call to llm-chat-streaming" log))
              (should (string-match-p "test prompt" log)))))
      (let ((buf (get-buffer "*ellama-fake-log*")))
        (when buf
          (kill-buffer buf))))))

(ert-deftest test-ellama-stream-retry-with-llm-fake-tool-call-error ()
  (let* ((call-count 0)
         (error-captured nil)
         (done-text nil)
         (_ (unless (get 'ellama-test-tool-call-error 'error-conditions)
              (define-error 'ellama-test-tool-call-error
                "Tool call error used in tests"
                'llm-tool-call-error)))
         (ellama-provider
          (make-llm-fake
           :chat-action-func
           (lambda ()
             (setq call-count (1+ call-count))
             (if (= call-count 1)
                 '(ellama-test-tool-call-error "Temporary tool failure")
               "Recovered answer"))))
         (ellama-response-process-method 'async)
         (ellama-spinner-enabled nil)
         (ellama-fill-paragraphs nil))
    (cl-letf (((symbol-function 'llm-chat-async)
               (lambda (provider prompt response-callback error-callback
                        &optional _multi-output)
                 (condition-case err
                     (funcall response-callback (llm-chat provider prompt t))
                   (t (funcall error-callback (car err) (cdr err))))
                 nil)))
      (with-temp-buffer
        (ellama-stream "test retry"
                       :provider ellama-provider
                       :buffer (current-buffer)
                       :on-error (lambda (msg) (setq error-captured msg))
                       :on-done (lambda (text) (setq done-text text)))
        (should (= call-count 2))
        (should (null error-captured))
        (should (equal done-text "Recovered answer"))
        (should (equal (buffer-string) "Recovered answer"))))))

(ert-deftest test-ellama-context-element-format-buffer-markdown ()
  (let ((element (ellama-context-element-buffer :name "*scratch*")))
    (should (equal "```emacs-lisp\n(display-buffer \"*scratch*\")\n```\n"
                   (ellama-context-element-format element 'markdown-mode)))))

(ert-deftest test-ellama-context-element-format-buffer-org-mode ()
  (let ((element (ellama-context-element-buffer :name "*scratch*")))
    (should (equal "[[elisp:(display-buffer \"*scratch*\")][*scratch*]]"
                   (ellama-context-element-format element 'org-mode)))))

(ert-deftest test-ellama-context-element-format-file-markdown ()
  (let ((element (ellama-context-element-file :name "LICENSE")))
    (should (equal "[LICENSE](<LICENSE>)"
                   (ellama-context-element-format element 'markdown-mode)))))

(ert-deftest test-ellama-context-element-format-file-org-mode ()
  (let ((element (ellama-context-element-file :name "LICENSE")))
    (should (equal "[[file:LICENSE][LICENSE]]"
                   (ellama-context-element-format element 'org-mode)))))

(ert-deftest test-ellama-context-element-format-info-node-markdown ()
  (let ((element (ellama-context-element-info-node :name "(dir)Top")))
    (should (equal "```emacs-lisp\n(info \"(dir)Top\")\n```\n"
                   (ellama-context-element-format element 'markdown-mode)))))

(ert-deftest test-ellama-context-element-format-info-node-org-mode ()
  (let ((element (ellama-context-element-info-node :name "(dir)Top")))
    (should (equal "[[(dir)Top][(dir)Top]]"
                   (ellama-context-element-format element 'org-mode)))))

(ert-deftest test-ellama-context-element-format-text-markdown ()
  (let ((element (ellama-context-element-text :content "123")))
    (should (equal "123" (ellama-context-element-format element 'markdown-mode)))))

(ert-deftest test-ellama-context-element-format-text-org-mode ()
  (let ((element (ellama-context-element-text :content "123")))
    (should (equal "123" (ellama-context-element-format element 'org-mode)))))

(ert-deftest test-ellama-context-element-format-webpage-quote-disabled-markdown ()
  (let ((element (ellama-context-element-webpage-quote :name "test name" :url "https://example.com/" :content "1\n\n2"))
	(ellama-show-quotes nil))
    (should (string-match "\\[test name\\](https://example.com/):\n```emacs-lisp\n(display-buffer \"\\*ellama-quote-.+\\*\")\n```\n" (ellama-context-element-format element 'markdown-mode)))))

(ert-deftest test-ellama-context-element-format-webpage-quote-enabled-markdown ()
  (let ((element (ellama-context-element-webpage-quote :name "test name" :url "https://example.com/" :content "1\n\n2"))
	(ellama-show-quotes t))
    (should (equal "[test name](https://example.com/):
> 1
> 
> 2

"
		   (ellama-context-element-format element 'markdown-mode)))))

(ert-deftest test-ellama-context-element-format-webpage-quote-disabled-org-mode ()
  (let ((element (ellama-context-element-webpage-quote :name "test name" :url "https://example.com/" :content "1\n\n2"))
	(ellama-show-quotes nil))
    (should (string-match "\\[\\[https://example.com/\\]\\[test name\\]\\] \\[\\[elisp:(display-buffer \"\\*ellama-quote-.+\\*\")\\]\\[show\\]\\]" (ellama-context-element-format element 'org-mode)))))

(ert-deftest test-ellama-context-element-format-webpage-quote-enabled-org-mode ()
  (let ((element (ellama-context-element-webpage-quote :name "test name" :url "https://example.com/" :content "1\n\n* 2"))
	(ellama-show-quotes t))
    (should (equal "[[https://example.com/][test name]]:
#+BEGIN_QUOTE
1

 * 2
#+END_QUOTE
"
		   (ellama-context-element-format element 'org-mode)))))

(ert-deftest test-ellama-context-element-format-info-node-quote-disabled-markdown ()
  (let ((element (ellama-context-element-info-node-quote :name "(emacs)Top" :content "1\n\n2"))
	(ellama-show-quotes nil))
    (should (string-match "```emacs-lisp\n(info \"(emacs)Top\")\n```\nshow:\n```emacs-lisp\n(display-buffer \"\\*ellama-quote-.+\\*\")\n```\n" (ellama-context-element-format element 'markdown-mode)))))

(ert-deftest test-ellama-context-element-format-info-node-quote-enabled-markdown ()
  (let ((element (ellama-context-element-info-node-quote :name "(emacs)Top" :content "1\n\n2"))
	(ellama-show-quotes t))
    (should (equal "```emacs-lisp\n(info \"(emacs)Top\")\n```\n> 1\n> \n> 2\n\n"
		   (ellama-context-element-format element 'markdown-mode)))))

(ert-deftest test-ellama-context-element-format-info-node-quote-disabled-org-mode ()
  (let ((element (ellama-context-element-info-node-quote :name "(emacs)Top" :content "1\n\n2"))
	(ellama-show-quotes nil))
    (should (string-match "\\[\\[(emacs)Top\\]\\[(emacs)Top\\]\\] \\[\\[elisp:(display-buffer \"\\*ellama-quote-.+\\*\")\\]\\[show\\]\\]" (ellama-context-element-format element 'org-mode)))))

(ert-deftest test-ellama-context-element-format-info-node-quote-enabled-org-mode ()
  (let ((element (ellama-context-element-info-node-quote :name "(emacs)Top" :content "1\n\n* 2"))
	(ellama-show-quotes t))
    (should (equal "[[(emacs)Top][(emacs)Top]]:\n#+BEGIN_QUOTE\n1\n\n * 2\n#+END_QUOTE\n"
		   (ellama-context-element-format element 'org-mode)))))

(ert-deftest test-ellama-context-element-format-file-quote-disabled-markdown ()
  (let ((element (ellama-context-element-file-quote :path "/tmp/test.txt" :content "1\n\n2"))
	(ellama-show-quotes nil))
    (should (string-match "\\[/tmp/test.txt\\](/tmp/test.txt):\n```emacs-lisp\n(display-buffer \"\\*ellama-quote-.+\\*\")" (ellama-context-element-format element 'markdown-mode)))))

(ert-deftest test-ellama-context-element-format-file-quote-enabled-markdown ()
  (let ((element (ellama-context-element-file-quote :path "/tmp/test.txt" :content "1\n\n2"))
	(ellama-show-quotes t))
    (should (equal "[/tmp/test.txt](/tmp/test.txt):
> 1
> 
> 2

"
		   (ellama-context-element-format element 'markdown-mode)))))

(ert-deftest test-ellama-context-element-format-file-quote-disabled-org-mode ()
  (let ((element (ellama-context-element-file-quote :path "/tmp/test.txt" :content "1\n\n2"))
	(ellama-show-quotes nil))
    (should (string-match "\\[\\[/tmp/test.txt\\]\\[/tmp/test.txt\\]\\] \\[\\[elisp:(display-buffer \"\\*ellama-quote-.+\\*\")\\]\\[show\\]\\]" (ellama-context-element-format element 'org-mode)))))

(ert-deftest test-ellama-context-element-format-file-quote-enabled-org-mode ()
  (let ((element (ellama-context-element-file-quote :path "/tmp/test.txt" :content "1\n\n* 2"))
	(ellama-show-quotes t))
    (should (equal "[[/tmp/test.txt][/tmp/test.txt]]:
#+BEGIN_QUOTE
1

 * 2
#+END_QUOTE
"
		   (ellama-context-element-format element 'org-mode)))))

(ert-deftest test-ellama-context-element-extract-buffer ()
  (with-temp-buffer
    (insert "123")
    (let ((element (ellama-context-element-buffer :name (buffer-name))))
      (should (equal "123" (ellama-context-element-extract element))))))

(ert-deftest test-ellama-context-element-extract-file ()
  (let* ((filename (expand-file-name "LICENSE" (locate-dominating-file "." ".git")))
         (element (ellama-context-element-file :name filename)))
    (should (string-match "GNU GENERAL PUBLIC LICENSE"
                          (ellama-context-element-extract element)))))

(ert-deftest test-ellama-context-element-extract-info-node ()
  (let ((element (ellama-context-element-info-node :name "(dir)Top")))
    (should (string-match "This" (ellama-context-element-extract element)))))

(ert-deftest test-ellama-context-element-extract-text ()
  (let ((element (ellama-context-element-text :content "123")))
    (should (string-match "123" (ellama-context-element-extract element)))))

(ert-deftest test-ellama-context-element-extract-webpage-quote ()
  (let ((element (ellama-context-element-webpage-quote :content "123")))
    (should (equal "123" (ellama-context-element-extract element)))))

(ert-deftest test-ellama-context-element-extract-info-node-quote ()
  (let ((element (ellama-context-element-info-node-quote :content "123")))
    (should (equal "123" (ellama-context-element-extract element)))))

(ert-deftest test-ellama-context-element-extract-file-quote ()
  (let ((element (ellama-context-element-file-quote :content "123")))
    (should (equal "123" (ellama-context-element-extract element)))))

(ert-deftest test-ellama-context-element-display-buffer ()
  (with-temp-buffer
    (let ((element (ellama-context-element-buffer :name (buffer-name))))
      (should (equal (buffer-name) (ellama-context-element-display element))))))

(ert-deftest test-ellama-context-element-display-file ()
  (let* ((filename (expand-file-name "LICENSE" (locate-dominating-file "." ".git")))
         (element (ellama-context-element-file :name filename)))
    (should (equal (file-name-nondirectory filename) (ellama-context-element-display element)))))

(ert-deftest test-ellama-context-element-display-info-node ()
  (let ((element (ellama-context-element-info-node :name "(dir)Top")))
    (should (equal "(info \"(dir)Top\")" (ellama-context-element-display element)))))

(ert-deftest test-ellama-context-element-display-text ()
  (let ((element (ellama-context-element-text :content "123")))
    (should (equal "\"123...\"" (ellama-context-element-display element)))))

(ert-deftest test-ellama-context-element-display-webpage-quote ()
  (let ((element (ellama-context-element-webpage-quote :name "Example" :url "http://example.com" :content "123")))
    (should (equal "Example" (ellama-context-element-display element)))))

(ert-deftest test-ellama-context-element-display-info-node-quote ()
  (let ((element (ellama-context-element-info-node-quote :name "Example" :content "123")))
    (should (equal "(info \"Example\")" (ellama-context-element-display element)))))

(ert-deftest test-ellama-context-element-display-file-quote ()
  (let ((element (ellama-context-element-file-quote :path "/path/to/file" :content "123")))
    (should (equal "file" (ellama-context-element-display element)))))

(ert-deftest test-ellama-context-element-extract-buffer-quote ()
  (with-temp-buffer
    (insert "123")
    (let ((element (ellama-context-element-buffer-quote :name (buffer-name) :content "123")))
      (should (equal "123" (ellama-context-element-extract element))))))

(ert-deftest test-ellama-context-element-display-buffer-quote ()
  (with-temp-buffer
    (let ((element (ellama-context-element-buffer-quote :name (buffer-name) :content "123")))
      (should (equal (buffer-name) (ellama-context-element-display element))))))

(ert-deftest test-ellama-md-to-org-code-simple ()
  (let ((result (ellama--translate-markdown-to-org-filter "Here is your TikZ code for a blue rectangle:
```tex
\\documentclass{article}
\\usepackage{tikz} \\begin{document}
\\begin{tikzpicture} \\node[rectangle, draw=blue] (mynode) {Text};
\\end{tikzpicture}
\\end{document}
```
This code will create a rectangle with a blue border and light
blue filling. You can replace \'Text\' with your desired text
or other TikZ elements.")))
    (should (string-equal result "Here is your TikZ code for a blue rectangle:
#+BEGIN_SRC tex
\\documentclass{article}
\\usepackage{tikz} \\begin{document}
\\begin{tikzpicture} \\node[rectangle, draw=blue] (mynode) {Text};
\\end{tikzpicture}
\\end{document}
#+END_SRC
This code will create a rectangle with a blue border and light
blue filling. You can replace \'Text\' with your desired text
or other TikZ elements."))))

(ert-deftest test-ellama-md-to-org-code-hard ()
  (let ((result (ellama--translate-markdown-to-org-filter "Here is your TikZ code for a blue rectangle:
```
\\documentclass{article}
\\usepackage{tikz} \\begin{document}
\\begin{tikzpicture} \\node[rectangle, draw=blue] (mynode) {Text};
\\end{tikzpicture}
\\end{document}
```
This code will create a rectangle with a blue border and light
blue filling. You can replace \'Text\' with your desired text or other
TikZ elements.")))
    (should (string-equal result "Here is your TikZ code for a blue rectangle:
#+BEGIN_SRC
\\documentclass{article}
\\usepackage{tikz} \\begin{document}
\\begin{tikzpicture} \\node[rectangle, draw=blue] (mynode) {Text};
\\end{tikzpicture}
\\end{document}
#+END_SRC
This code will create a rectangle with a blue border and light
blue filling. You can replace \'Text\' with your desired text or other
TikZ elements."))))

(ert-deftest test-ellama-md-to-org-code-nightmare ()
  (let ((result (ellama--translate-markdown-to-org-filter "Here is your TikZ code for a blue rectangle:
```
\\documentclass{article}
\\usepackage{tikz} \\begin{document}
\\begin{tikzpicture} \\node[rectangle, draw=blue] (mynode) {Text};
\\end{tikzpicture}
\\end{document}```This code will create a rectangle with a blue border and light
blue filling. You can replace \'Text\' with your desired text
or other TikZ elements.")))
    (should (string-equal result "Here is your TikZ code for a blue rectangle:
#+BEGIN_SRC
\\documentclass{article}
\\usepackage{tikz} \\begin{document}
\\begin{tikzpicture} \\node[rectangle, draw=blue] (mynode) {Text};
\\end{tikzpicture}
\\end{document}
#+END_SRC
This code will create a rectangle with a blue border and light
blue filling. You can replace \'Text\' with your desired text
or other TikZ elements."))))

(ert-deftest test-ellama-md-to-org-code-multiple-bad-blocks ()
  (let ((result (ellama--translate-markdown-to-org-filter "Some text:
```
First block
```
other text:
```
Second block
```
more text:
```
third block
```
That's it.")))
    (should (string-equal result "Some text:
#+BEGIN_SRC
First block
#+END_SRC
other text:
#+BEGIN_SRC
Second block
#+END_SRC
more text:
#+BEGIN_SRC
third block
#+END_SRC
That's it."))))

(ert-deftest test-ellama-md-to-org-code-multiple-bad-blocks-good-before ()
  (let ((result (ellama--translate-markdown-to-org-filter "Some text:
```text
First block
```
other text:
```
Second block
```
more text:
```
third block
```
That's it.")))
    (should (string-equal result "Some text:
#+BEGIN_SRC text
First block
#+END_SRC
other text:
#+BEGIN_SRC
Second block
#+END_SRC
more text:
#+BEGIN_SRC
third block
#+END_SRC
That's it."))))

(ert-deftest test-ellama-md-to-org-code-multiple-bad-blocks-good-after ()
  (let ((result (ellama--translate-markdown-to-org-filter "Some text:
```
First block
```
other text:
```
Second block
```
more text:
```text
third block
```
That's it.")))
    (should (string-equal result "Some text:
#+BEGIN_SRC
First block
#+END_SRC
other text:
#+BEGIN_SRC
Second block
#+END_SRC
more text:
#+BEGIN_SRC text
third block
#+END_SRC
That's it."))))

(ert-deftest test-ellama-md-to-org-code-skiped-line-break ()
  (let ((result (ellama--translate-markdown-to-org-filter "Sure! ```emacs-lisp
(message \"ok\")
```")))
    (should (string-equal result "Sure! 
#+BEGIN_SRC emacs-lisp
(message \"ok\")
#+END_SRC"))))

(ert-deftest test-ellama-md-to-org-inline-fence-long-line ()
  (let* ((long-part (make-string 150000 ?a))
         (text (concat "<think>\n" long-part "```text\nbody\n```\n</think>"))
         (result (ellama--translate-markdown-to-org-filter text)))
    (should (string-match-p "#\\+BEGIN_QUOTE" result))
    (should (string-match-p "#\\+BEGIN_SRC text" result))
    (should (string-match-p "#\\+END_SRC" result))
    (should (string-match-p "#\\+END_QUOTE" result))))

(ert-deftest test-ellama-replace-bad-code-blocks-no-src-blocks ()
  (let ((text "\n#+BEGIN_QUOTE\n((shell_command . ))\n#+END_QUOTE\n"))
    (should (string-equal (ellama--replace-bad-code-blocks text) text))))

(ert-deftest test-ellama-md-to-org-code-inline-latex ()
  (let ((result (ellama--translate-markdown-to-org-filter "_some italic_
$$P_\\theta(Y_T, ..., Y_2|Y_1, x_1, ..., x_T)$$
_more italic_
$P_\\theta$
_more italic_")))
    (should (string-equal result "/some italic/
$$P_\\theta(Y_T, ..., Y_2|Y_1, x_1, ..., x_T)$$
/more italic/
$P_\\theta$
/more italic/"))))


(ert-deftest test-ellama-md-to-org-code-inline-latex-with-code ()
  (let ((result (ellama--translate-markdown-to-org-filter "_some italic_
$$P_\\theta(Y_T, ..., Y_2|Y_1, x_1, ..., x_T)$$
_more italic_
```emacs-lisp
(msg \"ok\")
```
$P_\\theta$
_more italic_")))
    (should (string-equal result "/some italic/
$$P_\\theta(Y_T, ..., Y_2|Y_1, x_1, ..., x_T)$$
/more italic/
#+BEGIN_SRC emacs-lisp
(msg \"ok\")
#+END_SRC
$P_\\theta$
/more italic/"))))

(ert-deftest test-ellama-replace-outside-of-code-blocks-minimal ()
  (let ((result (ellama--replace-outside-of-code-blocks "This")))
    (should (string-equal result "This"))))

(ert-deftest test-ellama-md-to-org-code-snake-case ()
  (let* ((fill-column 70)
	 (result (ellama--translate-markdown-to-org-filter "```python
# Example of snake case variables and functions

# Variable names using snake_case
student_name = \"Alice Johnson\"
class_name = \"Mathematics\"
grade_level = 10

# Function name using snake_case
def calculate_average_score(math_score, science_score, english_score):
    average_score = (math_score + science_score + english_score) / 3
    return average_score

# Using the function
student_math_score = 85
student_science_score = 90
student_english_score = 78

average_score = calculate_average_score(student_math_score, student_science_score, student_english_score)
print(f\"The average score of {student_name} in {class_name} is: {average_score:.2f}\")
```

In this example:
- Variable names like `student_name`, `class_name`, and `grade_level` use snake_case.
- The function name `calculate_average_score` also follows the snake_case convention.

Snake case helps improve readability, especially in languages that are sensitive to capitalization like Python.")))
    (should (string-equal result "#+BEGIN_SRC python\n# Example of snake case variables and functions\n\n# Variable names using snake_case\nstudent_name = \"Alice Johnson\"\nclass_name = \"Mathematics\"\ngrade_level = 10\n\n# Function name using snake_case\ndef calculate_average_score(math_score, science_score, english_score):\n    average_score = (math_score + science_score + english_score) / 3\n    return average_score\n\n# Using the function\nstudent_math_score = 85\nstudent_science_score = 90\nstudent_english_score = 78\n\naverage_score = calculate_average_score(student_math_score, student_science_score, student_english_score)\nprint(f\"The average score of {student_name} in {class_name} is: {average_score:.2f}\")\n#+END_SRC\n\nIn this example:\n- Variable names like ~student_name~, ~class_name~, and ~grade_level~\n  use snake_case.\n- The function name ~calculate_average_score~ also follows the\n  snake_case convention.\n\nSnake case helps improve readability, especially in languages that are\nsensitive to capitalization like Python."))))

(ert-deftest test-ellama--fix-file-name ()
  (should (string=
	   (ellama--fix-file-name "a/\\?%*:|\"<>.;=")
	   "a_____________")))

(ert-deftest test-ellama-md-to-org-code-inline-code ()
  (let ((result (ellama--translate-markdown-to-org-filter "_some italic_
`some_snake_case`
_more italic_
```emacs-lisp
(msg \"ok\")
```
$P_\\theta$
_more italic_")))
    (should (string-equal result "/some italic/
~some_snake_case~
/more italic/
#+BEGIN_SRC emacs-lisp
(msg \"ok\")
#+END_SRC
$P_\\theta$
/more italic/"))))

(ert-deftest test-ellama-md-to-org-inline-code ()
  (let* ((fill-column 80)
         (result (ellama--translate-markdown-to-org-filter "```go
package main
```
### Explanation:
1. **Initialization**: We create a boolean slice `prime` of size `n+1`, where each
index represents whether the number is prime (`true`) or not (`false`).")))
    (should (string= result "#+BEGIN_SRC go
package main
#+END_SRC
*** Explanation:
1. *Initialization*: We create a boolean slice ~prime~ of size ~n+1~, where each
index represents whether the number is prime (~true~) or not (~false~)."))))

(ert-deftest test-ellama-md-to-org-lists ()
  (let* ((fill-column 80)
         (result (ellama--translate-markdown-to-org-filter "<think>Okay, the user asked me to create a list of fruits. Let me think about how to approach this.</think> Here’s a comprehensive list of fruits, categorized for clarity:

---

### **Common Fruits**
1. **Apple**
2. **Banana**
3. **Orange**
4. **Grape**
5. **Strawberry**
6. **Blueberry**

---

### **Additional Notes**
- **Tomatoes** are technically fruits (part of the nightshade family).
- **Coconut** is a tropical fruit, often used in cooking.
- **Papaya** is a versatile fruit with nutritional value.

Let me know if you'd like a simplified version or a specific category (e.g., by region, season, or type)! 🍎🍊")))
    (should (string= result "#+BEGIN_QUOTE
Okay, the user asked me to create a list of fruits. Let me think about how to
approach this.
#+END_QUOTE
 Here’s a comprehensive list of fruits, categorized for clarity:

---

*** *Common Fruits*
1. *Apple*
2. *Banana*
3. *Orange*
4. *Grape*
5. *Strawberry*
6. *Blueberry*

---

*** *Additional Notes*
- *Tomatoes* are technically fruits (part of the nightshade family).
- *Coconut* is a tropical fruit, often used in cooking.
- *Papaya* is a versatile fruit with nutritional value.

Let me know if you'd like a simplified version or a specific category (e.g., by
region, season, or type)! 🍎🍊"))))

(defun ellama-test-max-common-prefix ()
  "Test the `ellama-max-common-prefix` function."
  (should (equal (ellama-max-common-prefix "" "") ""))
  (should (equal (ellama-max-common-prefix "abc" "abcd") "abc"))
  (should (equal (ellama-max-common-prefix "abcd" "abc") "abc"))
  (should (equal (ellama-max-common-prefix "abcdef" "abcefg") "abc"))
  (should (equal (ellama-max-common-prefix "a" "b") ""))
  (should (equal (ellama-max-common-prefix "a" "") ""))
  (should (equal (ellama-max-common-prefix "" "b") "")))

(ert-deftest ellama-test-max-common-prefix ()
  "Run the tests for `ellama-max-common-prefix`."
  (ellama-test-max-common-prefix))

(ert-deftest ellama--string-without-last-two-lines-test ()
  "Test `ellama--string-without-last-two-lines` function."
  (should (equal (ellama--string-without-last-two-lines "Line1\nLine2\nLine3")
                 "Line1"))
  (should (equal (ellama--string-without-last-two-lines "SingleLine")
                 ""))
  (should (equal (ellama--string-without-last-two-lines "")
                 ""))
  (should (equal (ellama--string-without-last-two-lines "Line1\nLine2")
                 "")))

(ert-deftest test-ellama--append-tool-error-to-prompt-uses-llm-message ()
  (let (captured)
    (cl-letf (((symbol-function 'llm-chat-prompt-append-response)
	       (lambda (_prompt msg role)
		 (setq captured (list msg role)))))
      (ellama--append-tool-error-to-prompt
       'prompt
       "Unknown tool 'search' called"))
    (should (equal captured
		   '("Unknown tool 'search' called" system)))))

(ert-deftest test-ellama-remove-reasoning ()
  (should (equal
           (ellama-remove-reasoning "<think>\nabc\n</think>\nFinal")
           "Final"))
  (should (equal
           (ellama-remove-reasoning "Before <think>x</think> After")
           "Before  After")))

(ert-deftest test-ellama-mode-derived-helpers ()
  (let ((ellama-major-mode 'org-mode)
        (ellama-nick-prefix-depth 3))
    (should (equal (ellama-get-nick-prefix-for-mode) "***"))
    (should (equal (ellama-get-session-file-extension) "org")))
  (let ((ellama-major-mode 'text-mode)
        (ellama-nick-prefix-depth 2))
    (should (equal (ellama-get-nick-prefix-for-mode) "##"))
    (should (equal (ellama-get-session-file-extension) "md"))))

(ert-deftest test-ellama--tool-call-error-p ()
  (unless (get 'ellama-test-tool-call-error-2 'error-conditions)
    (define-error 'ellama-test-tool-call-error-2
      "Tool call test error"
      'llm-tool-call-error))
  (should (ellama--tool-call-error-p 'ellama-test-tool-call-error-2))
  (should-not (ellama--tool-call-error-p 'error))
  (should-not (ellama--tool-call-error-p nil)))

(ert-deftest test-ellama--error-handler-retry-on-tool-call-error ()
  (unless (get 'ellama-test-tool-call-error-3 'error-conditions)
    (define-error 'ellama-test-tool-call-error-3
      "Tool call retry error"
      'llm-tool-call-error))
  (let ((retry-called nil)
        (err-called nil)
        (appended nil))
    (with-temp-buffer
      (cl-letf (((symbol-function 'ellama--append-tool-error-to-prompt)
                 (lambda (_prompt msg)
                   (setq appended msg))))
        (let ((handler
               (ellama--error-handler
                (current-buffer)
                (lambda (_msg) (setq err-called t))
                'prompt
                (lambda () (setq retry-called t)))))
          (funcall handler 'ellama-test-tool-call-error-3 "tool failed"))))
    (should retry-called)
    (should-not err-called)
    (should (equal appended "tool failed"))))

(ert-deftest test-ellama--error-handler-calls-errcb-for-non-tool-errors ()
  (let ((err-msg nil)
        (request-mode-arg nil)
        (spinner-stop-called nil)
        (ellama--change-group (prepare-change-group))
        (ellama-spinner-enabled t))
    (with-temp-buffer
      (setq-local ellama--current-request 'request)
      (activate-change-group ellama--change-group)
      (cl-letf (((symbol-function 'cancel-change-group)
                 (lambda (_cg) nil))
                ((symbol-function 'spinner-stop)
                 (lambda () (setq spinner-stop-called t)))
                ((symbol-function 'ellama-request-mode)
                 (lambda (arg)
                   (setq request-mode-arg arg))))
        (let ((handler
               (ellama--error-handler
                (current-buffer)
                (lambda (msg) (setq err-msg msg))
                'prompt
                (lambda () (error "Retry should not run")))))
          (funcall handler 'error "bad")))
      (should (null ellama--current-request)))
    (should (equal err-msg "bad"))
    (should (equal request-mode-arg -1))
    (should spinner-stop-called)))

(ert-deftest test-ellama-chat-done-appends-user-header-and-callbacks ()
  (let* ((ellama-major-mode 'org-mode)
         (ellama-user-nick "Tester")
         (ellama-nick-prefix-depth 2)
         (ellama-session-auto-save nil)
         (global-callback-text nil)
         (local-callback-text nil)
         (ellama-chat-done-callback (lambda (text)
                                      (setq global-callback-text text))))
    (with-temp-buffer
      (insert "Assistant output")
      (cl-letf (((symbol-function 'ellama--scroll)
                 (lambda (&optional _buffer _point) nil)))
        (ellama-chat-done "final"
                          (lambda (text)
                            (setq local-callback-text text))))
      (should (equal (buffer-string)
                     "Assistant output\n\n** Tester:\n"))
      (should (equal global-callback-text "final"))
      (should (equal local-callback-text "final")))))

(defun ellama-test--ensure-local-ellama-tools ()
  "Ensure tests use local `ellama-tools.el' from project root."
  (unless (fboundp 'ellama-tools--sanitize-tool-text-output)
    (load-file (expand-file-name "ellama-tools.el" ellama-test-root))))

(defun ellama-test--wait-shell-command-result (cmd)
  "Run shell tool CMD and wait for a result string."
  (ellama-test--ensure-local-ellama-tools)
  (let ((result :pending)
	(deadline (+ (float-time) 3.0)))
    (ellama-tools-shell-command-tool
     (lambda (res)
       (setq result res))
     cmd)
    (while (and (eq result :pending)
		(< (float-time) deadline))
      (accept-process-output nil 0.01))
    (when (eq result :pending)
      (ert-fail (format "Timeout while waiting result for: %s" cmd)))
    result))

(defun ellama-test--named-tool-no-args ()
  "Return constant string."
  "zero")

(defun ellama-test--named-tool-one-arg (arg)
  "Return ARG with prefix."
  (format "one:%s" arg))

(defun ellama-test--named-tool-two-args (arg1 arg2)
  "Return ARG1 and ARG2 with prefix."
  (format "two:%s:%s" arg1 arg2))

(defun ellama-test--make-confirm-wrapper-old (function)
  "Make wrapper for FUNCTION using old confirm call style."
  (lambda (&rest args)
    (apply #'ellama-tools-confirm function args)))

(defun ellama-test--make-confirm-wrapper-new (function name)
  "Make wrapper for FUNCTION and NAME using wrapper factory."
  (ellama-tools--make-confirm-wrapper function name))

(defun ellama-test--invoke-confirm-with-yes (wrapper &rest args)
  "Call WRAPPER with ARGS and auto-answer confirmation with yes.
Return list with result and prompt."
  (let ((ellama-tools-confirm-allowed (make-hash-table))
        (ellama-tools-allow-all nil)
        (ellama-tools-allowed nil)
        result
        prompt)
    (cl-letf (((symbol-function 'read-char-choice)
               (lambda (message _choices)
                 (setq prompt message)
                 ?y)))
      (setq result (apply wrapper args)))
    (list result prompt)))

(ert-deftest test-ellama-shell-command-tool-empty-success-output ()
  (should
   (string=
    (ellama-test--wait-shell-command-result "sh -c 'true'")
    "Command completed successfully with no output.")))

(ert-deftest test-ellama-shell-command-tool-empty-failure-output ()
  (should
   (string-match-p
    "Command failed with exit code 7 and no output\\."
    (ellama-test--wait-shell-command-result "sh -c 'exit 7'"))))

(ert-deftest test-ellama-shell-command-tool-returns-stdout ()
  (should
   (string=
    (ellama-test--wait-shell-command-result "printf 'ok\\n'")
    "ok")))

(ert-deftest test-ellama-shell-command-tool-rejects-binary-output ()
  (should
   (string-match-p
    "binary data"
    (ellama-test--wait-shell-command-result
     "awk 'BEGIN { printf \"%c\", 0 }'"))))

(ert-deftest test-ellama-read-file-tool-rejects-binary-content ()
  (ellama-test--ensure-local-ellama-tools)
  (let ((file (make-temp-file "ellama-read-file-bin-")))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'no-conversion))
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (insert "%PDF-1.5\n%")
              (insert (char-to-string 143))
              (insert "\n")
              (write-region (point-min) (point-max) file nil 'silent)))
          (let ((result (ellama-tools-read-file-tool file)))
            (should (string-match-p "binary data" result))
            (should (string-match-p "bad idea" result))))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest test-ellama-read-file-tool-accepts-utf8-markdown-text ()
  (ellama-test--ensure-local-ellama-tools)
  (let ((file (make-temp-file "ellama-read-file-utf8-" nil ".md")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "# Research Plan\n\n")
            (insert "Sub‑topics: temporal reasoning overview.\n"))
          (let ((result (ellama-tools-read-file-tool file)))
            (should-not (string-match-p "binary data" result))
            (should (string-match-p "Research Plan" result))))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest test-ellama-tools-confirm-wrapped-named-no-args-old-and-new ()
  (ellama-test--ensure-local-ellama-tools)
  (let* ((old-wrapper (ellama-test--make-confirm-wrapper-old
                       #'ellama-test--named-tool-no-args))
         (new-wrapper (ellama-test--make-confirm-wrapper-new
                       #'ellama-test--named-tool-no-args
                       "named_tool"))
         (old-call (ellama-test--invoke-confirm-with-yes old-wrapper))
         (new-call (ellama-test--invoke-confirm-with-yes new-wrapper)))
    (should (equal (car old-call) "zero"))
    (should (equal (car new-call) "zero"))
    (should
     (string-match-p
      "Allow calling ellama-test--named-tool-no-args with arguments: \\?"
      (cadr old-call)))
    (should
     (string-match-p
      "Allow calling ellama-test--named-tool-no-args with arguments: \\?"
      (cadr new-call)))))

(ert-deftest test-ellama-tools-confirm-wrapped-named-one-arg-old-and-new ()
  (ellama-test--ensure-local-ellama-tools)
  (let* ((old-wrapper (ellama-test--make-confirm-wrapper-old
                       #'ellama-test--named-tool-one-arg))
         (new-wrapper (ellama-test--make-confirm-wrapper-new
                       #'ellama-test--named-tool-one-arg
                       "named_tool"))
         (old-call (ellama-test--invoke-confirm-with-yes old-wrapper "A"))
         (new-call (ellama-test--invoke-confirm-with-yes new-wrapper "A")))
    (should (equal (car old-call) "one:A"))
    (should (equal (car new-call) "one:A"))
    (should
     (string-match-p
      "Allow calling ellama-test--named-tool-one-arg with arguments: A\\?"
      (cadr old-call)))
    (should
     (string-match-p
      "Allow calling ellama-test--named-tool-one-arg with arguments: A\\?"
      (cadr new-call)))))

(ert-deftest test-ellama-tools-confirm-wrapped-named-two-args-old-and-new ()
  (ellama-test--ensure-local-ellama-tools)
  (let* ((old-wrapper (ellama-test--make-confirm-wrapper-old
                       #'ellama-test--named-tool-two-args))
         (new-wrapper (ellama-test--make-confirm-wrapper-new
                       #'ellama-test--named-tool-two-args
                       "named_tool"))
         (old-call (ellama-test--invoke-confirm-with-yes old-wrapper "A" "B"))
         (new-call (ellama-test--invoke-confirm-with-yes new-wrapper "A" "B")))
    (should (equal (car old-call) "two:A:B"))
    (should (equal (car new-call) "two:A:B"))
    (should
     (string-match-p
      "Allow calling ellama-test--named-tool-two-args with arguments: A, B\\?"
      (cadr old-call)))
    (should
     (string-match-p
      "Allow calling ellama-test--named-tool-two-args with arguments: A, B\\?"
      (cadr new-call)))))

(ert-deftest test-ellama-tools-confirm-prompt-uses-tool-name-for-lambda ()
  (ellama-test--ensure-local-ellama-tools)
  (let* ((ellama-tools-confirm-allowed (make-hash-table))
         (ellama-tools-allow-all nil)
         (ellama-tools-allowed nil)
         (tool-plist `(:function ,(lambda (_arg) "ok")
                       :name "mcp_tool"
                       :args ((:name "arg" :type string))))
         (wrapped (ellama-tools-wrap-with-confirm tool-plist))
         (wrapped-func (plist-get wrapped :function))
         seen-prompt)
    (cl-letf (((symbol-function 'read-char-choice)
               (lambda (prompt _choices)
                 (setq seen-prompt prompt)
                 ?n)))
      (funcall wrapped-func "value"))
    (should
     (string-match-p
      "Allow calling mcp_tool with arguments: value\\?"
      seen-prompt))))

(ert-deftest test-ellama-tools-wrap-with-confirm-preserves-arg-types ()
  (ellama-test--ensure-local-ellama-tools)
  (let* ((tool-plist '(:function ignore
                       :name "typed_tool"
                       :args ((:name "a" :type string)
                              (:name "b" :type number))))
         (wrapped (ellama-tools-wrap-with-confirm tool-plist))
         (types (mapcar (lambda (arg) (plist-get arg :type))
                        (plist-get wrapped :args))))
    (should (equal types '(string number)))))

(ert-deftest test-ellama-tools-edit-file-tool-replace-at-file-start ()
  (ellama-test--ensure-local-ellama-tools)
  (let ((file (make-temp-file "ellama-edit-start-")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "abcde"))
          (ellama-tools-edit-file-tool file "ab" "XX")
          (with-temp-buffer
            (insert-file-contents file)
            (should (equal (buffer-string) "XXcde"))))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest
    test-ellama-tools-enable-by-name-tool-missing-name-does-not-add-nil ()
  (ellama-test--ensure-local-ellama-tools)
  (let ((ellama-tools-enabled nil)
        (ellama-tools-available nil))
    (ellama-tools-enable-by-name-tool "missing")
    (should (null ellama-tools-enabled))))

(ert-deftest test-ellama-tools-confirm-answer-always-caches-approval ()
  (ellama-test--ensure-local-ellama-tools)
  (let ((ellama-tools-confirm-allowed (make-hash-table))
        (ellama-tools-allow-all nil)
        (ellama-tools-allowed nil)
        (prompt-count 0))
    (cl-letf (((symbol-function 'read-char-choice)
               (lambda (_prompt _choices)
                 (setq prompt-count (1+ prompt-count))
                 ?a)))
      (should (equal (ellama-tools-confirm 'ellama-test--named-tool-one-arg "A")
                     "one:A"))
      (should (equal (ellama-tools-confirm 'ellama-test--named-tool-one-arg "B")
                     "one:B")))
    (should (= prompt-count 1))
    (should (gethash 'ellama-test--named-tool-one-arg
                     ellama-tools-confirm-allowed))))

(ert-deftest test-ellama-tools-confirm-answer-reply-returns-user-text ()
  (ellama-test--ensure-local-ellama-tools)
  (let ((ellama-tools-confirm-allowed (make-hash-table))
        (ellama-tools-allow-all nil)
        (ellama-tools-allowed nil))
    (cl-letf (((symbol-function 'read-char-choice)
               (lambda (_prompt _choices) ?r))
              ((symbol-function 'read-string)
               (lambda (_prompt &rest _args) "custom reply")))
      (should (equal
               (ellama-tools-confirm 'ellama-test--named-tool-one-arg "A")
               "custom reply")))))

(ert-deftest test-ellama-tools-confirm-answer-no-returns-forbidden ()
  (ellama-test--ensure-local-ellama-tools)
  (let ((ellama-tools-confirm-allowed (make-hash-table))
        (ellama-tools-allow-all nil)
        (ellama-tools-allowed nil))
    (cl-letf (((symbol-function 'read-char-choice)
               (lambda (_prompt _choices) ?n)))
      (should (equal
               (ellama-tools-confirm 'ellama-test--named-tool-one-arg "A")
               "Forbidden by the user")))))

(ert-deftest test-ellama-read-file-tool-missing-file ()
  (ellama-test--ensure-local-ellama-tools)
  (let ((missing-file
         (expand-file-name "missing-file-ellama-test.txt"
                           (make-temp-name temporary-file-directory))))
    (should (string-match-p "doesn't exists"
                            (ellama-tools-read-file-tool missing-file)))))

(ert-deftest test-ellama-tools-write-append-prepend-roundtrip ()
  (ellama-test--ensure-local-ellama-tools)
  (let ((file (make-temp-file "ellama-file-tools-")))
    (unwind-protect
        (progn
          (ellama-tools-write-file-tool file "middle")
          (ellama-tools-append-file-tool file "-tail")
          (ellama-tools-prepend-file-tool file "head-")
          (with-temp-buffer
            (insert-file-contents file)
            (should (equal (buffer-string) "head-middle-tail"))))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest test-ellama-tools-directory-tree-excludes-dotfiles-and-sorts ()
  (ellama-test--ensure-local-ellama-tools)
  (let* ((dir (make-temp-file "ellama-tree-" t))
         (a-file (expand-file-name "a.txt" dir))
         (b-file (expand-file-name "b.txt" dir))
         (hidden (expand-file-name ".hidden" dir))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file b-file (insert "b"))
          (with-temp-file a-file (insert "a"))
          (with-temp-file hidden (insert "h"))
          (setq result (ellama-tools-directory-tree-tool dir))
          (should-not (string-match-p "\\.hidden" result))
          (should (< (string-match-p "a\\.txt" result)
                     (string-match-p "b\\.txt" result))))
      (when (file-exists-p dir)
        (delete-directory dir t)))))

(ert-deftest test-ellama-tools-move-file-success-and-error ()
  (ellama-test--ensure-local-ellama-tools)
  (let* ((src (make-temp-file "ellama-move-src-"))
         (dst (concat src "-dst")))
    (unwind-protect
        (progn
          (with-temp-file src (insert "x"))
          (ellama-tools-move-file-tool src dst)
          (should (file-exists-p dst))
          (should-not (file-exists-p src))
          (should-error (ellama-tools-move-file-tool src dst) :type 'error))
      (when (file-exists-p src)
        (delete-file src))
      (when (file-exists-p dst)
        (delete-file dst)))))

(ert-deftest test-ellama-tools-lines-range-boundary ()
  (ellama-test--ensure-local-ellama-tools)
  (let ((file (make-temp-file "ellama-lines-range-")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "alpha\nbeta\ngamma\n"))
          (let ((single-line
                 (json-parse-string
                  (ellama-tools-lines-range-tool file 2 2)))
                (full-range
                 (json-parse-string
                  (ellama-tools-lines-range-tool file 1 3))))
            (should (equal single-line "beta"))
            (should (equal full-range "alpha\nbeta\ngamma"))))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest test-ellama-tools-apply-patch-validation-branches ()
  (ellama-test--ensure-local-ellama-tools)
  (should (equal (ellama-tools-apply-patch-tool nil "patch")
                 "file-name is required"))
  (should (equal (ellama-tools-apply-patch-tool "missing-file" nil)
                 "file missing-file doesn't exists"))
  (let ((file (make-temp-file "ellama-patch-validate-")))
    (unwind-protect
        (should (equal (ellama-tools-apply-patch-tool file nil)
                       "patch is required"))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest test-ellama-tools-role-and-provider-resolution ()
  (ellama-test--ensure-local-ellama-tools)
  (let* ((ellama-provider 'default-provider)
         (ellama-tools-subagent-roles
          (list (list "all" :tools :all)
                (list "subset" :tools '("read_file" "task"))))
         (ellama-tools-available
          (list (llm-make-tool :name "task" :function #'ignore)
                (llm-make-tool :name "read_file" :function #'ignore)
                (llm-make-tool :name "grep" :function #'ignore))))
    (should-not
     (member "task"
             (mapcar #'llm-tool-name (ellama-tools--for-role "all"))))
    (should (equal
             (mapcar #'llm-tool-name (ellama-tools--for-role "subset"))
             '("task" "read_file")))
    (should (null (ellama-tools--for-role "missing")))
    (should (eq (ellama-tools--provider-for-role "all")
                'default-provider))))

(ert-deftest test-ellama-subagent-loop-handler-max-steps-and-continue ()
  (ellama-test--ensure-local-ellama-tools)
  (let ((updated-extra nil)
        (callback-msg nil)
        (stream-call nil))
    (let* ((session-max
            (make-ellama-session
             :id "worker-max"
             :extra (list :task-completed nil
                          :step-count 2
                          :max-steps 2
                          :result-callback (lambda (msg)
                                             (setq callback-msg msg)))))
           (ellama--current-session session-max))
      (cl-letf (((symbol-function 'ellama-tools--set-session-extra)
                 (lambda (_session extra)
                   (setq updated-extra extra)))
                ((symbol-function 'ellama-stream)
                 (lambda (prompt &rest args)
                   (setq stream-call (list prompt args)))))
        (ellama--subagent-loop-handler "ignored")
        (should (equal callback-msg "Max steps (2) reached."))
        (should (plist-get updated-extra :task-completed))
        (setq callback-msg nil)
        (setq updated-extra nil)
        (setq stream-call nil)
        (let* ((session-continue
                (make-ellama-session
                 :id "worker-continue"
                 :extra (list :task-completed nil
                              :step-count 1
                              :max-steps 3
                              :result-callback (lambda (msg)
                                                 (setq callback-msg msg)))))
               (ellama--current-session session-continue))
          (ellama--subagent-loop-handler "ignored")
          (should (equal (plist-get updated-extra :step-count) 2))
          (should (equal (car stream-call)
                         ellama-tools-subagent-continue-prompt))
          (should (eq (plist-get (cadr stream-call) :session)
                      session-continue))
          (should (eq (plist-get (cadr stream-call) :on-done)
                      #'ellama--subagent-loop-handler))
          (should (null callback-msg)))))))

(ert-deftest test-ellama-tools-task-tool-role-fallback-and-report-priority ()
  (ellama-test--ensure-local-ellama-tools)
  (let ((ellama--current-session-id "parent-1")
        (ellama-tools-subagent-default-max-steps 7)
        (worker (make-ellama-session :id "worker-1"))
        (resolved-provider nil)
        (resolved-provider-role nil)
        (resolved-tools-role nil)
        (captured-extra nil)
        (stream-call nil)
        (role-tool (llm-make-tool :name "read_file" :function #'ignore)))
    (cl-letf (((symbol-function 'ellama-tools--provider-for-role)
               (lambda (role)
                 (setq resolved-provider-role role)
                 'provider))
              ((symbol-function 'ellama-tools--for-role)
               (lambda (role)
                 (setq resolved-tools-role role)
                 (list role-tool)))
              ((symbol-function 'ellama-new-session)
               (lambda (provider _prompt ephemeral)
                 (setq resolved-provider provider)
                 (should ephemeral)
                 worker))
              ((symbol-function 'ellama-tools--set-session-extra)
               (lambda (_session extra)
                 (setq captured-extra extra)))
              ((symbol-function 'ellama-stream)
               (lambda (prompt &rest args)
                 (setq stream-call (list prompt args))))
              ((symbol-function 'message)
               (lambda (&rest _args) nil)))
      (should (null (ellama-tools-task-tool (lambda (_res) nil)
                                            "Do work"
                                            "unknown-role")))
      (should (eq resolved-provider 'provider))
      (should (equal resolved-provider-role "general"))
      (should (equal resolved-tools-role "general"))
      (should (equal (plist-get captured-extra :role)
                     "general"))
      (should (equal (car stream-call) "Do work"))
      (should (eq (plist-get (cadr stream-call) :session) worker))
      (should (equal (plist-get (cadr stream-call) :tools)
                     (plist-get captured-extra :tools)))
      (should (string=
               (llm-tool-name
                (car (plist-get captured-extra :tools)))
               "report_result"))
      (should (eq (cadr (plist-get captured-extra :tools))
                  role-tool)))))

(provide 'test-ellama)

;;; test-ellama.el ends here
