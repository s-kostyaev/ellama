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
(require 'ellama-transient)
(require 'ert)
(require 'llm-fake)


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

(ert-deftest test-ellama-ask-about-add-selection-ephemeral ()
  (let (captured-ephemeral)
    (with-temp-buffer
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _args) "question"))
                ((symbol-function 'region-active-p)
                 (lambda () t))
                ((symbol-function 'ellama-context-add-selection)
                 (lambda (&optional ephemeral)
                   (setq captured-ephemeral ephemeral)))
                ((symbol-function 'ellama-context-add-buffer)
                 (lambda (&rest _args)
                   (ert-fail "Unexpected call to ellama-context-add-buffer")))
                ((symbol-function 'ellama-chat)
                 (lambda (&rest _args) nil)))
        (ellama-ask-about)))
    (should (eq captured-ephemeral t))))

(ert-deftest test-ellama-ask-about-add-buffer-ephemeral ()
  (let (captured-ephemeral captured-buffer)
    (with-temp-buffer
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _args) "question"))
                ((symbol-function 'region-active-p)
                 (lambda () nil))
                ((symbol-function 'ellama-context-add-selection)
                 (lambda (&rest _args)
                   (ert-fail "Unexpected call to ellama-context-add-selection")))
                ((symbol-function 'ellama-context-add-buffer)
                 (lambda (buf &optional ephemeral)
                   (setq captured-buffer buf)
                   (setq captured-ephemeral ephemeral)))
                ((symbol-function 'ellama-chat)
                 (lambda (&rest _args) nil)))
        (ellama-ask-about)
        (should (equal captured-buffer (buffer-name (current-buffer))))))
    (should (eq captured-ephemeral t))))

(ert-deftest test-ellama-code-review-add-selection-ephemeral ()
  (let (captured-ephemeral)
    (with-temp-buffer
      (cl-letf (((symbol-function 'region-active-p)
                 (lambda () t))
                ((symbol-function 'ellama-context-add-selection)
                 (lambda (&optional ephemeral)
                   (setq captured-ephemeral ephemeral)))
                ((symbol-function 'ellama-context-add-buffer)
                 (lambda (&rest _args)
                   (ert-fail "Unexpected call to ellama-context-add-buffer")))
                ((symbol-function 'ellama-chat)
                 (lambda (&rest _args) nil)))
        (ellama-code-review)))
    (should (eq captured-ephemeral t))))

(ert-deftest test-ellama-code-review-add-buffer-ephemeral ()
  (let (captured-ephemeral captured-buffer)
    (with-temp-buffer
      (cl-letf (((symbol-function 'region-active-p)
                 (lambda () nil))
                ((symbol-function 'ellama-context-add-selection)
                 (lambda (&rest _args)
                   (ert-fail "Unexpected call to ellama-context-add-selection")))
                ((symbol-function 'ellama-context-add-buffer)
                 (lambda (buf &optional ephemeral)
                   (setq captured-buffer buf)
                   (setq captured-ephemeral ephemeral)))
                ((symbol-function 'ellama-chat)
                 (lambda (&rest _args) nil)))
        (ellama-code-review)
        (should (equal captured-buffer (current-buffer)))))
    (should (eq captured-ephemeral t))))

(ert-deftest test-ellama-write-add-selection-ephemeral ()
  (let (captured-ephemeral)
    (with-temp-buffer
      (cl-letf (((symbol-function 'region-active-p)
                 (lambda () t))
                ((symbol-function 'ellama-context-add-selection)
                 (lambda (&optional ephemeral)
                   (setq captured-ephemeral ephemeral)))
                ((symbol-function 'ellama-stream)
                 (lambda (&rest _args) nil)))
        (ellama-write "instruction")))
    (should (eq captured-ephemeral t))))

(ert-deftest test-ellama-code-add-add-selection-ephemeral ()
  (let (captured-ephemeral)
    (with-temp-buffer
      (cl-letf (((symbol-function 'region-active-p)
                 (lambda () t))
                ((symbol-function 'ellama-context-add-selection)
                 (lambda (&optional ephemeral)
                   (setq captured-ephemeral ephemeral)))
                ((symbol-function 'ellama-stream)
                 (lambda (&rest _args) nil)))
        (ellama-code-add "description")))
    (should (eq captured-ephemeral t))))

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

(provide 'test-ellama)

;;; test-ellama.el ends here
