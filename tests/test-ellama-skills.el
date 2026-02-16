;;; test-ellama-skills.el --- Ellama skills tests -*- lexical-binding: t; package-lint-main-file: "../ellama.el"; -*-

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
;; Ellama skills tests.
;;

;;; Code:

(require 'cl-lib)
(require 'ellama)
(require 'ert)

(ert-deftest test-ellama-skills-parse-frontmatter-valid ()
  (let ((file (make-temp-file "ellama-skill-frontmatter-" nil ".md")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "---\n")
            (insert "name: Build Skill\n")
            (insert "description: Build projects with make.\n")
            (insert "---\n")
            (insert "# SKILL\n"))
          (let ((meta (ellama-skills--parse-frontmatter file)))
            (should (equal (alist-get 'name meta) "Build Skill"))
            (should
             (equal (alist-get 'description meta)
                    "Build projects with make."))))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest test-ellama-skills-parse-frontmatter-invalid-or-missing ()
  (let ((invalid-file (make-temp-file "ellama-skill-invalid-" nil ".md"))
        (plain-file (make-temp-file "ellama-skill-plain-" nil ".md")))
    (unwind-protect
        (progn
          (with-temp-file invalid-file
            (insert "---\n")
            (insert "name: [broken\n")
            (insert "---\n")
            (insert "# SKILL\n"))
          (with-temp-file plain-file
            (insert "# SKILL\n")
            (insert "No frontmatter.\n"))
          (should-not (ellama-skills--parse-frontmatter invalid-file))
          (should-not (ellama-skills--parse-frontmatter plain-file)))
      (when (file-exists-p invalid-file)
        (delete-file invalid-file))
      (when (file-exists-p plain-file)
        (delete-file plain-file)))))

(ert-deftest test-ellama-skills-scan-directory-filters-invalid-skills ()
  (let* ((root (make-temp-file "ellama-skills-root-" t))
         (valid-dir (expand-file-name "valid-skill" root))
         (missing-desc-dir (expand-file-name "missing-desc" root))
         (no-file-dir (expand-file-name "no-skill-file" root))
         (hidden-dir (expand-file-name ".hidden-skill" root))
         (valid-file (expand-file-name "SKILL.md" valid-dir))
         (missing-desc-file (expand-file-name "SKILL.md" missing-desc-dir))
         (hidden-file (expand-file-name "SKILL.md" hidden-dir)))
    (unwind-protect
        (progn
          (make-directory valid-dir t)
          (make-directory missing-desc-dir t)
          (make-directory no-file-dir t)
          (make-directory hidden-dir t)
          (with-temp-file valid-file
            (insert "---\n")
            (insert "name: Valid Skill\n")
            (insert "description: Works.\n")
            (insert "---\n")
            (insert "# SKILL\n"))
          (with-temp-file missing-desc-file
            (insert "---\n")
            (insert "name: No Description\n")
            (insert "---\n")
            (insert "# SKILL\n"))
          (with-temp-file hidden-file
            (insert "---\n")
            (insert "name: Hidden Skill\n")
            (insert "description: Should be ignored.\n")
            (insert "---\n")
            (insert "# SKILL\n"))
          (let ((skills (ellama-skills--scan-directory root)))
            (should (= (length skills) 1))
            (should (equal (ellama-skill-id (car skills)) "valid-skill"))
            (should
             (equal (ellama-skill-name (car skills))
                    "Valid Skill"))
            (should
             (equal (ellama-skill-description (car skills))
                    "Works."))
            (should (equal (ellama-skill-path (car skills)) valid-dir))
            (should (equal (ellama-skill-file-path (car skills))
                           valid-file))))
      (when (file-exists-p root)
        (delete-directory root t)))))

(ert-deftest test-ellama-skills-get-project-dir ()
  (let ((ellama-skills-local-path "my-skills"))
    (cl-letf (((symbol-function 'ellama-tools-project-root-tool)
               (lambda () "/tmp/my-project")))
      (should (equal (ellama-skills-get-project-dir)
                     "/tmp/my-project/my-skills")))
    (cl-letf (((symbol-function 'ellama-tools-project-root-tool)
               (lambda () nil)))
      (should-not (ellama-skills-get-project-dir)))))

(ert-deftest test-ellama-get-skills-appends-global-then-local ()
  (let ((ellama-skills-global-path "/tmp/global-skills"))
    (cl-letf (((symbol-function 'ellama-skills-get-project-dir)
               (lambda () "/tmp/project-skills"))
              ((symbol-function 'ellama-skills--scan-directory)
               (lambda (dir)
                 (if (equal dir "/tmp/global-skills")
                     '(global-skill)
                   '(local-skill)))))
      (should (equal (ellama-get-skills)
                     '(global-skill local-skill))))))

(ert-deftest test-ellama-skills-generate-prompt-empty-list ()
  (cl-letf (((symbol-function 'ellama-get-skills)
             (lambda () nil)))
    (should (equal (ellama-skills-generate-prompt) ""))))

(ert-deftest test-ellama-skills-generate-prompt-renders-skills ()
  (let* ((skill-a (make-ellama-skill
                   :id "a"
                   :name "Skill A"
                   :description "Do A"
                   :path "/tmp/a"
                   :file-path "/tmp/a/SKILL.md"))
         (skill-b (make-ellama-skill
                   :id "b"
                   :name "Skill B"
                   :description "Do B"
                   :path "/tmp/b"
                   :file-path "/tmp/b/SKILL.md"))
         (prompt nil))
    (cl-letf (((symbol-function 'ellama-get-skills)
               (lambda () (list skill-a skill-b))))
      (setq prompt (ellama-skills-generate-prompt)))
    (should (string-match-p "<available_skills>" prompt))
    (should (string-match-p "<name>Skill A</name>" prompt))
    (should (string-match-p "<description>Do A</description>" prompt))
    (should (string-match-p "<location>/tmp/a/SKILL.md</location>" prompt))
    (should (string-match-p "<name>Skill B</name>" prompt))
    (should (string-match-p "<description>Do B</description>" prompt))
    (should (string-match-p "<location>/tmp/b/SKILL.md</location>" prompt))
    (should
     (string-match-p
      "You have access to the skills listed above\\."
      prompt))))


(provide 'test-ellama-skills)

;;; test-ellama-skills.el ends here
