;;; ellama-skills.el --- Working with skills -*- lexical-binding: t; package-lint-main-file: "ellama.el"; -*-

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
(require 'ellama-tools)
(require 'yaml)

(defcustom ellama-skills-global-path
  (expand-file-name "ellama/skills" user-emacs-directory)
  "Path to the global directory containing Agent Skills."
  :type 'directory
  :group 'ellama)

(defcustom ellama-skills-local-path "skills"
  "Project-relative path for local Agent Skills."
  :type 'string
  :group 'ellama)

(cl-defstruct ellama-skill
  id          ; The folder name
  name        ; From frontmatter
  description ; From frontmatter
  path        ; Absolute path to the skill directory
  file-path)  ; Absolute path to SKILL.md

(defun ellama-skills--parse-frontmatter (file)
  "Parse YAML frontmatter from FILE using yaml.el."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (if (and (looking-at "^---[ \t]*$")
             (search-forward-regexp "^---[ \t]*$" nil t 2))
        (let* ((end (match-beginning 0))
               (start (save-excursion (goto-char (point-min)) (forward-line) (point)))
               (yaml-string (buffer-substring-no-properties start end)))
          (condition-case nil
              (yaml-parse-string yaml-string
                                 :object-type 'alist
                                 :object-key-type 'symbol)
            (error nil)))
      nil)))

(defun ellama-skills--scan-directory (root-dir)
  "Return list of `ellama-skill` structs found in ROOT-DIR."
  (let (skills)
    (when (and root-dir (file-exists-p root-dir))
      (dolist (skill-dir (directory-files root-dir t "^[^.]"))
        (when (file-directory-p skill-dir)
          (let ((skill-file (expand-file-name "SKILL.md" skill-dir)))
            (when (file-exists-p skill-file)
              (let* ((meta (ellama-skills--parse-frontmatter skill-file))
                     (name (alist-get 'name meta))
                     (desc (alist-get 'description meta)))
                (when (and name desc)
                  (push (make-ellama-skill
                         :id (file-name-nondirectory skill-dir)
                         :name name
                         :description desc
                         :path skill-dir
                         :file-path skill-file)
                        skills))))))))
    skills))

(defun ellama-skills-get-project-dir ()
  "Get the absolute path to the project local skills directory."
  (let ((root (ellama-tools-project-root-tool)))
    (when root
      (expand-file-name ellama-skills-local-path root))))

(defun ellama-get-skills ()
  "Scan and return all available skills (global and local)."
  (append (ellama-skills--scan-directory ellama-skills-global-path)
          (ellama-skills--scan-directory (ellama-skills-get-project-dir))))

;;;###autoload
(defun ellama-skills-generate-prompt ()
  "Generate the <available_skills> XML block for the system prompt."
  (let ((skills (ellama-get-skills)))
    (if skills
        (concat
         "\n<available_skills>\n"
         (mapconcat
          (lambda (skill)
            (format "  <skill>\n    <name>%s</name>\n    <description>%s</description>\n    <location>%s</location>\n  </skill>"
                    (ellama-skill-name skill)
                    (ellama-skill-description skill)
                    (ellama-skill-file-path skill)))
          skills
          "\n")
         "\n</available_skills>\n"
         "You have access to the skills listed above. To use a skill, use the `read_file` tool on the file path specified in <location> to load its instructions.")
      "")))

(provide 'ellama-skills)
;;; ellama-skills.el ends here
