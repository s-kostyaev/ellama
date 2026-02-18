# Makefile for ellama project

.PHONY: build test check-compile-warnings manual format-elisp refill-news

# This order is based on the packages dependency graph.
ELLAMA_COMPILE_ORDER = \
	ellama-tools.el \
	ellama-skills.el \
	ellama.el \
	ellama-context.el \
	ellama-transient.el \
	ellama-blueprint.el \
	ellama-community-prompts.el \
	ellama-manual.el

build:
	emacs -batch --eval "(package-initialize)" -f batch-byte-compile ellama*.el

test:
	emacs -batch --eval "(package-initialize)" \
		-l ellama.el \
		-l tests/test-ellama.el \
		-l tests/test-ellama-context.el \
		-l tests/test-ellama-tools.el \
		-l tests/test-ellama-skills.el \
		-l tests/test-ellama-transient.el \
		-l tests/test-ellama-blueprint.el \
		-l tests/test-ellama-manual.el \
		-l tests/test-ellama-community-prompts.el \
		--eval "(ert t)"

check-compile-warnings:
	emacs --batch --eval "(package-initialize)" --eval "(setq native-comp-eln-load-path (list default-directory))" -L . -f batch-native-compile $(ELLAMA_COMPILE_ORDER)

manual:
	emacs -batch --eval "(package-initialize)" \
		--eval "(require 'project)" \
		-l ellama-manual.el \
		--eval "(ellama-manual-export)"

format-elisp:
	emacs -batch --eval "(let ((files (append (file-expand-wildcards \"ellama*.el\") (file-expand-wildcards \"tests/*.el\")))) (package-initialize) (require 'transient) (dolist (file files) (with-current-buffer (find-file-noselect file) (emacs-lisp-mode) (indent-region (point-min) (point-max)) (untabify (point-min) (point-max)) (delete-trailing-whitespace) (save-buffer))))"

refill-news:
	emacs -batch --eval "(with-current-buffer (find-file-noselect \"./NEWS.org\") (setq fill-column 80) (fill-region (point-min) (point-max)) (save-buffer))"
