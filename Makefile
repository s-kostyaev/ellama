# Makefile for ellama project

.PHONY: build test check-compile-warnings

build:
	emacs -batch --eval "(package-initialize)" -f batch-byte-compile ellama*.el

test:
	emacs -batch --eval "(package-initialize)" \
		-l ellama.el \
		-l tests/test-ellama.el \
		-l tests/test-ellama-transient.el \
		-l tests/test-ellama-blueprint.el \
		-l tests/test-ellama-manual.el \
		-l tests/test-ellama-community-prompts.el \
		--eval "(ert t)"

check-compile-warnings:
	emacs --batch --eval "(package-initialize)" --eval "(setq native-comp-eln-load-path (list default-directory))" -L . -f batch-native-compile ellama*.el

refill-news:
	emacs -batch --eval "(with-current-buffer (find-file-noselect \"./NEWS.org\") (setq fill-column 80) (fill-region (point-min) (point-max)) (save-buffer))"
