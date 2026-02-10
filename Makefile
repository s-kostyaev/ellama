# Makefile for ellama project

.PHONY: build test

build:
	emacs -batch --eval "(package-initialize)" -f batch-byte-compile ellama*.el

test:
	emacs -batch --eval "(package-initialize)" -l ellama.el -l tests/test-ellama.el --eval "(ert t)"

refill-news:
	emacs -batch --eval "(with-current-buffer (find-file-noselect \"./NEWS.org\") (setq fill-column 80) (fill-region (point-min) (point-max)) (save-buffer))"
