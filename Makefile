# Makefile for ellama project

.PHONY: build test

build:
	emacs -batch --eval "(package-initialize)" -f batch-byte-compile ellama*.el

test:
	emacs -batch --eval "(package-initialize)" -l ellama.el -l tests/test-ellama.el --eval "(ert t)"
