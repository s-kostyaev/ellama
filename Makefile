# Makefile for ellama project

.PHONY: build test test-detailed test-srt-integration docker-build-srt-parity test-srt-integration-linux check-compile-warnings manual format-elisp refill-news refill-readme

SRT_PARITY_DOCKER_IMAGE ?= ellama-srt-parity:latest
SRT_PARITY_DOCKERFILE ?= docker/srt-parity-linux.Dockerfile
SRT_PARITY_DOCKER_RUN_FLAGS ?= --rm --privileged

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

test-detailed:
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
		--eval "(setq ert-batch-backtrace-right-margin 200)" \
		--eval "(ert-run-tests-batch-and-exit t)"

test-srt-integration:
	ELLAMA_SRT_INTEGRATION=1 emacs -batch --eval "(package-initialize)" \
		-l tests/test-ellama-tools-srt-integration.el \
		--eval "(ert-run-tests-batch-and-exit \"test-ellama-tools-srt-integration-\")"

docker-build-srt-parity:
	docker build -t $(SRT_PARITY_DOCKER_IMAGE) -f $(SRT_PARITY_DOCKERFILE) .

test-srt-integration-linux: docker-build-srt-parity
	docker run $(SRT_PARITY_DOCKER_RUN_FLAGS) \
		-v $(CURDIR):/work \
		-w /work \
		$(SRT_PARITY_DOCKER_IMAGE) \
		make test-srt-integration

check-compile-warnings:
	emacs --batch --eval "(package-initialize)" --eval "(setq native-comp-eln-load-path (list default-directory))" -L . -f batch-native-compile $(ELLAMA_COMPILE_ORDER)

manual:
	emacs -batch --eval "(package-initialize)" \
		--eval "(require 'project)" \
		-l ellama-manual.el \
		--eval "(ellama-manual-export)"

format-elisp:
	emacs -batch --eval "(let ((files (append (file-expand-wildcards \"ellama*.el\") (file-expand-wildcards \"tests/*.el\")))) (package-initialize) (require 'transient) (dolist (file files) (with-current-buffer (find-file-noselect file) (emacs-lisp-mode) (indent-region (point-min) (point-max)) (untabify (point-min) (point-max)) (delete-trailing-whitespace) (save-buffer))))"

refill-org := "(with-current-buffer (find-file-noselect \"FILE\") (package-initialize) (require (quote org)) (require (quote org-element)) (setq fill-column 80) (save-excursion (org-with-wide-buffer (cl-loop for el in (reverse (org-element-map (org-element-parse-buffer) (quote (paragraph quote-block item)) (quote identity))) do (goto-char (org-element-property :contents-begin el)) (org-fill-paragraph)))) (save-buffer))"

refill-news:
	emacs -batch --eval $(subst FILE,./NEWS.org,$(refill-org))

refill-readme:
	emacs -batch --eval $(subst FILE,./README.org,$(refill-org))
