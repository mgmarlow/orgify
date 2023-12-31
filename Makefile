EMACS := emacs -Q

LOADPATH := -L .

PACKAGE_FILES := $(filter-out build-docs.el $(wildcard *-test.el), $(wildcard *.el))

TEST_FILES := $(wildcard test/*-test.el)

COMPILED_FILES := $(wildcard *.elc)

.PHONY: install compile test clean clean-docs build-docs publish-docs

install:
	npm install wrangler --global

test: compile
	$(EMACS) -batch $(LOADPATH) -l ert $(foreach file,$(TEST_FILES), -l $(file)) -f ert-run-tests-batch-and-exit

compile: clean
	$(EMACS) -batch $(LOADPATH) -f batch-byte-compile $(PACKAGE_FILES)

clean: clean-docs
	rm -f $(COMPILED_FILES)

clean-docs:
	rm -rf output/

build-docs: clean-docs
	$(EMACS) --script build-docs.el

publish-docs: build-docs
	wrangler pages deploy output/
