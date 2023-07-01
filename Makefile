EMACS := emacs -Q

LOADPATH := -L .

PACKAGE_FILES := $(filter-out $(wildcard *-test.el), $(wildcard *.el))

TEST_FILES := $(wildcard *-test.el)

COMPILED_FILES := $(wildcard *.elc)

.PHONY: compile test clean

test: compile
	$(EMACS) -batch $(LOADPATH) -l ert -l $(TEST_FILES) -f ert-run-tests-batch-and-exit

compile: clean
	$(EMACS) -batch $(LOADPATH) -f batch-byte-compile $(PACKAGE_FILES)

clean: clean-docs
	rm -f $(COMPILED_FILES)

clean-docs:
	rm -rf output/

build-docs: clean-docs
	$(EMACS) --script build-docs.el
