EMACS ?= emacs
CASK ?= cask

.PHONY: all build test clean

all:
	${MAKE} clean
	${MAKE} test
	${MAKE} build
	${MAKE} test
	${MAKE} clean

build:
	${CASK} exec ${EMACS} -batch -Q -L . -eval \
	" (batch-byte-compile)" ess-r-spreadsheet.el

test:
	${CASK} exec ${EMACS} -Q -batch -L . -l test/ess-r-spreadsheet-tests.el -f ert-run-tests-batch-and-exit

clean:
	rm -f ess-r-spreadsheet.elc
