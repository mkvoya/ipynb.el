EMACS ?= emacs
PYTHON ?= python3

.PHONY: test compile test-kernel test-live test-lsp test-preview
compile:
	$(EMACS) -Q --batch -L . -f batch-byte-compile ipynb.el ipynb-lsp.el ipynb-inspector.el ipynb-preview.el

test:
	$(EMACS) -Q --batch -L . -l test/ipynb-test.el -l test/ipynb-lsp-test.el -l test/ipynb-preview-test.el -f ert-run-tests-batch-and-exit

test-kernel:
	$(PYTHON) -m unittest discover -s test -p 'test_*.py' -v

test-live:
	IPYNB_TEST_PYTHON=$(PYTHON) $(EMACS) -Q --batch -L . -l test/ipynb-live-test.el -f ert-run-tests-batch-and-exit

test-lsp:
	$(EMACS) -Q --batch -L . -l test/ipynb-lsp-live-test.el -f ert-run-tests-batch-and-exit

test-preview:
	IPYNB_TEST_PYTHON=$(PYTHON) $(EMACS) -Q --batch -L . -l test/ipynb-preview-test.el -f ert-run-tests-batch-and-exit
