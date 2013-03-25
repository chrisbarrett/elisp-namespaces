SRC=./namespaces.el
TESTS=./namespaces-tests.el

EMACS=emacs
FILES=-l $(SRC) -l $(TESTS)
OPTS=-q $(FILES) --batch -f ert-run-tests-batch-and-exit

all: test

clean:
	rm -f ./*.elc

test:
	$(EMACS) --version
	$(EMACS) $(OPTS)
