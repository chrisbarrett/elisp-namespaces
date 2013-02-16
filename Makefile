SRC=./namespaces.el
TESTS=./namespaces-tests.el

EMACS=emacs
FILES=-l $(SRC) -l $(TESTS)
CMD="(ert-run-tests-batch-and-exit)"
OPTS=-q $(FILES) --batch --eval $(CMD)

all: test

clean:
	rm -f ./*.elc

test:
	$(EMACS) --version
	$(EMACS) $(OPTS)
