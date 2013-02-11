#!/usr/bin/env bash

SRC="./namespaces.el"
TESTS="./namespaces-tests.el"
CMD="(ert-run-tests-batch-and-exit)"

emacs -q -l $SRC -l $TESTS --batch --eval ${CMD} || exit 1;
