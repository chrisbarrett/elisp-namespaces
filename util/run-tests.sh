#!/usr/bin/env bash
#
# USAGE:
#  run-tests.sh [emacs-path]

SRC="./namespaces.el"
TESTS="./namespaces-tests.el"
CMD="(ert-run-tests-batch-and-exit)"

EMACS=$1
if [ -z $EMACS ]; then
    EMACS=emacs
fi

$EMACS -q -l $SRC -l $TESTS --batch --eval ${CMD} || exit 1;
