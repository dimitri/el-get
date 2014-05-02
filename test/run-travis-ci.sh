#!/bin/bash

$EMACS --version

set -ex

# for now, just require no warnings or errors during compilation
$EMACS -Q -L pkg/ -L . -L methods/ -batch --eval '(setq byte-compile-error-on-warn t)' \
    -f batch-byte-compile *.el methods/*.el

if [ "$EMACS" = emacs ] ; then # only run this for 1 emacs version
     $EMACS -Q -L . -batch -l el-get-recipes -f el-get-check-recipe-batch \
         -Wno-features -Wno-github -Wno-autoloads \
         recipes/
fi

# TODO: actually run some tests
# cd "$(dirname "$0")"
# ./run-ert.sh
# ./run-test.sh el-get-*.el 2>/dev/null
