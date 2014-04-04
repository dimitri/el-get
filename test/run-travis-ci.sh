#!/bin/bash

set -ex

# for now, just require no warnings or errors during compilation
$EMACS -Q -L . -L methods -batch --eval '(setq byte-compile-error-on-warn t)' \
    -f batch-byte-compile *.el methods/*.el

# TODO: actually run some tests
# cd "$(dirname "$0")"
# ./run-ert.sh
# ./run-test.sh el-get-*.el 2>/dev/null
