#!/bin/bash

source "$(dirname $0)"/test-utils.sh
ERT_TEST="$EL_GET_LIB_DIR/test/test.el"

HOME="$TEST_HOME" exec \
    "$EMACS" -Q -L "$EL_GET_LIB_DIR" -l "$ERT_TEST" \
    --eval "(ert t)"
