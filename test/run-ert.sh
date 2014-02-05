#!/bin/bash

source "$(dirname $0)"/test-utils.sh
ERT_TEST="$EL_GET_LIB_DIR/test/test.el"


if [ -n "$DO_NOT_CLEAN" ]; then
    echo "Running test without removing $TEST_HOME first";
else
    add_on_exit "rm -rf $TEST_HOME"
    rm -rf "$TEST_HOME"
fi
mkdir -p "$TEST_HOME"/.emacs.d/el-get/


HOME="$TEST_HOME" exec \
    "$EMACS" -Q -batch -L "$EL_GET_LIB_DIR" -l "$ERT_TEST" \
    -f ert-run-tests-batch-and-exit
