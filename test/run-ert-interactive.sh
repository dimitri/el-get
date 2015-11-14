#!/usr/bin/env bash

source "$(dirname $0)"/test-utils.sh
ERT_TEST="$EL_GET_LIB_DIR/test/el-get-tests.el"

emacs_with_test_home interactive "$ERT_TEST" --eval "(ert t)"
