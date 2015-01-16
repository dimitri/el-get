#!/usr/bin/env bash

set_default () {
  eval "
if [ -z \$$1 ]; then
  $1=$2
fi
"
}

set_default EL_GET_LIB_DIR "$(dirname "$(dirname "$(readlink -f "$0")")")"
set_default TMPDIR "$(dirname "$(mktemp --dry-run)")"
set_default TEST_HOME "$TMPDIR/el-get-test-home"
set_default EMACS "$(which emacs)"

mkdir -p "$TEST_HOME"/.emacs.d
if [ -n "$DO_NOT_CLEAN" ]; then
  echo "Running test without removing $TEST_HOME first";
else
  rm -rf "$TEST_HOME"/.emacs.d/el-get/
fi
HOME="$TEST_HOME" "$EMACS" -Q -L "$EL_GET_LIB_DIR" \
  -l "$EL_GET_LIB_DIR/el-get.el" -l "$EL_GET_LIB_DIR/test/test-setup.el"
