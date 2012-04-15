#!/bin/bash

if [ -z "$1" ]; then
  echo "Usage: $0 testfile1 [testfile2 ...]"
  exit 0
fi

set_default () {
  eval "
if [ -z \$$1 ]; then
  $1=$2
fi
"
}

set_default EL_GET_LIB_DIR "$(dirname "$(dirname "$(readlink -f "$0")")")"
set_default TMPDIR "$(dirname "$(mktemp --dry-run)")"
set_default TEMP_HOME "$TMPDIR/el-get-test-home"
set_default EMACS "$(which emacs)"
set_default TEST_DIR "$(dirname $0)"

# 5 seconds in between tests to avoid accidental DoS from running too
# many tests in a short time
set_default DELAY_BETWEEN_TESTS 5

run_test () {
  for x in "$1" "$TEST_DIR/$1" "$TEST_DIR/$1.el" "$TEST_DIR/el-get-issue-$1.el"; do
    if [ -f "$x" ]; then
      testfile="$x"
    fi
  done
  if [ -z "$testfile" ]; then
    echo "*** ERROR $1: Could not find test file ***"
  else
    echo "*** Running el-get test $testfile ***"
    mkdir -p "$TEMP_HOME"/.emacs.d
    rm -rf "$TEMP_HOME"/.emacs.d/el-get/
    HOME="$TEMP_HOME" "$EMACS" -Q -batch -L "$EL_GET_LIB_DIR" \
      -l "$EL_GET_LIB_DIR/el-get.el" -l "$EL_GET_LIB_DIR/test/test-setup.el" \
      -l "$testfile"
    result="$?"
    if [ "$result" = 0 ]; then
      echo "*** SUCCESS $testfile ***"
    else
      echo "*** FAILED $testfile ***"
    fi
  fi
}

while [ -n "$1" ]; do
  run_test "$1"
  shift
  if [ -n "$1" ]; then
    sleep "$DELAY_BETWEEN_TESTS"
  fi
done
