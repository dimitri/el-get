#!/bin/bash

if [ -z "$1" ]; then
  echo "Usage: $0 testfile1 [testfile2 ...]"
  exit 0
fi

source "$(dirname $0)"/test-utils.sh

# 5 seconds in between tests to avoid accidental DoS from running too
# many tests in a short time
set_default DELAY_BETWEEN_TESTS 5

FAILED_TESTS=0
ALL_TESTS=0

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
    if [ -n "$DO_NOT_CLEAN" ]; then
      echo "Running test without removing $TEST_HOME first";
    else
      add_on_exit "rm -rf $TEST_HOME"
      rm -rf "$TEST_HOME"
    fi
    mkdir -p "$TEST_HOME"/.emacs.d/el-get/
    TMPDIR="$TEST_HOME"

    HOME="$TEST_HOME" "$EMACS" -Q -batch -L "$EL_GET_LIB_DIR" \
      -l "$EL_GET_LIB_DIR/el-get.el" -l "$EL_GET_LIB_DIR/test/test-setup.el" \
      -l "$testfile"
    result="$?"
    if [ "$result" = 0 ]; then
      echo "*** ${EL_GET_SUCCESS_COLOR}SUCCESS${EL_GET_END} $testfile ***"
    else
      echo "*** ${EL_GET_FAILURE_COLOR}FAILED${EL_GET_END} $testfile ***"
      FAILED_TESTS="$(expr $FAILED_TESTS + 1)"
    fi
    ALL_TESTS="$(expr $ALL_TESTS + 1)"
  fi
}

echo "*** Emacs version ***"
echo "EMACS =" $(which $EMACS)
$EMACS --version
echo

while [ -n "$1" ]; do
  run_test "$1"
  shift
  if [ -n "$1" ]; then
    sleep "$DELAY_BETWEEN_TESTS"
  fi
done

echo "Ran $ALL_TESTS tests (FAILED: $FAILED_TESTS)."
if [ "$FAILED_TESTS" -gt 0 ]; then
    exit 1
fi
