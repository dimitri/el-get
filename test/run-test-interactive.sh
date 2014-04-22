#!/bin/bash

if [ -z "$1" ]; then
  echo "Usage: $0 testfile1 [testfile2 ...]"
  exit 0
fi

source "$(dirname $0)"/test-utils.sh

run_test () {
  for x in "$1" "$TEST_DIR/$1" "$TEST_DIR/$1.el" "$TEST_DIR/el-get-issue-$1.el"; do
    if [ -f "$x" ]; then
      testfile="$x"
    fi
  done
  if [ -z "$testfile" ]; then
    echo "*** ERROR $1: Could not find test file ***"
  else
    echo "*** Running el-get test $testfile interactively ***"
    if [ -n "$DO_NOT_CLEAN" ]; then
      echo "Running test without removing $TEST_HOME first";
    else
      rm -rf "$TEST_HOME"/.emacs.d/el-get/
    fi
    mkdir -p "$TEST_HOME"/.emacs.d/el-get/
    HOME="$TEST_HOME" "$EMACS" -Q -L "$EL_GET_LIB_DIR" \
      -l "$EL_GET_LIB_DIR/el-get.el" -l "$EL_GET_LIB_DIR/test/test-setup.el" \
      -l "$testfile"
  fi
}

for t in "$@"; do
  run_test "$t"
done
