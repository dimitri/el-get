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
set_default TEST_HOME "$TMPDIR/el-get-test-home"
set_default EMACS "$(which emacs)"

run_test () {
  testfile="$1"
  echo "*** Running el-get test $testfile ***"
  mkdir -p "$TEST_HOME"/.emacs.d
  rm -rf "$TEST_HOME"/.emacs.d/el-get/
  HOME="$TEST_HOME" "$EMACS" -Q -batch -L "$EL_GET_LIB_DIR" -l "$testfile"
  result="$?"
  if [ "$result" = 0 ]; then
    echo "*** SUCCESS $testfile ***"
  else
    echo "*** FAILED $testfile ***"
  fi
}

for t in "$@"; do
  run_test "$t"
done
