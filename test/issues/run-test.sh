#!/usr/bin/env bash

if [ -z "$1" ]; then
  echo "Usage: $0 testfile1 [testfile2 ...]"
  exit 0
fi

EL_GET_LIB_DIR=$(dirname "$(dirname "$(dirname "$(readlink -f "$0")")")")
source "$(dirname $0)"/../test-utils.sh xx

# 5 seconds in between tests to avoid accidental DoS from running too
# many tests in a short time
set_default DELAY_BETWEEN_TESTS 5

FAILED_TESTS=0
ALL_TESTS=0

echo "*** Emacs version ***"
echo "EMACS =" $(which $EMACS)
$EMACS --version
echo

while [ -n "$1" ]; do
  if run_test batch "$1" ; then
    echo "*** ${EL_GET_SUCCESS_COLOR}SUCCESS${EL_GET_END} $testfile ***"
  else
    echo "*** ${EL_GET_FAILURE_COLOR}FAILED${EL_GET_END} $testfile ***"
    FAILED_TESTS="$(expr $FAILED_TESTS + 1)"
  fi
  ALL_TESTS="$(expr $ALL_TESTS + 1)"
  shift
  if [ -n "$1" ]; then
    sleep "$DELAY_BETWEEN_TESTS"
  fi
done

echo "Ran $ALL_TESTS tests (FAILED: $FAILED_TESTS)."
if [ "$FAILED_TESTS" -gt 0 ]; then
    exit 1
fi
