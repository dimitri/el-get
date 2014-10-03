#!/usr/bin/env bash

if [ -z "$1" ]; then
  echo "Usage: $0 recipe1 [recipe2 ...]"
  exit 0
fi

source "$(dirname $0)"/test-utils.sh

# 5 seconds in between tests to avoid accidental DoS from running too
# many tests in a short time
set_default DELAY_BETWEEN_TESTS 5

while [ -n "$1" ]; do
  if test_recipe batch "$1"; then
    echo "${EL_GET_SUCCESS_COLOR}*** SUCCESS $recipe_file ***${EL_GET_END}"
  else
    echo "${EL_GET_FAILURE_COLOR}*** FAILED $recipe_file ***${EL_GET_END}"
  fi
  shift
  if [ -n "$1" ]; then
    sleep "$DELAY_BETWEEN_TESTS"
  fi
done
