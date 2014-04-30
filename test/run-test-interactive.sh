#!/bin/bash

if [ -z "$1" ]; then
  echo "Usage: $0 testfile1 [testfile2 ...]"
  exit 0
fi

source "$(dirname $0)"/test-utils.sh

for t in "$@"; do
  run_test interactive "$t"
done
