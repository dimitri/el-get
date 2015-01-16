#!/usr/bin/env bash

if [ -z "$1" ]; then
  echo "Usage: $0 recipe1 [recipe2 ...]"
  exit 0
fi

source "$(dirname $0)"/test-utils.sh

for r in "$@"; do
  test_recipe interactive "$r"
done
