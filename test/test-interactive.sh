#!/usr/bin/env bash

source "$(dirname $0)"/test-utils.sh

emacs_with_test_home interactive "$@"
