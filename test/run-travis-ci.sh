#!/bin/bash

set -ex
cd "$(dirname "$0")"

./run-ert.sh
./run-test.sh el-get-*.el 2>/dev/null
