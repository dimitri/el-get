#!/bin/bash

cd "$(dirname "$0")"

exec ./run-test.sh el-get-*.el 2>/dev/null
