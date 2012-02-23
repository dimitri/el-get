#!/bin/sh

cd "$(dirname "$0")"

echo "If you are sure you want to test ALL the recipes, run the following command:"
echo "exec $PWD/test-recipe.sh $PWD/../recipes/*.rcp 2>/dev/null"
