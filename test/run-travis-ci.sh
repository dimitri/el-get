#!/usr/bin/env bash

$EMACS --version

set -ex

# for now, just require no warnings or errors during compilation
$EMACS -Q -L pkg/ -L . -L methods/ -batch --eval '(setq byte-compile-error-on-warn t)' \
    -f batch-byte-compile *.el methods/*.el

if [ "$EMACS" = emacs ] ; then # only run this for 1 emacs version
     $EMACS -Q -L . -batch -l el-get-recipes -f el-get-check-recipe-batch \
         -Wno-features -Wno-github -Wno-autoloads \
         recipes/

     if [ -z "$TRAVIS_COMMIT_RANGE" ] ; then
         # Contrary to http://docs.travis-ci.com/user/ci-environment,
         # $TRAVIS_COMMIT_RANGE is not defined for pull requests.
         # See https://github.com/travis-ci/travis-ci/issues/1719
         TRAVIS_COMMIT_RANGE=$TRAVIS_BRANCH..FETCH_HEAD
     fi
     git --no-pager -c core.whitespace=tab-in-indent diff --check "$TRAVIS_COMMIT_RANGE"
fi

# TODO: actually run some tests
# cd "$(dirname "$0")"
# ./run-ert.sh
# ./run-test.sh el-get-*.el 2>/dev/null
