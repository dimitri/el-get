# source me
# Define the following functions here to keep .travis.yml nice and tidy
#   - prereqs()
#   - byte-compile()
#   - check-recipes()
#   - check-whitespace()
# TODO: run-tests()

# show execution in log
set -v

# Contrary to http://docs.travis-ci.com/user/ci-environment,
# $TRAVIS_COMMIT_RANGE is not defined for pull requests.
# See https://github.com/travis-ci/travis-ci/issues/1719
: ${TRAVIS_COMMIT_RANGE:=$TRAVIS_BRANCH..FETCH_HEAD}

if [ "$EMACS" = 'emacs-snapshot' ]; then
    # If we have only changes to recipe files, there is no need to run
    # with more than 1 emacs version.
    git diff --name-only "$TRAVIS_COMMIT_RANGE" | grep -Fvq recipes/ || exit 0

    prereqs() {
        sudo add-apt-repository -y ppa:cassou/emacs || exit $?;
        sudo apt-get update -qq || exit $?;
        sudo apt-get install -qq emacs-snapshot-nox || exit $?;
    }
    # Only need to run these for 1 version, so make them nops here
    check-recipes() { :; }
    check-whitespace() { :; }
else
    prereqs() {
        mkdir pkg && \
            curl --silent --show-error -L -o pkg/package.el \
            https://github.com/mirrors/emacs/raw/1a0a666f941c99882093d7bd08ced15033bc3f0c/lisp/emacs-lisp/package.el;
    }
    check-recipes() {
        "$EMACS" -Q -L . -batch -l el-get-recipes -f el-get-check-recipe-batch \
            -Wno-features -Wno-github -Wno-autoloads \
            recipes/
    }
    check-whitespace() {
        git --no-pager -c core.whitespace=tab-in-indent diff --check "$TRAVIS_COMMIT_RANGE"
    }
fi

byte-compile() {
    "$EMACS" -Q -L pkg/ -L . -L methods/ -batch --eval '(setq byte-compile-error-on-warn t)' \
        -f batch-byte-compile *.el methods/*.el
}

