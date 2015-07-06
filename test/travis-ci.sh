# source me
# Define the following functions here to keep .travis.yml nice and tidy
#   - prereqs()
#   - byte-compile()
#   - check-recipes()
#   - check-whitespace()
# TODO: run-tests()

if [ "$EMACS" = 'emacs-snapshot' ]; then
    # If we have only changes to recipe files, there is no need to run
    # with more than 1 emacs version.
    git diff --name-only "$TRAVIS_COMMIT_RANGE" | grep -Fvq recipes/ || exit 0

    prereqs() {
        sudo add-apt-repository -y ppa:cassou/emacs || exit $?;
        sudo apt-get update -qq || exit $?;
        sudo apt-get install -qq --force-yes emacs-snapshot-nox || exit $?;
    }
    # Only need to run these for 1 version, so make them nops here
    check-recipes() { :; }
    check-whitespace() { :; }
else
    prereqs() {
        mkdir pkg && \
            curl --silent --show-error -L -o pkg/package.el \
            https://github.com/mirrors/emacs/raw/ba08b24186711eaeb3748f3d1f23e2c2d9ed0d09/lisp/emacs-lisp/package.el;
    }
    check-recipes() {
        "$EMACS" -Q -L . -batch -l el-get-check -f el-get-check-recipe-batch \
            -Wno-features -Wno-github -Wno-autoloads \
            recipes/
    }
    check-whitespace() {
        git --no-pager -c core.whitespace=tab-in-indent diff --check "$TRAVIS_COMMIT_RANGE"
    }
fi

byte-compile() {
    travis_fold start byte-compiling
    "$EMACS" -Q -L pkg/ -L . -L methods/ -batch --eval '(setq byte-compile-error-on-warn t)' \
        -f batch-byte-compile *.el methods/*.el
    travis_fold end byte-compiling
}

# show definitions for log
declare -f prereqs byte-compile check-recipes check-whitespace
