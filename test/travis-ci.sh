# source me
# Define the following functions here to keep .travis.yml nice and tidy
#   - prereqs()
#   - byte-compile()
#   - check-recipes()
#   - check-whitespace()
#   - ert-tests()

# Installs Emacs plus ert and package.el as needed.
prereqs() {
    curl -LO https://github.com/npostavs/emacs-travis/releases/download/bins/emacs-bin-${EMACS_VERSION}.tar.gz
    tar -xaf emacs-bin-${EMACS_VERSION}.tar.gz -C /
    # Configure $PATH: Emacs installed to /tmp/emacs
    export PATH=/tmp/emacs/bin:${PATH}

    # Put external elisp into pkg/.
    (mkdir -p pkg && cd pkg
     if ! emacs -Q --batch --eval "(require 'ert)" ; then
         ert_compat=https://raw.githubusercontent.com/ohler/ert/c619b56c5bc6a866e33787489545b87d79973205
         curl -LO $ert_compat/lisp/emacs-lisp/ert.el \
              -O  $ert_compat/lisp/emacs-lisp/ert-x.el
     fi
     if ! emacs -Q --batch --eval "(require 'package)" ; then
         pkg_compat23=https://raw.githubusercontent.com/mirrors/emacs/ba08b24186711eaeb3748f3d1f23e2c2d9ed0d09
         curl -LO $pkg_compat23/lisp/emacs-lisp/package.el
     fi)
}

if [ "$EMACS" = '24.5' ]; then
    # check-recipes [-W<check>...] <path>
    check-recipes() {
        emacs -Q -L . -batch -l el-get-check -f el-get-check-recipe-batch "$@"
    }
    check-whitespace() {
        git --no-pager -c core.whitespace=tab-in-indent diff --check "$TRAVIS_COMMIT_RANGE"
    }
else
    # If we have only changes to recipe files, there is no need to run
    # with more than 1 emacs version.
    git diff --name-only "$TRAVIS_COMMIT_RANGE" | grep -Fvq recipes/ || exit 0

    # Only need to run these for 1 version, so make them nops here
    check-recipes() { :; }
    check-whitespace() { :; }
fi

folded_call() {
    travis_fold start $1
    "$@"
    ret=$?
    travis_fold end $1
    return $ret
}

ert-tests() {
    emacs -batch -Q -L pkg/ -L . -l test/el-get-tests.el -f ert-run-tests-batch-and-exit
}

# byte-compile [-Werror] <files>...
byte-compile() {
    error_on_warning=nil
    if [ "$1" = "-Werror" ] ; then
        error_on_warning=t
        shift
    fi
    emacs -Q -L pkg/ -L . -L methods/ -batch \
          --eval "(setq byte-compile-error-on-warn $error_on_warning)" \
          -f batch-byte-compile "$@"
}

shopt -s nullglob

# show definitions for log
declare -f prereqs byte-compile ert-tests check-recipes check-whitespace
