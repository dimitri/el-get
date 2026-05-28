#!/bin/bash
# Define the following functions here to keep .github/workflows/test.yml nice and tidy
#   - check-recipes()
#   - check-whitespace() // requires $COMMIT_RANGE
#   - check-filenames()   // requires $COMMIT_RANGE
#   - byte-compile()
#   - ert-tests()
Usage() {
    echo 'Usage: <check-whitespace|check-filenames|check-recipes|byte-compile|ert-tests> [args...]' >&2
    exit 1
}

: ${EMACS:=emacs}

# check-recipes [-W<check>...] <path>
check-recipes() {
    "$EMACS" -Q -L . -batch -l el-get-check -f el-get-check-recipe-batch "$@"
}

if [ -n "$COMMIT_RANGE" ] ; then
   check-whitespace() {
       git --no-pager -c core.whitespace=tab-in-indent diff --check "$COMMIT_RANGE"
   }
   # Adapted from Emacs' build-aux/git-hooks/pre-commit.
   check-filenames() {
       git diff --name-only --diff-filter=A "$COMMIT_RANGE" |
           grep -E '^-|/-|[^-+./_0-9A-Z_a-z]' |
           while IFS= read -r new_name; do
               case $new_name in
                   -* | */-*)
                       echo "$new_name: File name component begins with '-'."
                       return 1;;
                   *)
                       echo "$new_name: File name does not consist of -+./_ or ASCII letters or digits."
                       return 1;;
               esac
           done
   }
fi

ert-tests() {
    "$EMACS" -batch -Q $EMACS_OPT -L . -l test/el-get-tests.el -f ert-run-tests-batch-and-exit
}

# byte-compile [-Werror] [files]...
byte-compile() {
    error_on_warning=nil
    if [ "$1" = "-Werror" ] ; then
        error_on_warning=t
        shift
    fi
    if [ "$#" -eq 0 ] ; then
        set -- *.el methods/*.el
    fi
    args=(-Q -L . -L methods/ -batch
          --eval "(setq byte-compile-error-on-warn $error_on_warning)")
    for el in "$@" ; do
        printf 'ELC %s\n' "$el"
        "$EMACS" "${args[@]}" -f batch-byte-compile "$el"
    done
}

shopt -s nullglob

[ "$#" -eq 0 ] && Usage
fun=$1
shift 1

case $fun in
    (check-whitespace|check-filenames|check-recipes|byte-compile|ert-tests)
        # show definition for log
        declare -f $fun
        "$fun" "$@";;
    (*) Usage;;
esac
