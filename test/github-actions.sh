# source me
# Define the following functions here to keep .github/workflows/test.yml nice and tidy
#   - check-recipes()
#   - check-whitespace() // requires $COMMIT_RANGE
#   - check-filename()   // requires $COMMIT_RANGE
#   - byte-compile()
#   - ert-tests()

: ${EMACS:=emacs}

# check-recipes [-W<check>...] <path>
check-recipes() {
    "$EMACS" -Q -L . -batch -l el-get-check -f el-get-check-recipe-batch "$@"
}

if [ -r "$GITHUB_EVENT_PATH" ] ; then
   BASE=$(cat "$GITHUB_EVENT_PATH" | jq -r '.pull_request.base.sha')
   if [ "$BASE" = 'null' ]; then
       BASE=$(cat "$GITHUB_EVENT_PATH" | jq -r '.before')
   fi

   HEAD=$(cat "$GITHUB_EVENT_PATH" | jq -r '.pull_request.head.sha')
   if [ "$HEAD" = 'null' ]; then
       HEAD=$(cat "$GITHUB_EVENT_PATH" | jq -r '.after')
   fi

   COMMIT_RANGE="${BASE}..${HEAD}"
fi

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

# show definitions for log
declare -f byte-compile ert-tests check-recipes check-whitespace
