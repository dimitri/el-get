# source me
# Define the following functions here to keep .github/workflows/test.yml nice and tidy
#   - byte-compile()
#   - check-recipes()
#   - check-whitespace()
#   - ert-tests()

BASE=$(cat "$GITHUB_EVENT_PATH" | jq -r '.pull_request.base.sha')
if [ "$BASE" = 'null' ]; then
    BASE=$(cat "$GITHUB_EVENT_PATH" | jq -r '.before')
fi

HEAD=$(cat "$GITHUB_EVENT_PATH" | jq -r '.pull_request.head.sha')
if [ "$HEAD" = 'null' ]; then
    HEAD=$(cat "$GITHUB_EVENT_PATH" | jq -r '.after')
fi

COMMIT_RANGE="${BASE}..${HEAD}"

if [ "$EMACS_VERSION" = '24.5' ]; then
    # check-recipes [-W<check>...] <path>
    check-recipes() {
        emacs -Q -L . -batch -l el-get-check -f el-get-check-recipe-batch "$@"
    }
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
else
    # If we have only changes to recipe files, there is no need to run
    # with more than 1 emacs version.
    git diff --name-only "$COMMIT_RANGE" | grep -Fvq recipes/ ||
        { echo 'Recipe only diff, skipping tests' ; exit 0 ; }

    # Only need to run these for 1 version, so make them nops here
    check-recipes() { :; }
    check-whitespace() { :; }
    check-filenames() { :; }
fi

ert-tests() {
    emacs -batch -Q $EMACS_OPT -L . -l test/el-get-tests.el -f ert-run-tests-batch-and-exit
}

# byte-compile [-Werror] <files>...
byte-compile() {
    error_on_warning=nil
    if [ "$1" = "-Werror" ] ; then
        error_on_warning=t
        shift
    fi
    emacs -Q -L . -L methods/ -batch \
          --eval "(setq byte-compile-error-on-warn $error_on_warning)" \
          -f batch-byte-compile "$@"
}

shopt -s nullglob

# show definitions for log
declare -f byte-compile ert-tests check-recipes check-whitespace
