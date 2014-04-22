set_default () {
  eval ": \${$1:=$2}"
}

# http://www.linuxjournal.com/content/use-bash-trap-statement-cleanup-temporary-files
on_exit()
{
    for i in "${on_exit_items[@]}"
    do
        eval $i
    done
}

add_on_exit()
{
    local n=${#on_exit_items[*]}
    on_exit_items[$n]="$*"
    if [[ $n -eq 0 ]]; then
        trap on_exit EXIT
    fi
}

set_default EL_GET_LIB_DIR "$(dirname "$(dirname "$(readlink -f "$0")")")"
set_default TMPDIR "$(dirname "$(mktemp --dry-run)")"
set_default TEST_HOME "$TMPDIR/el-get-test-home"
set_default EMACS "$(which emacs)"

set_default TEST_DIR "$(dirname $0)"
set_default RECIPE_DIR "$EL_GET_LIB_DIR/recipes"

if [[ -t 1 ]]; then
    EL_GET_END="$(tput sgr0)"
    set_default EL_GET_FAILURE_COLOR "$(tput setaf 1)" # red
    set_default EL_GET_SUCCESS_COLOR "$(tput setaf 2)" # green
fi
