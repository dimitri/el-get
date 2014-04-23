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

get_recipe_file () {
  for x in "$1" "$RECIPE_DIR/$1" "$RECIPE_DIR/$1.rcp" "$RECIPE_DIR/$1.el"; do
    if [ -e "$x" ]; then
      echo "$x"
      break
    fi
  done
}

test_recipe () {
  # $1 = <interactive|batch>
  # $2 = <recipe>
  mode=$1
  recipe_file="$(get_recipe_file "$2")"

  if [ ! -n "$recipe_file" ]; then
    echo "*** Skipping nonexistent recipe $2 ***"
    return
  fi
  echo "*** Testing el-get recipe $recipe_file ***"
  mkdir -p "$TEST_HOME"/.emacs.d
  if [ -n "$DO_NOT_CLEAN" ]; then
    echo "Running test without removing $TEST_HOME first";
  else
    add_on_exit "rm -rf $TEST_HOME"
    rm -rf "$TEST_HOME"
  fi
  mkdir -p "$TEST_HOME"/.emacs.d/el-get/
  TMPDIR="$TEST_HOME"

  lisp_temp_file=`mktemp`
  add_on_exit "rm -f $lisp_temp_file"
  cat >"$lisp_temp_file" <<EOF

(progn
  (setq debug-on-error (not noninteractive)
        el-get-default-process-sync t
        pdef (el-get-read-recipe-file "$recipe_file")
        pname (plist-get pdef :name)
        el-get-sources (list pdef))
  (el-get (quote sync) pname)
  (message "*** Initial install successful ***")
  (el-get-update pname)
  (message "*** Update successful ***")
  (el-get-remove pname)
  (message "*** Removal successful ***")
  (el-get-install pname)
  (message "*** Second install successful ***")
  (assert (el-get-package-is-installed pname) nil
          "Package %s should be installed right now but isn't" pname))

EOF

  [ "$mode" = batch ] && args=(-Q -batch) || args=(-Q)

  HOME="$TEST_HOME" "$EMACS" "${args[@]}" -L "$EL_GET_LIB_DIR" \
      -l "$EL_GET_LIB_DIR/el-get.el" -l "$EL_GET_LIB_DIR/test/test-setup.el" \
      -l "$lisp_temp_file"
  return $?
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
