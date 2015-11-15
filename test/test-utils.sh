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

get_file () {
  # <name> <fmts>...
  name=$1
  shift
  for fmt in "$@" ; do
    file=$(printf "$fmt" "$name")
    if [ -e "$file" ]; then
      echo "$file"
      return 0
    fi
  done
  echo "*** ERROR $name: Could not find file ***"
  return 1
}

emacs_with_test_home() {
  mode=$1
  testfile=$2
  shift 2

  if [ -n "$DO_NOT_CLEAN" ]; then
    echo "Running test without removing $TEST_HOME first";
  else
    add_on_exit "rm -rf $TEST_HOME"
    rm -rf "$TEST_HOME"
  fi
  mkdir -p "$TEST_HOME"/.emacs.d/el-get/
  TMPDIR="$TEST_HOME"

  [ "$mode" = batch ] && args=(-Q -batch) || args=(-Q)

  HOME="$TEST_HOME" "$EMACS" "${args[@]}" -L "$EL_GET_LIB_DIR" \
      -l "$EL_GET_LIB_DIR/el-get.el" -l "$EL_GET_LIB_DIR/test/test-setup.el" \
      --eval '(setq enable-local-variables :safe)' \
      -l "$testfile" "$@"
  return $?
}

test_recipe () {
  # $1 = <interactive|batch>
  # $2 = <recipe>
  mode=$1
  recipe_file=$(get_file "$2" "%s" "$RECIPE_DIR/%s" \
      "$RECIPE_DIR/%s.rcp" "$RECIPE_DIR/%s.el") || return 1

  lisp_temp_file=`mktemp`
  add_on_exit "rm -f $lisp_temp_file"
  cat >"$lisp_temp_file" <<EOF

(progn
  (setq debug-on-error (not noninteractive)
        el-get-default-process-sync t
        pdef (el-get-read-from-file "$recipe_file")
        pname (plist-get pdef :name)
        el-get-sources (cons pdef (bound-and-true-p el-get-sources)))
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

  echo "*** Testing el-get recipe $recipe_file ***"
  emacs_with_test_home "$mode" "$lisp_temp_file"
  return $?
}

run_test () {
  # $1 = <interactive|batch>
  # $2 = <test>
  mode=$1
  testfile=$(get_file "$2" "%s" "$TEST_DIR/%s" \
      "$TEST_DIR/%s.el" "$TEST_DIR/el-get-issue-%s.el") || return 1

  echo "*** Running el-get test $testfile ***"
  emacs_with_test_home "$mode" "$testfile"
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
