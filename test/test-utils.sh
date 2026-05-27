set_default () {
  eval ": \${$1:=$2}"
}

rm_on_exit_items=()
on_exit()
{
    if [ ${#rm_on_exit_items[*]} -gt 0 ] ; then
        rm -rf "${#rm_on_exit_items[@]}"
    fi
}
trap on_exit EXIT

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
  shift 1

  if [ -n "$DO_NOT_CLEAN" ]; then
      echo "Running test with persistent $TEST_HOME first"
  else
      echo "Running test with clean $TEST_HOME"
      rm_on_exit_items+=("$TEST_HOME")
      rm -rf "$TEST_HOME"
  fi
  mkdir -p "$TEST_HOME"/.emacs.d/el-get/
  TMPDIR="$TEST_HOME"

  args=()
  [ "$mode" = batch ] && args+=(-batch)
  args+=(-L "$EL_GET_LIB_DIR" -l "$EL_GET_LIB_DIR/test/test-setup.el"
         --eval '(setq enable-local-variables :safe)')

  HOME="$TEST_HOME" "$EMACS" "${args[@]}" "$@"
  return $?
}

test_recipe () {
  # $1 = <interactive|batch>
  # $2 = <recipe>
  mode=$1
  recipe_file=$(get_file "$2" "%s" "$RECIPE_DIR/%s" \
      "$RECIPE_DIR/%s.rcp" "$RECIPE_DIR/%s.el") || return 1
  echo "*** Testing el-get recipe $recipe_file ***"
  emacs_with_test_home "$mode" -f el-get-test-rcp-life-cycle "$recipe_file"
  return $?
}

run_test () {
  # $1 = <interactive|batch>
  # $2 = <test>
  mode=$1
  testfile=$(get_file "$2" "%s" "$TEST_DIR/%s" \
      "$TEST_DIR/%s.el" "$TEST_DIR/el-get-issue-%s.el") || return 1

  echo "*** Running el-get test $testfile ***"
  emacs_with_test_home "$mode" -l "$testfile"
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
