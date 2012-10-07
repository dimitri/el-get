#!/bin/bash

if [ -z "$1" ]; then
  echo "Usage: $0 recipe1 [recipe2 ...]"
  exit 0
fi

set_default () {
  eval "
if [ -z \$$1 ]; then
  $1=$2
fi
"
}

# http://www.linuxjournal.com/content/use-bash-trap-statement-cleanup-temporary-files
on_exit ()
{
    for i in "${on_exit_items[@]}"
    do
        eval $i
    done
}

add_on_exit ()
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

# 5 seconds in between tests to avoid accidental DoS from running too
# many tests in a short time
set_default DELAY_BETWEEN_TESTS 5

set_default RECIPE_DIR "$EL_GET_LIB_DIR/recipes"

get_recipe_file () {
  for x in "$1" "$RECIPE_DIR/$1" "$RECIPE_DIR/$1.rcp" "$RECIPE_DIR/$1.el"; do
    if [ -e "$x" ]; then
      echo "$x"
      break
    fi
  done
}

test_recipe () {
  recipe_file="$(get_recipe_file "$1")"
  if [ ! -n "$recipe_file" ]; then
    echo "*** Skipping nonexistent recipe $1 ***"
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
  (setq el-get-default-process-sync t
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

  HOME="$TEST_HOME" "$EMACS" -Q -batch -L "$EL_GET_LIB_DIR" \
    -l "$EL_GET_LIB_DIR/el-get.el" -l "$EL_GET_LIB_DIR/test/test-setup.el" \
    -l "$lisp_temp_file"
  result="$?"
  if [ "$result" = 0 ]; then
    echo "*** SUCCESS $recipe_file ***"
  else
    echo "*** FAILED $recipe_file ***"
  fi
  rm -f "$lisp_temp_file"
}

while [ -n "$1" ]; do
  test_recipe "$1"
  shift
  if [ -n "$1" ]; then
    sleep "$DELAY_BETWEEN_TESTS"
  fi
done
