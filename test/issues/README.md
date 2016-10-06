# Old issue reproducers

This directory contains tests to replicate old issues that came up,
but since they generally require network access to install packages
they can't reliably be used for an automated test suite.

## el-get manual tests

Tries to cover some known issues, example that will allow building a
regression test suite some day.

Typical run (in bash):

    mkdir -p /tmp/test/.emacs.d/el-get/
    rm -rf /tmp/test/.emacs.d/el-get/*
    HOME=/tmp/test /usr/bin/emacs-snapshot-gtk -Q -batch -L . -l el-get \
        -l test/el-get-issue-303.el

At the moment you need to run the el-get-issue scripts with some known
working el-get version to know what to expect, and you probably need
to have el-get installed at +~/dev/emacs/el-get/+ for this to work
as-is.

Yet it might be useful.

## el-get automated tests

### Simple script-based automation

In the test directory, there is a simple bash script called
+run-test.sh+ that attempts to automate the manual steps detailed
above. The script creates a temporary home directory and runs emacs in
batch mode, first loading el-get and then the specified test file. An
invocation equivalent to the above would look like this (starting from
the main el-get directory):

    test/run-test.sh el-get-issue-303.el

or even just

    test/run-test.sh 303

The script takes care of cleaning up the temporary directory after the
test, so you can run tests one after the other without previous tests
causing problems for later ones. The bahavior of the +run-test.sh+
script can be customized with a few environment variables:

- `TMPDIR` and `TEST_HOME`, for controlling where the temporary home
  directory is created;
- `EL_GET_LIB_DIR`, the directory where the el-get code resides;
- `EMACS`, which can be used to override the copy of emacs found in
  your `PATH`, or to provide one if there is no emacs in your `PATH`
  or it is installed under a nonstandard name.

You can run multiple tests by simply providing multiple test files to
`run-test.sh`:

    test/run-test.sh 200 303 310

When running multiple tests, the script will wait 5 seconds between
tests, since many tests involve downloading files from the same few
servers (Github, emacswiki, marmalade, etc.), and it is impolite to
download a large number of files in rapid succession just for
testing. There is a script called +run-all-tests.sh+ that acts as a
shortcut for running all the available tests.

Sometimes you may want to run a test in an interactive emacs session
so that you can use Emacs' debugging facilities to investigate what
went wrong. To do this, use `run-test-interactive.sh` instead of
`run-test.sh`.

If simply want to test installation, updating, and removal of one or
more recipes, you can use the `test-recipe.sh` script.
