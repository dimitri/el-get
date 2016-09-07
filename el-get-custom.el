;;; el-get-recipes.el --- Manage the external elisp bits and pieces you depend upon
;;
;; Copyright (C) 2010-2011 Dimitri Fontaine
;;
;; Author: Dimitri Fontaine <dim@tapoueh.org>
;; URL: http://www.emacswiki.org/emacs/el-get
;; GIT: https://github.com/dimitri/el-get
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/
;;
;; This file is NOT part of GNU Emacs.
;;
;; Install
;;     Please see the README.md file from the same distribution

;;; Commentary:
;;
;; el-get-recipes provides the API to manage the el-get recipes.
;;

;; el-get-core provides basic el-get API, intended for developers of el-get
;; and its methods.  See the methods directory for implementation of them.
;;

(require 'el-get-core)

(declare-function widget-editable-list-match "wid-edit" (widget value))

;;; First, some essential variables, used in other parts of the code.
(defgroup el-get nil "el-get customization group"
  :group 'convenience)

(defconst el-get-version "5.1" "el-get version number")

(defconst el-get-script (or load-file-name buffer-file-name))

(defcustom el-get-dir (expand-file-name "el-get" user-emacs-directory)
  "Path where to install the packages."
  :group 'el-get
  :type 'directory)

(defcustom el-get-status-file
  (concat (file-name-as-directory el-get-dir) ".status.el")
  "Define where to store and read the package statuses")

(defvar el-get-autoload-file
  (concat (file-name-as-directory el-get-dir) ".loaddefs.el")
  "Where generated autoloads are saved")

(defvar el-get-emacs (concat invocation-directory invocation-name)
  "Where to find the currently running emacs, a facility for :build commands")

;;
;; Define some code-level customs.
;;
(defcustom el-get-post-init-hooks nil
  "Hooks to run after a package init.
Each hook is a unary function accepting a package"
  :group 'el-get
  :type 'hook)

(defvar find-function-source-path)
(defun el-get-add-load-path-to-ffsp (package)
  "Adds a package's :load-path to `find-function-source-path'.
Can be added to `el-get-post-init-hooks'."
  (setq find-function-source-path
        (append (el-get-load-path package)
                (bound-and-true-p find-function-source-path))))

(defcustom el-get-post-install-hooks nil
  "Hooks to run after installing a package.
Each hook is a unary function accepting a package"
  :group 'el-get
  :type 'hook)

(defcustom el-get-post-update-hooks nil
  "Hooks to run after updating a package.
Each hook is a unary function accepting a package"
  :group 'el-get
  :type 'hook)

(defcustom el-get-post-error-hooks nil
  "Hooks to run after package installation fails.
Each hook is a binay function accepting a package and error data"
  :group 'el-get
  :type 'hook)

(defcustom el-get-byte-compile t
  "Whether or not to byte-compile packages. Can be used to
disable byte-compilation globally, unless this process is not
controlled by `el-get' itself.

The cases when `el-get' loses control are with \"advanced\"
packaging systems (apt-get, fink, pacman, elpa) or when the
recipe contains a :build rule (using a Makefile for example)."
  :group 'el-get
  :type 'boolean)

(defcustom el-get-parallel-make-args
  (let* ((call-prog
          (lambda (name &rest args)
            (with-temp-buffer
              (when (ignore-errors
                      (= (apply #'call-process name nil t nil args) 0))
                (buffer-string)))))
         (nprocs
          (or (getenv "NUMBER_OF_PROCESSORS") ; Windows
              (or (funcall call-prog "getconf" "_NPROCESSORS_ONLN") ; Linux
                  (funcall call-prog "sysctl" "-n" "hw.ncpu") ; *BSD
                  (funcall call-prog "/usr/sbin/psrinfo" "-p")) ; Solaris
              "1")))
    (list (format "-j%d" (1+ (string-to-number nprocs)))))
  "List of args for recipes that invoke parallel make.

Typically, this would be (\"-jN\"), where N is the number of
processors + 1. To use in a recipe call make from `:build' like
this: `((\"make\" ,@el-get-parallel-make-args))"
  :group 'el-get
  :type '(repeat string))

(defcustom el-get-verbose nil
  "Non-nil means print messages describing progress of el-get even for fast operations."
  :group 'el-get
  :type 'boolean)

(defcustom el-get-byte-compile-at-init nil
  "Whether or not to byte-compile packages at init.

Turn this to t if you happen to update your packages from under
`el-get', e.g. doing `cd el-get-dir/package && git pull`
directly."
  :group 'el-get
  :type 'boolean)

(define-obsolete-variable-alias 'el-get-generate-autoloads 'el-get-use-autoloads
  "June, 2014")
(defcustom el-get-use-autoloads t
  "Whether or not to use the generated autoloads for packages. Can be used
to disable autoloads globally."
  :group 'el-get
  :type 'boolean)

(defcustom el-get-is-lazy nil
  "Whether or not to defer evaluation of :post-init and :after
functions until libraries are required.  Will also have el-get
skip the :load and :features properties when set.  See :lazy to
force their evaluation on some packages only."
  :group 'el-get
  :type 'boolean)

(defcustom el-get-auto-update-cached-recipes t
  "When non-nil, auto-update certain properties in cached recipes.

When El-get installs a package, it stores a copy of the package's
recipe that becomes independent from the recipe in
`el-get-sources'. The cached copy is updated only when the
package itself is updated or reinstalled. However, if this
preference is t (the default), select properties of the cached
recipe copy will be updated from `el-get-sources' whenever the
package is initialized (see
`el-get-status-recipe-updatable-properties').

If this is set to nil, then the cached copy will *only* be
updated when the package itself is."
  :group 'el-get
  :type 'boolean)


;;; "Fuzzy" data structure customization widgets
(defun el-get-repeat-value-to-internal (widget element-or-list)
  (el-get-as-list element-or-list))

(defun el-get-repeat-match (widget value)
  (widget-editable-list-match widget (el-get-repeat-value-to-internal widget value)))

(define-widget 'el-get-repeat 'repeat
  "A variable length list of non-lists that can also be represented as a single element"
  :value-to-internal 'el-get-repeat-value-to-internal
  :match 'el-get-repeat-match)

(defun el-get-symbol-match (widget value)
  (or (symbolp value) (stringp value)))

(define-widget 'el-get-symbol 'symbol
  "A string or a symbol, rendered as a symbol"
  :match 'el-get-symbol-match
  )
;;; END "Fuzzy" data structure support

(defconst el-get-build-recipe-body
  '(choice :tag "Format"

           (repeat :tag "List of shell commands"
                   (string :doc "Note: arguments will not be shell-quoted.
Choose `Evaluated expression' format for a more portable recipe" :format "%v%h"))
           (sexp :tag "Evaluated expression" :format "%t: %v%h"
                 :value `(("./configure" ,(concat "--with-emacs=" el-get-emacs)) ("make") ("make" ("install")))

                 :doc "Evaluation should yield a list of lists.
Each sub-list, representing a single shell command, is expected to have
strings and/or lists as elements, sub-sub-lists can have string and/or
list elements, and so on.  Each sub-list will be \"flattened\" to produce
a list of strings, each of which will be `shell-quote-argument'ed before
being sent to the underlying shell."
                 )
           ))


(defcustom el-get-sources nil
  "Additional package recipes

Each entry is a PLIST where the following properties are
supported.

If your property list is missing the :type property, then it's
merged with the recipe one, so that you can override any
definition provided by `el-get' recipes locally.

:name

    The name of the package. It can be different from the name of
    the directory where the package is stored (after a `git
    clone' for example), in which case a symlink will be created.

:depends

    A single package name, or a list of package names, on which
    the package depends.  All of a packages dependencies will be
    installed before the package is installed.

:builtin

    A string containing the Emacs version (see `emacs-version' variable)
    from which Emacs includes the recipe as a builtin.  For example,
    `package.el' (the implementation of ELPA) is part of Emacs 24
    but needs an external recipe for previous major versions.

:type

    The type of the package, currently el-get offers support for
    `apt-get', `elpa', `git', `github', `emacsmirror', `git-svn',
    `bzr' `svn', `cvs', `darcs', `fink', `ftp', `emacswiki',
    `http-tar', `pacman', `hg' and `http'. You can easily support
    your own types here, see the variable `el-get-methods'.

:branch

    Which branch to pull when using `git' (and by extension,
    `github' and `emacsmirror', which are derived form `git') and
    `hg' backends.  Also supported when installing el-get using
    `el-get-install'.

:url

    Where to fetch the package, only meaningful for `git' and
    `http' types.

:pkgname

    The name of the package for the underlying package management
    system (`apt-get', `fink' or `pacman', also supported by
    `github' and `emacsmirror'), which can be different from the
    Emacs package name.

    For the `github' type, this specifies the repo name to clone
    from Github. For example, for el-get, the package name would
    be \"dimitri/el-get\". Note that the `emacsmirror' type is
    just like the `github' type with the prefix \"emacsmirror/\"
    automatically supplied.

:build

    Your build recipe, a list.

    A build command C whose `car' is a symbol (see `symbolp') will be
    replaced by (eval C).

    Then, each element of the recipe will be interpreted as
    a command:

    * If the element is a list of string, the first element of
      the list must be the program to call, and each following
      element a different argument to the call.  No whitespace
      parsing is done.

    * Otherwise, if it is a list, any list sub-elements will be
      recursively \"flattened\" (see `el-get-flatten').  The
      resulting strings will be interpreted as individual shell
      arguments, appropriately quoted.

:build/system-type

    Your specific build recipe for a given `system-type' gets
    there and looks like :build.

:load-path

    A directory or a list of directories you want `el-get' to add
    to your `load-path'. Those directories are relative to where
    the package gets installed.

:compile

    Allow to restrict what to byte-compile: by default, `el-get'
    will compile all elisp files in the :load-path directories,
    unless a :build command exists for the package source. Given
    a :compile property, `el-get' will only byte-compile those
    given files, directories or filename-regexpes in the property
    value. This property can be a `listp' or a `stringp' if you
    want to compile only one of those.

:info

    This string allows you to setup a directory where to find a
    'package.info' file, or a path/to/whatever.info file. It will
    even run `ginstall-info' for you to create the `dir' entry so
    that C-h i will be able to list the newly installed
    documentation. Note that you might need to kill (C-x k) your
    info buffer then C-h i again to be able to see the new menu
    entry.

:load

    List of files to load, or a single file to load after having
    installed the source but before `require'ing its features.

:features

    List of features el-get will `require' for you.

:autoloads

    Control whether el-get should generate autoloads for this
    package. Setting this to nil prevents el-get from generating
    autoloads for the package. Default is t. Setting this to a
    string or a list of string will load the named autoload
    files.

:library

    When lazy, :library sets the file against which to register
    the :after and :post-init forms for `eval-after-load'.  It
    defaults to the first :feature, :pkgname (removing the
    \"username\" part for github and emacsmirror) or :package,
    in that order.  See also `el-get-eval-after-load'.

:options

    Currently used by http-tar and cvs support.

    When using http-tar, it allows you to give the tar options
    you want to use. Typically would be \"xzf\", but you might
    want to choose \"xjf\" for handling .tar.bz files e.g.

    When using CVS, when it's set to \"login\", `el-get' will
    first issue a `cvs login' against the server, asking you
    interactively (in the minibuffer) any password you might to
    enter, and only then it will run the `cvs checkout' command.

:module

    Currently only used by the `cvs' support, allow you to
    configure the module you want to checkout in the given URL.

:repo

    Only used by the `elpa' support, a cons cell with the
    form (NAME . URL), as in `package-archives'.  If the package
    source only specifies a URL, the URL will be used for NAME as
    well.

:prepare

    This should be a lisp form to evaluate after both the
    `Info-directory-list' and the `load-path' variables have been
    taken care of, but before loading the package or any further
    action from `el-get-init'.  It will be run with
    `default-directory' set to the package directory.

:before

    This exactly like `:prepare' property, but is reserved for
    user customizations in `el-get-sources'.  Recipe files should
    not use this property.  It will be run just after `:prepare'.

:post-init

    Intended for use from recipes.  This should be a lisp form to
    evaluate once the :load and :features have been setup.  When
    lazy, it will be registered for `eval-after-load' against the
    recipe :library instead.  Like `:prepare', it will be run
    with `default-directory' set to the package directory.

:after

    This exactly like the `:post-init' property, but is reserved
    for user customizations in `el-get-sources'.  Recipe files
    should not use this property.  It will be run just after
    `:post-init' and after any per-package user-init-file (see
    `el-get-user-package-directory').

:lazy

    Default to nil.  Allows to override `el-get-is-lazy' per
    package.

:localname

    Currently only used by both `http' and `ftp' supports, allows
    to specify the target name of the downloaded file.

    This option is useful if the package should be retrieved using
    a presentation interface (such as as web SCM tool).

    For example, destination should be set to \"package.el\" if
    the package url has the following scheme:

   \"http://www.example.com/show-as-text?file=path/package.el\"

:website

    The website of the project.

:description

    A short description of the project.

:checksum

    Some methods in el-get download files that are not so
    trusted (e.g. anyone is allowed to modify emacswiki anytime).
    For these cases you can set up a checksum value for a
    package, so you will notice if el-get tries to install a
    version that you haven't seen before.  The installation will
    simply fail and you will see the good and bad checksum values
    in `*Messages*'.  To get the checksum value for a package
    initially, install the package while `el-get-verbose' is on.

    Checksum calculation is currently supported by these methods
    with the following meaning:

      * `http', `ftp' and `emacswiki' with the SHA1 of the downloaded file
      * `git' and `hg' in which it is an alias for `:checkout' (see below)

    In the `git' and `hg' recipe types, note that while
    `:checkout' will accept other things like branches, tags, and
    so forth,`:checksum' must only be given a commit hash. This
    will effectively lock the repository at a particular
    revision. Note also that `:checksum' must be set to the full
    hash, not just the first 6 or 8 characters (this restriction
    may be removed in the future).

:checkout

    For `git' or `hg' recipes, a branch name, tag, or other valid
    argument to `git checkout'/`hg update'. If provided, this
    overrides any value for the `:branch' property. Unlike the
    `:branch' property, this can be any valid argument to
    `checkout', including a tag name or a commit hash.

    When given a commit hash or tag, this will lock the repo at
    that specific revision. When given a branch name, this will
    always update the repo to the latest version of that branch,
    regardless of any changes made locally (in contrast to simply
    using the `:branch' property, which will cause el-get-to try
    to merge your local changes).

    Currently this property only has meaning for `git' and `hg'
    type recipes. Other VCS-based methods may implement support
    in the future.

:shallow

    If set to t in a git recipe, git-clone will be run with
    `--depth 1', which will create a so-called shallow clone by
    not downloading all the history of the repository. The
    default is controlled by the variable
    `el-get-git-shallow-clone', which is nil by default.

:submodule

   If set to nil in a git recipe, submodules will not be updated.

"
  :group 'el-get
  :type
  `(repeat
    (choice
     :tag "Entry"
     :value (:name "")
     (el-get-symbol :tag "Name of EL-Get Package")
     (list
      :tag "Full Recipe (or Recipe Override)"
      (group :inline t :tag "EL-Get Package Name" :format "%t: %v"
             (const :format "" :name) (el-get-symbol :format "%v"))
      (set
       :inline t :format "%v\n"
       (group
        :inline t  (const :format "" :depends)
        (el-get-repeat
         :tag "Names of packages on which this one depends" el-get-symbol))
       (group
        :inline t :format "%t: %v%h"
        :tag "Underlying Package Name"
        :doc "When there is an underlying package manager (e.g. `apt')
this is the name to fetch in that system"
        (const :format "" :pkgname) (string :format "%v"))

       (group
        :inline t :tag "Type" :format "%t: %v%h"
        :doc "(If omitted, this recipe provides overrides for one in recipes/)"
        (const :format "" :type)
        ,(append '(choice :value emacswiki :format "%[Value Menu%] %v"
                          )
                 ;; A sorted list of method names
                 (sort
                  (reduce
                   (lambda (r e)
                     (if (symbolp e)
                         (cons
                          (list 'const
                                (intern (substring (el-get-print-to-string e) 1)))
                          r)
                       r))
                   el-get-methods
                   :initial-value nil)
                  (lambda (x y)
                    (string< (el-get-print-to-string (cadr x))
                             (el-get-print-to-string (cadr y)))))))

       (group :inline t :format "Source URL: %v"
              (const :format "" :url) (string :format "%v"))
       (group :inline t :format "Package Website: %v"
              (const :format "" :website) (string :format "%v"))
       (group :inline t :format "Description: %v"
              (const :format "" :description) (string :format "%v"))
       (group :inline t :format "General Build Recipe\n%v"
              (const :format "" :build) ,el-get-build-recipe-body)
       (group :inline t  (const :format "" :load-path)
              (el-get-repeat
               :tag "Subdirectories to add to load-path" directory))
       (group :inline t  (const :format "" :compile)
              (el-get-repeat
               :tag "File/directory regexps to compile" regexp))
       (group :inline t :format "%v" (const :format "" :info)
              (string :tag "Path to .info file or to its directory"))
       (group :inline t (const :format "" :load)
              (el-get-repeat :tag "Relative paths to force-load" string))
       (group :inline t :format "%v" (const :format "" :features)
              (repeat :tag "Features to `require'" el-get-symbol))
       (group :inline t :format "Autoloads: %v" :value (:autoloads t)
              (const :format "" :autoloads)
              (choice
               :tag "Type"
               (boolean :format "generation %[Toggle%] %v\n")
               (el-get-repeat
                :tag "Relative paths to force-load" string)))
       (group :inline t :format "Options (`http-tar' and `cvs' only): %v"
              (const :format "" :options) (string :format "%v"))
       (group
        :inline t :format "%t: %v%h"
        :tag "Checksum"
        :doc "Used to verify downloaded package
 (SHA1 in hex for `http', `ftp' and `emacswiki')"
        (const :format "" :checksum) (string :format "%v"))
       (group
        :inline t :format "%t: %v"
        :tag "Checkout this `git' revision"
        (const :format "" :checkout) (string :format "%v"))
       (group
        :inline t :format "%t: %v%h"
        :tag "Shallow clone"
        :doc "git-clone with `--depth 1'"
        (const :format "" :shallow) (boolean :format "%[Toggle%] %v\n"))
       (group
        :inline t :format "%t: %v" :value (:submodule t)
        :tag "Update submodules (`git' only)"
        (const :format "" :submodule) (boolean :format "%[Toggle%] %v\n"))
       (group :inline t :format "CVS Module: %v"
              (const :format "" :module)
              (string :format "%v"))
       (group :inline t :format "`Prepare' Function: %v"
              (const :format "" :prepare) (function :format "%v"))
       (group :inline t :format "`Post-Init' Function: %v"
              (const :format "" :post-init) (function :format "%v"))
       (group :inline t
              :format "Name of downloaded file (`http' and `ftp' only): %v"
              (const :format "" :localname) (string :format "%v"))
       (group :inline t :format "Lazy: %v"  :value (:lazy t)
              (const :format "" :lazy) (boolean :format "%[Toggle%] %v\n"))
       (group :inline t
              :format "Repository specification (`elpa' only): %v"
              (const :format "" :repo)
              (cons :format "\n%v"
                    (string :tag "Name")
                    (string :tag "URL")))
       (group :inline t
              :format "`Before' Function (`Prepare' recommended instead): %v"
              (const :format "" :before) (function :format "%v"))
       (group :inline t
              :format "`After' Function (`Post-Init' recommended instead): %v"
              (const :format "" :after) (function :format "%v"))
       )
      (repeat
       :inline t :tag "System-Specific Build Recipes"
       (group :inline t
              (symbol :value ,(concat ":build/"
                                      (el-get-print-to-string system-type))
                      :format "Build Tag: %v%h"
                      :doc "Must be of the form `:build/<system-type>',
where `<system-type>' is the value of `system-type' on
platforms where this recipe should apply"
                      )
              ,el-get-build-recipe-body))))))

;; TODO: this should be nil; change at the next major version bump
(defcustom el-get-allow-insecure t
  "Allow packages to be installed over insecure connections.

See `el-get-insecure-check'."
  :group 'el-get
  :type 'boolean)

(defcustom el-get-secure-protocols '("https" "ssh" "git+ssh" "bzr+ssh" "sftp")
  "List of secure protocols.

See `el-get-insecure-check'."
  :group 'el-get
  :type '(repeat string))

(provide 'el-get-custom)
