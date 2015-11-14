;;; When installing a package whose name is a prefix of an already
;;; installed package, `el-get-elpa-package-directory' gets confused.

(setq el-get-sources
      '((:name load-dir ; I tried to pick the smallest package
               :type elpa
               :description "Load all Emacs Lisp files in a given directory"
               :repo ("gnu" . "http://elpa.gnu.org/packages/"))))

(el-get 'sync 'load-dir)

(assert (not (equal (el-get-elpa-package-directory 'load)
                    (el-get-elpa-package-directory 'load-dir)))
        nil
        "a package name `load' shouldn't use the same directory
as `load-dir'.")
