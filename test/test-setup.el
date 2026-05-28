(require 'debug)
(require 'el-get)

(setq debugger-batch-max-lines (+ 50 max-lisp-eval-depth)
      debug-on-error t
      el-get-verbose t
      el-get-notify-type 'message)

(defun el-get-test-rcp-life-cycle (&optional recipe-file)
  "Test out RECIPE-FILE.

It is tested by installing, updating, removing, and then
installing again."
  ;; RECIPE-FILE is &optional only to allow easier batch calling.
  (declare (advertised-calling-convention (recipe-file) "Always"))
  (when noninteractive
    (setq recipe-file (pop command-line-args-left)))
  (cl-assert (file-exists-p recipe-file) nil
             "No recipe file: %s" recipe-file)
  (setq debug-on-error (not noninteractive)
        el-get-default-process-sync t
        pdef (el-get-read-from-file recipe-file)
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
  (cl-assert (el-get-package-is-installed pname) nil
             "Package %s should be installed right now but isn't" pname))
