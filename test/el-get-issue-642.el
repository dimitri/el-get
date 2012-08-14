;; https://github.com/dimitri/el-get/issues/642
;;
;; Reinstall on type change

(setq debug-on-error t
      el-get-verbose t
      el-get-default-process-sync t)

;; Install pkg with type builtin
(let ((el-get-sources
       (list `(:name pkg
                     :type builtin))))
  (el-get 'sync 'pkg))
(assert (eq 'builtin
            (el-get-package-method (el-get-read-package-status-recipe 'pkg)))
        t
        "Package type should be 'builtin.")
;; Even though "no-op" is an alias for the same behavior as
;; "builtin", they are still considered different types. Thus, this
;; should trigger a reinstall.
(let ((el-get-sources
       (list `(:name pkg
                     :type no-op))))
  (el-get-update 'pkg))
(assert (eq 'no-op
            (el-get-package-method (el-get-read-package-status-recipe 'pkg)))
        t
        "Package type should now be 'no-op, not 'builtin.")
