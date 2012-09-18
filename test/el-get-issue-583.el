;; https://github.com/dimitri/el-get/issues/583
;;
;; Installing, removing, and installing a package doesn't
;;
;; Also related: https://github.com/dimitri/el-get/issues/576

(setq el-get-default-process-sync t
      el-get-verbose t
      el-get-sources
      '((:name a :type builtin :depends
               (b c d e f))
        (:name b :type builtin)
        (:name c :type builtin)
        (:name d :type builtin)
        (:name e :type builtin)
        (:name f :type builtin)))

;; Ensure a is uninstalled
(ignore-errors (el-get-remove 'a))
;; Install a and all deps
(el-get-install 'a)
;; Remove a, leaving deps installed
(el-get-remove 'a)
;; Try to install a again, this fails and only inits b and c.
(el-get-install 'a)
(assert (el-get-package-is-installed 'a) nil
        "Package A should be installed but isn't.")
