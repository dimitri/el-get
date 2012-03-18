;; https://github.com/dimitri/el-get/issues/652
;;
;; Handle changing dependencies in `el-get-update'

(setq el-get-default-process-sync t
      el-get-verbose t
      el-get-sources
      '((:name a :type builtin :depends
               (b c d))
        (:name b :type builtin)
        (:name c :type builtin)
        (:name d :type builtin)
        (:name e :type builtin)
        (:name f :type builtin)
        (:name g :type builtin)))

;; Ensure a is uninstalled
(ignore-errors (el-get-remove 'a))
;; Install a and some deps
(el-get-install 'a)
;; Make sure deps got installed
(assert (el-get-package-is-installed 'd) nil
        "Package D should be installed after installing A.")
;; Add some more deps
(setf (car el-get-sources)
      '(:name a
              :type builtin
              :depends (b c d e f g)))
;; Run update with the new dependencies
(el-get-update 'a)
(assert (el-get-package-is-installed 'g) nil
        "Package G should be installed after updating A.")
