;;; https://github.com/dimitri/el-get/issues/1472
;;; (el-get) fails when installed packages acquire dependencies that
;;; have not been installed.

(el-get-register-method-alias :test :builtin)

;;; install a
(setq el-get-sources
      '((:name a :type test)))

(el-get 'sync "a")

;;; make a depend on b
(setq el-get-sources
      '((:name a :type test :depends (b))
        (:name b :type test)))

;;; init a, now requires b!
(el-get 'sync)
