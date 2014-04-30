;; https://github.com/dimitri/el-get/issues/1454
;;
;; Bootstrapping package.el when installing it and then an ELPA recipe

(setq debug-on-error t
      el-get-verbose t
      el-get-is-lazy t)

(defadvice require (after require-package-verbosely activate)
  (when (and (eq feature 'package)
             (featurep 'package))
    (message "LOADED PACKAGE.EL")))

(let ((elpa-requiring-pkg 'svg-clock))
  (el-get 'sync (list 'package elpa-requiring-pkg))
  (assert (el-get-package-installed-p 'package))
  (assert (featurep 'package))
  (assert (el-get-package-installed-p elpa-requiring-pkg)))
