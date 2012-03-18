;; https://github.com/dimitri/el-get/issues/596
;;
;; package.el fails to install

(setq debug-on-error t
      el-get-verbose t)

(el-get 'sync 'package)
