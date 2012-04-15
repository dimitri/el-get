;; https://github.com/dimitri/el-get/issues/200
;;
;; yasnippet recipe fails to install

(setq debug-on-error t
      el-get-verbose t)

(el-get 'sync 'yasnippet)
