;; https://github.com/dimitri/el-get/issues/581
;;
;; ipython should depend on python-mode

(setq debug-on-error t
      el-get-verbose t)

(el-get 'sync 'ipython)
