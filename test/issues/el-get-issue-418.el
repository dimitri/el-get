;; https://github.com/dimitri/el-get/issues/418
;;
;; info installation broken

(setq debug-on-error t
      el-get-default-process-sync t
      el-get-verbose t
      debug-ignored-errors 'nil)

(el-get 'sync 'magit)
(message "%S" (symbol-function 'magit-status))
