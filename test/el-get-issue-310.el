;; https://github.com/dimitri/el-get/issues/310
;;
;; el-get refuses to compile

(setq debug-on-error t
      debug-ignored-errors 'nil)

(setq el-get-default-process-sync t
      el-get-verbose t)
(el-get-install "mailq")
