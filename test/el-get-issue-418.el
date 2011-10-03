;; https://github.com/dimitri/el-get/issues/418
;;
;; info installation broken

(let ((debug-on-error t)
      (el-get-default-process-sync t)
      (el-get-verbose t)
      (debug-ignored-errors '()))
  (require 'el-get)
  (el-get 'sync '(el-get magit)))
