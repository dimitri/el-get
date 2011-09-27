;; https://github.com/dimitri/el-get/issues/200
;;
;; yasnippet recipe fails to install

(let ((debug-on-error t)
      ;; (el-get-byte-compile nil)
      (el-get-verbose t))
  (require 'el-get)
  (el-get 'sync 'yasnippet))

