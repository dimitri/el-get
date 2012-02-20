;; https://github.com/dimitri/el-get/issues/581
;;
;; ipython should depend on python-mode

(let ((debug-on-error t)
      ;; (el-get-byte-compile nil)
      (el-get-verbose t))
  (require 'el-get)
  (el-get 'sync 'ipython))
