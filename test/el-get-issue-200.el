;; https://github.com/dimitri/el-get/issues/200
;;
;; yasnippet recipe fails to install

(let ((debug-on-error t)
      ;; (el-get-byte-compile nil)
      (el-get-verbose t))
  (add-to-list 'load-path "/Users/dim/dev/emacs/el-get")
  (require 'el-get)
  (el-get 'sync 'yasnippet))

