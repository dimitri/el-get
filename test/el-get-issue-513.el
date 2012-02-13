;; https://github.com/dimitri/el-get/issues/513
;;
;; Testing the github and emacsmirror methods

(let ((debug-on-error t)
      ;; (el-get-byte-compile nil)
      (el-get-verbose t))
  (require 'el-get)
  ;; Install a github-type recipe
  (el-get 'sync 'window-layout)
  ;; Install an emacsmirror-type recipe
  (el-get 'sync 'dired-plus))
