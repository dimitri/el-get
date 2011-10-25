;; https://github.com/dimitri/el-get/issues/310
;;
;; el-get refuses to compile

(let ((debug-on-error t)
      (debug-ignored-errors '()))
  (add-to-list 'load-path "/Users/dim/dev/emacs/el-get")
  (require 'el-get)
  (setq el-get-default-process-sync t
	el-get-verbose t)
  (el-get-install "mailq"))
