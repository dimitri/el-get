;; https://github.com/dimitri/el-get/issues/176
;;
;; Git clone should be done with the depth flag

(let ((debug-on-error t)
      ;; (el-get-byte-compile nil)
      (el-get-verbose t)
      (el-get-git-shallow-clone t))
  (require 'el-get)
  (el-get 'sync 'yasnippet))

;; After running the test, check the git log for yasnippet to see if
;; it only has a few log entries, rather than hundreds.
