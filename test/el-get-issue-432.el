;; https://github.com/dimitri/el-get/issues/432
;;
;; Real-life example: cperl-mode depends on mode-compile. Try installing
;; mode-compile first, then cperl-mode.

(let ((debug-on-error t)
      (debug-ignored-errors '())
      (el-get-default-process-sync t)
      (el-get-verbose t)
      (el-get-byte-compile-at-init t))
  (require 'el-get)
  (message "%S" (el-get-dependencies 'cperl-mode))
  (el-get-install "mode-compile")
  (el-get-install "cperl-mode"))
