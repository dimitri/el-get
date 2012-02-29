;; https://github.com/dimitri/el-get/issues/613
;;
;; Do recipe autoloads in :prepare instead of :post-init
(let ((debug-on-error t)
      ;; (el-get-byte-compile nil)
      (el-get-default-process-sync t)
      (el-get-verbose t)
      (el-get-is-lazy t))
  (el-get 'sync 'n3-mode)
  (assert (not (featurep 'n3-mode)) nil.
          "n3-mode should not be loaded because el-get is lazy")
  (assert (functionp 'n3-mode) nil
          "n3-mode function should be defined because it is autoloaded")
  (assert (equal 'autoload (car (symbol-function 'n3-mode))) nil
          "n3-mode function definition should be an autoload"))
