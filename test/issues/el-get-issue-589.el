;; https://github.com/dimitri/el-get/issues/589
;;
;; Lazy loading is broken

(setq debug-on-error t
      el-get-default-process-sync t
      el-get-verbose t
      el-get-is-lazy t
      post-init-function-ran nil
      prepare-function-ran nil
      el-get-sources
      '((:name test-pkg :type builtin :features ido :prepare
               (setq prepare-function-ran t)
               :post-init
               (setq post-init-function-ran t)
               :lazy t)))

(assert (not post-init-function-ran) nil
        "Post-init function should not run before installation")
(el-get 'sync 'test-pkg)
(assert prepare-function-ran nil
        "Prepare function should have run after package installation.")
(assert (not post-init-function-ran) nil
        "Post-init function should not run during installation")
(el-get-init 'test-pkg)
(assert (not post-init-function-ran) nil
        "Post-init function should not run during init")
(require 'ido)
(assert post-init-function-ran nil
        "Post-init function should have run when package feature was required")
