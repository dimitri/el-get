;; https://github.com/dimitri/el-get/issues/613
;;
;; Do recipe autoloads in :prepare instead of :post-init
(require 'loadhist)

(setq debug-on-error t
      el-get-default-process-sync t
      el-get-verbose t
      el-get-is-lazy t)

(el-get 'sync 'n3-mode)
(assert (not (file-loadhist-lookup "n3-mode")) nil
        "n3-mode package should not be loaded because el-get is lazy")
(assert (functionp 'n3-mode) nil
        "n3-mode function should be defined because it is autoloaded")
(assert (equal 'autoload (car (symbol-function 'n3-mode))) nil
        "n3-mode function definition should be an autoload")
(with-temp-buffer (n3-mode))
(assert (file-loadhist-lookup "n3-mode") nil
        "n3-mode package should be loaded after calling n3-mode function")
(assert (not (equal 'autoload (car-safe (symbol-function 'n3-mode)))) nil
        "n3-mode function should no longer be an autoload after calling it")
