;; https://github.com/dimitri/el-get/issues/535
;;
;; CVS proxy support

;; Run this test with the environment variable HTTP_PROXY set
;; appropriately!

(setq debug-on-error t
      el-get-verbose t)

(require 'el-get)
(el-get 'sync 'mailcrypt)
