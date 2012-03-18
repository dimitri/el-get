;;
;; https://github.com/dimitri/el-get/issues/284
;;
;; set HOME to an empty directory, launch emacs and evaluate the
;; following. It fails because initsplit has not been added to the load
;; path.

(setq debug-on-error t)

(setq el-get-byte-compile nil
      el-get-verbose t)
(el-get 'sync '(initsplit))
(find-library "initsplit")
