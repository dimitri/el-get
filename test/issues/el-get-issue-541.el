;; https://github.com/dimitri/el-get/issues/541
;;
;; Pass load-path to byte-compiling child process

(setq debug-on-error t
      el-get-verbose t)

(el-get 'sync 'js2-mode)

(setq compiled-file
      (concat
       (file-name-as-directory
        (el-get-package-directory 'js2-mode))
       "js2-mode.elc"))

(if (file-exists-p compiled-file)
    (message "Byte-compiling succeeded.")
  (error "Byte-compiling failed."))
