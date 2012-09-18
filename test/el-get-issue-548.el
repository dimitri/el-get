;; https://github.com/dimitri/el-get/issues/548
;;
;; Use `default-directory' in :post-init and similar recipe init hooks.

(setq debug-on-error t
      el-get-verbose t)

(el-get 'sync 'pcmpl-git)
(el-get-init 'pcmpl-git)
;; Verify that the option in :post-init was set correctly
(assert (string= pcmpl-git-options-file
                 (expand-file-name "git-options" (el-get-package-directory 'pcmpl-git))))
