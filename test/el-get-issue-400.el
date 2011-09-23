;; https://github.com/dimitri/el-get/issues/400
;;
;; el-get-install does not run the init hooks

(let ((debug-on-error t)
      (el-get-verbose t))
      ;; (el-get-sources '((:name php-mode :features php-mode))))
  (require 'el-get)
  (el-get 'sync 'php-mode)
  (message "auto-mode-alist: %S" (rassoc 'php-mode auto-mode-alist))
  (with-current-buffer (find-file "foo.php")
    (message "%s: %s" (buffer-file-name) major-mode)))
