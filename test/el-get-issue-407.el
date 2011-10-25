;; https://github.com/dimitri/el-get/issues/400
;;
;; use url-retrieve-synchronously in init file
;;
;; Drop the -L . option from the README.asciidoc for this very test.

(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (end-of-buffer)
    (eval-print-last-sexp))
  (message "el-get is ready now"))
