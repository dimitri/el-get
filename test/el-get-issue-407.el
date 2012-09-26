;; https://github.com/dimitri/el-get/issues/407
;;
;; use url-retrieve-synchronously in init file
;;
;; Drop the -L . option from the README.asciidoc for this very test.

(require 'cl)
;; Unload el-get and delete it from the load path
(when (require 'el-get nil t)
  ;; Remove from load-path
  (setq load-path
        (remove-if (apply-partially
                    #'string-prefix-p
                    (expand-file-name
                     "."
                     (file-name-directory
                      (symbol-file 'el-get 'defun))))
                   load-path))
  ;; Unload features
  (loop for feat in features
        if (string-prefix-p "el-get" (symbol-name feat))
        do (unload-feature feat 'force))
  (assert (not (require 'el-get nil t)) nil
          "el-get should not be loadable now"))

(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (end-of-buffer)
    (eval-print-last-sexp))
  (message "el-get is ready now"))

;; Make sure el-get is now available
(require 'el-get)
