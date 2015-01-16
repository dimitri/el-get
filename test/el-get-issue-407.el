;; https://github.com/dimitri/el-get/issues/407
;;
;; use url-retrieve-synchronously in init file
;;
;; Drop the -L . option from the README.md for this very test.
;;
;;
;; https://github.com/dimitri/el-get/issues/1457
;;
;; The basic setup installer should make el-get available immediately.


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
        do (progn
             (unload-feature feat 'force)
             (assert (not (require feat nil t)) Nil
                     "%s should not be loadable now" feat)))
  (assert (not (or (require 'el-get nil t)
                   (boundp 'el-get-sources))) nil
          "el-get should not be loadable now"))

(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-install-skip-emacswiki-recipes)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (message "el-get is ready now"))

(assert (and (featurep 'el-get)
             (boundp 'el-get-sources)) nil "el-get MUST have been loaded!")
