;; https://github.com/dimitri/el-get/issues/303
;;
;; error in process sentinel

(setq debug-on-error t
      debug-ignored-errors 'nil
      el-get-verbose t
      el-get-default-process-sync t
      el-get-sources
      '((:name xmlunicode
               :depends (unichars)
               :type http
               :url "http://nwalsh.com/emacs/xmlchars/xmlunicode.el")
        (:name unichars
               :type http
               :url "http://nwalsh.com/emacs/xmlchars/unichars.el")))

(el-get-install "xmlunicode")
