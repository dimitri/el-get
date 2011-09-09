(:name package
       :description "ELPA(Emacs Lisp Package Archive)"
       :type http
       :url "http://tromey.com/elpa/package.el"
       :features package
       :post-init (lambda ()
            (setq package-user-dir 
              (expand-file-name 
               (convert-standard-filename 
                (concat (file-name-as-directory 
                     (el-get-package-directory "package")) 
                    "elpa")))
              package-directory-list 
              (list (file-name-as-directory package-user-dir) 
                "/usr/share/emacs/site-lisp/elpa/"))
            (make-directory package-user-dir t)
            (package-initialize)))
