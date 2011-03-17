;; Same as "package" except that it takes the version from Emacs 24
(:name package24
       :type http
       :url "http://repo.or.cz/w/emacs.git/blob_plain/HEAD:/lisp/emacs-lisp/package.el"
       :features package
       :post-init (lambda ()
		    (setq package-user-dir
			  (expand-file-name
			   (convert-standard-filename
			    (concat (file-name-as-directory
				     (el-get-package-directory "package24"))
				    "elpa")))
			  package-directory-list
			  (list (file-name-as-directory package-user-dir)
				"/usr/share/emacs/site-lisp/elpa/"))
		    (make-directory package-user-dir t)
		    (package-initialize)))
