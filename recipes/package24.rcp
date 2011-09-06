;; Same as "package" except that it takes the version from Emacs 24
(:name package24
       :description "Same as \"package\" except that it takes the version from Emacs 24"
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
            (unless (boundp 'package-subdirectory-regexp)
              (defconst package-subdirectory-regexp
            "^\\([^.].*\\)-\\([0-9]+\\(?:[.][0-9]+\\)*\\)$"
            "Regular expression matching the name of
 a package subdirectory. The first subexpression is the package
 name. The second subexpression is the version string."))
            (setq
             package-archives
             '(("ELPA" . "http://tromey.com/elpa/")
               ("gnu" . "http://elpa.gnu.org/packages/")
               ("marmalade" . "http://marmalade-repo.org/packages")))))

            ;; Don't init, elpa packages installed by el-get is
            ;; initialized from loaddefs (package-initialize)
