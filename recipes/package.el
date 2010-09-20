(:name package
       :type http
       :url "http://tromey.com/elpa/package.el"
       :features package
       :after (lambda ()
                (make-directory (expand-file-name "~/.emacs.d/elpa") t)
                (package-initialize)))
