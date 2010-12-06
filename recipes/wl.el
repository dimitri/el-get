(:name wl
       :type cvs 
       :module "wanderlust" 
       :url ":pserver:anonymous@cvs.m17n.org:/cvs/root" 
       :build (mapcar
               (lambda (target-and-dirs) 
                 (list el-get-emacs 
                       (mapcar (lambda (pkg) 
                                 (mapcar (lambda (d) `("-L" ,d)) (el-get-load-path pkg)))
                               '("apel" "flim" "semi"))

                       "--eval" (prin1-to-string 
                                 '(progn (setq wl-install-utils t)
                                         (setq wl-info-lang "en")
                                         (setq wl-news-lang "en")))

                       (split-string "-batch -q -no-site-file -l WL-MK -f")
                       target-and-dirs
                       ))
               '(("wl-texinfo-format" "doc")
                 ("compile-wl-package"  "site-lisp" "icons") 
                 ("install-wl-package" "site-lisp" "icons")))
       :info "doc"
       :load-path ("site-lisp/wl")
       )
