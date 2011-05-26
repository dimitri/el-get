(:name flim
       :depends apel
       :type cvs
       :module "flim"
       :url ":pserver:anonymous@cvs.m17n.org:/cvs/root"
       :build 
       (append
        `((,(executable-find "cvs") ,(split-string "up -r flim-1_14-wl")))
        (mapcar
         (lambda (target) 
           (list el-get-emacs 
                 (mapcar (lambda (pkg) 
                           (mapcar (lambda (d) `("-L" ,d)) (el-get-load-path pkg)))
                         '("apel"))

                 (split-string "-batch -q -no-site-file -l FLIM-MK -f")
                 target
                 "prefix" "site-lisp" "site-lisp"))
         '("compile-flim" "install-flim")))
       :load-path ("site-lisp/flim"))

