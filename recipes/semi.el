(:name semi
       :type cvs
       :module "semi"
       :url ":pserver:anonymous@cvs.m17n.org:/cvs/root"
       :build 
       (append
        `((,(executable-find "cvs") ,(split-string "up -r semi-1_14-wl")))
        (mapcar
         (lambda (target) 
           (list el-get-emacs 
                 (mapcar (lambda (pkg) 
                           (mapcar (lambda (d) `("-L" ,d)) (el-get-load-path pkg)))
                         '("apel" "flim"))

                 (split-string "-batch -q -no-site-file -l SEMI-MK -f")
                 target
                 "prefix" "NONE" "NONE"))
         '("compile-semi" "install-semi"))))

