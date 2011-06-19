(:name apel
       :type cvs
       :module "apel"
       :url ":pserver:anonymous@cvs.m17n.org:/cvs/root"
       :build 
        (mapcar
         (lambda (target)
           (list el-get-emacs
                 (split-string "-batch -q -no-site-file -l APEL-MK -f")
                 target
                 "prefix" "site-lisp" "site-lisp"))
         '("compile-apel" "install-apel"))
        :load-path ("site-lisp/apel" "site-lisp/emu"))

