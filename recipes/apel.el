(:name apel
       :type cvs
       :module "apel"
       :url ":pserver:anonymous@cvs.m17n.org:/cvs/root"
       :build `(,(concat el-get-emacs
                         " -batch -q -no-site-file -l APEL-MK -f compile-apel"
                         " NONE NONE NONE"))
       )
