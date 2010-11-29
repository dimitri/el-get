(:name apel
       :type cvs
       :module "apel"
       :url ":pserver:anonymous@cvs.m17n.org:/cvs/root"
       :build `(,(concat invocation-directory invocation-name
                         " -batch -q -no-site-file -l APEL-MK -f compile-apel"
                         " NONE NONE NONE"))
       )
