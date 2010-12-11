(:name semi
       :type cvs
       :module "semi"
       :url ":pserver:anonymous@cvs.m17n.org:/cvs/root"
       :build `(,(concat (executable-find "cvs") " up -r semi-1_14-wl")
                ,(concat el-get-emacs
                         " -L " (file-name-as-directory "..") "apel"
                         " -L " (file-name-as-directory "..") "flim"
                         " -batch -q -no-site-file -l SEMI-MK -f compile-semi"
                         " NONE NONE NONE"))
       )

