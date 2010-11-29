(:name flim
       :type cvs
       :module "flim"
       :url ":pserver:anonymous@cvs.m17n.org:/cvs/root"
       :build `(,(concat (executable-find "cvs") " up -r flim-1_14-wl")
                ,(concat invocation-directory invocation-name
                         " -L " (file-name-as-directory "..") "apel"
                         " -batch -q -no-site-file -l FLIM-MK -f compile-flim"
                         " NONE NONE NONE"))
       )
