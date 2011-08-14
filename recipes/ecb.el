(:name ecb
       :description "Emacs Code Browser"
       :type cvs
       :module "ecb"
       :url ":pserver:anonymous@ecb.cvs.sourceforge.net:/cvsroot/ecb"
       :build `(,(concat  "make CEDET=" " EMACS=" el-get-emacs)))
