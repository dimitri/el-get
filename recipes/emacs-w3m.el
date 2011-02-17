(:name emacs-w3m
      :type cvs
      :module "emacs-w3m"
      :url ":pserver:anonymous@cvs.namazu.org:/storage/cvsroot"
      :build `("autoconf" ("./configure" ,(concat "--with-emacs=" el-get-emacs)) "make")
      :info "doc")
