(:name emacs-w3m
       :description "A simple Emacs interface to w3m"
       :type cvs
       :module "emacs-w3m"
       :url ":pserver:anonymous@cvs.namazu.org:/storage/cvsroot"
       :build `("autoconf" ("./configure" ,(concat "--with-emacs=" el-get-emacs)) "make")
       :build/windows-nt ("sh /usr/bin/autoconf" "sh ./configure" "make")
       :info "doc")
