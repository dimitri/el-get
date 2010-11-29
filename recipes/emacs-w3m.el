(name :emacs-w3m
      :type cvs
      :module "emacs-w3m"
      :url ":pserver:anonymous@cvs.namazu.org:/storage/cvsroot"
      :build ("autoconf" "./configure" "make")
      :build/darwin `("autoconf" ,(concat "./configure --with-emacs=" invocation-directory invocation-name) "make")
      :info "doc"
      :features "w3m-load")
