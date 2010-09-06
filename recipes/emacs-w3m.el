(name :emacs-w3m
      :type cvs
      :module "emacs-w3m"
      :options "login"
      :url ":pserver:anonymous@cvs.namazu.org:/storage/cvsroot"
      :build ("autoconf" "./configure" "make")
      :info "doc"
      :features "w3m-load")
