(:name dvc
  :type bzr
  :url "http://bzr.xsteve.at/dvc/"
  :build ("autoconf" "./configure" "make")
  :features dvc-autoloads
  :info "texinfo"
  :load-path ("./lisp"))
