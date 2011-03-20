(:name dvc
  :type bzr
  :url "http://bzr.xsteve.at/dvc/"
  :build ("autoconf" "./configure" "make")
  :build/windows-nt ("sh /usr/bin/autoconf" "sh ./configure" "make")
  :features dvc-autoloads
  :info "texinfo"
  :load-path ("./lisp"))
