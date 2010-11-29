(:name cedet
  :type bzr
  :url "bzr://cedet.bzr.sourceforge.net/bzrroot/cedet/code/trunk"
  :build ("touch `find . -name Makefile`" "make")
  :load-path ("./common"))
