(:name cedet
  :type cvs
  :module "cedet"
  :url ":pserver:anonymous@cedet.cvs.sourceforge.net:/cvsroot/cedet"
  :build ("touch `find . -name Makefile`" "make")
  :load-path ("./common"))
