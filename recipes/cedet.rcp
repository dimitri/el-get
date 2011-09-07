(:name cedet
  :website "http://cedet.sourceforge.net/"
  :description "CEDET is a Collection of Emacs Development Environment Tools written with the end goal of creating an advanced development environment in Emacs."
  :type bzr
  :url "bzr://cedet.bzr.sourceforge.net/bzrroot/cedet/code/trunk"
  :build ("touch `find . -name Makefile`" "make")
  :build/windows-nt ("echo #!/bin/sh > tmp.sh & echo touch `/usr/bin/find . -name Makefile` >> tmp.sh & echo make FIND=/usr/bin/find >> tmp.sh" 
		     "sed 's/^M$//' tmp.sh  > tmp2.sh"
		     "sh ./tmp2.sh" "rm ./tmp.sh ./tmp2.sh")
  :load-path ("./common" "speedbar"))
