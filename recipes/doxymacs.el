(:name doxymacs
       :type git
       :url "git://doxymacs.git.sourceforge.net/gitroot/doxymacs/doxymacs"
       :load-path ("./lisp")
       :build ("./bootstrap" "./configure" "make")
       :features doxymacs
       )

