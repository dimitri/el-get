(:name scala-mode
       :type svn
       :url "http://lampsvn.epfl.ch/svn-repos/scala/scala-tool-support/trunk/src/emacs/"
       :build ("make")
       :load-path (".")
       :features scala-mode-auto)