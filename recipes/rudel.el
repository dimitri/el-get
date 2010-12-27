(:name rudel
       :type bzr
       :url "bzr://rudel.bzr.sourceforge.net/bzrroot/rudel/trunk"
       :load "rudel-loaddefs.el"
       :build ("emacs --script rudel-compile.el"))