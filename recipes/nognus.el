(:name nognus
       :type git
       :url "http://git.gnus.org/gnus.git"
       :build ("./configure" "make")
       :build/darwin ("./configure --with-emacs=/Applications/Emacs.app/Contents/MacOS/Emacs" "make")
       :info "texi"
       :load-path ("lisp")
       :features gnus-load)
