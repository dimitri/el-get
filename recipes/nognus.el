(:name nognus
       :type git
       :url "http://git.gnus.org/gnus.git"
       :build ("./configure" "make")
       :info "texi"
       :load-path ("lisp" "contrib")
       :features gnus-load)