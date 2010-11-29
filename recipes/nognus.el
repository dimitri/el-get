(:name nognus
       :type git
       :url "http://git.gnus.org/gnus.git"
       :build ("./configure" "make")
       :build/darwin `(,(concat "./configure --with-emacs=" el-get-emacs) "make")
       :info "texi"
       :load-path ("lisp")
       :features gnus-load)
