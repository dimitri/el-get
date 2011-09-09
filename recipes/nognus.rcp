(:name nognus
       :description "A newsreader for GNU Emacs"
       :type git
       :url "http://git.gnus.org/gnus.git"
       :build ("./configure" "make")
       :build/windows-nt `(,(concat "\"make.bat " invocation-directory "\""))
       :build/darwin `(,(concat "./configure --with-emacs=" el-get-emacs) "make")
       :info "texi"
       :load-path ("lisp")
       :autoloads nil
       :features gnus-load)
