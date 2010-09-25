(:name bbdb
       :type git
       :url "git://github.com/barak/BBDB.git"
       :load-path ("./lisp" "./bits")
       :build ("./configure" "make autoloads" "make")
       :build/darwin ("./configure --with-emacs=/Applications/Emacs.app/Contents/MacOS/Emacs" "make autoloads" "make")
       :features bbdb
       :after (lambda () (bbdb-initialize))
       :info "texinfo")

