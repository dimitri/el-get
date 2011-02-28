(:name bbdb
       :type git
       :url "https://github.com/barak/BBDB.git"
       :load-path ("./lisp" "./bits")
       :build ("./configure" "make autoloads" "make")
       :build/darwin `(,(concat "./configure --with-emacs=" el-get-emacs) "make autoloads" "make")
       :features bbdb
       :autoloads nil
       :post-init (lambda () (bbdb-initialize))
       :info "texinfo")

