(:name bbdb
       :type git
       :url "git://git.savannah.nongnu.org/bbdb.git"
       :load-path ("./lisp")
       :build `(,(concat "make EMACS=" el-get-emacs "-C lisp"))
       :features bbdb
       :autoloads nil
       :post-init (lambda () (bbdb-initialize))
       :info "texinfo")

