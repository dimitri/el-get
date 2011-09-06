(:name bbdb
       :website "http://bbdb.sourceforge.net/"
       :description "The Insidious Big Brother Database (BBDB) is a contact management utility."
       :type git
       :url "git://git.savannah.nongnu.org/bbdb.git"
       :load-path ("./lisp")
       :build `(,(concat "make VMDIR= EMACS=" el-get-emacs " -C lisp bbdb autoloadsc"))
       :features bbdb
       :autoloads nil
       :post-init (lambda () (bbdb-initialize)))

