(:name muse
       :type git
       :url "https://github.com/alexott/muse.git"
       :load-path ("./lisp")
       :build `(,(concat "make EMACS=" el-get-emacs))
       :autoloads "muse-autoloads")
