(:name emms
       :description "The Emacs Multimedia System"
       :type git
       :url "git://git.sv.gnu.org/emms.git"
       :info "doc"
       :load-path ("./lisp")
       :features emms-setup
       :build `(,(format "mkdir -p %s/emms " user-emacs-directory)
		,(concat "make EMACS=" el-get-emacs
			 " SITEFLAG=\"--no-site-file -L " el-get-dir "/emacs-w3m/ \""
			 " autoloads lisp docs"))
       :depends emacs-w3m)

