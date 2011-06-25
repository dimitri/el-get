(:name geiser
       :website "http://www.nongnu.org/geiser/"
       :description "Geiser is a collection of Emacs major and minor modes that conspire with one or more Scheme interpreters to keep the Lisp Machine Spirit alive. It draws inspiration (and a bit more) from environments such as Common Lisp’s Slime, Factor’s FUEL, Squeak or Emacs itself, and does its best to make Scheme hacking inside Emacs (even more) fun."
       :type git
       :url "git://git.sv.gnu.org/geiser.git"
       :load-path ("./elisp")
       :build `("./autogen.sh" "./configure"
		,(concat "make EMACS=" el-get-emacs)
		,(concat "make EMACS=" el-get-emacs "info-recursive"))
		;,(concat "cd doc ; " el-get-install-info " --dir-file=./dir *.info"))
       :build/windows-nt `("sh ./autogen.sh" "sh ./configure" "make"
	       ,(concat "cd doc & " el-get-install-info " --dir-file=./dir *.info"))
       :info "doc"
       :features geiser-load
       )
