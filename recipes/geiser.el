(:name geiser
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
