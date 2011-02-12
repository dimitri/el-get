(:name geiser
       :type git
       :url "git://git.sv.gnu.org/geiser.git"
       :load-path ("./elisp")
       :build `("./autogen.sh" "./configure" "make"
	       ,(concat "cd doc ; " el-get-install-info " --dir-file=./dir *.info"))
       :info "doc"
       :features geiser-load
       )
