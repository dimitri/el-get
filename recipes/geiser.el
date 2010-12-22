(:name geiser
       :type git
       :url "git://git.sv.gnu.org/geiser.git"
       :load-path ("./elisp")
       :build ("./autogen.sh" "./configure" "make" "cd doc ; install-info --dir-file=./dir *.info")
       :info "doc"
       :features geiser-load
       )
