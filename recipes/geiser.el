(:name geiser
       :type git
       :url "git://git.sv.gnu.org/geiser.git"
       :load-path ("./elisp")
       :build ("./autogen.sh" "./configure" "make" "cd doc ; ginstall-info --dir-file=./dir *.info")
       :info "doc"
       :features geiser-load
       )
