(:name emms
       :type git
       :url "git://git.sv.gnu.org/emms.git"
       :info "doc"
       :load-path ("./lisp")
       :features emms-setup
       :build ("make autoloads" "make")
       :build/darwin ("make EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs autoloads all"))
