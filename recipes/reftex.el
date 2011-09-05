(:name reftex
;       :type cvs
       :type git
       :module "reftex"
       :url "git://github.com/emacsmirror/reftex.git"
;       :url ":pserver:anonymous@cvs.sv.gnu.org:/sources/auctex"
       :build ("make" "make info")
       :features reftex
       :load-path ("lisp")
;       :info "doc"
       :after (lambda ()
		(load "reftex")))
