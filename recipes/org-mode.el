(:name org-mode
       :type git
       :url "git://repo.or.cz/org-mode.git"
       :info "doc"
       :build ("make clean" "make" "make doc")
       :load-path ("lisp" "contrib/lisp")
       :features org-install)
