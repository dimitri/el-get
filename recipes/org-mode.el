(:name org-mode
       :type git
       :url "git://repo.or.cz/org-mode.git"
       :info "doc"
       :build `,(mapcar
                 (lambda (target)
                   (concat "make " target " EMACS=" el-get-emacs))
                 '("clean" "all" "doc"))
       :load-path ("lisp" "contrib/lisp")
       :features org-install)

