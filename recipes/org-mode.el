(:name org-mode
       :type git
       :url "http://repo.or.cz/r/org-mode.git"
       :info "doc"
       :build `,(mapcar
                 (lambda (target)
                   (concat "make " target " EMACS=" el-get-emacs))
                 '("clean" "all"))
       :load-path ("lisp" "contrib/lisp")
       :autoloads nil
       :features org-install)

