(:name ess
       :description "Emacs Speaks Statistics: statistical programming within Emacs"
       :type svn
       :url "https://svn.r-project.org/ESS/trunk/"
       :info "doc/info/"
       :build `,(mapcar
                 (lambda (target)
                   (concat "make " target " EMACS=" el-get-emacs))
                 '("clean" "all"))
       :load-path ("lisp")
       :features ess-site)

