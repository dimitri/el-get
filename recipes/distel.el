(:name distel
       :type svn
       :url "http://distel.googlecode.com/svn/trunk/"
       :info "doc"
       :build `,(mapcar
                 (lambda (target)
                   (concat "make " target " EMACS=" el-get-emacs))
                 '("clean" "all"))
       :load-path ("elisp")
       :features distel)

