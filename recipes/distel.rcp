(:name distel
       :website "https://github.com/massemanet/distel"
       :description "Distributed Emacs Lisp for Erlang."
       :type git
       :url "https://github.com/massemanet/distel.git"
       :info "doc"
       :build `,(mapcar
                 (lambda (target)
                   (concat "make " target " EMACS=" el-get-emacs))
                 '("clean" "all"))
       :load-path ("elisp")
       :features distel)

