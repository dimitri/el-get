(:name org-mode
       :website "http://orgmode.org/"
       :description "Org-mode is for keeping notes, maintaining ToDo lists, doing project planning, and authoring with a fast and effective plain-text system."
       :type git
       :url "git://orgmode.org/org-mode.git"
       :info "doc"
       :build `,(mapcar
                 (lambda (target)
                   (concat "make " target " EMACS=" el-get-emacs))
                 '("clean" "all"))
       :load-path ("." "lisp" "contrib/lisp")
       :autoloads nil
       :features org-install)

