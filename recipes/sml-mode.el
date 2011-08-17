(:name sml-mode
       :description "SML-mode is a major Emacs mode for editing Standard ML source code."
       :type http-tar
       :options ("xzf")
       :url "http://www.iro.umontreal.ca/~monnier/elisp/sml-mode.tar.gz"
       :build `,(mapcar
         (lambda (target)
           (concat "make " target " EMACS=" el-get-emacs))
         '("clean" "default"))
       :load-path (".")
       :load ("sml-mode-startup.el"))