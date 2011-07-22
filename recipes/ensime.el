(:name ensime
       :description "ENhanced Scala Interaction Mode for Emacs"
       :type git
       :url "https://github.com/aemoncannon/ensime.git"
       :build ("sbt update stage")
       :features ensime
       :load-path ("./dist/elisp")
       :post-init (lambda ()
                    (require 'ensime)
                    ;; scala-mode can be found in the scala distribution:
                    (require 'scala-mode-auto)
                    (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)))
