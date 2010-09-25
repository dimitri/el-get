(:name ensime
 :type git
 :url "git://github.com/aemoncannon/ensime.git" 
 :build ("sbt update stage")
 :features ensime
 :load-path ("./dist/elisp")
 :after (lambda () 
          (require 'ensime)
          (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)))
