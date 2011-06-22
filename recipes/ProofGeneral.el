(:name ProofGeneral ;; Requires Emacs >= 23.3
       :type http-tar
       :options ("xzf")
       :url "http://proofgeneral.inf.ed.ac.uk/releases/ProofGeneral-4.1RC2.tgz"
       :build ("cd ProofGeneral && make clean" "cd ProofGeneral && make compile")
       :load  ("ProofGeneral/generic/proof-site.el")
       :info "./ProofGeneral/doc/")
