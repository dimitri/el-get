(:name matlab-mode
       :type cvs
       :module "matlab-emacs"
       :url ":pserver:anonymous@matlab-emacs.cvs.sourceforge.net:/cvsroot/matlab-emacs"
       :depends cedet
       :build ("make")
       :load-path (".")
       :features matlab-load)
       
