(:name matlab-mode
       :description "Major mode for MATLAB(R) dot-m files"
       :type cvs
       :module "matlab-emacs"
       :url ":pserver:anonymous@matlab-emacs.cvs.sourceforge.net:/cvsroot/matlab-emacs"
       :depends cedet
       :build ("make")
       :load-path (".")
       :features matlab-load)
       
