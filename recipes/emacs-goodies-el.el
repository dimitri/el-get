(:name emacs-goodies-el
       :website "http://packages.debian.org/sid/emacs-goodies-el"
       :description "Miscellaneous add-ons for Emacs"
       :type cvs
       :options login
       :module "emacs-goodies-el"
       :url ":pserver:anonymous@cvs.alioth.debian.org:/cvs/pkg-goodies-el"
       :load-path ("elisp/debian-el"
		   "elisp/devscripts-el"
		   "elisp/dpkg-dev-el"
		   "elisp/emacs-goodies-el"
		   ;; "elisp/gnus-bonus-el"
		   "elisp/vm-bonus-el")
       :load ("elisp/dpkg-dev-el/dpkg-dev-el-loaddefs.el"))
