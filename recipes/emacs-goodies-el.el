(:name emacs-goodies-el
       :type cvs
       :module "emacs-goodies-el"
       :url ":pserver:anonymous@cvs.alioth.debian.org:/cvsroot/pkg-goodies-el"
       :load-path ("elisp/debian-el"
		   "elisp/devscripts-el"
		   "elisp/dpkg-dev-el"
		   "elisp/emacs-goodies-el")
       :load ("elisp/dpkg-dev-el/dpkg-dev-el-loaddefs.el"))
