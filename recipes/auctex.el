(:name auctex
       :website "http://www.gnu.org/software/auctex/"
       :description "AUCTeX is an extensible package for writing and formatting TeX files in GNU Emacs and XEmacs. It supports many different TeX macro packages, including AMS-TeX, LaTeX, Texinfo, ConTeXt, and docTeX (dtx files)."
       :type cvs
       :module "auctex"
       :url ":pserver:anonymous@cvs.sv.gnu.org:/sources/auctex"
       :build `("./autogen.sh" ,(concat "./configure --with-lispdir=`pwd` --with-emacs=" el-get-emacs) "make")
       :load-path ("." "preview")
       :load  ("tex-site.el" "preview/preview-latex.el")
       :info "doc")
