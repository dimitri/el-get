(:name auctex
       :type cvs
       :module "auctex"
       :url ":pserver:anonymous@cvs.sv.gnu.org:/sources/auctex"
       :build ("./autogen.sh" (concat "./configure --with-lispdir=`pwd` --with-emacs=" el-get-emacs) "make")
       :load  ("tex-site.el" "preview/preview-latex.el")
       :info "doc")
