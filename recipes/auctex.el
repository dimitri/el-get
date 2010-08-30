(:name auctex
       :type cvs
       :module "auctex"
       :url ":pserver:anonymous@cvs.sv.gnu.org:/sources/auctex"
       :build ("./autogen.sh" "./configure" "make")
       :load  ("auctex.el" "preview/preview-latex.el")
       :info "doc")
