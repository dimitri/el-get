(:name auctex
       :type cvs
       :module "auctex"
       :url ":pserver:anonymous@cvs.sv.gnu.org:/sources/auctex"
       :build ("./autogen.sh" "./configure" "make")
       :load  ("tex-site.el" "preview/preview-latex.el")
       :info "doc")
