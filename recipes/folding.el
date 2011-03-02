(:name folding
       :type http
       :url "http://git.savannah.gnu.org/cgit/emacs-tiny-tools.git/plain/lisp/other/folding.el?h=devel"
       :localname "folding.el"
       :features (folding folding-isearch)
       :post-init 'folding-mode-add-find-file-hook)
