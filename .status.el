((el-get status "installed" recipe
	 (:name el-get :website "https://github.com/dimitri/el-get#readme" :description "Manage the external elisp bits and pieces you depend upon." :type github :branch "4.stable" :pkgname "dimitri/el-get" :info "." :load "el-get.el"))
 (gregorio-mode status "installed" recipe
		(:name gregorio-mode :website "http://christusrex.pl:8080" :description "Major derived mode for .gabc (gregorio) files" :type github :pkgname "cajetanus/gregorio-mode.el" :features gregorio-mode :post-init
		       (progn
			 (add-to-list 'auto-mode-alist
				      '("\\.gabc$" . gregorio-mode))))))
