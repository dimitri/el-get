(:name fsharp-mode
       :type svn
       :url "https://fsharp-mode.svn.sourceforge.net/svnroot/fsharp-mode"
       :load-path (".")
       :features (fsharp inf-fsharp)
       :post-init (lambda ()
		    (add-to-list 'auto-mode-alist '("\\.fs[iylx]?$" . fsharp-mode))))
