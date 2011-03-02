(:name yasnippet
       :type svn
       :url "http://yasnippet.googlecode.com/svn/trunk/"
       :features "yasnippet"
       :post-init (lambda ()
		    (yas/initialize)
		    (add-to-list 'yas/snippet-dirs (concat el-get-dir "yasnippet/snippets"))
		    (yas/reload-all)))
