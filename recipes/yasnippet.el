(:name yasnippet
       :type svn
       :url "http://yasnippet.googlecode.com/svn/trunk/"
       :features "yasnippet"
       :after (lambda ()
                (yas/initialize)
		(add-to-list 'yas/snippet-dirs (concat el-get-dir "yasnippet/snippets"))
		(yas/reload-all)))
