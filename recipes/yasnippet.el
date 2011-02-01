(:name yasnippet
       :type svn
       :url "http://yasnippet.googlecode.com/svn/trunk/"
       :features "yasnippet"
       :after (lambda ()
                (yas/initialize)
                (setq yas/snippet-dirs
                      (list (concat el-get-dir "yasnippet/snippets")))
                (yas/reload-all)))
