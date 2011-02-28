(:name flymake-ruby
       :type http
       :url "https://gist.github.com/raw/758976/b4562bca1645a5567d02e97f04b1909401caa1ed/flymake-ruby.el"
       :features flymake-ruby
       :post-init (lambda ()
		    (add-hook 'ruby-mode-hook 'flymake-ruby-load)))
