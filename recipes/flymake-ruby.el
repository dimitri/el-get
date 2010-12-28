(:name flymake-ruby
       :type http
       :url "https://github.com/purcell/emacs.d/raw/master/site-lisp/flymake-ruby/flymake-ruby.el"
       :features flymake-ruby
       :after (lambda ()
                (add-hook 'ruby-mode-hook 'flymake-ruby-load)))
