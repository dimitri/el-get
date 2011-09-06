 (:name coffee-mode
  :website "http://ozmm.org/posts/coffee_mode.html"
  :description "Emacs Major Mode for CoffeeScript"
  :type git
  :url "https://github.com/defunkt/coffee-mode.git"
  :features coffee-mode
  :post-init (lambda ()
	       (add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
	       (add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
	       ;; it defaults to js2-mode, which is not present in Emacs by default
	       (setq coffee-js-mode 'javascript-mode)))
