 (:name coffee-mode
  :type git
  :url "git://github.com/defunkt/coffee-mode.git"
  :features coffee-mode
  :after (lambda ()
           (add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
           (add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
           ;; it defaults to js2-mode, which is not present in Emacs by default
           (setq coffee-js-mode 'javascript-mode)))
