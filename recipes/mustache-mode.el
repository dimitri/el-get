(:name mustache-mode
       :features mustache-mode
       :type git
       :url "https://github.com/mustache/emacs.git"
       :post-init (lambda ()
                    (add-to-list 'auto-mode-alist '("\\.hs$" . mustache-mode))
                    (add-to-list 'auto-mode-alist '("\\.handlebars$" . mustache-mode))))
