(:name textile-mode
       :type http
       :url "http://dev.nozav.org/scripts/textile-mode.el"
       :after (lambda ()
                (autoload 'textile-mode "textile-mode" "Textile editing mode." t)
                (add-to-list 'auto-mode-alist '("\\.textile\\'". textile-mode))))
