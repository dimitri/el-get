(:name rhtml-mode
       :type git
       :url "https://github.com/eschulte/rhtml.git"
       :after (lambda ()
                (autoload 'rhtml-mode "rhtml-mode" nil t)
                (add-to-list 'auto-mode-alist '("\\.html\.erb$" . rhtml-mode))))
