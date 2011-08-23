(:name rhtml-mode
       :description "Major mode for editing RHTML files"
       :type git
       :url "https://github.com/eschulte/rhtml.git"
       :post-init (lambda ()
            (autoload 'rhtml-mode "rhtml-mode" nil t)
            (add-to-list 'auto-mode-alist '("\\.html\.erb$" . rhtml-mode))))
