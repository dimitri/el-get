(:name python-mode
       :type emacsmirror
       :description "Major mode for editing Python programs"
       :features python-mode
       :compile nil
       :post-init (lambda ()
            (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
            (add-to-list 'interpreter-mode-alist '("python" . python-mode))
            (autoload 'python-mode "python-mode" "Python editing mode." t)))
