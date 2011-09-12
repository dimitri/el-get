(:name q-mode
       :type git
       :url "git://github.com/little-arhat/q-mode.git"
       :description "kdbp-mode + q-minor-mode combined for easy installation and using."
       :load-path (".")
       :features (q-minor-mode kdbp-mode)
       :post-init
       (lambda ()
         (add-to-list 'auto-mode-alist '("\\.[kq]$" . kdbp-mode))
         (autoload 'q-mode "q-mode")
         (autoload 'q-help "q-mode")
         (autoload 'run-q "q-mode")
         (autoload 'kdbp-mode "kdbp-mode")))
