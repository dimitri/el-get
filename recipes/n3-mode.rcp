(:name n3-mode
       :description "Mode for Notation 3"
       :type git
       :url "https://github.com/kurtjx/n3-mode-for-emacs.git"
       :post-init (lambda ()
            (autoload 'n3-mode "n3-mode" "Major mode for OWL or N3 files" t)
            (add-hook 'n3-mode-hook 'turn-on-font-lock)
            (add-to-list 'auto-mode-alist '("\\.\\(n3\\|owl\\)\\'" . n3-mode))))
