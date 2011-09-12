(:name shadow
       :description "That's not the file. That's shadow."
       :type git :url "https://github.com/mooz/shadow.el.git"
       :features shadow
       :post-init (lambda()
                    (add-hook 'find-file-hooks 'shadow-on-find-file)
                    (add-hook 'shadow-find-unshadow-hook
                              (lambda () (auto-revert-mode t)))))
