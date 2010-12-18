(:name yaml-mode
       :type git
       :url "git://github.com/yoshiki/yaml-mode.git"
       :features yaml-mode
       :after (lambda ()
                (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
                (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))))
