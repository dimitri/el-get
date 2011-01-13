(:name yaml-mode
       :type git
       :url "git://github.com/yoshiki/yaml-mode.git"
       :after (lambda ()
                (autoload 'yaml-mode "yaml-mode" nil t)
                (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))))
