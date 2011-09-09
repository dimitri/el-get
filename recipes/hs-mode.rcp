(:name hs-mode
       :description "Haskell editing mode."
       :type git
       :url "http://github.com/chrisdone/haskell-emacs.git"
       :depends auto-complete
       :compile nil
       :load-path ("./src/")

       :features (hs-align-imports hs-cabal hs-cabal-mode hs-completion hs-config
                                   hs hs-errors hs-faces hs-indent hs-indent-test
                                   hs-interactive-mode hs-lang-en hs-macros
                                   hs-mode hs-move-nested
                                   hs-navigate-imports hs-process hs-project
                                   hs-sort-imports
                                   hs-string hs-tags hs-types hs-ui)

       :post-init (lambda ()
                    (add-to-list 'auto-mode-alist
                                 (cons "\\.hs\\'" 'hs-mode))
                    (add-to-list 'auto-mode-alist
                                 (cons "\\.cabal\\'" 'hs-cabal-mode)))
       )
