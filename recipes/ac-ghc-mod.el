(:name ac-ghc-mod
       :description "Smart auto-complete sources for haskell mode"
       :type emacswiki
       :prepare (progn
                  (defun haskell-ac-ghc-mod-hook ()
                    (when (bound-and-true-p ac-sources)
                      (make-local-variable 'ac-sources)
                      (nconc ac-sources '(ac-source-ghc-module
                                          ac-source-ghc-symbol
                                          ac-source-ghc-pragmas
                                          ac-source-ghc-langexts))))
                  (add-hook 'haskell-mode-hook 'haskell-ac-ghc-mod-hook))
       :depends (auto-complete ghc-mod))
