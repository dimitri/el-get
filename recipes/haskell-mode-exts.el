(:name haskell-mode-exts
       :type git
       :url "http://github.com/pheaver/haskell-mode-exts.git"
       :load-path (".")
       :features (haskell-align-imports haskell-installed-packages
                  haskell-navigate-imports haskell-sort-imports
                  inf-haskell-send-cmd)
       )
