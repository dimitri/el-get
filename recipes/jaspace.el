(:name jaspace :type http
       :url "http://homepage3.nifty.com/satomii/software/jaspace.el"
       :features jaspace
       :post-init (lambda()
                    (global-font-lock-mode t)
                    (setq jaspace-alternate-jaspace-string "?")
                    (setq jaspace-alternate-eol-string "\n")
                    (setq jaspace-highlight-tabs ?^)))
