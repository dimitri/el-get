(:name company-lsp
       :description "Company completion backend for lsp-mode "
       :type "github"
       :pkgname "tigersoldier/company-lsp"
       :depends (dash cl-lib s seq company-mode lsp-mode)
       :post-init (eval-after-load 'company
                    '(add-to-list 'company-backends 'company-lsp)))
