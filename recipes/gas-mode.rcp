(:name gas-mode
       :website "http://www.hczim.de/software/gas-mode.html"
       :description "Mode for editing assembler code."
       :type http
       :url "http://www.hczim.de/software/gas-mode.el-1.10.gz"
       :build (("sh" "-c" "gunzip -c gas-mode.el-1.10.gz > gas-mode.el"))
       :compile "gas-mode.el"
       :prepare (progn
                  (add-to-list 'auto-mode-alist '("\\.S\\'" . gas-mode))))
