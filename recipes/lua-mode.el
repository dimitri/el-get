(:name lua-mode
       :type http
       :url "http://luaforge.net/plugins/scmcvs/cvsweb.php/lua-mode/lua-mode.el?rev=HEAD;content-type=text%2Fplain;cvsroot=lua-mode"
       :features lua-mode
       :build '("mv lua-mode.el* lua-mode.el")
       :after (lambda ()
                (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))))
