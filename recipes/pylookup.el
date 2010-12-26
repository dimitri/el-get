(:name pylookup
       :type git
       :url "git://github.com/tsgates/pylookup.git"
       :features pylookup
       :after (lambda ()
                (setq pylookup-dir (concat el-get-dir "pylookup/"))
                (setq pylookup-program (concat pylookup-dir "pylookup.py"))
                (setq pylookup-db (concat pylookup-dir "pylookup.db"))
                (autoload 'pylookup "pylookup")
                (autoload 'pylookup-update "pylookup")))
