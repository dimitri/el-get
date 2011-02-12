(:name pylookup
       :type git
       :url "git://github.com/tsgates/pylookup.git"
       :after (lambda ()
                (setq pylookup-dir (expand-file-name "pylookup" el-get-dir)
                      pylookup-program (expand-file-name "pylookup.py" pylookup-dir)
                      pylookup-db-file (expand-file-name "pylookup.db" pylookup-dir))
                (autoload 'pylookup-lookup "pylookup" "Lookup SEARCH-TERM in the Python HTML indexes." t)
                (autoload 'pylookup-update "pylookup" "Run pylookup-update and create the database at `pylookup-db-file'." t)))
