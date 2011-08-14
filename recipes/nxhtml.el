(:name nxhtml
       :type emacsmirror
       :build
         (list (concat el-get-emacs " -batch -q -no-site-file -L . -l nxhtmlmaint.el -f nxhtmlmaint-start-byte-compilation"))
       :load "autostart.el")
