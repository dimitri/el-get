(:name nxhtml
       :description "An addon for Emacs mainly for web development."
       :type git
       :build
         (list (concat el-get-emacs " -batch -q -no-site-file -L . -l nxhtmlmaint.el -f nxhtmlmaint-start-byte-compilation"))
       :url "http://github.com/emacsmirror/nxhtml.git"
       :load "autostart.el")
