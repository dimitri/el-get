(:name uim-el
       :description "A multilingual input method framework for Emacs"
       :type svn
       :url "http://uim.googlecode.com/svn/trunk/"
       :load-path ("./emacs")
       :build ("LC_MESSAGES=C ./make-wc.sh --prefix=`pwd`/build\
               --disable-gnome-applet --disable-fep --without-gtk2" 
           "make"
           "make install")
       :post-init (lambda () 
            (setq uim-el-agent (concat 
                    (file-name-as-directory
                     (el-get-package-directory "uim-el"))
                    "emacs/uim-el-agent"))
            (setq uim-el-helper-agent (concat 
                           (file-name-as-directory
                        (el-get-package-directory "uim-el"))
                           "emacs/uim-el-helper-agent"))))

