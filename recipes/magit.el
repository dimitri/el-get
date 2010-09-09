(:name magit
       :type git
       :url "http://github.com/philjackson/magit.git"
       :info "."
       ;; that used to be how to build it :build ("./autogen.sh" "./configure" "make")
       :build ("make all")
       :build/darwin ("PATH=/Applications/Emacs.app/Contents/MacOS:$PATH make all")
       :features magit)
