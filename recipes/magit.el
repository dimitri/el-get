(:name magit
       :website "https://github.com/magit/magit#readme"
       :description "It's Magit! An Emacs mode for Git."
       :type git
       :url "http://github.com/magit/magit.git"
       :info "."
       ;; that used to be how to build it :build ("./autogen.sh" "./configure" "make")
       :build ("make all")
       :build/darwin `(,(concat "PATH=" invocation-directory ":$PATH make all")))
