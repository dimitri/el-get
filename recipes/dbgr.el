(:name dbgr
       :type git
       :url "https://github.com/rocky/emacs-dbgr.git"
       :build ("./autogen.sh" "./configure" "make")
       :features (loc-changes load-relative test-unit)
       )
