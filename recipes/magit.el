(:name magit
       :type git
       :url "http://github.com/philjackson/magit.git"
       :info "."
       :build ("./autogen.sh" "./configure" "make")
       :features magit)
