(:name rt-liberation
       :description "An interface to the RT system."
       :type git
       :url "http://yrk.nfshost.com/repos/rt-liberation.git/"
       :build ("make -C doc" "cp doc/rt-liberation.info doc/rt-liber.info")
       :info "./doc"
       :features rt-liberation)
