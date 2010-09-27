(:name rt-liberation
       :type git
       :url "http://yrk.nfshost.com/repos/rt-liberation.git/"
       :build ("make -C doc" "cp doc/rt-liberation.info doc/rt-liber.info")
       :info "./doc"
       :features rt-liberation)
