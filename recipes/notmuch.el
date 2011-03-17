(:name notmuch
       :type git
       :url "git://notmuchmail.org/git/notmuch"
       :load-path ("./emacs")
       :build ("./configure" "make"))
