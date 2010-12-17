(:name emacs-jabber
       :type git
       :url "git://catap.ru/emacs-jabber/emacs-jabber.git"
       :features jabber
       :build `("mkdir build-aux" "aclocal -I m4" "autoconf" "automake -a -c" "./configure" "make")
       )


