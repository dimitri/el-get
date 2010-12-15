(:name emacs-jabber
       :type git
       :url "git://catap.ru/emacs-jabber/emacs-jabber.git"
       :feature jabber
       :build `("mkdir build-aux" "aclocal -I m4" "autoconf" "automake -a -c" "./configure" "make")
       )


