(:name emacs-jabber
       :description "A minimal jabber client"
       :type git
       :url "git://emacs-jabber.git.sourceforge.net/gitroot/emacs-jabber/emacs-jabber"
       :info "."
       :load-path (".")
       :features jabber-autoloads
       :build ("autoreconf -i" "./configure" "make"))
