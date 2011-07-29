(:name session
       :description "When you start Emacs, package Session restores various variables (e.g., input histories) from your last session. It also provides a menu containing recently changed/visited files and restores the places (e.g., point) of such a file when you revisit it."
       :type http-tar
       :options ("xzf")
       :load-path ("lisp")
       :url "http://downloads.sourceforge.net/project/emacs-session/session/2.2a/session-2.2a.tar.gz"
       :info "session management"
       :autoloads nil
       )
