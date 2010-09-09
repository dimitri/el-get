(:name color-theme
       :type http-tar
       :options ("xzf")
       :url "http://download.savannah.gnu.org/releases/color-theme/color-theme-6.6.0.tar.gz"
       :load "color-theme.el"
       :feature "color-theme"
       :after (lambda ()
                (color-theme-initialize)
                (setq color-theme-is-global t)))
