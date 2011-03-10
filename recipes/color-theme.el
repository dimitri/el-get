(:name color-theme
       :type http-tar
       :options ("xzf")
       :url "http://download.savannah.gnu.org/releases/color-theme/color-theme-6.6.0.tar.gz"
       :load "color-theme.el"
       :features "color-theme"
       :post-init (lambda ()
		    (color-theme-initialize)
		    (setq color-theme-is-global t)))
