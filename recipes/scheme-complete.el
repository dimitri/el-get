(:name scheme-complete
       :type http
       :url "http://synthcode.com/emacs/scheme-complete-0.8.7.el.gz"
       :build ("gunzip -c scheme-complete-0.8.7.el.gz > scheme-complete.el")
       :load    "scheme-complete.el"
       :compile "scheme-complete.el"
       :features "scheme-complete")
