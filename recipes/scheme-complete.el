(:name scheme-complete
       :website "http://synthcode.com/wiki/scheme-complete"
       :description "Context-sensitive completion for Scheme code."
       :type http
       :url "http://synthcode.com/emacs/scheme-complete-0.8.7.el.gz"
       :build ("gunzip -c scheme-complete-0.8.7.el.gz > scheme-complete.el")
       :load    "scheme-complete.el"
       :compile "scheme-complete.el"
       :features "scheme-complete")
