(:name ido-hacks
       :description "Advices for ido-mode."
       :type http
       :url "http://www0.fh-trier.de/~politza/emacs/ido-hacks.el.gz"
       :build ("gunzip -c ido-hacks.el.gz > ido-hacks.el")
       :load    "ido-hacks.el"
       :compile "ido-hacks.el")
