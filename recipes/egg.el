(:name egg
       :type git
       :url "https://github.com/byplayer/egg.git"
       :load-path (".")
       :compile nil ;; egg uses eval at places which breaks compilation
       :features egg)