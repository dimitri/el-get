(:name point-stack
       :type git
       :url "git://github.com/mattharrison/point-stack.git"
       :features point-stack
       :compile "point-stack.el")

;;
;; Configuration example:
;;
;;        :after (lambda()
;;        		(global-set-key '[(f5)] 'point-stack-push)
;;        		(global-set-key '[(f6)] 'point-stack-pop)
;;        		(global-set-key '[(f7)] 'point-stack-forward-stack-pop))
