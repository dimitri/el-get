(:name point-stack
       :description "Like forward/back for browser, but for emacs point"
       :type git
       :url "https://github.com/mattharrison/point-stack.git"
       :features point-stack
       :compile "point-stack.el")

;;
;; Configuration example:
;;
;;        :post-init (lambda()
;;              (global-set-key '[(f5)] 'point-stack-push)
;;              (global-set-key '[(f6)] 'point-stack-pop)
;;              (global-set-key '[(f7)] 'point-stack-forward-stack-pop))
