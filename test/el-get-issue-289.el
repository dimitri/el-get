;; https://github.com/dimitri/el-get/issues/289
;;
;; flymake-fringe-icons recipe won't initialize
;;
;; Because it eagerly loads flymake-fringe-icons by using :features, and
;; that recipe require's fringe-helper, which isn't yet init'ed. The
;; solution is either to remove :features from the flymake-fringe-icons
;; recipe or to make el-get-init respect the :depends clause.

(setq debug-on-error t)

(setq el-get-byte-compile nil
      el-get-verbose t)
(el-get 'sync '(flymake-fringe-icons))
