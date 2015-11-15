;; https://github.com/dimitri/el-get/issues/1028
;;
;; Git clone should ignore depth flag when using http protocol


(setq debug-on-error t
      el-get-verbose t
      el-get-git-shallow-clone t)

(el-get 'sync 'undo-tree)
