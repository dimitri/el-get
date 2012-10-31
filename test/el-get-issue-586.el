;; https://github.com/dimitri/el-get/issues/586
;;
;; Many recipes use git-emacs github repo

(setq debug-on-error t
      el-get-verbose t)

(el-get 'sync 'git-blame 'git-modeline)
(assert (reduce '(lambda (a b) (and a b))
                (mapcar 'el-get-package-is-installed '(git-emacs git-blame git-modeline)))
        nil
        "Git-emacs, git-blame, and git-modeline packages should all be installed.")
