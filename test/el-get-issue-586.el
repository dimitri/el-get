;; https://github.com/dimitri/el-get/issues/586
;;
;; Many recipes use git-emacs github repo

(let ((debug-on-error t)
      ;; (el-get-byte-compile nil)
      (el-get-verbose t))
  (el-get 'sync 'git-blame 'git-modeline)
  (assert (reduce '(lambda (a b) (and a b))
                  (mapcar 'el-get-package-is-installed '(git-emacs git-blame git-modeline)))
          nil
          "Git-emacs, git-blame, and git-modeline pacakges should all be installed."))
