;;; https://github.com/dimitri/el-get/issues/446
;;; el-get-remove doesn't remove autoloads from .loaddefs

(el-get-register-method-alias :test :builtin)

(setq
 el-get-verbose t
 el-get-sources
 `((:build
    (("sh" "-c"
      ,(format "echo %s > a.el"
               (shell-quote-argument
                (mapconcat (lambda (x) (prin1-to-string x t))
                           `(,(make-symbol ";;;###autoload")
                             (defun a-utoloaded-func () t)
                             (defun a-nother-func () t)
                             (provide 'a))
                           "\n")))))
    :name a :type test :compile "."
    :autoloads t)))

(el-get 'sync 'a)
(el-get-remove "a")

;; installing `a' autoloads it, undo that
(and (fboundp 'a-utoloaded-func) (fmakunbound 'a-utoloaded-func))

;; reload current autoload file
(el-get-eval-autoloads)

(assert (not (fboundp 'a-utoloaded-func)) nil
        "autoloads for `a' should have been removed from %s"
        el-get-autoload-file)
