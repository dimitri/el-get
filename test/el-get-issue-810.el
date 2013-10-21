;;; https://github.com/dimitri/el-get/issues/810
;;; :autoloads nil as no effect

(el-get-register-method-alias :test :builtin)

(setq
 el-get-default-process-sync t
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
    :autoloads nil)))

(el-get 'sync 'a)

;;; check nothing was loaded (this fails)
(assert (not (or (fboundp 'a-nother-func)
                 (fboundp 'a-utoloaded-func))))

;;; check we can load everything (for sanity)
(require 'a)

(assert (and (fboundp 'a-nother-func)
             (fboundp 'a-utoloaded-func)))
