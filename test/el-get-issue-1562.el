;;; https://github.com/dimitri/el-get/issues/1562 Removing a package
;;; that comes with an info manual, causes an error when calling info

(el-get-register-method-alias :test :builtin)

;;; install a
(setq el-get-sources
      `((:name a :type test
               :build (("sh" "-c"
                        ,(format "echo %s > a.info"
                                 (shell-quote-argument
                                  (concat
                                   "\x1f\n"
                                   "Tag Table:\n"
                                   "\x1f\n"
                                   "End Tag Table\n")))))
               :info ".")))

(el-get 'sync "a")
(el-get-remove "a")

(info)
