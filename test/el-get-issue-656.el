;; https://github.com/dimitri/el-get/issues/656
;;
;; Init deps in el-get-init

(require 'cl)
(require 'pp)

(el-get-register-method-alias :test :builtin)

(setq el-get-default-process-sync t
      el-get-verbose t
      el-get-sources
      `((:name a :type test :compile "." :features a :build
               (("sh" "-c" ,(format "echo %s > a.el"
                                    (shell-quote-argument
                                     (mapconcat #'el-get-print-to-string
                                                '((require 'b)
                                                  (provide 'a))
                                                "\n")))))
               :depends b)
        (:name b :type test :compile "." :features nil :build
               (("sh" "-c" ,(format "echo %s > b.el"
                                    (shell-quote-argument
                                     (el-get-print-to-string
                                      '(provide 'b)))))))))

;; Ensure both are uninstalled
(ignore-errors (el-get-remove 'a))
(ignore-errors (el-get-remove 'b))
;; Install a and b
(el-get-install 'a)
;; Make sure B got installed
(assert (el-get-package-is-installed 'b) nil
        "Package B should be installed after installing A.")
;; Make sure that B got loaded
(assert (featurep 'b) nil
        "Installing package A should have loaded package B.")
;; Unload both features and remove their load-paths
(unload-feature 'a)
(unload-feature 'b)
(assert (not (featurep 'b)) nil
        "Feature B should be unloaded")
;; Remove load paths
(setq load-path
      (set-difference load-path (mapcan #'el-get-load-path '(a b))
                      :test #'string=))
;; Make sure B is no longer loadable
(condition-case err
    (progn
      (require 'b)
      (signal 'test-failure
              '("Loading B should have failed")))
  (error nil))

;; Now init A again, and make sure it *again* requires B
(condition-case err
    (el-get-init 'a)
  (error (error "HIT ISSUE #656: Need to init B before A. Error was: %S" err)))
(assert (featurep 'b) nil
        "Initializing package A should have loaded package B.")
