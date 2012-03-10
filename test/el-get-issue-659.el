;; https://github.com/dimitri/el-get/issues/659
;;
;; Updatable properties in cached recipes

(require 'cl)
(require 'pp)

(el-get-register-method-alias :test :builtin)

(let ((debug-on-error t)
      (el-get-default-process-sync t)
      (el-get-verbose t)
      (el-get-auto-update-cached-recipes t)
      (el-get-sources
       `((:name a
                :type test
                :compile "."
                :features a
                :build (("sh" "-c"
                         ,(format "echo %s > a.el"
                                  (shell-quote-argument
                                   (mapconcat
                                    #'pp-to-string
                                    '((provide 'a))
                                    "\n")))))
                :prepare (message "Preparing A")
                :post-init (message "Post-init A")
                :lazy nil)))
      (update-source
       '(:name a
               :before (message "Before A")
               :after (message "After A")
               :features nil
               :load "a.el"
               :library "a"
               ;; This should not cause an error because it matches the
               ;; cached value.
               :prepare (message "Preparing A")
               :lazy t))
      (update-source-2
       '(:name a
               :lazy nil
               :before (message "Before A2")
               :after (progn (setq second-update-succeeded t)
                             (message "After A2"))))
      (invalid-update-source
       '(:name a
               :post-init (message "New post-init A"))))
  ;; Install a and b
  (el-get-install 'a)
  (assert (featurep 'a) nil
          "Package A should be installed and loaded")
  (el-get-merge-updatable-properties update-source)
  (assert (plist-get (el-get-read-package-status-recipe 'a) :lazy) nil
          "New values should be merged into cached recipe")

  (condition-case err
      (progn
        (el-get-merge-updatable-properties invalid-update-source)
        (signal 'test-failure "Failed to raise error when trying to update non-updatable property."))
    (error (message "Got error as expected. Error was:\n%S" err)))
  ;; Try the no-error option. Obviously, it shouldn't throw an error.
  (el-get-merge-updatable-properties invalid-update-source 'noerror)

  ;; Now make sure `el-get-init' updates things.from `el-get-sources'
  (let ((el-get-sources (list update-source-2)))
    (el-get-init 'a))
  (assert second-update-succeeded nil
          "el-get-init should auto-update the recipe"))
