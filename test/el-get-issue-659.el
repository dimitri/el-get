;; https://github.com/dimitri/el-get/issues/659
;;
;; Updatable properties in cached recipes

(require 'cl)
(require 'pp)

(el-get-register-method-alias :test :builtin)

(let* ((debug-on-error t)
       (el-get-default-process-sync t)
       (el-get-verbose t)
       (el-get-auto-update-cached-recipes t)
       (initial-source
        `(:name a
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
                :lazy nil))
       (el-get-sources
        (list initial-source))
      (update-source
       (append '(:features nil
                           :lazy t
                           :before (message "Before A")
                           :after (message "After A")
                           :load "a.el"
                           :library "a"
                           ;; This should not cause an error because it matches the
                           ;; cached value.
                           :prepare (message "Preparing A"))
               initial-source))
      (update-source-2
       (append '(:lazy nil
                       :before (message "Before A2")
                       :after (progn (setq second-update-succeeded t)
                                     (message "After A2")))
               initial-source))
      (invalid-update-source
       (append '(:non-updatable-property value)
               initial-source)))
  ;; Install A
  (el-get 'sync 'a)
  (require 'a)
  (assert (featurep 'a) nil
          "Package A should be installed and loaded")
  (el-get-merge-properties-into-status update-source)
  (assert (plist-get (el-get-read-package-status-recipe 'a) :lazy) nil
          "New values should be merged into cached recipe")

  (condition-case err
      (progn
        (el-get-merge-properties-into-status invalid-update-source)
        (signal 'test-failure "Failed to raise error when trying to update non-updatable property."))
    (error (message "Got error as expected. Error was:\n%S" err)))
  ;; Try the no-error option. Obviously, it shouldn't throw an error,
  ;; and it also shouldn't update anything.
  (el-get-merge-properties-into-status invalid-update-source nil :noerror t)
  (assert (null (plist-get (el-get-read-package-status-recipe 'a) :features)) nil
          "Cached recipe should not be updated")
  ;; Verify that new properties were actually added.
  (let ((el-get-sources (list update-source-2)))
    (el-get-init 'a))
  (assert (bound-and-true-p second-update-succeeded) nil
          "el-get-init should auto-update the recipe")
)
