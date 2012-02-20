;; https://github.com/dimitri/el-get/issues/579
;;
;; Lambda should be optional in :post-init and similar

(defun post-init-function ()
  (message "Post-init pre-defined function"))

(let ((debug-on-error nil)
      ;; (el-get-verbose t)
      (el-get-sources
       '(
         ;; These recipes should all be valid
         (:name post-init-function
                :type builtin
                :post-init post-init-function)
         (:name post-init-lambda
                :type builtin
                :post-init (lambda ()
                             (message "Post-init with lambda")))
         (:name post-init-expr-list
                :type builtin
                :post-init ((message "Post-init list of expressions")
                            (message "Post-init list of expressions 2")))
         (:name post-init-single-funcall
                :type builtin
                :post-init (message "Post-init single function call"))
         (:name no-post-init
                :type builtin)
         ;; This recipe should fail to install
         (:name post-init-broken
                :type builtin
                :post-init "Broken post-init"
                ))))
  (require 'el-get)
  ;; Should succeed
  (el-get 'sync
          'post-init-function
          'post-init-lambda
          'post-init-expr-list
          'post-init-single-funcall
          'no-post-init)
  (condition-case err
      (progn
        ;; Should fail
        (el-get 'sync 'post-init-broken)
        (signal 'test-failure
                '("The package\"post-init-broken\" should have caused an error, but it didn't.")))
    (error (message "Installing \"post-init-broken\" failed as expected"))))
