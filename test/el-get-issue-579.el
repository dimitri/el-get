;; https://github.com/dimitri/el-get/issues/579
;;
;; Lambda should be optional in :post-init and similar

(require 'cl)

(defun post-init-function ()
  (message "Post-init pre-defined function"))

(let* ((debug-on-error t)
       ;; (el-get-verbose t)
       (real-recipes
        '(
          ;; Some actual recipes that have :post-init properties
          go-mode
          eclim
          theme-roller
          ))
       (good-sources
        (append
         '(
           ;; These recipes should all be valid
           (:name post-init-function
                  :type builtin
                  :post-init post-init-function)
           (:name post-init-function-call
                  :type builtin
                  :post-init (post-init-function))
           (:name post-init-quoted-function-call
                  :type builtin
                  :post-init '(post-init-function))
           (:name post-init-lambda
                  :type builtin
                  :post-init (lambda ()
                               (message "Post-init with lambda")))
           (:name post-init-single-form
                  :type builtin
                  :post-init (message "Post-init single function call"))
           (:name post-init-quoted-form
                  :type builtin
                  :post-init '(message "Post-init quoted function call"))
           (:name post-init-expr-list
                  :type builtin
                  :post-init (progn
                               (message "Post-init list of expressions")
                               (message "Post-init list of expressions 2")))
           (:name no-post-init
                  :type builtin))
         real-recipes))
       (broken-sources
        '(
          ;; These recipes should fail to install
          (:name post-init-broken
                 :type builtin
                 :post-init "Broken post-init"
                 )
          (:name post-init-missing-function
                 :type builtin
                 :post-init this-function-does-not-exist
                 )))
       (el-get-sources
        (append good-sources broken-sources)))
  ;; Should succeed
  (message "Good recipes: %S" (mapcar 'el-get-source-name good-sources))
  (apply 'el-get 'sync (mapcar 'el-get-source-name good-sources))
  (loop for broken-pkg in (mapcar 'el-get-source-name broken-sources)
        ;; Each package should fail to install
        do (condition-case err
               (progn
                 (let ((debug-on-error nil))
                   (el-get 'sync broken-pkg))
                 ;; If installation succeeded, then throw a
                 ;; non-standard error that will not be caught.
                 (signal 'test-failure
                         (list (format "The package \"%s\" should have caused an error, but it didn't." broken-pkg))))
             ;; This catches and suppresses the expected package
             ;; installation error.
             (error (message "Installing \"%s\" failed as expected" broken-pkg)))))
