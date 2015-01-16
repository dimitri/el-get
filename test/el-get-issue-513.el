;; https://github.com/dimitri/el-get/issues/513
;;
;; Testing the github and emacsmirror methods

(setq debug-on-error t
      el-get-verbose t)

(condition-case nil
    (error "THROW")
  (error (message "Caught an error")))

;; ;; Install a github-type recipe
;; (el-get 'sync 'window-layout)
;; ;; Install an emacsmirror-type recipe
;; (el-get 'sync 'dired-plus)

;; Try to install a recipe for a nonexistent github repo
(condition-case err
    (progn
      ;; Should fail
      (let ((el-get-sources
             '((:name valid-recipe-for-nonexistent-repo
                      :pkgname "xjklfjdlfs/fdjsklfdsj"
                      :type github)))
            ;; This seems to escape error handlers when
            ;; `debug-on-error' is t.
            (debug-on-error nil))
        (el-get 'sync 'valid-recipe-for-nonexistent-repo))
      (signal 'test-failure
              '("The package \"valid-recipe-for-nonexistent-repo\" should have caused an error, but it didn't.")))
  (error (message "Installing \"valid-recipe-for-nonexistent-repo\" failed as expected. The error message was: %S" err)))

(condition-case err
    (progn
      ;; Should fail
      (let ((el-get-sources
             '((:name broken-github-recipe
                      :type github))))
        (el-get 'sync 'broken-github-recipe))
      (signal 'test-failure
              '("The package \"broken-github-recipe\" should have caused an error, but it didn't.")))
  (error (message "Installing \"broken-github-recipe\" failed as expected. The error message was: %S" err)))
