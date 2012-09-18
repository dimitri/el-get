;; https://github.com/dimitri/el-get/issues/513
;;
;; Testing the github and emacsmirror methods

(setq debug-on-error t
      el-get-verbose t)

;; Install a github-type recipe
(el-get 'sync 'window-layout)
;; Install an emacsmirror-type recipe
(el-get 'sync 'dired-plus)

(condition-case err
    (progn
      ;; Should fail
      (let ((el-get-sources
             '((:name broken-pkg
                      :type github))))
        (el-get 'sync 'broken-pkg))
      (signal 'test-failure
              '("The package\"broken-pkg\" should have caused an error, but it didn't.")))
  (error (message "Installing \"broken-pkg\" failed as expected. The error message was: %S" err)))
