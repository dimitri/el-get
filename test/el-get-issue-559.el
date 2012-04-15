;; https://github.com/dimitri/el-get/issues/559
;;
;; Testing the github-tar and github-zip methods

(setq debug-on-error t
      el-get-verbose t
      el-get-sources
      '((:name indirect-region-http-tar :type http-tar :options
               ("xzf")
               :description "Act like indirect buffer for region." :url "https://github.com/renard/indirect-region/tarball/master")
        (:name indirect-region-tar :type github-tar :description "Act like indirect buffer for region." :pkgname "renard/indirect-region")
        (:name indirect-region-zip :type github-zip :description "Act like indirect buffer for region." :pkgname "renard/indirect-region")))

;; Install a http-tar recipe
(el-get 'sync 'indirect-region-http-tar)
;; Install a github-tar recipe
(el-get 'sync 'indirect-region-tar)
;; Install a github-zip recipe
(el-get 'sync 'indirect-region-zip)

(condition-case err
    (progn
      ;; Should fail
      (let ((el-get-sources
             '((:name broken-pkg
                      :type github-tar))))
        (el-get 'sync 'broken-pkg))
      (signal 'test-failure
              '("The package\"broken-pkg\" should have caused an error, but it didn't.")))
  (error (message "Installing \"broken-pkg\" failed as expected. The error message was: %S" err)))
