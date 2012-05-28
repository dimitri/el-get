;; https://github.com/dimitri/el-get/issues/730
;;
;; el-get-remove needs to be more flexible

(setq versions-to-test (list 0 10 23 24 25 40 500 "24.1.50.1" "21.4" "500.2.3" '(23 3 50 1))
      recipes-to-test (mapcar (lambda (version)
                               `(:name a :type builtin :minimum-emacs-version ,version))
                             versions-to-test)
      el-get-default-process-sync t)

;; Simulate same version, higher version, and lower version
(loop for version in versions-to-test
      do
      (let* ((version-list (el-get-version-to-list version))
             (should-install (not (version-list-< (version-to-list emacs-version)
                                                  version-list)))
             (el-get-sources
              (list `(:name a :type builtin :minimum-emacs-version ,version))))
        (message "Testing installing a package requiring version %S. Current emacs version is %s. Package install is expected to %s."
                 version emacs-version (if should-install "succeed" "fail"))
        (if should-install
            (progn
              (el-get-install 'a)
              (el-get-init 'a)
              (el-get-remove 'a))
          (condition-case err
              (progn
                (el-get-install 'a)
                (el-get-init 'a)
                (el-get-remove 'a)
                (signal 'test-failure
                        '("Package should have thrown an error due to unmet emacs version, but it didn't.")))
            (error (message "Installing package with unmet emacs version failed as expected. The error message was: %S" err))))))
