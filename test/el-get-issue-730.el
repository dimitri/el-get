;; https://github.com/dimitri/el-get/issues/730
;;
;; el-get-remove needs to be more flexible

(setq package-a-required-version 24
      el-get-sources (list
                      `(:name a
                              :type builtin
                              :from-emacs-version ,package-a-required-version))
      el-get-default-process-sync t)

;; Simulate same version, higher version, and lower version
(loop for version in (list package-a-required-version
                           (1+ package-a-required-version)
                           (1- package-a-required-version))
      do
      (let ((emacs-major-version version))
        (if (>= emacs-major-version package-a-required-version)
            ;; Package should install without error
            (progn
              (el-get-install 'a)
              (el-get-init 'a)
              (el-get-remove 'a)
              (message "Installing package with met emacs version succeeded."))
          ;; Lower version: package should throw an error
          (condition-case err
              (progn
                (el-get-install 'a)
                (el-get-init 'a)
                (el-get-remove 'a)
                (signal 'test-failure
                        '("Package should have thrown an error due to unmet emacs version, but it didn't.")))
            (error (message "Installing package with unmet emacs version failed as expected. The error message was: %S" err))))))
