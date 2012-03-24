;; https://github.com/dimitri/el-get/issues/683
;;
;; el-get-remove needs to be more flexible

(setq el-get-sources (list '(:name a :type builtin))
      el-get-default-process-sync t)

(defun assert-package-fully-removed (pkg)
  (assert (string= "removed"
                   (el-get-read-package-status pkg))
          nil
          "Package %s should have status \"removed\", but actually has status \"%s\"."
          pkg (el-get-read-package-status pkg))
  (assert (not (or (file-exists-p (el-get-package-directory pkg))
                   (file-symlink-p (el-get-package-directory pkg))))
          nil
          "Package directory for %s should not exist, but it does" pkg))

;; Try to just install and remove
(el-get-install 'a)
(el-get-remove 'a)
(assert-package-fully-removed 'a)

;; Install, then set status to "required", then uninstall.
(el-get-install 'a)
(el-get-save-package-status 'a "required")
(el-get-remove 'a)
(assert-package-fully-removed 'a)

;; Install, then set status to "removed", then uninstall.
(el-get-install 'a)
(el-get-save-package-status 'a "removed")
(el-get-remove 'a)
(assert-package-fully-removed 'a)

;; Set status to "installed" but don't actually install, then remove
(el-get-save-package-status 'a "installed")
(el-get-remove 'a)
(assert-package-fully-removed 'a)

;; Install, then set status to "required", then call install again,
;; which should remove and then reinstall.
(el-get-install 'a)
(el-get-save-package-status 'a "required")
(el-get-install 'a)
(el-get-remove 'a)
(assert-package-fully-removed 'a)

;; Install, then set status to "required", then call install again,
;; which should remove and then reinstall.
(el-get-install 'a)
(el-get-save-package-status 'a "required")
(el-get-install 'a)
(el-get-remove 'a)
(assert-package-fully-removed 'a)

;; Create the package directory but don't install it. Then try to
;; install, which should remove and reinstall.
(make-directory (el-get-package-directory 'a) 'recursive)
(el-get-install 'a)
(el-get-remove 'a)
(assert-package-fully-removed 'a)

;; Install, then set the recipe to nil in the status file, then uninstall
(el-get-install 'a)
(flet ((el-get-package-def (&rest ignored) nil))
  (el-get-save-package-status 'a "installed"))
(el-get-remove 'a)
(assert-package-fully-removed 'a)
