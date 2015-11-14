;; https://github.com/dimitri/el-get/issues/672
;;
;; Status file migration fails with removed and unavailable recipe

;; Just install some package to make sure el-get creates the
;; appropriate directories
(setq el-get-sources
      (list '(:name a
                    :type builtin))
      debug-on-error t)
(el-get-install 'a)

;; Set up the status file with a removed package that has no current
;; recipe available.
(with-temp-buffer
  (insert (el-get-print-to-string
           '(:nonexistent-package "removed")))
  (write-file el-get-status-file))

;; Now try to read the status file. Without a fix, it will try to read
;; the recipe for "nonexistent-package" and fail, throwing an error.
(el-get-read-status-file)

;; Try to write package's status as "removed", which will also convert
;; the status file to the new format. Without a fix, it will again try
;; and fail to find the nonexistent recipe.
(el-get-save-package-status 'nonexistent-package "removed")

;; Now the list of package status recipes should be nil because no
;; packages are installed.
(assert (equal (el-get-package-status-recipes) nil) nil)
