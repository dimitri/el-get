;; https://github.com/dimitri/el-get/issues/650
;;
;; Take more advantage of saved status recipes

;; Define a checksum method just for testing purposes
(defun el-get-builtin-compute-checksum (package &rest ignored)
  "A builtin package always has a checksum of zero."
  "0")

(el-get-register-derived-method :builtin-with-checksum :builtin
  :compute-checksum #'el-get-builtin-compute-checksum)

(setq debug-on-error t
      el-get-verbose t
      el-get-default-process-sync t)

;; Install pkg with type builtin
(let ((el-get-sources
       (list `(:name pkg
                     :type builtin-with-checksum
                     :checksum "0"))))
  (el-get 'sync 'pkg))
;; Now, with the recipe no longer in `el-get-sources', do a bunch of
;; things that require the recipe, to make sure that they obtain it
;; from the status file.
(el-get-checksum 'pkg)
(el-get-reload 'pkg)
(el-get-remove 'pkg)
