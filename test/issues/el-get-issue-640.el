;; https://github.com/dimitri/el-get/issues/640
;;
;; void-function el-get-package-name

(setq debug-on-error t
      el-get-verbose t
      el-get-default-process-sync t)

;; Install pkg with type builtin
(let ((el-get-sources
       (list `(:name pkg
                     :type builtin))))
  (el-get 'sync 'pkg))
;; Force a re-read of the status file after installing and updating
(setq el-get-status-file-cache nil)
(el-get-read-status-file)
