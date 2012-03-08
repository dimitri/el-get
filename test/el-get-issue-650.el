;; https://github.com/dimitri/el-get/issues/650
;;
;; Take more advantage of saved status recipes

(let ((debug-on-error t)
      ;; (el-get-byte-compile nil)
      (el-get-verbose t)
      (el-get-default-process-sync t))
  ;; Install pkg with type builtin
  (let ((el-get-sources
         (list `(:name pkg
                       :type builtin))))
    (el-get 'sync 'pkg))
  ;; Now remove the package, without having its recipe in
  ;; `el-get-sources'.
  (el-get-remove 'pkg))
