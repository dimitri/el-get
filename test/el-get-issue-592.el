;; https://github.com/dimitri/el-get/issues/592
;;
;; old elc files interfere with compilation on update

(let ((debug-on-error t)
      ;; (el-get-byte-compile nil)
      (el-get-verbose t)
      (el-get-default-process-sync t)
      (recipe1
       '(:name test-pkg
               :type builtin
               :build ("echo 't' > test-pkg.el")
               :compile "."))
       (recipe2
        '(:name test-pkg
                :type builtin
                :build ("echo 't' > test-pkg.el")
                :compile nil)))
  (require 'el-get)
  ;; Install with recipe1, which does compilation
  (let ((el-get-sources (list recipe1)))
    (el-get 'sync 'test-pkg)
    ;; Verify that the file was compiled
    (assert (file-exists-p (expand-file-name
                            "test-pkg.elc"
                            (el-get-package-directory 'test-pkg)))
            nil
            "test-pkg.el should be compiled"))
  (let ((el-get-sources (list recipe2)))
    (el-get 'sync 'test-pkg)
    ;; Verify that the file was compiled
    (assert (not (file-exists-p (expand-file-name
                                 "test-pkg.elc"
                                 (el-get-package-directory 'test-pkg))))
            nil
            "test-pkg.el should no longer be compiled")))
