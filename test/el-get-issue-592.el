;; https://github.com/dimitri/el-get/issues/592
;;
;; old elc files interfere with compilation on update

(require 'cl)
(let* ((debug-on-error t)
      ;; (el-get-byte-compile nil)
      (el-get-verbose t)
      (el-get-default-process-sync t)
      (recipe-stub
       '(:name test-pkg
               :type builtin
               :build (("echo" "BUILDING TEST PACKAGE")
                       ("sh" "-c" "echo 't' > test-pkg.el"))))
      (recipe1 (append recipe-stub '(:compile ".")))
      (recipe2 (append recipe-stub '(:compile nil))))
  ;; Install with recipe1, which does compilation
  (let ((el-get-sources (list recipe1)))
    (el-get 'sync 'test-pkg)
    ;; Verify that the file was compiled
    (assert (file-exists-p (expand-file-name
                            "test-pkg.elc"
                            (el-get-package-directory 'test-pkg)))
            nil
            "test-pkg.el should be compiled"))
  ;; Now do an update with recipe2, which does not want anything
  ;; compiled
  (let ((el-get-sources (list recipe2)))
    (el-get-update 'test-pkg)
    ;; Verify that the file was compiled
    (assert (not (file-exists-p (expand-file-name
                                 "test-pkg.elc"
                                 (el-get-package-directory 'test-pkg))))
            nil
            "test-pkg.el should no longer be compiled")))
