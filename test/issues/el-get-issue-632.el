;; https://github.com/dimitri/el-get/issues/632
;;
;; Do not add package directory to load-path if :load-path property is nil

(require 'cl)
(let* ((debug-on-error t)
       (el-get-verbose t)
       ;; Just need to install something
       (pkg1 'zenburn-theme)
       (pkg2 'color-theme)
       (el-get-sources
        (list `(:name ,pkg1 :load-path nil)
              `(:name ,pkg2))))
  (el-get 'sync pkg1 pkg2)
  (assert (el-get-package-is-installed pkg1))
  (assert (el-get-package-is-installed pkg2))
  (assert (plist-member (el-get-package-def pkg1) :load-path))
  (assert (not (plist-member (el-get-package-def pkg2) :load-path)))
  (let ((normalized-load-path
         (mapcar #'file-name-as-directory
                 (mapcar #'expand-file-name load-path)))
        (pkg-in-load-path
         (lambda (pkg)
           (member (file-name-as-directory (el-get-package-directory pkg))
                   normalized-load-path))))
    (assert (not (funcall pkg-in-load-path pkg1))
            nil "Package directory of %s should not be in `load-path'"
            pkg1)
    (assert (funcall pkg-in-load-path pkg2)
            nil "Package directory of %s should be in `load-path'"
            pkg2)))
