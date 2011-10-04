(require 'el-get-core)

(defcustom el-get-builtin-install-hook nil
  "Hook run after 'installing' a builtin package."
  :group 'el-get
  :type 'hook)

(defun el-get-builtin-install (package url post-install-fun)
  (let ((pdir (el-get-package-directory package)))
    (unless (file-directory-p pdir)
      (make-directory pdir))
    (funcall post-install-fun package)))

(el-get-register-method
 :builtin #'el-get-builtin-install #'el-get-builtin-install #'el-get-rmdir
 #'el-get-builtin-install-hook)

(provide 'el-get-builtin)
