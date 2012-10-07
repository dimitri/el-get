(require 'cl)
(el-get-register-method-alias :test :builtin)

(let ((debug-on-error t)
      (el-get-default-process-sync t)
      (el-get-verbose t)
      (el-get-auto-update-cached-recipes t)
      (a-rcp-1
       '(:name a
               :type test
               :before (message "Before A")
               :after (message "After A")))
      (a-rcp-2
       '(:name a
               :type test
               :before (message "Before A")))
      (test
       (lambda (func rcp)
         (let ((el-get-sources (list rcp)))
           (funcall func 'a)
           (assert (equal (el-get-package-status-recipes) el-get-sources)
                   nil "Cached recipe is different from given source.")))))

  (funcall test #'el-get-install a-rcp-1)
  (funcall test #'el-get-update a-rcp-2))
