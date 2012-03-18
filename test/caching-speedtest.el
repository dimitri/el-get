;; Benchmark for caching the package status alist

(require 'cl)

(defmacro timeit (&rest body)
  "Run body and report real time taken to do so."
  `(let ((start-time (float-time)))
     (progn ,@body)
     (let ((end-time (float-time)))
       (message "Completed in %.4f seconds" (- end-time start-time))
       (- end-time start-time))))
(put 'timeit 'lisp-indent-function
     (get 'progn 'lisp-indent-function))

(defmacro suppress-messages (&rest body)
  "Run body with `message' redefined as a no-op."
  `(flet ((message (&rest ignored) nil))
     (progn ,@body)))
(put 'suppress-messages 'lisp-indent-function
     (get 'progn 'lisp-indent-function))

(setq debug-on-error t
      el-get-verbose t
      el-get-default-process-sync t
      el-get-notify-type 'message
      el-get-generate-autoloads nil
      el-get-byte-compile nil
      repetitions 50
      el-get-sources
      (list
       `(:name pkg :type builtin)))

;; Install and remove once to warm up the disk cache and whatever
(suppress-messages
 (el-get-install 'pkg)
 (el-get-remove 'pkg))
(message "Beginning benchmark")
;; Repeat benchmark 5 times to see consistency
(let ((times
       (loop
        for rep from 1 upto 5
        collect
        (timeit
         (suppress-messages
          ;; Benchmark is to repeatedly install, hit the cache 5 times,
          ;; and then remove the same package. Repeat N times.
          (dotimes (x repetitions)
            (el-get-install 'pkg)
            (dotimes (y 5)
              (el-get-read-package-status-recipe 'pkg)
              (el-get-init 'pkg))
            (el-get-remove 'pkg)))))))
  (message "Summary: %S" (mapcar (lambda (n) (string-to-number (format "%.4f" n))) times)))
(message "Finished benchmark")
