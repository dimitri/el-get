;; https://github.com/dimitri/el-get/issues/594
;;
;; handle empty/missing PYTHONPATH correctly in pymacs

(let ((debug-on-error t)
      ;; (el-get-byte-compile nil)
      (el-get-verbose t)
      (el-get-default-process-sync t))
  (require 'el-get)
  ;; Test Pymacs recipe with unset PYTHONPATH
  (setenv "PYTHONPATH" nil)
  (el-get 'sync 'pymacs)
  (el-get-init 'pymacs)
  (let ((pp (getenv "PYTHONPATH")))
    (assert pp nil
            "PYTHONPATH should be non-nil")
    (assert (not (string= pp "")) nil
            "PYTHONPATH should be non-empty")
    (assert (not (string-match-p ":" pp)) nil
            "PYTHONPATH should have only one element")))
