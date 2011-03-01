(setq debug-on-error t)
(setq message-log-max t)

(defconst dwa:test-log
  (find-file-noselect "test.log" 'nowarn))

(with-current-buffer dwa:test-log
  ;; in case there's an old buffer hanging around
  (erase-buffer))

(defun dwa:test-result (exit-status)
  (with-current-buffer dwa:test-log
      (insert "\n\n=================== Messages =====================\n\n")
      (insert-buffer "*Messages*")
      (save-buffer))
  (kill-emacs exit-status))
  
(defadvice debugger-setup-buffer (after dwa:debug-exit 0 (debugger-args) activate preactivate)
  (let ((backtrace-buffer (current-buffer)))
    (with-current-buffer dwa:test-log
      (insert "\n\n=================== Backtrace =====================\n\n")
      (insert-buffer backtrace-buffer)))
  (dwa:test-result 666))

;; When el-get is installed, we're done
(run-with-idle-timer 
 1 t (lambda () (message "waiting for el-get...")
       (when (and (featurep 'el-get) (null el-get-outdated-autoloads))
         (dwa:test-result 0)
         )))

;; But if it takes more than 15 seconds, time out
(run-with-idle-timer 
 30 nil (lambda () 
        (with-current-buffer dwa:test-log
          (insert "\n\n** Timeout Reached **\n\n"))
        (dwa:test-result 111)))


;;
;; Test bootstrapping initially
;;
(defconst el-get-root-directory
  (file-name-directory 
   (directory-file-name
    (file-name-directory load-file-name))))

;; Extract the instructions directly from the documentation
(save-window-excursion
  (find-file (concat el-get-root-directory "README.asciidoc"))
  (search-forward-regexp "^-\\{9,\\}
\\(\\(?:.\\|
\\)+?\\)-\\{9,\\}")
  (message "evaluating %s" (match-string 1))
  (eval (read (match-string 1))))
