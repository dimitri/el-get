(setq debug-on-error t)
(setq message-log-max t)
(setq eval-expression-debug-on-error t)

(defconst dwa:test-log
  (find-file-noselect "test.log" 'nowarn))

(with-current-buffer dwa:test-log
  ;; in case there's an old buffer hanging around
  (erase-buffer))

(defun el-get-compiled ()
  (file-exists-p 
   (concat
    (mapconcat
     'file-name-as-directory `(,user-emacs-directory "el-get" "el-get") 
     "") "el-get.elc")))
   
(defun dwa:test-result (exit-status)
  (with-current-buffer dwa:test-log
    (insert (format"Testing %s
el-get installed: %s
el-get compiled: %s
autoloads updated: %s"
                   (if (zerop exit-status) 
                       "passed" 
                     (format "failed with status %s" exit-status))
                   (featurep 'el-get)
                   (el-get-compiled)
                   (null el-get-outdated-autoloads)))
      (insert "\n\n=================== Messages =====================\n\n")
      (insert-buffer "*Messages*")
      (save-buffer))
  (kill-emacs exit-status))
  
(defadvice debugger-setup-buffer (after dwa:debug-exit 0 (debugger-args) activate preactivate)
  (message "Entering debugger...")
  (let ((backtrace-buffer (current-buffer)))
    (with-current-buffer dwa:test-log
      (insert "\n\n=================== Backtrace =====================\n\n")
      (insert-buffer backtrace-buffer)))
  (dwa:test-result 666))

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

;; if this takes more than 15 seconds, time out
(run-at-time 15 nil 
 (lambda () 
      (condition-case err
          (with-current-buffer dwa:test-log
            (insert "\n\n** Timeout Reached **\n\n"))
        (dwa:test-result 111)
        ((debug error)
         (dwa:test-result 888)))))

;; When el-get is installed and compiled, and autoloads are generated,
;; we're done.  Check for it every half second.
(run-at-time 0.5 0.5
    (lambda () (message "waiting for el-get installation...")
      (condition-case err
       (when (and (featurep 'el-get)
                  (null el-get-outdated-autoloads)
                  (el-get-compiled))
         (dwa:test-result 0))
       ((debug error)
        (dwa:test-result 777)))))

