;; From https://github.com/purcell/emacs.d/raw/master/init-el-get.el

(let ((n (remove nil
		 (mapcar (lambda (s)
			   (unless
			       (file-exists-p (format "~/dev/emacs/el-get/recipes/%s.el" (el-get-source-name s))) s))
			 (loop for r in purcell-el-get-sources when (not (symbolp r)) collect r)))))
  (loop for s in n
	do (with-temp-file (format "~/temp/purcell-el-get-recipes/%s.el" (el-get-source-name s))
	     (insert (prin1-to-string s)))))
