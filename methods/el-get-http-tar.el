;;; el-get --- Manage the external elisp bits and pieces you depend upon
;;
;; Copyright (C) 2010-2011 Dimitri Fontaine
;;
;; Author: Dimitri Fontaine <dim@tapoueh.org>
;; URL: http://www.emacswiki.org/emacs/el-get
;; GIT: https://github.com/dimitri/el-get
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/
;;
;; This file is NOT part of GNU Emacs.
;;
;; Install
;;     Please see the README.md file from the same distribution

(require 'el-get-http)

(defun el-get-http-unpack-cleanup-extract-hook (package)
  "Cleanup after tar xzf: if there's only one subdir, move all the files up."

  (let* ((pdir    (el-get-package-directory package))
	 (url     (plist-get (el-get-package-def package) :url))
	 (tarfile (el-get-filename-from-url url))
	 (files   (remove tarfile (directory-files pdir nil "[^.]$")))
	 (dir     (car files)))
    (el-get-verbose-message "el-get: unpack cleanup %s [%s]: %S" package pdir files)
    ;; if there's only one directory, move its content up and get rid of it
    (when (and (not (cdr files))
               (file-directory-p (expand-file-name dir pdir)))
      (loop for fname in (directory-files
			  (expand-file-name dir pdir) nil "[^.]$")
	    for fullname = (expand-file-name fname (expand-file-name dir pdir))
	    for newname  = (expand-file-name fname pdir)
	    do (progn
		 (el-get-verbose-message "mv %S %S" fullname newname)
		 (rename-file fullname newname)))
      (el-get-verbose-message "delete-directory: %s" (expand-file-name dir pdir))
      (delete-directory (expand-file-name dir pdir)))))

(defun el-get-http-tar-install (package url post-install-fun)
  "Dowload a tar archive package over HTTP."
  (let* ((source  (el-get-package-def package))
	 (options (plist-get source :options))
	 (pdir    (el-get-package-directory package))
	 (tarfile (el-get-filename-from-url url))
	 (dest    (concat (file-name-as-directory pdir) tarfile))
	 (name    (format "*tar %s %s*" options url))
	 (ok      (format "Package %s installed." package))
	 (ko      (format "Could not install package %s." package))
	 (post `(lambda (package)
                  ;; Remove all files from previous install before
                  ;; extracting the tar file.
                  (let ((files-to-delete (remove ,tarfile (directory-files ,pdir nil "[^.]$"))))
                    (loop for fname in files-to-delete
                          for fullpath = (expand-file-name fname ,pdir)
                          do (el-get-verbose-message "el-get-http-tar: Deleting old file %S" fname)
                          do (if (file-directory-p fullpath)
                                 (delete-directory fullpath 'recursive)
                               (delete-file fullpath))))
		  ;; tar xzf `basename url`
		  (let ((el-get-sources '(,@el-get-sources)))
		    (el-get-start-process-list
		     package
		     '((:command-name ,name
				      :buffer-name ,name
				      :default-directory ,pdir
				      :program ,(executable-find "tar")
				      :args (,@options ,tarfile)
				      :message ,ok
				      :error ,ko))
		     ,(symbol-function post-install-fun))))))
    (el-get-http-install package url post dest)))

(add-hook 'el-get-http-tar-install-hook 'el-get-http-unpack-cleanup-extract-hook)

(el-get-register-method :http-tar
  :install #'el-get-http-tar-install
  :update #'el-get-http-tar-install
  :remove #'el-get-rmdir
  :install-hook #'el-get-http-tar-install-hook
  :update-hook #'el-get-http-tar-install-hook)

(provide 'el-get-http-tar)
