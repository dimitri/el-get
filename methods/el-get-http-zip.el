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
(require 'el-get-http-tar)

(defun el-get-http-zip-install (package url post-install-fun)
  "Dowload a zip archive package over HTTP."
  (let* ((source  (el-get-package-def package))
         (options (plist-get source :options))
         (pdir    (el-get-package-directory package))
         (zipfile (el-get-filename-from-url url))
         (dest    (concat (file-name-as-directory pdir) zipfile))
         (name    (format "*unzip %s %s*" options url))
         (ok      (format "Package %s installed." package))
         (ko      (format "Could not install package %s." package))
         (post `(lambda (package)
                  ;; Remove all files from previous install before
                  ;; extracting the tar file.
                  (let ((files-to-delete (remove ,zipfile (directory-files ,pdir nil "[^.]$"))))
                    (loop for fname in files-to-delete
                          for fullpath = (expand-file-name fname ,pdir)
                          do (el-get-verbose-message "el-get-http-tar: Deleting old file %S" fname)
                          do (if (file-directory-p fullpath)
                                 (delete-directory fullpath 'recursive)
                               (delete-file fullpath))))
                  ;; verify checksum before operating on untrusted data
                  (el-get-verify-checksum package)
                  ;; unzip `basename url`
                  (let ((el-get-sources '(,@el-get-sources)))
                    (el-get-start-process-list
                     package
                     '((:command-name ,name
                                      :buffer-name ,name
                                      :default-directory ,pdir
                                      :program ,(el-get-executable-find "unzip")
                                      :args (,@options ,zipfile)
                                      :message ,ok
                                      :error ,ko))
                     ,(symbol-function post-install-fun))))))
    (el-get-http-install package url post dest)))

(add-hook 'el-get-http-zip-install-hook 'el-get-http-unpack-cleanup-extract-hook)

(el-get-register-method :http-zip
  :install #'el-get-http-zip-install
  :update #'el-get-http-zip-install
  :remove #'el-get-rmdir
  :install-hook 'el-get-http-zip-install-hook
  :update-hook 'el-get-http-zip-install-hook
  :compute-checksum #'el-get-http-compute-checksum)

(provide 'el-get-http-zip)
