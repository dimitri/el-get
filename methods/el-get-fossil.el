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

(require 'el-get-core)
(require 'el-get-custom)

(defcustom el-get-fossil-clone-hook nil
  "Hook run after fossil clone"
  :group 'el-get
  :type 'hook)

(defcustom el-get-fossil-dir nil
  "Define where to store .fossils for packages, if nil .fossils
are stored in the package directory"
  :group 'el-get
  :type 'directory)

(defun el-get-fossil-executable ()
  "Return fossil executable to use, or signal error when not found."
  (let ((fossil-executable (executable-find "fossil")))
    (unless (and fossil-executable (file-executable-p fossil-executable))
      (error
       (concat "el-get-fossil-clone requires the binary `fossil' to be "
               "found in your PATH")))
    fossil-executable))

(defun el-get-fossil-clone (package url post-install-fun)
  "Clone the given package following the URL."
  (let* ((fossil-executable (el-get-executable-find "fossil"))
         (pdir (el-get-package-directory package))
         (pname (el-get-as-string package))
         (fossil-name (format "%s.fossil" pname))
         (name (format "*fossil clone %s*" package))
         (source (el-get-package-def package))
         ;; Fossil can check out any commit by tag or hash, so they
         ;; are all treated equally
         (checkout (or (plist-get source :checkout)
                       (plist-get source :branch)
                       (plist-get source :tag)
                       "trunk"))
         (clone-args (list "clone" url fossil-name))
         ;; Allow per-recipe overrides for .fossil location
         (fossil-dir (or (plist-get source :fossil-dir)
                         el-get-fossil-dir
                         pdir))
         (open-args (list "open" "--nested" (expand-file-name fossil-name fossil-dir) checkout))
         (ok (format "Package %s installed." package))
         (ko (format "Could not install package %s." package)))
    (el-get-insecure-check package url)

    (el-get-start-process-list
     package
     (list
      ;; Create the location for the fossil package
      `(:command-name ,(format "*mkdir %s*" pdir)
                      :buffer-name ,name
                      :default-directory ,el-get-dir
                      :program "mkdir"
                      :args ,(list pdir)
                      :message ,(format "Created folder for package %s." package)
                      :error ,(format "Could not create folder for package %s." package))
      ;; Obtain the .fossil to be able to open it
      (list :command-name name
            :buffer-name name
            :default-directory fossil-dir
            :program fossil-executable
            :args clone-args
            :message (format "Fossil package %s cloned" package)
            :error (format "Could not clone Fossil package %s" package))
      ;; Open the .fossil
      (list :command-name (format "*fossil open %s*" fossil-name)
            :buffer-name name
            :default-directory pdir
            :program fossil-executable
            :args open-args
            :message ok
            :error ko))
     post-install-fun)))


(defun el-get-fossil-update (package url post-update-fun)
  "fossil update the package"
  (let* ((fossil-executable (el-get-executable-find "fossil"))
         (pdir (el-get-package-directory package))
         (name (format "*fossil update %s*" package))
         (source (el-get-package-def package))
         ;; If no checkout is specified, "current" updates to tip of
         ;; the current branch
         (checkout (or (plist-get source :checkout)
                       (plist-get source :checksum)
                       "current"))
         (update-args (list "update" checkout))
         (ok (format "Updated package %s." package))
         (ko (format "Could not update package %s." package)))
    (el-get-insecure-check package url)

    (el-get-start-process-list
     package
     `((:command-name ,name
                      :buffer-name ,name
                      :default-directory ,pdir
                      :program ,fossil-executable
                      :args ,update-args
                      :message ,ok
                      :error ,ko))
     post-update-fun)))

(defun el-get-fossil-info (package)
  "Obtain current revision and status of PACKAGE.

Fossil provides a large amount of information about the current
checkout of the repository using this command."
  (with-temp-buffer
    (cd (el-get-package-directory package))
    (let* ((fossil-executable (el-get-executable-find "fossil"))
           (args "info")
           (error (call-process fossil-executable nil t nil args))
           (fossil-info
            (split-string
             ;; Fix multi-line comment output to be a single string
             (replace-regexp-in-string "\n +" " " (buffer-string))
             "\n" t)))
      (assert (= error 0) nil
              "Package %s is not a fossil package" package)
      (loop for x in fossil-info
            collect (split-string x ": +")))))

(defun el-get-fossil-compute-checksum (package)
  "Return the hash of the checked-out revision of PACKAGE."
  (let* ((fossil-info (el-get-fossil-info package)))
    ;; checkout returns current hash as well as commit date, only need
    ;; the hash so strip the rest
    (car (split-string
          (cadr (assoc "checkout" fossil-info))
          " "))))

(defun el-get-fossil-rmdir (package url post-remove-fun)
  "Ensure .fossil is deleted as well as the package directory.

Since .fossils can be stored elsewhere, ensure that it is found
and removed when removing the package.  Then remove the package
using `el-get-rmdir' as usual."
  (ignore-errors
    ;; Try to find the .fossil file, remove it if it exists.
    (let* ((fossil-info (el-get-fossil-info package))
           (fossil-location (cadr (assoc "repository" fossil-info))))
      (when (file-exists-p fossil-location)
        (el-get-verbose-message "%s found and removed"
                                (file-name-nondirectory fossil-location))
        (delete-file fossil-location))))
  ;; Remove the package as usual
  (el-get-rmdir package url post-remove-fun))

(el-get-register-method :fossil
  :install #'el-get-fossil-clone
  :update #'el-get-fossil-update
  :remove #'el-get-fossil-rmdir
  :install-hook 'el-get-fossil-clone-hook
  :compute-checksum #'el-get-fossil-compute-checksum)

(provide 'el-get-fossil)
