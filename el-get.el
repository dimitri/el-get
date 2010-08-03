;;; el-get.el
;;
;; Manage the external elisp bits and pieces you depend upon
;;
;; Copyright (C) 2010 Dimitri Fontaine
;;
;; Author: Dimitri Fontaine <dim@tapoueh.org>
;; URL: http://www.emacswiki.org/emacs/el-get.el
;; Version: 0.3
;; Created: 2010-06-17
;; Keywords: emacs package elisp install elpa git apt-get fink debian macosx
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/
;;
;; This file is NOT part of GNU Emacs.
;;
;; Install:
;;  1. Install ELPA, see http://tromey.com/elpa/install.html
;;  2. (require 'el-get)
;;  3. then define your el-get-sources
;;  4. Use (el-get) from your .emacs or M-x el-get-install etc.
;;

(require 'dired)
(require 'package nil t) ; that's ELPA, but you can use el-get to install it

(defgroup el-get nil "el-get customization group"
  :group 'convenience)

(defvar el-get-git-clone-hook       nil "Hook run after git clone.")
(defvar el-get-git-svn-clone-hook   nil "Hook run after git svn clone.")
(defvar el-get-apt-get-install-hook nil "Hook run after apt-get install.")
(defvar el-get-fink-install-hook    nil "Hook run after fink install.")
(defvar el-get-elpa-install-hook    nil "Hook run after ELPA package install.")
(defvar el-get-http-install-hook    nil "Hook run after http retrieve.")

(defcustom el-get-methods
  '(:git     (:install el-get-git-clone 
		       :install-hook el-get-git-clone-hook
		       :update el-get-git-pull 
		       :remove el-get-rmdir)
    :git-svn (:install el-get-git-svn-clone
		       :install-hook el-get-git-svn-clone-hook
		       :update el-get-git-svn-update
		       :remove el-get-rmdir)
    :apt-get (:install el-get-apt-get-install 
		       :install-hook el-get-apt-get-install-hook
		       :update el-get-apt-get-install
		       :remove el-get-apt-get-remove)
    :fink    (:install el-get-fink-install 
		       :install-hook el-get-fink-install-hook
		       :update el-get-fink-install
		       :remove el-get-fink-remove)
    :elpa    (:install el-get-elpa-install 
		       :install-hook el-get-elpa-install-hook
		       :update el-get-elpa-update
		       :remove el-get-rmdir)
    :http    (:install el-get-http-install
		       :install-hook el-get-http-install-hook
		       :update el-get-http-install
		       :remove el-get-rmdir))
  "Register methods that el-get can use to fetch and update a given package.

The methods list is a PLIST, each entry has a method name
property which value is another PLIST, which must contain values
for :install, :install-hook, :update and :remove
properties. Those should be the elisp functions to call for doing
the named package action in the given method."
  :type '(repeat (cons symbol function))
  :group 'el-get)

(defvar el-get-dir "~/.emacs.d/el-get/"
  "Define where to fetch the packages.")

(defvar el-get-apt-get (executable-find "apt-get")
  "The apt-get executable.")

(defvar el-get-apt-get-base "/usr/share/emacs/site-lisp"
  "Where to link the el-get symlink to, /<package> will get appended.")

(defvar el-get-fink (executable-find "fink")
  "The fink executable.")

(defvar el-get-fink-base "/sw/share/doc"
  "Where to link the el-get symlink to, /<package> will get appended.")

;; debian uses ginstall-info and it's compatible to fink's install-info on
;; MacOSX, so:
(defvar el-get-install-info (or (executable-find "ginstall-info")
				(executable-find "install-info")))

(defvar el-get-sources nil
  "List of sources for packages.

Each source entry is a PLIST where the following properties are supported:

name

    The name of the package. It can be different from the name of
    the directory where the package is stored (after a `git
    clone' for example, in which case a symlink will be created.

type

    The type of the package, currently el-get offers support for
    `apt-get', `elpa', `git' and `http'. You can easily support
    your own types here, see the variable `el-get-methods'.

url

    Where to fetch the package, only meaningful for `git' and `http' types.

build

    Your build recipe gets there, often it looks like (\"./configure\" \"make\")

load-path

    This should be a list of directories you want `el-get' to add
    to your `load-path'.

info

    This string allows you to setup a directory where to find a
    'package.info' file, or a path/to/whatever.info file. It will
    even run `ginstall-info' for you to create the `dir' entry so
    that C-h i will be able to list the newly installed
    documentation. Note that you might need to kill (C-x k) your
    info buffer then C-h i again to be able to see the new menu
    entry.

load

    List of files to load, or a single file to load after having
    installed the source but before `require'ing its features.

features

    List of features el-get will `require' for you.
")


(defun el-get-method (method-name action)
  "return the function to call for doing action (e.g. install) in given method"
  (let* ((method  (intern-soft (concat ":" (format "%s" method-name))))
	 (actions (plist-get el-get-methods method)))
    (plist-get actions action)))

(defun el-get-check-init ()
  "Check that we can run el-get"
  (unless (file-directory-p el-get-dir)
    (make-directory el-get-dir)))

(defun el-get-package-directory (package)
  "Returns the package installation directory absolute name"
  (concat (file-name-as-directory el-get-dir) package))

(defun el-get-add-path-to-list (package list path)
  "(add-to-list list path) checking for path existence within
given package directory."
  (let* ((pdir     (el-get-package-directory package))
	 (fullpath (expand-file-name (concat (file-name-as-directory pdir) path))))
    (unless (file-directory-p fullpath)
      (error "el-get could not find directory `%s' for package %s" path package))
    (add-to-list list fullpath)))

(defun el-get-package-exists-p (package)
  "true only when the given package name is either a directory or a symlink in el-get-dir"
  (let ((pdir (el-get-package-directory package)))
    ;; seems overkill as file-directory-p will always be true
    (or (file-directory-p pdir)
	(file-symlink-p   pdir))))


;;
;; call-process-list utility, to do same as bash && feature
;;
(defun el-get-call-process-list-sentinel (proc change)
  "When proc has exited and was successful, chain next command"
  (when (eq (process-status proc) 'exit)
    (let ((status  (process-exit-status proc))
	  (cname   (process-get proc :command-name))
	  (cbuf    (process-get proc :buffer-name))
	  (message (process-get proc :message))
	  (errorm  (process-get proc :error))
	  (package (process-get proc :el-get-package))
	  (final-f (process-get proc :el-get-final-func))
	  (next    (process-get proc :el-get-call-process-list)))
      (if (not (eq 0 status))
	  (progn
	    (set-window-buffer (selected-window) cbuf)
	    (error "el-get: %s" cname errorm))
	(message "el-get: %s" message))
      
      (when cbuf (kill-buffer cbuf))
      (if next
	  (el-get-call-process-list package next final-f)
	(funcall final-f package)))))

(defun el-get-call-process-list (package commands final-func)
  "run each command one after the other, in order, stopping at
first error.

commands should be a list of plists with at least the following
properties:

:default-directory

   default-directory from where to start the command

:command-name

   Name of the command to start, gives the name of the Emacs subprocess.

:buffer-name

   Name of the buffer associated with the command.

:program

   The program to start

:args

   The list of arguments for the program to start

:message

   The message to send upon success

:error

   The error to send upon failure

Any other property will get put into the process object.
"
  (let* ((c       (car commands))
	 (cdir    (plist-get c :default-directory))
	 (cname   (plist-get c :command-name))
	 (cbuf    (plist-get c :buffer-name))
	 (killed  (when (get-buffer cbuf) (kill-buffer cbuf)))
	 (program (plist-get c :program))
	 (args    (plist-get c :args))
	 (default-directory (if cdir (file-name-as-directory cdir) 
			      default-directory))
	 (proc    (apply 'start-process cname cbuf program args)))

    ;; add the properties to the process, then set the sentinel
    (mapc (lambda (x) (process-put proc x (plist-get c x))) c)
    (process-put proc :el-get-package package)
    (process-put proc :el-get-final-func final-func)
    (process-put proc :el-get-call-process-list (cdr commands))
    (set-process-sentinel proc 'el-get-call-process-list-sentinel)))


;;
;; git support
;;
(defun el-get-git-executable ()
  "return git executable to use, or signal an error when not found"
  (let ((git-executable (if (file-executable-p magit-git-executable)
			    magit-git-executable
			  (executable-find "git"))))
    (unless (file-executable-p git-executable)
      (error 
       (concat "el-get-git-clone requires `magit-git-executable' to be set, "
	       "or the binary `git' to be found in your PATH")))
    git-executable))

(defun el-get-git-clone (package url post-install-fun)
  "clone the given package following the url"
  (let* ((git-executable (el-get-git-executable))
	 (name (format "*git clone %s*" package))
	 (ok   (format "Package %s installed." package))
	 (ko   (format "Could not install package %s." package)))

    (el-get-call-process-list 
     package
     `((:command-name ,name
		      :buffer-name ,name
		      :default-directory ,el-get-dir
		      :program ,git-executable
		      :args ( "--no-pager" "clone" ,url ,package)
		      :message ,ok
		      :error ,ko))
     post-install-fun)))

(defun el-get-git-pull (package url post-update-fun)
  "git pull the package"
  (let* ((git-executable (el-get-git-executable))
	 (pdir (el-get-package-directory package))
	 (name (format "*git pull %s*" package))
	 (ok   (format "Pulled package %s." package))
	 (ko   (format "Could not update package %s." package)))

    (el-get-call-process-list 
     package
     `((:command-name ,name
		      :buffer-name ,name
		      :default-directory ,pdir
		      :program ,git-executable
		      :args ( "--no-pager" "pull")
		      :message ,ok
		      :error ,ko))
     post-update-fun)))


;;
;; git-svn support
;;
(defun el-get-git-svn-clone (package url post-install-fun)
  "Clone the given svn PACKAGE following the URL using git."
  (let ((git-executable (el-get-git-executable))
	(name (format "*git svn clone %s*" package))
	(ok   (format "Package %s installed." package))
	(ko   (format "Could not install package %s." package)))

    (el-get-call-process-list 
     package
     `((:command-name ,name
		      :buffer-name ,name
		      :default-directory ,el-get-dir
		      :program ,git-executable
		      :args ( "--no-pager" "svn" "clone" ,url ,package)
		      :message ,ok
		      :error ,ko))
     post-install-fun)))

(defun el-get-git-svn-update (package url post-update-fun)
  "Update PACKAGE using git-svn. URL is given for compatibility reasons."
  (let ((git-executable (el-get-git-executable))
	(pdir   (el-get-package-directory package))
	(f-name (format "*git svn fetch %s*" package))
	(f-ok   (format "Fetched package %s." package))
	(f-ko   (format "Could not fetch package %s." package))
	(r-name (format "*git svn rebase %s*" package))
	(r-ok   (format "Rebased package %s." package))
	(r-ko   (format "Could not rebase package %s." package)))

    (el-get-call-process-list
     package
     `((:command-name ,f-name
		      :buffer-name ,f-name
		      :default-directory ,pdir
		      :program ,git-executable
		      :args ("--no-pager" "svn" "fetch")
		      :message ,f-ok
		      :error ,f-ko)
       
       (:command-name ,r-name
		      :buffer-name ,r-name
		      :default-directory ,pdir
		      :program ,git-executable
		      :args ("--no-pager" "svn" "rebase")
		      :message ,r-ok
		      :error ,r-ko))
     post-update-fun)))


;;
;; apt-get support
;;
(defun el-get-sudo-shell-command-to-string (command) 
  "Ask a password then use sudo, when required"
  (let* ((sudo-test (shell-command-to-string "sudo echo el-get"))
	 (sudo-pass (if (string= sudo-test "el-get\n")
			nil
		      (read-passwd "sudo password: "))))
    (shell-command-to-string
     (concat (when sudo-pass (concat "echo " sudo-pass " | "))
	     "sudo -S " command))))

(defun el-get-apt-get-symlink (basedir package)
  "ln -s /usr/share/emacs/site-lisp/package ~/.emacs.d/el-get/package"
  (let ((debdir (concat (file-name-as-directory basedir) package)))
    (shell-command 
     (concat "cd " el-get-dir " && ln -s " debdir  " " package))))

(defun el-get-apt-get-package-status (package)
  "returns the package status from dpkg --get-selections"
  (substring 
   (shell-command-to-string 
    (format
     "dpkg -l %s| awk '/^ii/ && $2 = \"%s\" {print \"ok\"}'" package package)) 0 -1))

(defun el-get-apt-get-install (package &optional url)
  "apt-get install package, url is there for API compliance"
  ;; this can be somewhat chatty but I guess you want to know about it
  (message "%S" (el-get-sudo-shell-command-to-string 
		 (concat el-get-apt-get " install " package)))
  (unless (string= (el-get-apt-get-package-status package) "install")
    (error "Error: apt-get reports package %s status is not \"install\"" package))
  (el-get-apt-get-symlink el-get-apt-get-base package)
  (run-hooks 'el-get-apt-get-install-hook)
  nil)

(defun el-get-apt-get-remove (package &optional url)
  "apt-get remove package, url is there for API compliance"
  ;; this can be somewhat chatty but I guess you want to know about it
  (message "%S" (el-get-sudo-shell-command-to-string 
		 (concat el-get-apt-get " remove -y " package))))


;;
;; fink support
;;
(defun el-get-fink-install (package &optional url)
  "fink install package, url is there for API compliance"
  ;; this can be somewhat chatty but I guess you want to know about it
  (message "%S" (el-get-sudo-shell-command-to-string 
		 (concat el-get-fink " install " package)))
  (unless (string= (el-get-apt-get-package-status package) "ok")
    (error "Error: fink reports package %s status is not \"ok\"" package))
  (el-get-apt-get-symlink el-get-fink-base package)
  (run-hooks 'el-get-fink-install-hook)
  nil)

(defun el-get-fink-remove (package &optional url)
  "fink remove package, url is there for API compliance"
  ;; this can be somewhat chatty but I guess you want to know about it
  (message "%S" (el-get-sudo-shell-command-to-string 
		 (concat el-get-fink " remove -r " package))))


;;
;; ELPA support
;;
(defun el-get-elpa-package-directory (package)
  "return the directory where ELPA stores a package, or nil if
package isn't currently installed by ELPA."
  (let* ((pname (format "%s" package))  ; easy way to cope with symbols etc.

	 (l
	  ;; we use try-completion to find the realname of the directory
	  ;; ELPA used, and this wants an alist, we trick ls -i -1 into
	  ;; that.
	  (mapcar 'split-string
		  (split-string
		   (shell-command-to-string
		    (concat "ls -i1 "
			    (file-name-as-directory package-user-dir))))))

	 (realname (try-completion pname l)))

    (if realname (concat (file-name-as-directory package-user-dir) realname) 
      realname)))

(defun el-get-elpa-symlink-package (package)
  "ln -s ~/.emacs.d/elpa/<package> ~/.emacs.d/el-get/<package>"
  (let ((elpa-dir (el-get-elpa-package-directory package)))
    (unless (el-get-package-exists-p package)
      (shell-command
       (concat "cd " el-get-dir 
	       " && ln -s \"" elpa-dir "\" \"" package "\"")))))

(defun el-get-elpa-install (package &optional url)
  "ask elpa to install given package"
  (let ((elpa-dir (el-get-elpa-package-directory package)))
    (unless (and elpa-dir (file-directory-p elpa-dir))
      (package-install (intern-soft package)))
    ;; we symlink even when the package already is installed because it's
    ;; not an error to have installed ELPA packages before using el-get, and
    ;; that will register them
    (el-get-elpa-symlink-package package)))
  
(defun el-get-elpa-update (package &optional url)
  "ask elpa to update given package"
  (el-get-rmdir (concat (file-name-as-directory package-user-dir) package))
  (package-install (intern-soft package)))


;;
;; http support
;;
(defun el-get-http-retrieve (package url)
  "Get an elisp file using `url-retrieve-synchronously'. 

Returns the feature provided if any"
  (let* ((pdir   (el-get-package-directory package))
	 (dest   (concat (file-name-as-directory pdir) package ".el")))
    (with-current-buffer (url-retrieve-synchronously url)
      ;; prune HTTP headers before save
      (beginning-of-buffer)
      (re-search-forward "^$" nil 'move)
      (forward-char)
      (delete-region (point-min) (point))
      (write-file dest))))

(defun el-get-http-install (package url)
  "Dowload a single-file package over HTTP "
  (let ((pdir   (el-get-package-directory package)))
    (unless (file-directory-p pdir)
      (make-directory pdir))
    (el-get-http-retrieve package url)))

;;
;; Common support bits
;;
(defun el-get-rmdir (package url)
  "Just rm -rf the package directory. Follow symlinks."
  (dired-delete-file (el-get-package-directory package) 'always))

(defun el-get-build (package commands &optional subdir)
  "run each command from the package directory"
  (let* ((pdir   (el-get-package-directory package))
	 (wdir   (if subdir (concat (file-name-as-directory pdir) subdir) pdir)))
    (dolist (c commands)
      (message "el-get %s: cd %s && %s" package wdir c)
      (message "%S" (shell-command-to-string 
		     (concat "cd " wdir " && " c))))))

(defun el-get-package-def (package)
  "Return a single `el-get-sources' entry for given package"
  (dolist (s el-get-sources source)
    (when (string= package (format "%s" (plist-get s :name)))
      (setq source s))))


;;
;; User Interface, Interactive part
;;
(defun el-get-read-package-name (action &optional package)
  "Ask user for a package name in minibuffer, with completion.

When given a package name, check for its existence"
  (let ((plist 
	  (mapcar (lambda (x) (format "%s" (plist-get x :name))) el-get-sources)))
    (if package
	(unless (member package plist)
	  (error "el-get: can not find package name `%s' in `el-get-sources'" package))
      (completing-read (format "%s package: " action) plist))))

(defun el-get-init (&optional package)
  "Care about load-path, Info-directory-list, and (require 'features)"
  (interactive)
  (let* ((package  (or package (el-get-read-package-name "Init" package)))
	 (source   (el-get-package-def package))
	 (method   (plist-get source :type))
	 (loads    (plist-get source :load))
	 (feats    (plist-get source :features))
	 (el-path  (or (plist-get source :load-path) '(".")))
	 (infodir  (plist-get source :info))
	 (pdir     (el-get-package-directory package)))

    ;; apt-get and ELPA will take care of load-path, Info-directory-list
    (unless (member method '(elpa apt-get))
      ;; append entries to load-path and Info-directory-list
      (mapc (lambda (path) 
	      (el-get-add-path-to-list package 'load-path path))
	    (if (stringp el-path) (list el-path) el-path))

      (let* ((infodir-abs-conf (concat (file-name-as-directory pdir) infodir))
	     (infodir-abs (if (file-directory-p infodir-abs-conf)
			      infodir-abs-conf
			    (file-name-directory infodir-abs-conf)))
	     (infodir-rel (if (file-directory-p infodir-abs-conf)
			      infodir
			    (file-name-directory infodir)))
	     (infofile (if (and (file-exists-p infodir-abs-conf)
				(not (file-directory-p infodir-abs-conf)))
			   infodir-abs-conf
			 (concat (file-name-as-directory infodir-abs) package))))

	(when (and infodir (file-directory-p infodir-abs))
	  (require 'info)
	  (info-initialize)
	  ;; add to Info-directory-list
	  (el-get-add-path-to-list package 'Info-directory-list infodir-rel)
	  ;; build the infodir entry, too
	  (el-get-build
	   package
	   `(,(format "%s %s.info dir" el-get-install-info infofile)) infodir-rel))))

    ;; loads
    (when loads
      (mapc (lambda (file) 
	      (let ((pfile (concat (file-name-as-directory pdir) file)))
		(unless (file-exists-p pfile)
		  (error "el-get could not find file '%s'" pfile))
		(message "load '%s'" pfile)
		(load pfile)))
	    (if (stringp loads) (list loads) loads)))

    ;; features, only ELPA will handle them on its own
    (unless (eq method 'elpa)
      ;; if a feature is provided, require it now
      (when feats 
	(mapc (lambda (feature) (message "require '%s" (require feature)))
	      (if (symbolp feats) (list feats) feats))))

      ;; return the package
      package))

(defun el-get-post-install (package)
  "Post install a package. This will get run by a sentinel."
  (let* ((source   (el-get-package-def package))
	 (hooks    (el-get-method (plist-get source :type) :install-hook))
	 (commands (plist-get source :build)))
    ;; post-install is the right place to run install-hook
    (run-hooks hooks)
    ;; consider building when sources are thus setup
    (el-get-build package commands)
    ;; and init
    (el-get-init package)))

(defun el-get-install (&optional package)
  "Install given package. Read the package name with completion when not given."
  (interactive)
  (let* ((package (or package (el-get-read-package-name "Install" package)))
	 (source   (el-get-package-def package))
	 (method   (plist-get source :type))
	 (install  (el-get-method method :install))
	 (url      (plist-get source :url)))

    ;; check we can install the package
    (el-get-check-init)

    ;; and install the package now
    (message "el-get install %s" package)
    (funcall install package url 'el-get-post-install)))

(defun el-get-post-update (package)
  "Post update a package. This will get run by a sentinel."
  (let* ((source   (el-get-package-def package))
	 (commands (plist-get (plist-get source :type) :build)))
    (el-get-build package commands)))

(defun el-get-update (&optional package)
  "Update given package. Read the package name with completion when not given."
  (interactive)
  (let* ((package (or package (el-get-read-package-name "Install" package)))
	 (source   (el-get-package-def package))
	 (method   (plist-get source :type))
	 (update   (el-get-method method :update))
	 (url      (plist-get source :url))
	 (commands (plist-get source :build)))
    ;; update the package now
    (message "el-get update %s" package)
    (funcall update package url 'el-get-post-update)))

(defun el-get-remove (&optional package)
  "Remove given package. Read the package name with completion when not given."
  (interactive)
  (let* ((package (or package (el-get-read-package-name "Install" package)))
	 (source   (el-get-package-def package))
	 (method   (plist-get source :type))
	 (remove   (el-get-method method :remove))
	 (url      (plist-get source :url)))
    ;; remove the package now
    (message "el-get remove %s" package)
    (funcall remove package url)))

(defun el-get-cd (&optional package)
  "Open dired in the package directory."
  (interactive)
  (let* ((package (or package (el-get-read-package-name "Install" package)))
	 (pdir    (el-get-package-directory package)))
    (dired pdir)))


;;
;; User Interface, Non Interactive part
;;
(defun el-get ()
  "Check that all sources have been downloaded once, and init them as needed.

This will not update the sources by using `apt-get install' or
`git pull', but it will ensure the sources have been installed
and will set the load-path and Info-directory-list depending on
the el-get-sources setup.

el-get is also responsible for doing (require 'feature) for each
and every feature declared in `el-get-sources', so that it's
suitable for use in your emacs init script.
"
  (mapcar 
   (lambda (source)
     (let* ((package  (format "%s" (plist-get source :name))))
       ;; check if the package needs to be fetched (and built)
       (if (el-get-package-exists-p package)
	   (el-get-init package)
	 (el-get-install package))))
   el-get-sources))

(provide 'el-get)
