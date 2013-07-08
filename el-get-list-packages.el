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

;;
;;
;; Description of packages.  (Code based on `describe-function').
;;
(require 'el-get-core)
(require 'cl)

(define-button-type 'el-get-help-package-def
  :supertype 'help-xref
  'help-function (lambda (package) (find-file (el-get-recipe-filename package)))
  'help-echo (purecopy "mouse-2, RET: find package's recipe"))

(define-button-type 'el-get-help-install
  :supertype 'help-xref
  'help-function (lambda (package)
                   (when (y-or-n-p
                          (format "Do you really want to install `%s'? "
                                  package))
                       (el-get-install package)))
  'help-echo (purecopy "mouse-2, RET: install package"))

(define-button-type 'el-get-help-remove
  :supertype 'help-xref
  'help-function (lambda (package)
                   (when (y-or-n-p
                        (format "Do you really want to uninstall `%s'? "
                                package))
                       (el-get-remove package)))
  'help-echo (purecopy "mouse-2, RET: remove package"))

(define-button-type 'el-get-help-update
  :supertype 'help-xref
  'help-function (lambda (package)
                   (when (y-or-n-p
                        (format "Do you really want to update `%s'? "
                                package))
                       (el-get-update package)))
  'help-echo (purecopy "mouse-2, RET: update package"))

(define-button-type 'el-get-help-describe-package
  :supertype 'help-xref
  'help-function #'el-get-describe
  'help-echo (purecopy "mouse-2, RET: describe package"))

(defun el-get-describe-princ-button (label regex type &rest args)
  "Princ a new button with label LABEL.

The LABEL is made clickable by calling `help-xref-button' for a backwards
matching REGEX with TYPE and ARGS as parameter."
  (princ label)
  (with-current-buffer standard-output
    (save-excursion
      (re-search-backward regex nil t)
      (apply #'help-xref-button 1 type args))))

(defun el-get-guess-website (package)
  (let* ((type (el-get-package-type package))
         (guesser (el-get-method type :guess-website)))
    (when guesser
      (funcall guesser package))))

(defun el-get-describe-1 (package)
  (let* ((psym (el-get-as-symbol package))
         (pname (symbol-name psym))
         (status (el-get-read-package-status package))
         (def (el-get-package-def pname))
         (name (plist-get def :name))
         (website (plist-get def :website))
         (descr (plist-get def :description))
         (type (el-get-package-method def))
         (builtin (plist-get def :builtin))
         (url (plist-get def :url))
         (depends (plist-get def :depends)))
    (princ (format "%s is an `el-get' package.  " name))
    (if (eq type 'builtin)
        (princ (format "It is built-in since Emacs %s" builtin))
      (princ (format "It is currently %s "
                     (if status
                         status
                       "not installed")))
      (cond
       ((string= status "installed")
        (el-get-describe-princ-button "[update]" "\\[\\([^]]+\\)\\]"
                                      'el-get-help-update package)
        (el-get-describe-princ-button "[remove]" "\\[\\([^]]+\\)\\]"
                                      'el-get-help-remove package))
       ((string= status "required")
        (el-get-describe-princ-button "[update]" "\\[\\([^]]+\\)\\]"
                                      'el-get-help-update package))
       (t
        (el-get-describe-princ-button "[install]" "\\[\\([^]]+\\)\\]"
                                      'el-get-help-install package))))
    (princ ".\n\n")

    (let ((website (or website
                       (el-get-guess-website package))))
      (when website
        (el-get-describe-princ-button (format "Website: %s\n" website)
                                      ": \\(.+\\)" 'help-url website)))
    (when descr
      (princ (format "Description: %s\n" descr)))
    (when depends
      (if (listp depends)
          (progn
            (princ "Dependencies: ")
            (loop for i in depends
                  do (el-get-describe-princ-button
                      (format "`%s'" i) "`\\([^`']+\\)"
                      'el-get-help-describe-package i)))
        (princ "Dependency: ")
        (el-get-describe-princ-button
         (format "`%s'" depends) "`\\([^`']+\\)"
         'el-get-help-describe-package depends))
      (princ ".\n"))
    (if (eq type 'builtin)
        (princ (format "The package is built-in since Emacs %s.\n\n" builtin))
      (princ (format "The default installation method is %s %s\n\n" type
                     (if url (format "from %s" url) ""))))
    (princ "Full definition")
    (let ((file (el-get-recipe-filename package)))
      (if (not file)
          (princ ":\n")
        (el-get-describe-princ-button (format " in `%s':\n" file)
                                      "`\\([^`']+\\)"
                                      'el-get-help-package-def package)))
    (princ (el-get-print-to-string def))))

(defun el-get-describe (package)
  "Generate a description for PACKAGE."
  (interactive
   (list
    (el-get-read-package-name "Describe")))

  (if (null package)
      (message "You didn't specify a package")
    (help-setup-xref (list #'el-get-describe package)
		     (called-interactively-p 'interactive))
    (save-excursion
      (with-help-window (help-buffer)
        (el-get-describe-1 package)
        (with-current-buffer standard-output
          (buffer-string))))))

(defcustom el-get-package-menu-view-recipe-function
  'find-file-other-window
  "`find-file' compatible function used to display recipe content
in el-get package menu."
  :group 'el-get
  :type 'symbol)


;;
;; Package Menu
;;
(defvar el-get-package-menu-mode-hook nil
  "Hooks to run after el-get package menu init.")

(defvar el-get-package-menu-mode-map nil
  "Keymap for el-get-package-menu-mode")

(defvar el-get-package-menu-sort-key nil
  "sort packages by key")

(defun el-get-package-menu-get-package-name ()
  (save-excursion
    (beginning-of-line)
    (if (looking-at ". \\([^ \t]*\\)")
		(match-string 1))))

(defun el-get-package-menu-view-recipe ()
  "Show package recipe in a read-only mode."
  (interactive)
  (let* ((package (el-get-package-menu-get-package-name))
         (recipe-file (el-get-recipe-filename package)))
    (funcall el-get-package-menu-view-recipe-function recipe-file)
    (view-mode)))

(defun el-get-package-menu-get-status ()
  (save-excursion
    (beginning-of-line)
    (if (looking-at ". [^ \t]*[ \t]*\\([^ \t\n]*\\)")
		(match-string 1))))

(defun el-get-package-menu-mark (what)
  (unless (eobp)
    (let ((buffer-read-only nil))
      (beginning-of-line)
      (delete-char 1)
      (insert what)
      (forward-line)
	  (setq buffer-read-only t))))

(defun el-get-package-menu-mark-install ()
  (interactive)
  (if (or (string= (el-get-package-menu-get-status) "available")
		  (string= (el-get-package-menu-get-status) "removed"))
	  (el-get-package-menu-mark "I")))

(defun el-get-package-menu-mark-update ()
  (interactive)
  (if (or (string= (el-get-package-menu-get-status) "installed")
		  (string= (el-get-package-menu-get-status) "required"))
	  (el-get-package-menu-mark "U")))

(defun el-get-package-menu-mark-delete ()
  (interactive)
  (if (or (string= (el-get-package-menu-get-status) "installed")
		  (string= (el-get-package-menu-get-status) "required"))
	  (el-get-package-menu-mark "D")))

(defun el-get-package-menu-mark-unmark ()
  (interactive)
  (el-get-package-menu-mark " "))

(defun el-get-package-menu-revert ()
  (interactive)
  (let ((current-point (point)))
	(el-get-package-menu)
	(goto-char current-point)
	(beginning-of-line)))

(defun el-get-package-menu-execute ()
  (interactive)
  (let ((current-point (point)))
	(goto-char (point-min))
	(while (not (eobp))
	  (let ((command (char-after))
			(package-name (el-get-package-menu-get-package-name)))
		(cond
		 ((eq command ?I)
		  (message "Installing %s..." package-name)
		  (el-get-install package-name)
		  (message "Installing %s...done" package-name))
		 ((eq command ?U)
		  (message "Updating %s..." package-name)
		  (el-get-update package-name)
		  (message "Updating %s...done" package-name))
		 ((eq command ?D)
		  (message "Deleting %s..." package-name)
		  (el-get-remove package-name)
		  (message "Deleting %s..." package-name))))
	  (forward-line))
	(el-get-package-menu-revert)
	(goto-char current-point)
	(beginning-of-line)))

(defun el-get-package-menu-describe ()
  (interactive)
  (el-get-describe (el-get-package-menu-get-package-name)))

(defun el-get-package-menu-quick-help ()
  (interactive)
  (message "n-ext, p-revious, i-nstall, u-pdate, d-elete, SPC-unmark, g-revert, x-execute, ?-package describe, v-iew recipe, h-elp, q-uit"))

(unless el-get-package-menu-mode-map
  (setq el-get-package-menu-mode-map (make-keymap))
  (suppress-keymap el-get-package-menu-mode-map)
  (define-key el-get-package-menu-mode-map "n" 'next-line)
  (define-key el-get-package-menu-mode-map "p" 'previous-line)
  (define-key el-get-package-menu-mode-map "i" 'el-get-package-menu-mark-install)
  (define-key el-get-package-menu-mode-map "u" 'el-get-package-menu-mark-update)
  (define-key el-get-package-menu-mode-map "d" 'el-get-package-menu-mark-delete)
  (define-key el-get-package-menu-mode-map " " 'el-get-package-menu-mark-unmark)
  (define-key el-get-package-menu-mode-map "g" 'el-get-package-menu-revert)
  (define-key el-get-package-menu-mode-map "x" 'el-get-package-menu-execute)
  (define-key el-get-package-menu-mode-map "?" 'el-get-package-menu-describe)
  (define-key el-get-package-menu-mode-map "v" 'el-get-package-menu-view-recipe)
  (define-key el-get-package-menu-mode-map "h" 'el-get-package-menu-quick-help)
  (define-key el-get-package-menu-mode-map "q" 'quit-window))

(defun el-get-package-menu-mode ()
  "Major mode for browsing a list of packages.

\\{el-get-package-menu-mode-map}"
  (kill-all-local-variables)
  (use-local-map el-get-package-menu-mode-map)
  (setq major-mode 'el-get-package-menu-mode)
  (setq mode-name "Package-Menu")
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (if (fboundp 'run-mode-hooks)
	  (run-mode-hooks 'el-get-package-menu-mode-hook)
	(run-hooks 'el-get-package-menu-mode-hook)))

(defun el-get-print-package (package-name status desc)
  (let ((face
		 (cond
		  ((string= status "installed")
		   'font-lock-comment-face)
		  ((string= status "required")
		   'font-lock-keyword-face)
		  ((string= status "removed")
		   'font-lock-string-face)
		  (t
		   (setq status "available")
		   'default))))
	(indent-to 2 1)
	(insert (propertize package-name 'font-lock-face face))
	(indent-to 30 1)
	(insert (propertize status 'font-lock-face face))
	(when desc
	  (indent-to 41 1)
	  (insert (propertize
			   (replace-regexp-in-string "\n" " " desc)
			   'font-lock-face face)))
	(insert "\n")))

(defun el-get-list-all-packages ()
  (with-current-buffer (get-buffer-create "*el-get packages*")
	(setq buffer-read-only nil)
	(erase-buffer)
	(let ((packages (el-get-read-all-recipes)))
	  (let ((selector (cond
					   ((string= el-get-package-menu-sort-key "Status")
						#'(lambda (package)
							(let ((package-name (el-get-as-string (plist-get package :name))))
							  (el-get-read-package-status package-name))))
					   ((string= el-get-package-menu-sort-key "Description")
						#'(lambda (package)
							(plist-get package :description)))
					   (t
						#'(lambda (package)
							(el-get-as-string (plist-get package :name)))))))
		(setq packages
			  (sort packages
					(lambda (left right)
					  (let ((vleft (funcall selector left))
							(vright (funcall selector right)))
						(string< vleft vright))))))
	  (mapc (lambda (package)
			  (let ((package-name (el-get-as-string (plist-get package :name))))
				(el-get-print-package package-name
									  (el-get-read-package-status package-name)
									  (plist-get package :description))))
			packages))
	(goto-char (point-min))
	(current-buffer)))

(defun el-get-package-menu-sort-by-column (&optional e)
  "Sort the package menu by the last column clicked on."
  (interactive (list last-input-event))
  (if e (mouse-select-window e))
  (let* ((pos (event-start e))
		 (obj (posn-object pos))
		 (col (if obj
				  (get-text-property (cdr obj) 'column-name (car obj))
				(get-text-property (posn-point pos) 'column-name))))
    (setq el-get-package-menu-sort-key col)
	(el-get-package-menu)))

(defvar el-get-package-menu-sort-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] 'el-get-package-menu-sort-by-column)
    (define-key map [follow-link] 'mouse-face)
    map)
  "Local keymap for package menu sort buttons.")

(defun el-get-package-menu ()
  (with-current-buffer (el-get-list-all-packages)
	(el-get-package-menu-mode)
	(setq header-line-format
		  (mapconcat
		   (lambda (pair)
			 (let ((column (car pair))
				   (name (cdr pair)))
			   (concat
				;; Insert a space that aligns the button properly.
				(propertize " " 'display (list 'space :align-to column)
							'face 'fixed-pitch)
				;; Set up the column button.
				(propertize name
							'column-name name
							'help-echo "mouse-1: sort by column"
							'mouse-face 'highlight
							'keymap el-get-package-menu-sort-button-map))))
		   '((2 . "Package")
			 (30 . "Status")
			 (41 . "Description"))
		   ""))
	(pop-to-buffer (current-buffer))))

;;;###autoload
(defun el-get-list-packages ()
  "Display a list of packages."
  (interactive)
  (el-get-package-menu))

(provide 'el-get-list-packages)
