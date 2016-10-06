;;; el-get-recipes.el --- Manage the external elisp bits and pieces you depend upon
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

;;; Commentary:
;;
;; el-get-check provides some functions to check for some errors in recipes.
;;

;;; Code:

(require 'el-get-recipes)
(require 'el-get-build)

(defvar warning-minimum-log-level)
(defvar warning-minimum-level)
(declare-function warning-numeric-level "warnings" (level))

(defvar el-get-check--last-file-or-buffer nil
  "The last file-or-buffer checked.")

(defun el-get-check-redo ()
  "Rerun `el-get-check-recipe' with last recipe."
  (interactive)
  (when el-get-check--last-file-or-buffer
    (el-get-check-recipe
     el-get-check--last-file-or-buffer)))

(defvar el-get-check-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "g" #'el-get-check-redo)
    map)
  "Mode map for `el-get-check-mode'.")

(define-derived-mode el-get-check-mode special-mode "El-Get Check"
  "Special mode for `el-get-check-recipe' buffers.
See Info node `(el-get) Authoring Recipes'.")

(defvar el-get-check-suppressed-warnings ()
  "List of `el-get-check-recipe' warnings to suppress.

Current possibe elements are:
 `features', `github', `autoloads'")

(defun el-get-check-recipe-batch-1 (recipe-file)
  (let ((warning-prefix-function
         (lambda (level entry)
           (list level (format "%s:%s" el-get-check--last-file-or-buffer
                               (format (nth 1 entry) ""))))))
    (condition-case err
        (el-get-check-recipe (file-relative-name recipe-file))
      (error (lwarn '(el-get) :emergency "%s" (error-message-string err))
             1))))

(defun el-get-check-recipe-batch ()
  "emacs -Q -batch -f el-get-check-recipe-batch [-W<:level>] [-Wno-<warning>...] *.rcp

<:level> can be any valid warning level, see `warning-levels'.
See `el-get-check-suppressed-warnings' for possible <warning> values."
  (assert noninteractive nil
          "`el-get-check-recipe-batch' should only be used with -batch")
  (setq vc-handled-backends nil) ; avoid loading VC during batch mode
  (loop for arg in command-line-args-left
        if (string-match "\\`-Wno-\\(.*\\)" arg)
        do (push (intern (match-string 1 arg)) el-get-check-suppressed-warnings)
        else if (string-match "\\`-W\\(:[-a-z]*\\)" arg)
        do (setq warning-minimum-log-level
                 (setq warning-minimum-level (intern (match-string 1 arg))))
        else summing
        (if (file-directory-p arg)
            (reduce #'+ (directory-files arg t "\\.rcp$" t)
                    :key #'el-get-check-recipe-batch-1 :initial-value 0)
          (el-get-check-recipe-batch-1 arg))
        into errors
        finally (progn (message "%d warning/error(s) total." errors)
                       (kill-emacs (if (zerop errors) 0 1)))))

;;;###autoload
(defun el-get-check-recipe (file-or-buffer)
  "Check the format of the recipe.
Please run this command before sending a pull request.
Usage: M-x el-get-check-recipe RET

You can run this function from checker script like this:
    test/check-recipe.el PATH/TO/RECIPE.rcp

When used as a lisp function, FILE-OR-BUFFER must be a buffer
object or a file path."
  (interactive (list (current-buffer)))
  (setq el-get-check--last-file-or-buffer file-or-buffer)
  (if (bufferp file-or-buffer)
      (with-current-buffer file-or-buffer
        (el-get-check-recipe-in-current-buffer (buffer-file-name)))
    (with-temp-buffer
      (insert-file-contents file-or-buffer)
      (el-get-check-recipe-in-current-buffer file-or-buffer))))

(eval-and-compile
  (unless (fboundp 'file-name-base)     ; new in 24.3
    (defun file-name-base (&optional filename)
      "Return the base name of the FILENAME: no directory, no extension.
FILENAME defaults to `buffer-file-name'."
      (file-name-sans-extension
       (file-name-nondirectory (or filename (buffer-file-name)))))))

(defvar el-get-check-warning-buffer)
(defvar el-get-check-error-count)

(defun el-get-check-warning (level message &rest args)
  (declare (indent 1))
  (display-warning '(el-get recipe) (apply #'format message args)
                   level el-get-check-warning-buffer)
  (when (>= (warning-numeric-level level)
            (warning-numeric-level warning-minimum-level))
    (incf el-get-check-error-count)))

(defun el-get-check-recipe-in-current-buffer (recipe-file-name)
  (let ((inhibit-read-only t)
        (el-get-check-error-count 0)
        (el-get-check-warning-buffer (get-buffer-create "*el-get check recipe*")))
    (display-buffer el-get-check-warning-buffer)
    (with-current-buffer el-get-check-warning-buffer
      (erase-buffer)
      (el-get-check-mode))
    (let* ((recipe (save-excursion
                     (goto-char (point-min))
                     (prog1 (read (current-buffer))
                       (let ((lvl-err (condition-case err
                                          (progn (read (current-buffer))
                                                 `(:warning . "Extra data following recipe"))
                                        (end-of-file nil)
                                        (error `(:error . ,(error-message-string err))))))
                         (when lvl-err
                           (let ((el-get-check--last-file-or-buffer
                                  (format "%s:%d:%d" recipe-file-name
                                          (line-number-at-pos) (current-column))))
                             (el-get-check-warning (car lvl-err) (cdr lvl-err))))))))
           (el-get-sources (list recipe))
           (pkg-name (plist-get recipe :name)))
      (when (and recipe-file-name
                 (not (string= (file-name-base recipe-file-name) pkg-name)))
        (el-get-check-warning :error
          "File name should match recipe name."))
      ;; Check if userspace property is used.
      (loop for key in '(:before :after)
            for alt in '(:prepare :post-init)
            when (plist-get recipe key)
            do (el-get-check-warning :warning
                 "Property %S is for user.  Use %S instead."
                 key alt))
      ;; Check for misformatted plists
      (loop for key in recipe by #'cddr
            unless (keywordp key)
            do (el-get-check-warning :warning
                 "Property %S is not a keyword!"
                 key))
      (destructuring-bind (&key type url autoloads feats builtin
                                &allow-other-keys)
          recipe
        ;; let-binding `features' causes `provide' to throw error
        (setq feats (plist-get recipe :features))
        ;; Is github type used?
        (when (and (not (memq 'github el-get-check-suppressed-warnings))
                   (eq type 'git) (string-match "//github.com/" url))
          (el-get-check-warning :warning
            "Use `:type github' for github type recipe"))
        ;; Warn when `:autoloads nil' is specified.
        (when (and (not (memq 'autoloads el-get-check-suppressed-warnings))
                   (null autoloads) (plist-member recipe :autoloads))
          (el-get-check-warning :warning
            "Are you sure you don't need autoloads?
  This property should be used only when the library takes care of
  the autoload."))
        ;; Warn when `:features t' is specified
        (when (and (not (memq 'features el-get-check-suppressed-warnings))
                   feats)
          (el-get-check-warning :warning
            "Are you sure you need features?
  If this library has `;;;###autoload' comment (a.k.a autoload cookie),
  you don't need `:features'."))
        ;; Check if `:builtin' is used with an integer
        (when (integerp builtin)
          (el-get-check-warning :warning
            "Usage of integers for :builtin is obsolete.
  Use a version string like \"24.3\" instead.")))
      ;; Check for shell interpolated :build commands
      (let ((safe-functions '(backquote-list*
                              el-get-load-path el-get-package-exists-p
                              el-get-package-directory el-get-print-to-string
                              el-get-verbose-message
                              with-temp-buffer insert-file-contents
                              directory-files file-name-as-directory
                              expand-file-name shell-quote-argument)))
        (dolist (sys '("" "/darwin" "/berkeley-unix" "/windows-nt"))
          (let ((unsafe (catch 'unsafe-build
                          (when (some #'stringp (el-get-build-commands pkg-name 'safe-eval sys))
                            (el-get-check-warning :warning
                              ":build%s should be a *list* of string lists." sys))
                          nil)))
            (when unsafe
              (el-get-check-warning :debug ":build%s is unsafep: %s" sys unsafe)))))
      ;; Check for required properties.
      (loop for key in '(:description :name)
            unless (plist-get recipe key)
            do (el-get-check-warning :error
                 "Required property %S is not defined." key))
      (with-current-buffer el-get-check-warning-buffer
        (insert (format "\n%s: %s error(s) found." recipe-file-name el-get-check-error-count))))
    el-get-check-error-count))

(provide 'el-get-check)

;;; el-get-check.el ends here
