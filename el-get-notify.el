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
(require 'help-mode)     ; byte-compiling needs to know about xref-type buttons

;; we support notifications on darwin too, thanks to growlnotify
(defcustom el-get-growl-notify-path "/usr/local/bin/growlnotify"
  "Absolute path of the growlnotify tool"
  :group 'el-get
  :type 'file)

(define-obsolete-variable-alias 'el-get-growl-notify 'el-get-growl-notify-path "4.0")

;; notify user with emacs notifications API (new in 24)
;;
(when (and (eq system-type 'darwin)
           (file-executable-p el-get-growl-notify-path))
  (defun el-get-growl (title message)
    "Send a message to growl, that implements notifications for darwin"
    (let* ((name  "*growl*")
           (proc
            (start-process name name el-get-growl-notify-path title "-a" "Emacs")))
      (process-send-string proc (concat message "\n"))
      (process-send-eof proc))))

(defcustom el-get-notify-type 'both
  "Type of notification to use for changes in package statuses

Choices are `graphical', `message', or `both'. Note that if
graphical notification is impossible, `message' will be used as a
fallback."
  :group 'el-get
  :type '(choice (const :tag "Graphical notifications" graphical)
                 (const :tag "Minibuffer message" message)
                 (const :tag "Graphical & Minibuffer" both)))

;;
;; Notification support is either the internal one provided by Emacs 24, or
;; the external growl one as defined above, or the one provided by the
;; add-on found on http://www.emacswiki.org/emacs/notify.el (there's a
;; recipe) for older Emacs versions users
;;
(defun el-get-notify (title message)
  "Notify the user using either the dbus based API or the `growl' one"
  (when (not (eq el-get-notify-type 'message))
    (unless (and (fboundp 'dbus-register-signal)
                 ;; avoid a bug in Emacs 24.0 under darwin
                 (ignore-errors (require 'notifications nil t)))
      ;; else try notify.el, there's a recipe for it
      (unless (fboundp 'notify)
        (ignore-errors (require 'notify nil 'noerror)))))

  (condition-case nil
      (progn
        (cond
         ;; Message only
         ((equal el-get-notify-type 'message) (error "Use `message' instead"))
         ;; Graphical notification
         ((fboundp 'notifications-notify) (notifications-notify :title title
                                                                :app-name "el-get"
                                                                :app-icon (concat
                                                                           (el-get-package-directory "el-get")
                                                                           "/logo/el-get.png")
                                                                :body message))
         ((fboundp 'notify)               (notify title message))
         ((fboundp 'el-get-growl)         (el-get-growl title message))
         ;; Fallback
         (t                               (error "Fallback to `message'")))
        ;; Handle "both"
        (when (equal el-get-notify-type 'both)
          (error "Fallback to `message' even though graphical notification succeeded")))
    ;; when notification function errored out, degrade gracefully to `message'
    (error (message "%s: %s" title message))))

(defun el-get-post-install-notification (package)
  "Notify the PACKAGE has been installed."
  (el-get-notify (format "%s installed" package)
                 "This package has been installed successfully by el-get."))
(add-hook 'el-get-post-install-hooks 'el-get-post-install-notification)

(defun el-get-post-update-notification (package)
  "Notify the PACKAGE has been updated."
  (el-get-notify (format "%s updated" package)
                 "This package has been updated successfully by el-get."))
(add-hook 'el-get-post-update-hooks 'el-get-post-update-notification)

(defun el-get-post-remove-notification (package)
  "Notify the PACKAGE has been removed."
  (el-get-notify (format "%s removed" package)
                 "This package has been removed successfully by el-get."))
(add-hook 'el-get-post-remove-hooks 'el-get-post-remove-notification)

(defun el-get-post-error-notification (package info)
  "Notify the PACKAGE has failed to install."
  (el-get-notify (format "%s failed to install" package)
                 (format "%s" info)))
(add-hook 'el-get-post-error-hooks 'el-get-post-error-notification)

;;
;; Emacs `message' notifications
;;
(defun el-get-post-init-message (package)
  "After PACKAGE init is done, just message about it"
  (el-get-verbose-message "el-get initialized package %s" package))
(add-hook 'el-get-post-init-hooks 'el-get-post-init-message)

(defun el-get-post-update-message (package)
  "After PACKAGE update is done, message about it"
  (el-get-verbose-message "el-get updated package %s" package))
(add-hook 'el-get-post-update-hooks 'el-get-post-update-message)

(defun el-get-post-remove-message (package)
  "After PACKAGE remove is done, message about it"
  (el-get-verbose-message "el-get removed package %s" package))
(add-hook 'el-get-post-remove-hooks 'el-get-post-remove-message)

(defun el-get-post-error-message (package info)
  "After PACKAGE fails to install, just message about it"
  (el-get-verbose-message "el-get failed to initialize package %s" package))
(add-hook 'el-get-post-error-hooks 'el-get-post-error-message)

(provide 'el-get-notify)
