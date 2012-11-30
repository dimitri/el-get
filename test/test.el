(require 'el-get)
(require 'ert nil t)

(eval-when-compile
  (require 'cl)
  (unless (featurep 'ert)
    (defmacro* ert-deftest (name () &body docstring-keys-and-body)
      (message "Skipping tests, ERT is not available"))))

(defvar el-get-test-output-buffer nil)
(when noninteractive
  (defadvice message (around el-get-test-catch-output activate)
    "redirect all `message' output to `el-get-test-output-buffer'."
    (if el-get-test-output-buffer
        (with-current-buffer el-get-test-output-buffer
          (insert (apply #'format (ad-get-args 0)) "\n"))
      ad-do-it)))

(defconst el-get-test-files-dir
  (file-name-directory
   (or (eval-when-compile (bound-and-true-p byte-compile-file))
       load-file-name
       buffer-file-name)))

(defmacro el-get-with-temp-home (&rest body)
  "Evaluate BODY with a temporary `user-emacs-directory'.

In batch mode, `message' output is suppressed unless there is an
error.

Following variables are bound to temporal values:
* `user-emacs-directory'
* `el-get-dir'
* `el-get-status-file'
* `el-get-autoload-file'"
  `(let* ((user-emacs-directory
           (make-temp-file "emacs.d.el-get-testing" 'dir))
          (el-get-dir (mapconcat #'file-name-as-directory
                                 `(,user-emacs-directory "el-get") ""))
          (el-get-status-file (concat el-get-dir ".status.el"))
          (el-get-autoload-file (concat el-get-dir ".loaddefs.el"))
          (el-get-test-output-buffer
           (when noninteractive (get-buffer-create "*el-get-test-output*"))))
     (unwind-protect
         (condition-case err
             (progn
               (make-directory el-get-dir t)
               ,@body)
           (error (when el-get-test-output-buffer
                    (with-current-buffer el-get-test-output-buffer
                      (princ (buffer-string))))
                  (signal (car err) (cdr err))))
       (delete-directory user-emacs-directory t)
       (when el-get-test-output-buffer
         (kill-buffer el-get-test-output-buffer)))))

(ert-deftest el-get-recipe-dirs-test ()
  (let ((el-get-recipe-path
         `("/"
           ,(let ((f "/foo"))
              (while (file-exists-p f)
                (setq f (concat f f))) f))))
    (should (equal (el-get-recipe-dirs)
                   (loop for f in el-get-recipe-path
                         when (file-exists-p f)
                         collect f)))))

(ert-deftest el-get-trivial-install-test ()
  (el-get-with-temp-home
   (let* ((pkg 'el-get-test-package)
          (pkg-name (symbol-name pkg))
          (pkg-file (concat pkg-name ".el"))
          (pkg-source (expand-file-name
                       pkg-file (concat el-get-test-files-dir "pkgs")))
          (pkg-destination (expand-file-name
                            pkg-file (concat el-get-dir pkg-name)))
          (el-get-sources `((:name ,pkg
                                   :features (,pkg)
                                   :type http
                                   :url ,(concat "file://" pkg-source)))))
     (unwind-protect
         (progn
           (message "Checking %s is not loaded" pkg)
           (should-not (featurep pkg))
           (message "Installing %s" pkg)
           (should (progn (el-get 'sync pkg) t))
           (message "Verifying installed package file %s" pkg-destination)
           (should (file-exists-p pkg-destination))
           (should-not (zerop (nth 7 (file-attributes pkg-destination))))
           (message "Verifying package %s was loaded" pkg)
           (should (featurep pkg))
           (message "Unloading and removing package %s" pkg)
           (el-get-remove (symbol-name pkg))
           (message "Verifying package %s was unloaded and removed" pkg)
           (should-not (file-exists-p pkg-destination)))
       (when (featurep pkg)
         (unload-feature pkg))))))

