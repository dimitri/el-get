(require 'el-get)
(require 'ert nil t)

(eval-when-compile
  (require 'cl-lib)
  (unless (featurep 'ert)
    (cl-defmacro ert-deftest (name () &body docstring-keys-and-body)
      (message "Skipping tests, ERT is not available"))))

(defvar el-get-test-output-buffer nil)
(when noninteractive
  (defadvice message (around el-get-test-catch-output activate)
    "redirect all `message' output to `el-get-test-output-buffer'."
    (if (and el-get-test-output-buffer (ad-get-arg 0))
        (with-current-buffer el-get-test-output-buffer
          (insert (apply #'format (ad-get-args 0)) "\n"))
      ad-do-it))
  (defadvice cl--assertion-failed (around el-get-test-suppress-debugger activate)
    "Prevent failed `assert' from jumping into debugger."
    (let ((debug-on-error nil))
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
* `package-user-dir'
* `el-get-dir'
* `el-get-status-file'
* `el-get-status-cache'
* `el-get-autoload-file'"
  (declare (debug t))
  `(let* ((user-emacs-directory
           (make-temp-file "emacs.d.el-get-testing" 'dir "/"))
          (package-user-dir (locate-user-emacs-file "elpa"))
          (el-get-dir (mapconcat #'file-name-as-directory
                                 `(,user-emacs-directory "el-get") ""))
          (el-get-status-file (concat el-get-dir ".status.el"))
          (el-get-status-cache nil)
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
       (let ((kill-buffer-query-functions nil))
         (dolist (buf (buffer-list))
           (with-current-buffer buf
             (when (string-prefix-p user-emacs-directory default-directory)
               (set-buffer-modified-p nil)
               (kill-buffer)))))
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
                   (cl-loop for f in el-get-recipe-path
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

(ert-deftest el-get-elpa-install ()
  "Test installation with ELPA type."
  (el-get-with-temp-home
   (require 'package-x) ; create local package archive
   (let* ((pkg 'el-get-test-package)
          (package-archive-upload-base (expand-file-name "pkg-repo" user-emacs-directory))
          (package-archives nil)
          (el-get-sources
           `((:name package :post-init nil) ; avoid adding other repos
             (:name el-get-test-package
                    :type elpa
                    :repo ("test-repo" . ,package-archive-upload-base)))))
     (make-directory package-archive-upload-base t)
     (package-upload-file (expand-file-name "pkgs/el-get-test-package.el"
                                            el-get-test-files-dir))
     (el-get 'sync (mapcar 'el-get-source-name el-get-sources)))))

(ert-deftest el-get-elpa-feature ()
  "`:features' option should work for ELPA type recipe."
  :expected-result :failed
  (el-get-with-temp-home
   (require 'package-x) ; create local package archive
   (let* ((pkg 'el-get-test-package)
          (package-archive-upload-base (expand-file-name "pkg-repo" user-emacs-directory))
          (package-archives nil)
          (el-get-sources
           `((:name package :post-init nil) ; avoid adding other repos
             (:name el-get-test-package
                    :type elpa
                    :repo ("test-repo" . ,package-archive-upload-base)
                    :features el-get-test-package))))
     (make-directory package-archive-upload-base t)
     (package-upload-file (expand-file-name "pkgs/el-get-test-package.el"
                                            el-get-test-files-dir))
     (should-not (featurep pkg))
     (el-get 'sync (mapcar 'el-get-source-name el-get-sources))
     (should (featurep pkg)))))

(defconst insecure-urls '("http://example.com"
                          "ftp://example.com"
                          "file://example.com/home/user"
                          ":pserver:anonymous@example.com"
                        "
https://example.com"
                        "
file:///home/user"
                        "
John.Doe-123_@example.com"))

(ert-deftest el-get-insecure-check-insecure ()
  "Insecure URL for a package without :checksum"
  (dolist (url insecure-urls)
    (let ((el-get-allow-insecure nil)
          (el-get-sources '((:name "dummy" :type github))))
      ;; TODO check for error message?
      (should-error (el-get-insecure-check "dummy" url) :type 'error))))

(ert-deftest el-get-insecure-elpa ()
  (el-get-with-temp-home
   (require 'package-x)                 ; create local package archive
   (let* ((el-get-allow-insecure nil)
          (pkg 'el-get-test-package)
          (package-archive-upload-base (expand-file-name "pkg-repo" user-emacs-directory))
          (package-archives `(("test-repo" . ,package-archive-upload-base)))
          (el-get-sources
           `((:name package :post-init nil) ; avoid adding other repos
             (:name el-get-test-package :type elpa))))
     (make-directory package-archive-upload-base t)
     (package-upload-file (expand-file-name "pkgs/el-get-test-package.el"
                                            el-get-test-files-dir))
     (el-get 'sync 'el-get-test-package))))

(defconst secure-urls '("https://example.com"
                        "ssh://example.com"
                        "git+ssh://example.com/"
                        "bzr+ssh://example.com/"
                        "sftp://example.com/"
                        "file:///home/user"
                        "file:///c|/WINDOWS/clock.avi"
                        "file:///c:/WINDOWS/clock.avi"
                        "John.Doe-123_@example.com"))

(ert-deftest el-get-insecure-check-secure ()
  "Secure URL for a package without :checksum doesn't matter"
  (dolist (url secure-urls)
    (let ((el-get-allow-insecure nil)
          (el-get-sources '((:name "dummy" :type github))))
      (should-not (el-get-insecure-check "dummy" url)))))

(ert-deftest el-get-insecure-check-checksum ()
  "Either secure or insecure URL for a package with :checksum"
  (dolist (url (append insecure-urls secure-urls))
    (let ((el-get-allow-insecure nil)
          (el-get-sources '((:name "dummy" :type github :checksum "checksum"))))
      (should-not (el-get-insecure-check "dummy" url)))))

(ert-deftest el-get-insecure-check-checksum-empty ()
  "Insecure URL for a package with empty :checksum"
  (dolist (url insecure-urls)
    (dolist (checksum '("" "   "))
      (let ((el-get-allow-insecure nil)
            (el-get-sources '((:name "dummy" :type github :checksum checksum))))
        ;; TODO check for error message?
        (should-error (el-get-insecure-check "dummy" url) :type 'error)))))

(ert-deftest el-get-github-update ()
  "Update :type github"
  (let* ((github-pkgname "user/dummy.el")
         (el-get-sources `((:name "dummy" :type github :pkgname ,github-pkgname)))
         (el-get-allow-insecure nil))
    (cl-letf (((symbol-function 'el-get-package-is-installed)
               (lambda (package)
                 (string= package "dummy")))
              ((symbol-function 'el-get-git-pull)
               (lambda (package url post-install)
                 (should (string= package "dummy"))
                 (should (string= url (concat "https://github.com/" github-pkgname ".git")))))
              ((symbol-function 'el-get-insecure-check)
               (lambda (package url)
                 (error "Leave 'el-get-insecure-check to git"))))
      (should (el-get-do-update "dummy")))))
