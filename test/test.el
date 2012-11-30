(require 'el-get)
(require 'ert nil t)

(eval-when-compile
  (require 'cl)
  (unless (featurep 'ert)
    (defmacro* ert-deftest (name () &body docstring-keys-and-body)
      (message "Skipping tests, ERT is not available"))))

(defconst testing-destination-dir "/tmp/emacs.d.testing")

(defmacro* el-get-deftest (name () &body docstring-and-body)
  "`ert-deftest' with predefined context for el-get test cases.

Following variables are bound to temporal values during test:
* `user-emacs-directory'
* `el-get-dir'
* `el-get-status-file'"
  (declare (debug (&define :name test
                           name sexp [&optional stringp]
			   [&rest keywordp sexp] def-body))
           (doc-string 3)
           (indent 2))
  (let ((docstring (car docstring-and-body))
        (body (cdr docstring-and-body)))
    (unless (stringp docstring)
      (setq docstring nil)
      (setq body docstring-and-body))
    `(ert-deftest ,name ()
       ,@(when docstring (list docstring))
       (let* ((user-emacs-directory testing-destination-dir)
              (el-get-dir (concat (file-name-as-directory user-emacs-directory)
                                  "el-get"))
              (el-get-status-file
               (concat (file-name-as-directory el-get-dir) ".status.el")))
         (unwind-protect
             (progn
               (make-directory el-get-dir t)
               ,@body)
           (delete-directory el-get-dir t))))))

(put 'el-get-deftest 'lisp-indent-function 2)

(font-lock-add-keywords
 nil
 '(("(\\(\\<el-get-deftest\\)\\>\\s *\\(\\sw+\\)?"
    (1 font-lock-keyword-face nil t)
    (2 font-lock-function-name-face nil t))))

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

(el-get-deftest el-get-trivial-install-test ()
  (let* ((pkg 'el-get-trivial-install-test)
         (pkg-name (symbol-name pkg))
         (pkg-file (concat pkg-name ".el"))
         (pkg-source (concat "/tmp/" pkg-file))
         (pkg-destination-dir (mapconcat
                               'file-name-as-directory
                               (list user-emacs-directory "el-get" pkg-name)
                               ""))
         (pkg-destination (concat pkg-destination-dir pkg-file))
         (el-get-sources `((:name ,pkg
                                  :features (,pkg)
                                  :type http
                                  :url ,(concat "file://" pkg-source))))
	 (el-get-packages (mapcar 'el-get-source-name el-get-sources)))
    (unwind-protect
        (progn
          (message "Checking %s is not loaded" pkg)
          (should-not (featurep pkg))
          (message "Creating %s package file in %s" pkg pkg-source)
          (with-temp-file pkg-source
            (insert (format "(defun %s-empty ())\n(provide '%s)\n" pkg pkg)))
          (message "Verifying package file %s" pkg-source)
          (should (file-exists-p pkg-source))
          ;; and it's not empty
          (should-not (zerop (nth 7 (file-attributes pkg-source))))
          (message "Installing %s" pkg)
          (should (progn
                    (el-get 'sync el-get-packages)
                    t))
          (message "Verifying installed package file %s" pkg-destination)
          (should (file-exists-p pkg-destination))
          (should-not (zerop (nth 7 (file-attributes pkg-destination))))
          (message "Verifying package %s was loaded" pkg)
          (should (featurep pkg))
          (message "Unloading and removing package %s" pkg)
          (el-get-remove (symbol-name pkg))
          (message "Verifying package %s was unloaded and removed" pkg)
          (should-not (file-exists-p pkg-destination)))
      (delete-file pkg-source))))

(el-get-deftest el-get-elpa-feature ()
  "`:features' option should work for ELPA type recipe."
  (let* ((pkg 'wrap-region)              ; some package from elpa
         (el-get-sources `((:name ,pkg :features ,pkg))))
    (should-not (featurep pkg))
    (el-get 'sync (mapcar 'el-get-source-name el-get-sources))
    (should (featurep pkg))))

;(featurep 'el-get-trivial-install-test)
;(unload-feature 'el-get-trivial-install-test)
