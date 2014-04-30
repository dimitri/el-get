(require 'cl-lib)
(require 'package-x)

;;; Utility

(defun* create-package-archive (path &key name desc version)
  "Creates local archive at PATH that provides signle package
with NAME, DESCRIPTION and VERSION specified according to
values provided in arguments after corresponding keys."
  (cond ((not (file-exists-p path)) (mkdir path))
        ((or (not (file-directory-p path))
             (not (file-writable-p path)))
         (error "Specified path is not a writeable directory.")))
  (with-temp-buffer
    (insert (format ";;; %s.el --- %s"    name desc) ?\n
            (format ";; Version: %s"      version) ?\n
            (format "(provide '%s)"       name) ?\n
            (format ";;; %s.el ends here" name))
    (let ((package-archive-upload-base path))
      (package-upload-buffer))))

;;; Paths

(defconst old-archive-path
  (expand-file-name "old-archive" (getenv "HOME"))
  "Path to archive included into `package-archives'")

(defconst new-archive-path
  (expand-file-name "new-archive" (getenv "HOME"))
  "Path to archive introduced through a recipe")

;;; Archives

(create-package-archive old-archive-path
                        :name 'dummy-funny
                        :desc "This package goes in archive that was part of `package-archives'"
                        :version "0.3")

(create-package-archive new-archive-path
                        :name 'dummy-whammy
                        :desc "This package goes in archive that was introduced through a recipe"
                        :version "0.8")

;;; Recipes

(add-to-list 'el-get-sources
             `(:name dummy-funny
                     :type elpa
                     :repo ("old-archive" . ,old-archive-path)))

(add-to-list 'el-get-sources
             `(:name dummy-whammy
                     :type elpa
                     :repo ("new-archive" . ,new-archive-path)))

;; Only old-archive is specified in `package-archives' and will remain
;; active regardless of recipe.

(setq package-archives `(("old-archive" . ,old-archive-path)))

(el-get 'sync '(dummy-funny dummy-whammy))
