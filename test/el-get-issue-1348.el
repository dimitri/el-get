;;; https://github.com/dimitri/el-get/issues/1348
;;; `el-get-elpa-build-local-recipes' creates dependencies on `emacs' package.

(el-get 'sync 'package)
(el-get-elpa-build-local-recipes)

;;; use only the recipes from `el-get-elpa-build-local-recipes'
(setq el-get-recipe-path (list el-get-recipe-path-elpa))

(when (version-list-<=
       (el-get-version-to-list
        (el-get-package-required-emacs-version 'ido-ubiquitous))
       (version-to-list emacs-version))
  (el-get 'sync 'ido-ubiquitous))
