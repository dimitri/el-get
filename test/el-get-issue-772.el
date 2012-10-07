;; https://github.com/dimitri/el-get/issues/619
;;
;; :checkout property for hg

(require 'cl)
(let* ((debug-on-error t)
       (el-get-verbose t)
       ;; Just need to install something hg-method
       (pkg 'ahg)
       ;; https://bitbucket.org/agriggio/ahg/changeset/c732a739a7a1/raw/
       (checksum "c732a739a7a1505d5922e94e4368b656dccddf11")
       (el-get-sources
        (list
         `(:name ,pkg
                 :checksum ,checksum))))
  (el-get 'sync pkg)
  (assert (el-get-package-is-installed pkg))
  (let (installed-checksum)
    (setq installed-checksum (el-get-checksum pkg))
    (message "Installed checksum is %s" installed-checksum)
    (assert (string= checksum installed-checksum) nil
            "Package %s should have checksum %s but it is actually %s"
            pkg checksum installed-checksum)))
