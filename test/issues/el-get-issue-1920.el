;; Test for testing `el-get-git-shallow-clone-supported-p' function
;; the function detects whether shallow clone is supported for url

(require 'cl-lib)

;; Tests for lower level function [el-get-git-url-from-known-smart-domains-p]
(cl-assert (el-get-git-shallow-clone-supported-p "https://www.bitbucket.org/alfaromurillo/org-passwords.el.git"))
(cl-assert (el-get-git-url-from-known-smart-domains-p "https://www.github.com/dimitri/el-get"))
(cl-assert (el-get-git-url-from-known-smart-domains-p "https://bitbucket.org/alfaromurillo/org-passwords.el.git"))
(cl-assert (el-get-git-url-from-known-smart-domains-p "https://github.com/dimitri/el-get"))

;; Tests for lower level function [el-get-git-is-host-smart-http-p]
(cl-assert (el-get-git-is-host-smart-http-p "https://github.com/dimitri/el-get.git"))
(cl-assert (el-get-git-is-host-smart-http-p "http://repo.or.cz/r/anything-config.git"))
(cl-assert (not (el-get-git-is-host-smart-http-p "http://www.dr-qubit.org/git/undo-tree.git")))

;; Function should not fail for urls without '.git' prefix
(cl-assert (el-get-git-is-host-smart-http-p "https://github.com/dimitri/el-get"))
(cl-assert (el-get-git-is-host-smart-http-p "http://repo.or.cz/r/anything-config"))
(cl-assert (not (el-get-git-is-host-smart-http-p "http://www.dr-qubit.org/git/undo-tree")))

;; Tests for function [el-get-git-shallow-clone-supported-p]
;; `git', `ssh' and `file' support shallow clones
(cl-assert (el-get-git-shallow-clone-supported-p "git://gitorious.org/evil/evil.git"))
(cl-assert (el-get-git-shallow-clone-supported-p "file:///opt/git/project.git"))
(cl-assert (el-get-git-shallow-clone-supported-p "ssh://some_user@some_server/some_project.git"))

;; The following repos support shallow clones
(cl-assert (el-get-git-shallow-clone-supported-p "http://repo.or.cz/r/anything-config.git"))
(cl-assert (el-get-git-shallow-clone-supported-p "https://github.com/dimitri/el-get"))
(cl-assert (el-get-git-shallow-clone-supported-p "https://bitbucket.org/alfaromurillo/org-passwords.el.git"))

;; The following do not support shallow clones
(cl-assert (not (el-get-git-shallow-clone-supported-p "http://www.dr-qubit.org/git/undo-tree.git/")))
(cl-assert (not (el-get-git-shallow-clone-supported-p "http://michael.orlitzky.com/git/nagios-mode.git")))
