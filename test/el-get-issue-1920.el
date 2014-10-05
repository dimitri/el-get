;; Test for testing `el-get-git-shallow-clone-supported?' function
;; the function detects whether shallow clone is supported for url

(require 'cl-lib)

;; Tests for lower level function [el-get-git-url-from-known-smart-domains]
(cl-assert (el-get-git-shallow-clone-supported? "https://www.bitbucket.org/alfaromurillo/org-passwords.el.git"))
(cl-assert (el-get-git-url-from-known-smart-domains "https://www.github.com/dimitri/el-get"))
(cl-assert (el-get-git-url-from-known-smart-domains "https://bitbucket.org/alfaromurillo/org-passwords.el.git"))
(cl-assert (el-get-git-url-from-known-smart-domains "https://github.com/dimitri/el-get"))

;; Tests for lower level function [el-get-git-is-host-smart-http]
(cl-assert (el-get-git-is-host-smart-http "https://github.com/dimitri/el-get.git"))
(cl-assert (el-get-git-is-host-smart-http "http://repo.or.cz/r/anything-config.git"))
(cl-assert (not (el-get-git-is-host-smart-http "http://www.dr-qubit.org/git/undo-tree.git")))

;; Function should not fail for urls without '.git' prefix
(cl-assert (el-get-git-is-host-smart-http "https://github.com/dimitri/el-get"))
(cl-assert (el-get-git-is-host-smart-http "http://repo.or.cz/r/anything-config"))
(cl-assert (not (el-get-git-is-host-smart-http "http://www.dr-qubit.org/git/undo-tree")))

;; Tests for function [el-get-git-shallow-clone-supported?]
;; `git', `ssh' and `file' support shallow clones
(cl-assert (el-get-git-shallow-clone-supported? "git://gitorious.org/evil/evil.git"))
(cl-assert (el-get-git-shallow-clone-supported? "file:///opt/git/project.git"))
(cl-assert (el-get-git-shallow-clone-supported? "ssh://some_user@some_server/some_project.git"))

;; The following repos support shallow clones
(cl-assert (el-get-git-shallow-clone-supported? "http://repo.or.cz/r/anything-config.git"))
(cl-assert (el-get-git-shallow-clone-supported? "https://github.com/dimitri/el-get"))
(cl-assert (el-get-git-shallow-clone-supported? "https://bitbucket.org/alfaromurillo/org-passwords.el.git"))

;; The following do not support shallow clones
(cl-assert (not (el-get-git-shallow-clone-supported? "http://www.dr-qubit.org/git/undo-tree.git/")))
(cl-assert (not (el-get-git-shallow-clone-supported? "http://michael.orlitzky.com/git/nagios-mode.git")))
