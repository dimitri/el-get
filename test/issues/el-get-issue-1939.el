;; Tests for `el-get-url-host', fix for issue #1939
;; The function should extract host from url
;; URLs used for test were extracted from existing recipes

(require 'cl-lib)
(require 'url-parse)

(cl-assert (string= (el-get-url-host "http://os.inf.tu-dresden.de/~mp26/download/tbemail.el")
                    (url-host (url-generic-parse-url "http://os.inf.tu-dresden.de/~mp26/download/tbemail.el"))))
(cl-assert (string= (el-get-url-host "http://mumble.net/~campbell/emacs/paredit.el")
                    (url-host (url-generic-parse-url "http://mumble.net/~campbell/emacs/paredit.el"))))
(cl-assert (string= (el-get-url-host "http://repo.or.cz/r/anything-config.git")
                    (url-host (url-generic-parse-url "http://repo.or.cz/r/anything-config.git"))))
(cl-assert (string= (el-get-url-host "https://bitbucket.org/xemacs/edict")
                    (url-host (url-generic-parse-url "https://bitbucket.org/xemacs/edict"))))
(cl-assert (string= (el-get-url-host "git://gitorious.org/evil/evil.git")
                    (url-host (url-generic-parse-url "git://gitorious.org/evil/evil.git"))))
(cl-assert (string= (el-get-url-host "ftp://210.155.141.202/pub/morishima.net/naoto/ElScreen/elscreen-1.4.6.tar.gz")
                    (url-host (url-generic-parse-url "ftp://210.155.141.202/pub/morishima.net/naoto/ElScreen/elscreen-1.4.6.tar.gz"))))
(cl-assert (string= (el-get-url-host "svn://svn.forge.ocamlcore.org/svn/tuareg/trunk")
                    (url-host (url-generic-parse-url "svn://svn.forge.ocamlcore.org/svn/tuareg/trunk"))))
(cl-assert (string= (el-get-url-host "bzr://rudel.bzr.sourceforge.net/bzrroot/rudel/trunk")
                    (url-host (url-generic-parse-url "bzr://rudel.bzr.sourceforge.net/bzrroot/rudel/trunk"))))
