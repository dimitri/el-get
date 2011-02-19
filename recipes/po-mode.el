(:name po-mode
       :type http
       :url "http://cvs.savannah.gnu.org/viewvc/*checkout*/gettext/gettext/gettext-tools/misc/po-mode.el"
       :features po-mode
       :after (lambda ()
                (add-to-list 'auto-mode-alist '("\\.po$" . po-mode))
                (add-to-list 'auto-mode-alist '("\\.pot$" . po-mode))))
