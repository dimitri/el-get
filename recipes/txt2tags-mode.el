(:name txt2tags-mode
       :type http
       :url "http://txt2tags.googlecode.com/svn/trunk/extras/txt2tags-mode.el"
       :load "txt2tags-mode.el"
       :features t2t-mode
       :after (lambda ()
                (add-to-list 'auto-mode-alist '("\\.t2t$" . t2t-mode))
                (autoload 't2t-mode "t2t-mode" "txt2tags Mode" t)))