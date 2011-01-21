(:name bbcode-mode
       :type hg
       :url "https://bitbucket.org/jfm/emacs-bbcode"
       :after (lambda ()
                (autoload 'bbcode-mode "bbcode-mode" "BBCode editing mode." t)
                (add-to-list 'auto-mode-alist '("\\.bbc\\(ode\\)?\\'" . bbcode-mode))))
