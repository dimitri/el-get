(:name bbcode-mode
       :website "https://bitbucket.org/jfm/emacs-bbcode"
       :description "A simple derived-mode for editing bbcode in emacs."
       :type hg
       :url "https://bitbucket.org/jfm/emacs-bbcode"
       :post-init (lambda ()
		    (autoload 'bbcode-mode "bbcode-mode" "BBCode editing mode." t)
		    (add-to-list 'auto-mode-alist '("\\.bbc\\(ode\\)?\\'" . bbcode-mode))))
