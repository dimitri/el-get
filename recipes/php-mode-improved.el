(:name php-mode-improved
       :type emacswiki
       :load ("php-mode-improved.el")
       :features php-mode
       :after (lambda ()
		(dolist (ext '(php php3 php4 php5 phtml))
		  (add-to-list auto-mode-alist '((format "\\.%s$" ext) .php-mode)))
		(autoload 'php-mode "php-mode" "PHP improved editing mode." t)))
