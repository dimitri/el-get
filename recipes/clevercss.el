(:name clevercss
       :website "https://github.com/jschaf/CleverCSS-Mode#readme"
       :description "This is a Emacs major mode for editing CleverCSS files. It provides smart indentation, syntax highlighting and basic text manipulation commands."
       :type git
       :url "https://github.com/jschaf/CleverCSS-Mode.git"
       :post-init (lambda ()
		    (autoload 'clevercss-mode "clevercss" nil t)
		    (add-to-list 'auto-mode-alist
				 '("\\.ccss\\'" . clevercss-mode))))
