(:name java-mode-indent-annotations
       :type emacswiki
       :features java-mode-indent-annotations
       :after (lambda ()
		(add-hook 'java-mode-hook 'java-mode-indent-annotations-setup)))
