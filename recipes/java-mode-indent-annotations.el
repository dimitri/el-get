(:name java-mode-indent-annotations
       :description "Indentation for Java 5 annotations."
       :type emacswiki
       :features java-mode-indent-annotations
       :post-init (lambda ()
            (add-hook 'java-mode-hook 'java-mode-indent-annotations-setup)))
