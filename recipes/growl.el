(:name growl
       :type http
       :url "http://edward.oconnor.cx/elisp/growl.el"
       :after (lambda ()
                (autoload 'growl "growl" nil t)))
