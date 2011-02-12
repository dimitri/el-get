(:name drag-stuff
       :type http
       :url "https://github.com/rejeep/drag-stuff/raw/master/drag-stuff.el"
       :after (lambda ()
                (autoload 'drag-stuff-mode "drag-stuff" nil t)))
