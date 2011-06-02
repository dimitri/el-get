(:name babel
       :type git
       :url "git://github.com/juergenhoetzel/babel.git"
       :prepare
       (lambda ()
         (autoload 'babel "babel"
           "Use a web translation service to translate the message MSG." t)
         (autoload 'babel-region "babel"
           "Use a web translation service to translate the current region." t)
         (autoload 'babel-as-string "babel"
           "Use a web translation service to translate MSG, returning a string." t)
         (autoload 'babel-buffer "babel"
           "Use a web translation service to translate the current buffer." t)))
