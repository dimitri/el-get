(:name full-ack
       :type git
       :url "https://github.com/nschum/full-ack.git"
       :after (lambda ()
                (autoload 'ack "full-ack" nil t)
                (autoload 'ack-find-file "full-ack" nil t)
                (autoload 'ack-find-same-file "full-ack" nil t)
                (autoload 'ack-same "full-ack" nil t)))
