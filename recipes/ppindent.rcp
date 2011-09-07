(:name ppindent :type emacswiki
       :description "Indents C preprocessor directives"
       :features ppindent
       :post-init (lambda()
                    (setq ppindent-increment 4)
                    (message (format "%s" (current-buffer)))
                    (add-hook
                     'c-mode-common-hook
                     (lambda()
                       (add-hook
                        'before-save-hook
                        (lambda()
                          (if (string-match-p "\\.[Hh][a-z]*$" buffer-file-name) (ppindent-h)
                            (when (string-match-p "\\.[Cc][a-z]*$" buffer-file-name) (ppindent-c)))))))))
