(:name wl
       :type cvs
       :module "wanderlust"
       :url ":pserver:anonymous@cvs.m17n.org:/cvs/root"
       :build `,(let* ((pardir (file-name-as-directory ".."))
                       (emacs (concat el-get-emacs))
                       (deps (apply 'append (mapcar (lambda (x) `("-L" ,(concat pardir x)))
                                            '("apel" "flim" "semi"))))
                       (prep (prin1-to-string
                              '(progn (setq wl-install-utils t)(setq wl-info-lang "en")(setq wl-news-lang "en")))))

                  (mapcar (lambda (target)
                            (mapconcat 'shell-quote-argument
                                       (append `(,emacs) deps `("--eval" ,prep "-batch" "-q" "-no-site-file" "-l" "WL-MK" "-f" ,target "NONE" "NONE")) " "))
                          '("compile-wl-package" "wl-texinfo-format")))
       :info "doc"
       :load-path ("wl" "elmo" "utils")
       )
