(:name yasnippet
       :website "http://code.google.com/p/yasnippet/"
       :description "YASnippet is a template system for Emacs."
       :type svn
       :url "http://yasnippet.googlecode.com/svn/trunk/"
       :features "yasnippet"
       :prepare (lambda ()                      
                      ;; Set up the default snippets directory
                      ;;
                      ;; Principle: don't override any user settings
                      ;; for yas/snippet-dirs, whether those were made
                      ;; with setq or customize.  If the user doesn't
                      ;; want the default snippets, she shouldn't get
                      ;; them!
                      (unless (or (boundp 'yas/snippet-dirs) (get 'yas/snippet-dirs 'customized-value))
                        (setq yas/snippet-dirs 
                              (list (concat el-get-dir (file-name-as-directory "yasnippet") "snippets")))))

       :post-init (lambda ()
                      ;; Trick customize into believing the standard
                      ;; value includes the default snippets.
                      ;; yasnippet would probably do this itself,
                      ;; except that it doesn't include an
                      ;; installation procedure that sets up the
                      ;; snippets directory, and thus doesn't know
                      ;; where those snippets will be installed.  See
                      ;; http://code.google.com/p/yasnippet/issues/detail?id=179
                      (put 'yas/snippet-dirs 'standard-value 
                           ;; as cus-edit.el specifies, "a cons-cell
                           ;; whose car evaluates to the standard
                           ;; value"
                           (list (list 'quote
                                 (list (concat el-get-dir (file-name-as-directory "yasnippet") "snippets"))))))
       ;; byte-compile load vc-svn and that fails
       ;; see https://github.com/dimitri/el-get/issues/200
       :compile nil)
