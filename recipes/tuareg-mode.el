(:name tuareg-mode
       :type git
       :url "https://github.com/emacsmirror/tuareg.git"
       :load-path (".")
       :build ("make elc")
       :post-init
       (lambda ()
         (add-to-list 'auto-mode-alist '("\\.ml[iylp]?" . tuareg-mode))
         (autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
         (autoload 'camldebug "camldebug" "Run the Caml debugger" t)
         (dolist (ext '(".cmo" ".cmx" ".cma" ".cmxa" ".cmi"))
           (add-to-list 'completion-ignored-extensions ext))))
