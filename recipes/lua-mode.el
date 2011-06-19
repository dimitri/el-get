(:name lua-mode
       :type git
       :url "https://github.com/rrthomas/lua-mode.git"
       :features lua-mode
       :post-init (lambda ()
		    (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))
		    (autoload 'lua-mode "lua-mode" "Lua editing mode." t)))
