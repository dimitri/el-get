(:name lua-mode
       :type git
       :url "git://github.com/rrthomas/lua-mode.git"
       :features lua-mode
       :after (lambda ()
                (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))
		(autoload 'lua-mode "lua-mode" "Lua editing mode." t)))
