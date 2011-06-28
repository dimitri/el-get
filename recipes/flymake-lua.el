(:name flymake-lua
       :website "https://github.com/sroccaserra/emacs/blob/master/flymake-lua.el"
       :description "Flymake support for Lua."
       :type http
       :url "https://raw.github.com/sroccaserra/emacs/master/flymake-lua.el"
       :post-init (lambda ()
		(add-hook 'lua-mode-hook 'flymake-lua-load)))