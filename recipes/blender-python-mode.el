;; http://formgames.org/emacs/blender-python-mode/
(:name blender-python-mode
       :type bzr
       :url "http://bazaar.launchpad.net/~diresu/blender-python-mode/blender-python-mode/"
       :info "Edit blender python code in Emacs - blender must be patched"
       :prepare (lambda ()
		  (setq blender-python-mode-installation-dir
			(el-get-package-directory "blender-python-mode")))
       :features 'blender-python-mode)
