;; http://formgames.org/emacs/blender-python-mode/
(:name blender-python-mode
       :type bzr
       :url "http://bazaar.launchpad.net/~diresu/blender-python-mode/blender-python-mode/"
       :info "Edit blender python code in Emacs - blender must be patched"

       ;; :features 'blender-python-mode (must be required afetr blender-python-mode-installation-dir has been set!
       :after (lambda () 
                (setq blender-python-mode-installation-dir (el-get-package-directory "blender-python-mode"))
                (require 'blender-python-mode))
       )
