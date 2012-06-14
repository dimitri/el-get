(el-get 'sync 'package)
(el-get-elpa-build-local-recipes)
(el-get 'sync 'caps-mode)
(assert (el-get-package-is-installed 'caps-mode) nil
        "Package caps-mode should be installed")
