(setq
 el-get-sources
 '((:name el-get-install
          :type http
          :url "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
          :checksum "b3a5ada02e27597894210fa1ae2c857579a457ae")))

(el-get 'sync 'el-get-install)
