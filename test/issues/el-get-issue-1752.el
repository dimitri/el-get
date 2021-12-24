(setq
 el-get-sources
 '((:name el-get-install
          :type http
          :url "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
          :checksum "9495f609e1635dde17d70a89f021003fbc0372c8")))

(el-get 'sync 'el-get-install)
