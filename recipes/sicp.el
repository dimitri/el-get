(:name sicp
       :type http
       :url "http://www.neilvandyke.org/sicp-texi/sicp.info.gz"
       ;; el-get thinks it's downloading some .el source file
       :build ("mv sicp.el sicp.info.gz" "gunzip sicp.info.gz")
       :info ".")