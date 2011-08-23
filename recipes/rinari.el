(:name rinari
       :description "Rinari Is Not A Rails IDE"
       :type git
       :url "http://github.com/eschulte/rinari.git"
       :load-path ("." "util" "util/jump")
       :compile ("\\.el$" "util")
       :build ("rake doc:install_info")
       :info "doc"
       :features rinari)
