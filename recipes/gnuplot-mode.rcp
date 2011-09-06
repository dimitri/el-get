(:name gnuplot-mode
       :description "Drive gnuplot from within emacs"
       :type http-tar
       :url "http://cars9.uchicago.edu/~ravel/software/gnuplot-mode/gnuplot-mode.0.6.0.tar.gz"
       :options ("xzf")
       :build `("./configure"
		,(concat "make EMACS=" el-get-emacs " gnuplot.elc gnuplot-gui.elc"))
       :info "gnuplot.info")
