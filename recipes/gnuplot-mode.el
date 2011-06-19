(:name gnuplot-mode
       :type http-tar
       :url "http://cars9.uchicago.edu/~ravel/software/gnuplot-mode/gnuplot-mode.0.6.0.tar.gz"
       :options ("xzf")
       :build `("./configure" ,(concat "make EMACS=" el-get-emacs))
       :info "gnuplot.info")
