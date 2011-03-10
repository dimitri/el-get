(:name dtrt-indent
       :type git
       :url "git://git.savannah.nongnu.org/dtrt-indent.git"
       :features dtrt-indent
       :post-init (lambda () (dtrt-indent-mode 1)))
