(require 'el-get)
(require 'cl)
(setq debugger-batch-max-lines (+ 50 max-lisp-eval-depth)
      debug-on-error t
      el-get-verbose t
      el-get-notify-type 'message)
