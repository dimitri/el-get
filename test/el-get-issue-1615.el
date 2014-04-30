;;
;; https://github.com/dimitri/el-get/issues/1615
;;
;; `el-get-parse-proxy' should get proxy info from `url-proxy-services' by:
;;
;;     (cdr (assoc "http" url-proxy-services))
;;
;; According to the documentation of `url-proxy-services'.

(require 'url-vars)

(let ((url-proxy-services '(("http" . "127.0.0.1:8080")))
      (process-environment (cons "HTTP_PROXY" process-environment))
      ;; We have to bind this variable here.  It's from
      ;; `el-get-cvs-checkout-proxy-url', the caller of `el-get-parse-proxy',
      ;; and used by `el-get-parse-proxy'.
      ;;
      ;; This value is from `emacs-w3m'.
      (url ":pserver:anonymous@cvs.namazu.org:/storage/cvsroot"))
  (el-get-parse-proxy url))
