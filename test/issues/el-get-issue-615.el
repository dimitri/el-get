;; https://github.com/dimitri/el-get/issues/615
;;
;; Allow methods to provide default-website guesser

(el-get-describe 'js2-mode)
(with-current-buffer "*Help*"
  (assert (string-match-p "Website:" (buffer-string)) nil
          "Js2-mode should have a website"))
