;;; el-get-eval-after-load-compile.el --- Compiling version of eval-after-load

;; Author: INA Lintaro <tarao.gnn at gmail.com>
;; URL: https://github.com/tarao/bundle-el/tree/el-get
;; Version: 0.1
;; Keywords: emacs compile

;; This file is NOT part of GNU Emacs.

;;; License:
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(eval '(eval-when-compile (require 'cl)))

;;;###autoload
(defmacro el-get-eval-after-load-compile (feature &rest form)
  (declare (indent defun))
  (let ((feat (if (and (listp feature) (eq (nth 0 feature) 'quote))
                  (nth 1 feature) feature)) loaded)
    (eval '(eval-when (compile)
             (setq loaded
                   (condition-case ()
                       ;; don't eval after-loads during compilation
                       (let ((after-load-alist nil))
                         (cond ((stringp feat) (load feat))
                               ((symbolp feat) (require feat))))
                     (error nil)))))
    (if loaded
        ;; byte-compiled version
        `(eval-after-load ,feature
           '(funcall ,(byte-compile `(lambda () ,@form))))
      ;; normal version
      `(eval-after-load ,feature '(progn ,@form)))))

(provide 'el-get-eval-after-load-compile)
;;; el-get-eval-after-load-compile.el ends here
