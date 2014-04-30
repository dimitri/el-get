;;; https://github.com/dimitri/el-get/issues/1006
;;; autoloads haven't run in :after block
;;;
;;; NOTE: must run twice without cleaning to trigger failure: the
;;; problem only happens when package is already installed.
;;;
;;; DO_NOT_CLEAN=t ./run-test.sh 1006
;;; DO_NOT_CLEAN=t ./run-test.sh 1006 # fails on second run

(setq el-get-sources '((:name whole-line-or-region
                              :after (whole-line-or-region-mode))))

;;; setting el-get-is-lazy makes it work
                                        ;(setq el-get-is-lazy t)

(el-get 'sync 'whole-line-or-region)

(assert (fboundp 'whole-line-or-region-mode))
