;; Do some standard SLIME configuration.
(slime-setup '(slime-fancy slime-tramp))
;; Set the default lisp you want to use (here it's SBCL).
;;(setq inferior-lisp-program "ccl")
(setq inferior-lisp-program "sbcl --dynamic-space-size 4GB")

;; Set the directory where swank-loader.lisp resides.
(setq slime-docker-default-swank-path "/usr/share/common-lisp/source/slime/")

(provide 'my-slime-init)

(put 'def 'common-lisp-indent-function '(2 2 2 &rest 2))
(put 'defconfig 'common-lisp-indent-function '(2 2 &rest 2))
(put 'method-case 'common-lisp-indent-function '(2 &rest 2))

