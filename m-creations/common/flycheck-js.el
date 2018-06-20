;;;
;;; Copyright (c) 2018 m-creations gmbh, All rights reserved
;;;
;;; See LICENCE for details.
;;;

(require 'flycheck)
(require 'dash)

;; (c) lunaryorn
;; https://github.com/lunaryorn/old-emacs-configuration/blob/master/lisp/lunaryorn-flycheck.el#L62
;;
(defun mc-use-js-executables-from-node-modules ()
  "Set executables of JS checkers from local node modules."
  (-when-let* ((file-name (buffer-file-name))
               (root (locate-dominating-file file-name "node_modules"))
               (module-directory (expand-file-name "node_modules" root)))
    (pcase-dolist (`(,checker . ,module) '((javascript-jshint . "jshint")
                                           (javascript-eslint . "eslint")
                                           (javascript-jscs   . "jscs")))
      (let ((package-directory (expand-file-name module module-directory))
            (executable-var (flycheck-checker-executable-variable checker)))
        (when (file-directory-p package-directory)
          (set (make-local-variable executable-var)
               (expand-file-name (concat "bin/" module ".js")
                                 package-directory)))))))

(provide 'flycheck-js)
