;;
;; Setup one standard indentation for lisp-mode
;;
;; (c) m-creations gmbh 2018
;;

;; first, require the other dwim hooks to place us at the end
(require 'hu.dwim.syntax-sugar)
(require 'hu.dwim.def)
(require 'hu.dwim.logger)
(require 'hu.dwim.quasi-quote)

(defun mc-lisp-indent-hook ()
  (let ((overrides
         '((:default-initargs (&rest))
           ;; defsystem
           (:module (0 1 (&whole 1 &rest 1) 1 &body))
           (the (2 &body))
           (define-backend-method defmethod)
           (define-layered-function defgeneric)
           (defmethod/cc defmethod)
           (defgeneric/cc defgeneric)
           (define-layered-method defmethod)
           (define-application-method defmethod)
           (define-form-method defmethod)
           (define-dynamic-context defclass)
           (defclass* defclass)
           (defcondition* defcondition)
           (defresources (4 &rest (&whole 2 &lambda &body)))
           (make-xml-element (4 &lambda &body)))))
    (dolist (el overrides)
      (put (first el) 'common-lisp-indent-function
           (if (symbolp (second el))
               (get (second el) 'common-lisp-indent-function)
             (second el))))))

;; It is likely, that we are called in a lisp-mode hook, so it is a
;; good idea to call the hooks once, if not done, already.
(unless (find 'mc-lisp-indent-hook lisp-mode-hook)
  (add-hook 'lisp-mode-hook 'mc-lisp-indent-hook)
  (hu.dwim.syntax-sugar.install-greek-letter-rules)
  (hu.dwim.syntax-sugar.install-square-bracket-lambda-rule)
  (hu.dwim.logger.lisp-mode-hook)
  (hu.dwim.quasi-quote:lisp-mode-hook)
  (hu.dwim.def.lisp-mode-hook)
  (mc-lisp-indent-hook))

(provide 'mc-lisp-indent)
