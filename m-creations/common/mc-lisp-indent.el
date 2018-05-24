;;
;; Setup one standard indentation for lisp-mode
;;
;; (c) m-creations gmbh 2018
;;

(defun mc-lisp-indent-hook ()
  (let ((overrides
         '((:default-initargs (&rest))
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
           (def (2 2 (&whole 2 &rest 2) &body)))
           (make-xml-element (4 &lambda &body)))))
    (dolist (el overrides)
      (put (first el) 'common-lisp-indent-function
           (if (symbolp (second el))
               (get (second el) 'common-lisp-indent-function)
             (second el))))))

(add-hook 'lisp-mode-hook 'mc-lisp-indent-hook)

(provide 'mc-lisp-indent)
