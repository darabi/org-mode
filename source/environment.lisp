;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

;;;;;;
;;; Sets up the environment related to ASDF.
;;;
;;; NOTE: this file only works when eval'd (e.g. --load), because EVAL-WHEN's are missing

(in-package :common-lisp-user)

#+sbcl
(setf sb-ext:*compiler-print-variable-alist*
      '((*print-length* . 3)
        (*print-level* . 2)
        (*print-pretty* . nil)))

#+ccl
(require :asdf)

;;;;;;
;;; ASDF

(eval-when (:compile-toplevel :load-toplevel :execute)

#+clisp
(map nil (lambda (symbol)
           (shadow symbol :cl-user))
     '(#:getenv))

(require :asdf)

;; these are moslty KLUDGE'es to keep things uniform on lisps other than SBCL (which has no issue with (use-package :asdf :common-lisp-user))
#+(and ccl-1.8 (not ccl-1.9))
(when (find :ccl-1.8 *features*)
  (map nil (lambda (symbol)
             (shadow symbol :cl-user))
       '(asdf:directory-pathname-p
         asdf:getenvasdf:version
         asdf:operation
         asdf:load-system)))

#+allegro
(map nil (lambda (symbol)
           (shadow symbol :cl-user))
     '(asdf/backward-interface:run-shell-command
       asdf:defsystem
       asdf:load-system
       asdf:find-system
       asdf:compile-system
       asdf:source-file))

#+clisp
(map nil (lambda (symbol)
           (shadow symbol :cl-user))
     '(#:run-shell-command
       #:retry
       #:asdf-version))

(use-package :asdf :common-lisp-user)

) ; eval-when

;;;;;;
;;; hu.dwim.asdf

(asdf:clear-source-registry)

(labels
    ((as-asdf-entry (system-name)
       (let ((base-pathname (merge-pathnames (concatenate 'string "../../" (string-downcase system-name) "/")
                                             (or *compile-file-truename*
                                                 *load-truename*))))
         (handler-case
             (list :directory (truename (make-pathname :directory (pathname-directory base-pathname) :name nil :type nil :defaults base-pathname)))
           (serious-condition ()
             (warn "; environment.lisp is ignoring missing dir/system ~S" system-name)
             nil)))))
  (let ((entries (remove nil (mapcar #'as-asdf-entry (list "asdf/"
                                                           "asdf/uiop/"
                                                           :hu.dwim.asdf)))))
    (format *trace-output* "; environment.lisp is extending ASDF source registry with ~S using heuristics~%" entries)
    (asdf:initialize-source-registry `(:source-registry ,@entries :ignore-inherited-configuration))))

(format *trace-output* "; environment.lisp is (asdf:load-system :asdf) to make sure ASDF is upgraded~%")
;; make sure we load/upgrade asdf as early as possible if there's a newer version in the registry
(asdf:load-system :asdf)

(format *trace-output* "; environment.lisp ASDF version after the forced upgrade is ~S~%" (asdf:asdf-version))

;; this tells ASDF to signal a full error if there are any warnings at compile and/or load.
(let ((fn (find-symbol (string '#:enable-deferred-warnings-check) :asdf)))
  (funcall fn))

(asdf:load-system :hu.dwim.asdf)

(use-package :hu.dwim.asdf :common-lisp-user)

(format *trace-output* "; environment.lisp will now initialize the ASDF source registry, *workspace-directory* is ~S~%" (truename hu.dwim.asdf::*workspace-directory*))

(hu.dwim.asdf:initialize-asdf-source-registry (list *workspace-directory* (merge-pathnames "global/" *workspace-directory*)))

;; if we upgraded asdf after :hu.dwim.asdf has been loaded, then reload our customizations again because the asdf:perform method and stuff has been uninterned
;; (asdf:load-system :hu.dwim.asdf)

(format *trace-output* "; environment.lisp finished initializing the ASDF source registry~%")
