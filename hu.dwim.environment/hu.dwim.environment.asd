;;;
;;; This asd file ensures that quickdist does pick up
;;; the repository
;;;

(asdf:defsystem #:hu.dwim.environment
  :description "Contains emacs lisp and text files which should be bundled by quickdist"
  :author "The dwim.hu team"
  :license "Public Domain"
  :serial t
  :components ((:static-file "README")
               (:static-file "LICENCE")))
