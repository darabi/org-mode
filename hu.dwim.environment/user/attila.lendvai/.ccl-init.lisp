;;; -*- mode: common-lisp -*-

(setf *print-right-margin* 150)

(setf ccl:*default-external-format* (make-external-format :character-encoding :utf-8 :line-termination :unix))

(load (merge-pathnames "workspace/hu.dwim.environment/source/environment.lisp" (user-homedir-pathname)))
