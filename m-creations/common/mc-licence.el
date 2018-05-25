;;;
;;; Copyright (c) 2018 m-creations gmbh, All rights reserved
;;;
;;; See LICENCE for details.
;;;

(defun mc-licence ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (insert "\n Copyright (c) "
            (format-time-string "%Y")
            " m-creations gmbh, All rights reserved\n\n See LICENCE for details.\n")
    (string-insert-rectangle (point-min) (point)
                             (case major-mode
                               ((or lisp-mode emacs-lisp-mode) ";;;")
                               ((or js-mode js2-mode typescript-mode java-mode) "//")
                               (sh-mode "#")))))

(provide 'mc-licence)
