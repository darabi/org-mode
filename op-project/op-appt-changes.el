;;; Aenderungen zu dem File
;;; appt.el --- appointment notification functions.
;;; aus dem Emacs-Lisp-Directory



(defun appt-disp-window-softly (min-to-app new-time appt-msg)
  "Veränderte Funktion aus appt.el (ursprünglich appt-disp-window).
Diese hier poppt aber nicht ständig den Emacs nach vorne."
  (require 'electric)

  ;; Make sure we're not in the minibuffer
  ;; before splitting the window.

  (if (equal (selected-window) (minibuffer-window))
      (if (other-window 1) 
	  (select-window (other-window 1))
	(if window-system
	    (select-frame (other-frame 1)))))
      
  (let* ((this-buffer (current-buffer))
	 (this-window (selected-window))
	 (appt-disp-buf (set-buffer (get-buffer-create appt-buffer-name))))

    (appt-select-lowest-window)
    (if (cdr (assq 'unsplittable (frame-parameters)))
	;; In an unsplittable frame, use something somewhere else.
	(display-buffer appt-disp-buf)
      ;; Otherwise, split the bottom window and use the lower part.
      (split-window)
      (pop-to-buffer appt-disp-buf))
    (setq mode-line-format 
	  (concat "-------------------- ! Termin in "
		  min-to-app " Minuten ! " new-time " %-"))
    (insert-string appt-msg)
    (shrink-window-if-larger-than-buffer (get-buffer-window appt-disp-buf t))
    (set-buffer-modified-p nil)
    ;;; SMW (raise-frame (selected-frame))
    (select-window this-window)
    (if appt-audible
	(beep 1))))

(setq appt-disp-window-function 'appt-disp-window-softly)

