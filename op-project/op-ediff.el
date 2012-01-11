;;;
;;;
;;;   Leicht variierter Ediff-Mode zum Überladen für die 2er-Befehle des
;;;   Ediffs (ediff-buffers, ediff-files,...). 
;;;
;;;   Neue Eigenschaft: Möglichkeit, Fine-Diff-Regionen variieren und manipulieren 
;;;   zu können, ohne die großen Difference-Regionen zu beeinflussen (sehr nützlich
;;;   falls zum Beispiel bei kleinen Änderungen auch noch Verschiebungen in der 
;;;   Indentierung vorgenommen wurden)
;;;


;;XEmacs 21.11: BUG: ediff-diff3-programm ist nicht definiert
(if xemacsp
    (defcustom ediff-diff3-program "diff3"
      "*Program to be used for three-way comparison.
Must produce output compatible with Unix's diff3 program."
      :type 'string
      :group 'ediff-diff))


(condition-case var
    (require 'ediff)
  (error "Problem loading ediff package "))

(if gnuemacsp
    (progn
      (ediff-defvar-local ediff-current-fine-difference -1 "")
      (ediff-defvar-local ediff-current-difference-save -1 "")))




(defun ediff-goto-fine-difference-s (from-buffer to-buffer arg)
  (interactive "P")
  (ediff-barf-if-not-control-buffer)

  (let ((ediff-vector-from (symbol-value (intern (format "ediff-difference-vector-%S" from-buffer))))
	(ediff-vector-to   (symbol-value (intern (format "ediff-difference-vector-%S" to-buffer))))
	fine-diff-number fine-diff-max current-diff
	begin-from end-from begin-to end-to
	(ecd ediff-current-difference)
	(ediff-multiframe-sv ediff-multiframe)
	)

    (setq fine-diff-number (max 1 arg)
	  fine-diff-max (if (aref
			     (ediff-get-difference ediff-current-difference from-buffer)
			     1)
			    (progn
			      (setq ediff-current-difference ecd)
			      (length (aref
				       (ediff-get-difference ediff-current-difference from-buffer)
				       1)))
			  0)
	  fine-diff-number (max (min (- fine-diff-max 1) arg) 0))

    (if (>= ediff-current-fine-difference fine-diff-max)
	(setq ediff-current-fine-difference (- fine-diff-max 1)))
    (if (< ediff-current-fine-difference 0)
	(setq ediff-current-fine-difference 0))

    (if (> fine-diff-max 0)
	(progn
	  (setq begin-from (overlay-start
			    (aref
			     (aref
			      (ediff-get-difference ediff-current-difference from-buffer)
			      1)
			     fine-diff-number))
		begin-to   (overlay-start
			    (aref
			     (aref
			      (ediff-get-difference ediff-current-difference to-buffer)
			      1)
			     fine-diff-number)))

	  (let ((current-buf (current-buffer))
		(current-frame (window-frame (selected-window)))
		(b1 (symbol-value (intern (format "ediff-buffer-%S" from-buffer))))
		(b2 (symbol-value (intern (format "ediff-buffer-%S" to-buffer)))))
	    (pop-to-buffer b2)
	    (goto-char begin-to)
	    (pop-to-buffer b1)
	    (goto-char begin-from)
	    (if (and
		 (ediff-window-display-p)
		 ediff-multiframe-sv)  ;;; spät -- SMW
		(raise-frame current-frame)
	      (pop-to-buffer current-buf))
	    )))))


(defun ediff-copy-fine-difference (from-buffer to-buffer &optional kill)
  (interactive "P")

  (ediff-barf-if-not-control-buffer)
  (let ((ediff-vector-from (symbol-value (intern (format "ediff-difference-vector-%S" from-buffer))))
	(ediff-vector-to   (symbol-value (intern (format "ediff-difference-vector-%S" to-buffer))))
	fine-diff-number fine-diff-max current-diff
	begin-from end-from begin-to end-to
	(ecd ediff-current-difference))

    (setq fine-diff-max (if (aref
			     (ediff-get-difference ediff-current-difference from-buffer)
			     1)
			    (progn
			      (setq ediff-current-difference ecd)
			      (length (aref
				       (ediff-get-difference ediff-current-difference from-buffer)
				       1))
			      )
			  nil)
	  fine-diff-number (max (min ediff-current-difference 
				     (if fine-diff-max (- fine-diff-max 1) 1))
				0)
	  )
    (if fine-diff-max
	(progn
	  ;(while (> fine-diff-number 0)
	    (setq ediff-current-difference ecd)

	    (setq begin-from (overlay-start
			      (aref
			       (aref
				(ediff-get-difference ediff-current-difference from-buffer)
				1)
			       fine-diff-number))
		  end-from   (overlay-end
			      (aref
			       (aref
				(ediff-get-difference ediff-current-difference from-buffer)
				1)
			       fine-diff-number))
		  begin-to    (overlay-start
			       (aref
				(aref
				 (ediff-get-difference ediff-current-difference to-buffer)
				 1)
				fine-diff-number))
		  end-to      (overlay-end
			       (aref
				(aref
				 (ediff-get-difference ediff-current-difference to-buffer)
				 1)
				fine-diff-number)))

	    (let ((current-buf (current-buffer))
		  (b1 (symbol-value (intern (format "ediff-buffer-%S" from-buffer))))
		  (b2 (symbol-value (intern (format "ediff-buffer-%S" to-buffer)))))
	      ;; falls kill gesetzt ist, wird die andere fine-region gelöscht
	      (if kill
		  (progn
		    (pop-to-buffer b2)
		    (goto-char begin-to)
		    (kill-region begin-to end-to)))
	      (pop-to-buffer b1)
	      (goto-char begin-from)
	      (copy-region-as-kill begin-from end-from)
	      (pop-to-buffer b2)
	      (goto-char begin-to)
	      (yank)
	      (insert-char ?  1)
	      (pop-to-buffer current-buf))
	    ;)

	  (fset 'dummy-func (symbol-function 'modify-frame-parameters))
	  (unwind-protect
	      (progn
		(fset 'modify-frame-parameters 'ediff-dummy)
		(ediff-recenter t)
		(ediff-update-diffs))
	    (fset 'modify-frame-parameters (symbol-function 'dummy-func)))

	  (message "Recovery of buffer A or B with rfa or rfb"))
      (message "no fine diffs found")
      )))



(defun ediff-dummy (&optional arg1 arg2)
;;;  '(lambda()(message "not including \"#include\" files")))
  nil)




(defun ediff-next-fine-difference (&optional arg)
  (interactive "P")
  (if (not arg)
      (setq arg 1))
  (ediff-barf-if-not-control-buffer)
  (if (= ediff-current-difference-save ediff-current-difference)
      (progn
	(setq ediff-current-fine-difference (+ ediff-current-fine-difference arg))
	(ediff-goto-fine-difference-s 'A 'B ediff-current-fine-difference))
    (setq ediff-current-difference-save ediff-current-difference)
    (setq ediff-current-fine-difference 1)
    (ediff-goto-fine-difference 1)))  ; spät


(defun ediff-previous-fine-difference (&optional arg)
  (interactive "P")
  (if (not arg)
      (setq arg 1))
  (ediff-barf-if-not-control-buffer)
  (if (= ediff-current-difference-save ediff-current-difference)
      (progn
	(setq ediff-current-fine-difference (- ediff-current-fine-difference arg))
	(ediff-goto-fine-difference-s 'A 'B ediff-current-fine-difference))
    (setq ediff-current-difference-save ediff-current-difference)
    (setq ediff-current-fine-difference 10000)
    (ediff-goto-fine-difference 10000)))  ; spät



(defun ediff-copy-fine-differences-only-A-B ()
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (ediff-copy-fine-difference 'A 'B))


(defun ediff-copy-fine-differences-only-B-A ()
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (ediff-copy-fine-difference 'B 'A))

(defun ediff-copy-and-kill-fine-differences-only-A-B ()
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (ediff-copy-fine-difference 'A 'B t))


(defun ediff-copy-and-kill-fine-differences-only-B-A ()
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (ediff-copy-fine-difference 'B 'A t))

(defun ediff-goto-fine-difference (arg)
  (interactive "NJump to which fine difference ? ")
  (ediff-barf-if-not-control-buffer)
   (if (not arg)
      (setq arg 1))
  (setq ediff-current-difference-save ediff-current-difference)
  (setq ediff-current-fine-difference (- arg 1))
  (ediff-goto-fine-difference-s 'A 'B ediff-current-fine-difference))


(defun ediff-recover-A ()
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (let ((current-buf (current-buffer)))
    (pop-to-buffer ediff-buffer-A)
    (undo)
    (pop-to-buffer current-buf)
    (fset 'dummy-func (symbol-function 'modify-frame-parameters))
    (unwind-protect
	(progn
	  (fset 'modify-frame-parameters 'ediff-dummy)
	  (ediff-recenter t)
	  (ediff-update-diffs))
      (fset 'modify-frame-parameters (symbol-function 'dummy-func)))))


(defun ediff-recover-B ()
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (let ((current-buf (current-buffer)))
    (pop-to-buffer ediff-buffer-B)
    (undo)
    (pop-to-buffer current-buf)
    (unwind-protect
	(progn
	  (fset 'modify-frame-parameters 'ediff-dummy)
	  (ediff-recenter t)
	  (ediff-update-diffs))
      (fset 'modify-frame-parameters (symbol-function 'dummy-func)))))


(defun ediff-recover-C ()
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (let ((current-buf (current-buffer)))
    (pop-to-buffer ediff-buffer-C)
    (undo)
    (pop-to-buffer current-buf)
    (unwind-protect
	(progn
	  (fset 'modify-frame-parameters 'ediff-dummy)
	  (ediff-recenter t)
	  (ediff-update-diffs))
      (fset 'modify-frame-parameters (symbol-function 'dummy-func)))))




(defun ediff-install-fine-diff-keys ()
  (interactive)
  (save-excursion
    (let ((control-buffer (current-buffer)))
      (set-buffer control-buffer)

      (define-key ediff-mode-map "f" nil)
      (define-key ediff-mode-map "fn" 'ediff-next-fine-difference)
      (define-key ediff-mode-map "fp" 'ediff-previous-fine-difference)
      (define-key ediff-mode-map "fa" 'ediff-copy-fine-differences-only-A-B)
      (define-key ediff-mode-map "fb" 'ediff-copy-fine-differences-only-B-A)
      (define-key ediff-mode-map "fda" 'ediff-copy-and-kill-fine-differences-only-A-B)
      (define-key ediff-mode-map "fdb" 'ediff-copy-and-kill-fine-differences-only-B-A)
      (define-key ediff-mode-map "fg" 'ediff-goto-fine-difference)
      (define-key ediff-mode-map "rfa" 'ediff-recover-A)
      (define-key ediff-mode-map "rfb" 'ediff-recover-B)
      (define-key ediff-mode-map "rfc" 'ediff-recover-C)
      )))




(add-hook 'ediff-startup-hook 'ediff-install-fine-diff-keys t t)

(defconst ediff-long-help-message-head
  "   Moving around	|     Toggling features	    |	    Manipulations
=====================|===========================|======================================"
  "The head of the full help message.")
(defconst ediff-long-help-message-tail
  "=====================|===========================|======================================
    R -show registry |				 |  M	-show session group
    D -diff output   |	   E -browse Ediff manual|  G	-send bug report
    i -status info   |	   ? -help off		 |  z/q -suspend/quit
----------------------------------------------------------------------------------------
X,Y (x,y)  on the left are meta-symbols for the keys  A,B (a,b).
X,Y on the right are meta-symbols for buffers A,B.
A,B on the right denote the working buffers A,B respectively."
  "The tail of the full-help message.")

(setq ediff-long-help-message-compare2
  "
p,DEL -previous diff |	   | -vert/horiz split	 |a/b -copy A/B's region to B/A
n,SPC -next diff     |	   h -hiliting		 | rx -restore buf X's old diff
    j -jump to diff  |	   @ -auto-refinement	 |  * -refine current region
   gx -goto X's point|				 |  ! -update diff regions
  C-l -recenter	     |	  ## -ignore whitespace	 |
  v/V -scroll up/dn  | #f/#h -focus/hide regions | wx -save buf X
  </> -scroll lt/rt  |	   X -read-only in buf X | wd -save diff output
   fn -next fine diff|	   m -wide display	 |
   fp -previous fine |				 | fa -copy fine region from A to B
   fg -goto fine diff|				 | fb -copy fine region from B to A
		     |				 | fda -replace fine region from A to B
		     |				 | fdb -replace fine region from B to A
		     |				 | rfx - undo in buffer X (repeatable)
")


;;; op-ediff ends here
