(defun ediff-info ()
  (interactive)
  (ediff-barf-if-not-control-buffer)
    (ediff-print-diff-vector (intern "ediff-difference-vector-A"))
    (ediff-print-diff-vector (intern "ediff-difference-vector-B"))
    ;(ediff-print-diff-vector (intern "ediff-difference-vector-C"))
    ;(ediff-print-diff-vector (intern "ediff-difference-vector-Ancestor"))
    )



(defun ediff-op-diff-to-diff (arg &optional keys)
  "Copy buffer-X'th difference region to buffer Y \(X,Y are A, B, or C\).
If numerical prefix argument, copy the difference specified in the arg.
Otherwise, copy the difference given by `ediff-current-difference'.
This command assumes it is bound to a 2-character key sequence, `ab', `ba',
`ac', etc., which is used to determine the types of buffers to be used for
copying difference regions. The first character in the sequence specifies
the source buffer and the second specifies the target.

If the second optional argument, a 2-character string, is given, use it to
determine the source and the target buffers instead of the command keys."
  (interactive "P")
  (ediff-barf-if-not-control-buffer)
  (or keys (setq keys (this-command-keys)))
  (if (eq arg '-) (setq arg -1)) ; translate neg arg to -1
  (if (numberp arg) (ediff-jump-to-difference arg))

  (let* ((key1 (aref keys 0))
	 (key2 (aref keys 1))
	 (char1 (if (and ediff-xemacs-p (eventp key1)) (event-key key1) key1))
	 (char2 (if (and ediff-xemacs-p (eventp key1)) (event-key key2) key2))
	 ediff-verbose-p)
    (ediff-op-copy-diff ediff-current-difference
		     (ediff-char-to-buftype char1)
		     (ediff-char-to-buftype char2))
    ;; recenter with rehighlighting, but no messages
    (ediff-recenter)))











;;; AUS ediff-diffs.el

;; Convert diff list to overlays for a given DIFF-REGION
;; in buffer of type FROM-BUFFER
(defun ediff-op-copy-fine-diffs (from-buffer diff-list region-num to-buffer)




  (let* ((current-diff -1)
	 (reg-start-from (ediff-get-diff-posn from-buffer 'beg region-num))
	 (reg-start-to (ediff-get-diff-posn to-buffer 'beg region-num))
	 (buff-from (ediff-get-buffer from-buffer))
	 (buff-to   (ediff-get-buffer to-buffer))
	 combined-merge-diff-list
	 diff-overlay-list list-element
	 begin end overlay)

    (ediff-clear-fine-differences-in-one-buffer region-num from-buffer)
    (setq diff-list (cdr diff-list)) ; discard list type (words or points)
    (ediff-eval-in-buffer buff-from (goto-char reg-start-from))
    (ediff-eval-in-buffer buff-to   (goto-char reg-start-to))
    
    ;; if it is a combined merge then set overlays in buff C specially
    (if (and ediff-merge-job (eq from-buffer 'C)
	     (setq combined-merge-diff-list
		   (ediff-looks-like-combined-merge region-num)))
	(ediff-set-fine-overlays-for-combined-merge
	 combined-merge-diff-list region-num)
      ;; regular fine diff

      (let ((tmp-beginning-of-line)(before-change-funktion nil)
	    (after-change-function nil))
	

	(while diff-list
	  (setq current-diff (1+ current-diff)
		list-element (car diff-list)

		begin-from 	 (aref list-element (cond ((eq from-buffer 'A) 0)
						  ((eq from-buffer 'B) 2)
						  (t 4)))  ; buf C
		end-from 	 (aref list-element (cond ((eq from-buffer 'A) 1)
						  ((eq from-buffer 'B) 3)
						  (t 5)))) ; buf C

		begin-to 	 (aref list-element (cond ((eq to-buffer 'A) 0)
						  ((eq to-buffer 'B) 2)
						  (t 4)))  ; buf C
		end-to 		 (aref list-element (cond ((eq to-buffer 'A) 1)
						  ((eq to-buffer 'B) 3)
						  (t 5)))) ; buf C

	  (if (not (or begin-from end-from))
	      () ; skip this diff
	    ;; Put overlays at appropriate places in buffers
	    ;; convert lines to points, if necessary
	    (setq begin-from (ediff-goto-word (1+ begin-from) buff-from)
		  end-from (ediff-goto-word end-from buff-from 'end))
	    (setq begin-to (ediff-goto-word (1+ begin-to) buff-to)
		  end-to (ediff-goto-word end-to buff-to 'end))
	    ;; Die Variablen begin-from, end-from
	    ;;               begin-to,   end-to
	    ;; geben die jeweilige Point-Position der fine-diff-Region an
	    
	    (ediff-eval-in-buffer buff-to (goto-char begin-to))
	    (ediff-eval-in-buffer buff-from (copy-region-as-kill begin-from end-from))
	    (ediff-eval-in-buffer buff-to (yank))

	    (ediff-make-or-kill-fine-diffs t)

)))))





;;; AUS ediff-utils.el

(defun ediff-op-copy-diff (n from-buf-type to-buf-type
			  &optional batch-invocation reg-to-copy)
  (let* ((to-buf (ediff-get-buffer to-buf-type))
	 ;;(from-buf (if (not reg-to-copy) (ediff-get-buffer from-buf-type)))
	 (ctrl-buf ediff-control-buffer)
	 (saved-p t)
	 (three-way ediff-3way-job)
	 messg
	 ediff-verbose-p
	 reg-to-delete reg-to-delete-beg reg-to-delete-end)
	
    (setq reg-to-delete-beg
	  (ediff-get-diff-posn to-buf-type 'beg n ctrl-buf))
    (setq reg-to-delete-end
	  (ediff-get-diff-posn to-buf-type 'end n ctrl-buf))
	  
    (if reg-to-copy
	(setq from-buf-type nil)
      (setq reg-to-copy (ediff-get-region-contents n from-buf-type ctrl-buf)))
    
    (setq reg-to-delete (ediff-get-region-contents
			 n to-buf-type ctrl-buf
			 reg-to-delete-beg reg-to-delete-end))
    
    (if (string= reg-to-delete reg-to-copy)
	(setq saved-p nil) ; don't copy identical buffers
      ;; seems ok to copy
      (if (or batch-invocation (ediff-test-save-region n to-buf-type))
	  (condition-case conds
	      (progn
		(ediff-eval-in-buffer to-buf
		  ;; to prevent flags from interfering if buffer is writable
		  (let ((inhibit-read-only (null buffer-read-only)))
		    
		    (goto-char reg-to-delete-end)
		    (insert reg-to-copy)
		    




;;; hier Aufruf des Fine-Diff-Teils
		    (message "hier")

		 ;;   (ediff-op-copy-fine-diffs from-buf-type op-diff-list n to-buf-type)





		    (if (> reg-to-delete-end reg-to-delete-beg)
			(kill-region reg-to-delete-beg reg-to-delete-end))
		    ))
		(or batch-invocation
		    (setq 
		     messg
		     (ediff-save-diff-region n to-buf-type reg-to-delete))))
	    (error (message "ediff-copy-diff: %s %s"
			    (car conds)
			    (mapconcat 'prin1-to-string (cdr conds) " "))
		   (beep 1)
		   (sit-for 2) ; let the user see the error msg
		   (setq saved-p nil)
		   )))
      )
    
    ;; adjust state of difference in case 3-way and diff was copied ok
    (if (and saved-p three-way)
	(ediff-set-state-of-diff-in-all-buffers n ctrl-buf))

    (if batch-invocation
	(ediff-clear-fine-differences n)
      ;; If diff3 job, we should recompute fine diffs so we clear them
      ;; before reinserting flags (and thus before ediff-recenter).
      (if (and saved-p three-way)
	  (ediff-clear-fine-differences n))
      
      (ediff-refresh-mode-lines)
      
      ;; For diff2 jobs, don't recompute fine diffs, since we know there
      ;; aren't any. So we clear diffs after ediff-recenter.
      (if (and saved-p (not three-way))
	  (ediff-clear-fine-differences n))
      ;; Make sure that the message about saving and how to restore is seen
      ;; by the user
      (message messg))
    ))
