;;;
;;; Blendet alle Zeilen, die ein bestimmes Pattern matchen, aus.
;;; geschrieben am 23.11.1994 von Matthias Kapffer und Stefan Walther
;;;



;;; ----------------------------------------------------------------------------------------------------
;;; ----------------------------------------------------------------------------------------------------
;;; ----------------------------------------------------------------------------------------------------
;;; Benutzerdefiniertes (für andere Regexpressions -1 in -2 und "\\*+E|\\*+FW" austauschen)
;;;


(defun op-hide-at-end-of-show ()
;;
;; which headline signs (i.g.  ***E ) have to be shown?
;;
;; 		   ["Toggle Everything In Region [...]" ausblend-0 t]
;; 		   ["Toggle *E[rledigtes] / *F[ällt]W[eg] in region " ausblend-1 t]
;; 		   ["Toggle *S[tändiges] / *W[iederkehrendes] in region " ausblend-2 t]
;; 		   ["Toggle *V[erschobenes] in region " ausblend-3 t]
;; 		   ["Toggle *I[n]B[earbeitung] in region " ausblend-4 t]
;; 		   ["Toggle *T[ermin] in region " ausblend-6 t]
;; 		   ["Toggle *R[essourcen] in region " ausblend-5 t]
;; 		   ["Markierung   *D   für  D[ringendes] \t(ohne Funktion)" nil t]
    (if ausblend-toggle-status-1
	(hide-lines-matching-regexpr-end-mainheadlines "\\*+E\\|\\*+FW" (point-min) (point-max)))
    (if ausblend-toggle-status-2
	(hide-lines-matching-regexpr-end-mainheadlines "\\*+W\\|\\*+S" (point-min) (point-max)))
    (if ausblend-toggle-status-3
	(hide-lines-matching-regexpr-end-mainheadlines "\\*+V" (point-min) (point-max)))
    (if ausblend-toggle-status-4
	(hide-lines-matching-regexpr-end-mainheadlines "\\*+I" (point-min) (point-max)))
    (if ausblend-toggle-status-5
	(hide-lines-matching-regexpr-end "[ \t]*#+R" (point-min) (point-max)))
    (if ausblend-toggle-status-6
	(hide-lines-matching-regexpr-end "[ \t]*#+T" (point-min) (point-max))))


(defvar ausblend-toggle-status-1 nil)
(make-local-variable 'ausblend-toggle-status-1)
(defun ausblend-1 ()
  (interactive)
  (if xemacsp (setq mark-active (mark)))
  (hide-lines-matching-regexpr-mainheadlines "\\*+E\\|\\*+FW" 'ausblend-toggle-status-1
					     (if mark-active (region-beginning)(point-min))
					     (if mark-active (region-end)(point-max))))
(define-key outline-mode-prefix-map "1" 'ausblend-1)


(defvar ausblend-toggle-status-2 nil)
(make-local-variable 'ausblend-toggle-status-2)
(defun ausblend-2 ()
  (interactive)
  (if xemacsp (setq mark-active (mark)))
  (hide-lines-matching-regexpr-mainheadlines "\\*+W\\|\\*+S" 'ausblend-toggle-status-2
			       (if mark-active (region-beginning)(point-min))
			       (if mark-active (region-end)(point-max))))
(define-key outline-mode-prefix-map "2" 'ausblend-2)


(defvar ausblend-toggle-status-3 nil)
(make-local-variable 'ausblend-toggle-status-3)
(defun ausblend-3 ()
  (interactive)
  (if xemacsp (setq mark-active (mark)))
  (hide-lines-matching-regexpr-mainheadlines "\\*+V" 'ausblend-toggle-status-3
			       (if mark-active (region-beginning)(point-min))
			       (if mark-active (region-end)(point-max))))
(define-key outline-mode-prefix-map "3" 'ausblend-3)


(defvar ausblend-toggle-status-4 nil)
(make-local-variable 'ausblend-toggle-status-4)
(defun ausblend-4 ()
  (interactive)
  (if xemacsp (setq mark-active (mark)))
  (hide-lines-matching-regexpr-mainheadlines "\\*+I" 'ausblend-toggle-status-4
			       (if mark-active (region-beginning)(point-min))
			       (if mark-active (region-end)(point-max))))
(define-key outline-mode-prefix-map "4" 'ausblend-4)


(defvar ausblend-toggle-status-5 nil)
(make-local-variable 'ausblend-toggle-status-5)
(defun ausblend-5 ()
  (interactive)
  (if xemacsp (setq mark-active (mark)))
  (hide-lines-matching-regexpr "[ \t]*#+R" 'ausblend-toggle-status-5
			       (if mark-active (region-beginning)(point-min))
			       (if mark-active (region-end)(point-max))))
(define-key outline-mode-prefix-map "5" 'ausblend-5)


(defvar ausblend-toggle-status-6 nil)
(make-local-variable 'ausblend-toggle-status-6)
(defun ausblend-6 ()
  (interactive)
  (if xemacsp (setq mark-active (mark)))
  (hide-lines-matching-regexpr "[ \t]*#+T" 'ausblend-toggle-status-6
			       (if mark-active (region-beginning)(point-min))
			       (if mark-active (region-end)(point-max))))
(define-key outline-mode-prefix-map "6" 'ausblend-6)


(defun set-show-ausblend-all ()
  (interactive)
  (if xemacsp (setq mark-active (mark)))  
  (setq ausblend-toggle-status-1 t)
  (hide-lines-matching-regexpr-mainheadlines "\\*+E\\|\\*+FW" 'ausblend-toggle-status-1
					     (if mark-active (region-beginning)(point-min))
					     (if mark-active (region-end)(point-max)))
  (setq ausblend-toggle-status-2 t)
  (hide-lines-matching-regexpr-mainheadlines "\\*+W\\|\\*+S" 'ausblend-toggle-status-2
					     (if mark-active (region-beginning)(point-min))
					     (if mark-active (region-end)(point-max)))
  (setq ausblend-toggle-status-3 t)
  (hide-lines-matching-regexpr-mainheadlines "\\*+V" 'ausblend-toggle-status-3
					     (if mark-active (region-beginning)(point-min))
					     (if mark-active (region-end)(point-max)))
  (setq ausblend-toggle-status-4 t)
  (hide-lines-matching-regexpr-mainheadlines "\\*+I" 'ausblend-toggle-status-4
					     (if mark-active (region-beginning)(point-min))
					     (if mark-active (region-end)(point-max)))
  (setq ausblend-toggle-status-5 t)
  (hide-lines-matching-regexpr "[ \t]*#+R" 'ausblend-toggle-status-5
			       (if mark-active (region-beginning)(point-min))
			       (if mark-active (region-end)(point-max)))
  (setq ausblend-toggle-status-6 t)
  (hide-lines-matching-regexpr "[ \t]*#+T" 'ausblend-toggle-status-6
			       (if mark-active (region-beginning)(point-min))
			       (if mark-active (region-end)(point-max))))
(define-key outline-mode-prefix-map "0" 'set-show-ausblend-all)




;;;
;;; ----------------------------------------------------------------------------------------------------
;;; ----------------------------------------------------------------------------------------------------
;;; ----------------------------------------------------------------------------------------------------
;;;
;;; Mode-Funktionen



(defun hide-lines-matching-regexpr-mainheadlines (regexpr hidden-toggle-pat1-status startpkt ende)
  (save-excursion
    (let ((before-change-functions nil)(after-change-functions nil))
      (setq selective-display nil)
      (if xemacsp (setq mark-active (mark)))
      (if mark-active (narrow-to-region startpkt ende))
      (goto-char (point-min))
      (if (save-excursion (re-search-forward regexpr nil t))
	  (progn
	    (let ((already-modified (buffer-modified-p)))
	      (goto-char (point-min))
	      (if (symbol-value hidden-toggle-pat1-status)
		  ;; ausklappen  
		  (let ((p1))
		    (while (re-search-forward (concat "\r\\(" regexpr "\\)") nil t)
		      (setq p1 (point))
		      (replace-match "\n\\1" nil nil)
		      ;; bis zur naechsten Ueberschrift
		      (if (not (re-search-forward "[\n\r]\\*" nil t))
			  (goto-char (point-max)))
		      (backward-char 2)
		      ;; und jetzt alles ersetzen
		      (while (re-search-backward "\r" p1 t)
			(replace-match "\n" nil nil))))
		;; einklappen
		(let ((p1))
		  (while (re-search-forward (concat "\n\\(" regexpr "\\)") nil t)
		    (setq p1 (point))
		    (replace-match "\r\\1" nil nil)
		    ;; bis zur naechsten Ueberschrift
		    (if (not (re-search-forward "[\n\r]\\*" nil t))
			(goto-char (point-max)))
		    (backward-char 2)
		    (while (re-search-backward "\n" p1 t)
		      (replace-match "\r" nil nil))
		    )))
	      (set-buffer-modified-p already-modified))))
      (if xemacsp (setq mark-active (mark)))
      (if mark-active (widen))
      (if window-system
	  (hilit-repaint-command t))
      (setq selective-display t)
      (set hidden-toggle-pat1-status (not (symbol-value hidden-toggle-pat1-status))))))



(defun hide-lines-matching-regexpr-end-mainheadlines (regexpr startpkt ende)
  (let ((before-change-functions nil)(after-change-functions nil))
    (save-excursion
      (goto-char (point-min))
      (if (save-excursion (re-search-forward regexpr nil t))
	  (progn
	    (setq selective-display nil)
	    (let ((already-modified (buffer-modified-p))(tmp))
	      (while (re-search-forward (concat "\n\\(" regexpr "\\)") nil t)
		(replace-match "\r\\1" nil nil)
		(if (< (point-min) (match-beginning 0))
		    (setq tmp (- (match-beginning 0) 1))
		  (setq tmp (match-beginning 0)))
		(if (re-search-forward "[\n\r]\\*" nil t)
		    nil
		  (goto-char (point-max)))
		(backward-char 3)
		(while (re-search-backward "\n" tmp t)
		  (replace-match "\r" nil nil)))
	      (set-buffer-modified-p already-modified))
	    (setq selective-display t))))))




(defun hide-lines-matching-regexpr (regexpr hidden-toggle-pat1-status startpkt ende)
  (let ((before-change-functions nil)(after-change-functions nil))
    (save-excursion
      (setq selective-display nil)
      (goto-char (point-min))
      (if xemacsp (setq mark-active (mark)))
      (if mark-active (narrow-to-region startpkt ende))
      (if (save-excursion (re-search-forward regexpr nil t))
	  (progn
	    (let ((already-modified (buffer-modified-p)))
	      (if (symbol-value hidden-toggle-pat1-status)
		  (while (re-search-forward (concat "\r\\(" regexpr "\\)") nil t)
		    (replace-match "\n\\1" nil nil))
		(while (re-search-forward (concat "\n\\(" regexpr "\\)") nil t)
		  (replace-match "\r\\1" nil nil)))
	      (set-buffer-modified-p already-modified))))
      (if xemacsp (setq mark-active (mark)))
      (if mark-active (widen))
      (if window-system
	  (hilit-repaint-command t))
      (setq selective-display t)
      (set hidden-toggle-pat1-status (not (symbol-value hidden-toggle-pat1-status))))))



(defun hide-lines-matching-regexpr-end (regexpr startpkt ende)
  (let ((before-change-functions nil)(after-change-functions nil))
    (save-excursion
      (goto-char (point-min))
      (if (save-excursion (re-search-forward regexpr nil t))
	  (progn
	    (setq selective-display nil)
	    (let ((already-modified (buffer-modified-p)))
	      (while (re-search-forward (concat "\n\\(" regexpr "\\)") nil t)
		(replace-match "\r\\1" nil nil))
	      (set-buffer-modified-p already-modified))
	    (setq selective-display t))))))





;;;
;;; Mache Report-File: Alle Ellipsen werden in einen extra Buffer geschaufelt und vernichtet, zusätzlich
;;; noch sonst ein C-artige Kommentare und (find-file-other-window ... ...)
;;;
(defun make-report ()
  (interactive)
  (save-excursion
    (if xemacsp (setq mark-active (mark)))
    (kill-all-this-regexpr "\r.*"
			   (if mark-active (region-beginning)(point-min))
			   (if mark-active (region-end)(point-max)))
    (other-window 1)
    (mark-whole-buffer)
    (remove-simple-re-comments 1 "\n[ \t]*(find-file-other-window" ")[ \t]*\n" )
    (mark-whole-buffer)
    (remove-simple-re-comments 1 "(find-file-other-window" ")" )
    (mark-whole-buffer)
    (remove-simple-re-comments 1 "\n[ \t]*(insert-file" ")[ \t]*\n" )
    (mark-whole-buffer)
    (remove-simple-re-comments 1 "(insert-file" ")" )
    (if (boundp 'perlIsCallableFromEmacs)
	(if perlIsCallableFromEmacs 
	    (progn
	      (mark-whole-buffer)
	      (remove-/*-comments 1))))
    (other-window -1)))



(defun kill-ellipses ()
  (interactive)
  (save-excursion
    (if xemacsp (setq mark-active (mark)))
    (kill-all-this-regexpr "\r.*"
			   (if mark-active (region-beginning)(point-min))
			   (if mark-active (region-end)(point-max)))))


(defun kill-all-this-regexpr (regexpr startpkt ende)
  (save-excursion
    (setq selective-display t)
    (let ((already-modified (buffer-modified-p)))
      (copy-to-buffer "Removed--still_to_save" startpkt ende)
      (split-window-vertically)
      (other-window 1)
      (switch-to-buffer "Removed--still_to_save")
      (replace-regexp (concat "\n" regexpr) "")
      (replace-regexp regexpr "")
      (other-window -1)
      (set-buffer-modified-p already-modified))))





(defun show-ifdefs-in-region ()
  (interactive)
  (if xemacsp (setq mark-active (mark)))
  (show-ifdefs-in-region-sub (if mark-active (region-beginning)(point-min))
			 (if mark-active (region-end)(point-max)))
  (end-of-show))
(define-key (current-local-map) "\M-c\M-s" 'show-ifdefs-in-region)

(defun show-ifdefs-in-region-sub (startpkt ende)
  (save-excursion
    (setq selective-display nil)
    (let ((already-modified (buffer-modified-p))(before-change-functions nil)(after-change-functions nil))
      (narrow-to-region startpkt ende)
      (goto-char (point-min))
      (show-ifdefs)
      (widen)
      (setq selective-display t)
      (set-buffer-modified-p already-modified))))




(defun hide-ifdefs-in-region ()
  (interactive)
  (if xemacsp (setq mark-active (mark)))
  (hide-ifdefs-in-region-sub (if mark-active (region-beginning)(point-min))
			 (if mark-active (region-end)(point-max))))
(define-key (current-local-map) "\M-c\M-h" 'hide-ifdefs-in-region)

(defun hide-ifdefs-in-region-sub (startpkt ende)
  (save-excursion
    (setq selective-display t)
    (let ((already-modified (buffer-modified-p))(before-change-functions nil)(after-change-functions nil))
      (narrow-to-region startpkt ende)
      (goto-char (point-min))
      (show-ifdefs)
      (hide-ifdefs)
      (widen)
      (setq selective-display t)
      (set-buffer-modified-p already-modified))))


