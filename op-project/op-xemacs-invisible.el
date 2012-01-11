              ;
(if gnuemacsp ;
    nil       ;
;=============;



(defun region-invisible-unsearchable ()
  (interactive)
  (save-excursion
    (add-text-properties (region-beginning) (region-end) '(invisible t))
    ;; (put-text-property (region-beginning) (region-end) 'invisible t)
    (map-extents (lambda
		   (extent ignored)
		   (set-extent-property extent 'isearch-open-invisible nil))
		 nil
		 (region-beginning) (region-end)
		 nil
		 'all-extents-closed 'invisible)))

(defun region-invisible-searchable ()
  (interactive)
  (save-excursion
    (add-text-properties (region-beginning) (region-end) '(invisible t))
    (map-extents (lambda
		   (extent ignored)
		   (set-extent-property extent 'isearch-open-invisible t))
		 nil
		 (region-beginning) (region-end)
		 nil
		 'all-extents-closed 'invisible)))

(defun current-position-visible ()
  (interactive)
  (let ((mark-beginning nil) (mark-end nil)
	(already-modified (buffer-modified-p)))
    (save-excursion
      (setq mark-beginning (- (save-excursion (re-search-backward "[\r\n]" nil t) (point)) 1))
      (setq mark-end (save-excursion (re-search-forward "[\r\n]" nil t) (point)))
      (narrow-to-region mark-beginning mark-end)
      (add-text-properties (point-min) (point-max) '(invisible nil))
      (goto-char mark-beginning)
      (while (and (re-search-forward "\r" nil t)
		  (< (point) mark-end))
	(replace-match "\n" nil nil))
      (goto-char mark-beginning)
      (widen)
      (forward-char 2)
      (column-ruler)
      )
    (set-buffer-modified-p already-modified)))
;;
;; !!
;; 
(define-key isearch-mode-map "\C-j" 'current-position-visible)


;;
;; wenn Blöcke versteckt waren: diese alle wieder anzeigen
(defun show-invisible ()
  (interactive)
  (save-excursion
    (add-text-properties (save-excursion (beginning-of-buffer) (point)) ; Buffer-Anfang
			 (save-excursion (end-of-buffer) (point))       ; Buffer-Ende
			 '(invisible nil))
    (map-extents (lambda
		   (extent ignored)
		   (set-extent-property extent 'isearch-open-invisible t))
		 nil
		 (save-excursion (beginning-of-buffer) (point)) (save-excursion (end-of-buffer) (point))
		 nil
		 'all-extents-closed 'invisible)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Hilfsfunktionen


(defun region-invisible-searchable-s-e (start end)
  (add-text-properties start end '(invisible t))
  (map-extents (lambda
		 (extent ignored)
		 (set-extent-property extent 'isearch-open-invisible t))
	       nil
	       start end
	       nil
	       'all-extents-closed 'invisible))

;;;;;;;;;


(defun current-position-selecive-display-invisible-to-extent-invisible ()
  (let ((mark-beginning nil) (mark-end nil))
    ;; Region setzen
    (save-excursion
      (setq mark-beginning (save-excursion
			     (re-search-backward "\r\\|^" nil t) (+ 1 (point))))
      (setq mark-end (save-excursion 
		       (re-search-forward "\n" nil t)
		       (- (point) 1)))
      ;; Extent verändern
      (map-extents (lambda (extent ignored)
		     (set-extent-property extent 'invisible t)
		     )
		   nil
		   mark-beginning mark-end
		   nil
		   'all-extents-closed 'invisible)
      )))
;;    (add-text-properties mark-beginning mark-end '(invisible t))))
  
;;
;; ALLE nicht sichbare Anteile verstecken
(defun make-selective-display-hidden-invisible ()
  (let (tmp)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\r" nil t)
	(forward-char 1)
;;	(region-invisible-searchable-s-e (point) (save-excursion
;;					    (re-search-forward "\n" nil t)
;;					    (point)))
	(current-position-selecive-display-invisible-to-extent-invisible)
;;	(goto-char (save-excursion
;;		     (re-search-forward "\n" nil t)
;;		     (point)))
	))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  Anfangsfunktionen
;;;
;;;
;;; wenn auch nur einmal: alles zum Anfang mit der Option "isearch-open-invisible" markieren
(defun all-faces-with-isearch-open-invisisble ()
  (save-excursion
    (add-text-properties (save-excursion (goto-char (point-min))(point)) ; Buffer-Anfang
			 (save-excursion (goto-char (point-max))(point)) ; Buffer-Ende
			 '(isearch-open-invisible t))
    (map-extents (lambda
		   (extent ignored)
		   (set-extent-property extent 'isearch-open-invisible t))
		 nil
		 (save-excursion (goto-char (point-min))(point))
		 (save-excursion (goto-char (point-max))(point))
		 nil
		 'all-extents-closed 'isearch-open-invisible)))

;;
;; ...und ausführen:
;; An den Anfang von op-main an den Anfang bald nach dem Laden, noch bevor etwas zusammengefaltet wird
;;
(all-faces-with-isearch-open-invisisble)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;(setq isearch-mode-hook nil)
(setq isearch-mode-hook 'op-mode-isearch-prepare)
(defun op-mode-isearch-prepare ()
  (save-excursion
    (setq global-isearch-begin-point (point))
    (setq selective-display nil)
    (setq truncate-lines-tmp-save truncate-lines)
    (setq truncate-lines nil)))


;;;(setq isearch-mode-end-hook nil)
(setq isearch-mode-end-hook 'op-mode-isearch-restore)
(defun op-mode-isearch-restore ()
  (if (string= mode-name "Logbuch") ; ist op-mode
      (progn
	(save-excursion
	  (setq truncate-lines truncate-lines-tmp-save)
	  (setq selective-display t)
	  (if (and 
	       (not (= (point) global-isearch-begin-point))
	       ;;(and (not (backward-char (length isearch-string)))
	       ;;	 (looking-at isearch-string))
	       )
	      (progn
		(save-excursion
		  (re-search-backward "[\n\r]" nil t)
		  (forward-char 1)
		  (show-entry-hide))
		;? besser oder nicht? (recenter)
		;(column-ruler)
		))
	  ))))



;=============;
       )      ; Ende des ifs am Anfang: op-isearch gilt in der aktuellen Version nur für den XEmacs
              ;
