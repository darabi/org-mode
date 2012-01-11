



(defun insert_BF ()
  (interactive)
  (let ((erledigt-feld nil)(termin-feld "")(tmp nil))
    (goto-char 1)
    (while (re-search-forward "^\\*+" nil t)
      ;; mache die Analyse der BF-Felder
      (save-excursion
	;; nachsehen, ob dies eine mehrzeilige Überschrift ist, wenn ja, dann setzen 
	;; an das Ende der Mehrzeiler
	(end-of-line)
	(backward-char 1)
	(while (looking-at "\n[^*]")
	  (next-line 1)
	  (end-of-line)
	  (backward-char 1))
	;; jetzt die BF-Kommentarzeilen lesen und auslesen (falls vorhanden)
	(if (search-backward "(" (save-excursion (beginning-of-line)(point)) t)
	    (progn
	      (save-excursion
		;; wenn das Done-Feld gesetzt ist, dann ersetze dieses durch das "E"-Feld des OP-Modes
		(if (re-search-forward "Done" (save-excursion (end-of-line)(point)) t)
		    (progn
		      (setq erledigt-feld t)
		      (if (looking-at ",")
			  (forward-char 1))
		      (if (looking-at " ")
			  (forward-char 1))
		      (backward-kill-word 1))))
	      (end-of-line)
	      (search-backward "(" (save-excursion (beginning-of-line)(point)) t)
	      (save-excursion
		;; wenn das Termin-Feld gesetzt ist, dann setze das ersetze dieses durch das "E"-Feld des OP-Modes
		(if (re-search-forward "due" (save-excursion (end-of-line)(point)) t)
		    (progn
		      (setq termin-feld (buffer-substring (point) (+ (point) 11)))
		      (backward-char 4)
		      (kill-word 4)
		      (save-excursion
			(end-of-line)
			(insert-char ?\n 1)
			(re-indent)
			(insert "#T" termin-feld)))))
	      (end-of-line)
	      (search-backward "(" (save-excursion (beginning-of-line)(point)) t)
	      (save-excursion
		;; wenn das Termin-Feld gesetzt ist, dann setze das ersetze dieses durch das "E"-Feld des OP-Modes
		(if (re-search-forward "To do" (save-excursion (end-of-line)(point)) t)
		    (progn
		      (if (looking-at ",")
			  (forward-char 1))
		      (if (looking-at " ")
			  (forward-char 1))
		      (backward-kill-word 2))))
	      ;; reformatiere ein eventuell noch vorhandenes Prioritätsfeld
	      (end-of-line)
	      (search-backward "(" (save-excursion (beginning-of-line)(point)) t)
	      ;; dazu: eventuell noch von vorher vorhandenes Feld löschen (nach hinten in der Zeile gehen)
	      (save-excursion
					;Muster: (p1, )
		(while (re-search-backward "(p[0-9]+,[ ]*)" (save-excursion (beginning-of-line)(point)) t)
		  (replace-match "")))
	      (save-excursion
					;Muster: (p1, 
		(while (re-search-backward "(p[0-9]+,[ ]*" (save-excursion (beginning-of-line)(point)) t)
		  (replace-match "")))
	      (save-excursion
					;Muster: (p1  )
		(while (re-search-backward "(p[0-9]+[ ]*)" (save-excursion (beginning-of-line)(point)) t)
		  (replace-match "")))
	      (save-excursion
					;Muster: (p1
		(while (re-search-backward "(p[0-9]+" (save-excursion (beginning-of-line)(point)) t)
		  (replace-match "")))
	      ;; dann das aktuelle Feld reformatieren (nach vorne in der Zeile gehen)
	      (end-of-line)
	      (search-backward "(" (save-excursion (beginning-of-line)(point)) t)
	      (save-excursion
					;Muster: (p1,  )
		(if (re-search-forward "(p\\([0-9]+\\),[ ]*)" (save-excursion (end-of-line)(point)) t)
		    
		    (replace-match "(p\\1)")
					;Muster: (p1, 
		  (if (re-search-forward "(p\\([0-9]+\\),[ ]*" (save-excursion (end-of-line)(point)) t)
		      (replace-match "(p\\1)")
					;Muster: (p1  )
		    (if (re-search-forward "(p\\([0-9]+\\)[ ]*)" (save-excursion (end-of-line)(point)) t)
			(replace-match "(p\\1)")
					;Muster: (p1
		      (if (re-search-forward "(p\\([0-9]+\\)[ ]*" (save-excursion (end-of-line)(point)) t)
			  (replace-match "(p\\1)"))))))
	      )))
      ;; Einsetzen des "E"- (== erledigt-) Feldes
      (beginning-of-line)
      (re-search-forward "\\*+" (save-excursion (end-of-line)(point)) t) ; ****
      (if erledigt-feld 
	  (progn
	    (insert-char ?E 1)
	    (setq erledigt-feld nil)
	    ;; wenn ein E eingesetzt wurde und dies eine Hauptüberschrift ist (d.h. nur ein Stern),
	    ;; dann noch ein Freizeichen dazu
	    (save-excursion
	      (beginning-of-line)
	      (if (looking-at "^\\*[^*]")
		  (setq tmp t)))
	    (if tmp (insert-char ? 1))(setq tmp nil)))
      )
    ;; BF-Notizfeld zum normalen Mehrzeileneintrag uner des entsprechenden Überschrift machen
    (goto-char 1)
    (while (search-forward "<Note: " nil t)
      (replace-match "<Notiz: "))
    ))


(defun from_BF ()
  (interactive)
  (let ((tfilename))
    ;; setzt den Filenamen um von der Extention .txt auf die Extention .op - wenn .op
    ;; schon existiert, dann mache nicht weiter, um ein Überschreiben zu verhindern
    (setq tfilename (concat
		     (substring buffer-file-name 0 (string-match "\\.txt$" buffer-file-name))
		     ".op"))
    (if (not (file-exists-p tfilename))
	(progn
	  ;;
	  ;; erst einmal die Grundfunktionen aud BF übernehmen: Datum, Done, ...
	  (insert_BF)
	  ;;
	  ;;
	  ;; mache eine Rekonstruktion der Zeilen durch Ersetzen der Sonderzeichen
	  (debug)
	  (goto-char 1)
	  (while (re-search-forward "+" nil t)
	    (replace-match "\n"))
	  (goto-char 1)
	  (while (re-search-forward "+" nil t)
	    (replace-match "\n"))
 	  (goto-char 1)
	  (while (re-search-forward "\n" nil t) ; neu kreierte Punkte in BrainForest
	    (replace-match ""))
 	  (goto-char 1)
	  (while (re-search-forward "" nil t)
	    (replace-match "\n"))
 	  (goto-char 1)
	  (while (re-search-forward "" nil t)
	    (replace-match "\n"))
	  (goto-char 1)
	  (while (re-search-forward "$" nil t)
	    (replace-match "\n"))
	  (goto-char 1)
	  (while (re-search-forward "+" nil t)
	    (replace-match ""))
	  ;; jetzt: frage für jeden speziellen Punkt, ob er nicht vielleicht neu ist:
	  (goto-char 1)
	  (while (re-search-forward "^\\*+\\w" nil t)
	    (progn
	      (backward-char 1)
	      (if (not (looking-at "[-+EVDSR]"))
		  (if (not (looking-at "FW"))
		      (progn
			(isearch-highlight 
			 (save-excursion (beginning-of-line)(point))
			 (save-excursion (end-of-line)(point)))
			(if (y-or-n-p "Format headline ? ")
			    (progn
			      (insert-char ?\n 1)
			      (backward-char 1)
			      (re-indent)
			      (end-of-line)
			      (delete-char 1)
			    (isearch-dehighlight nil))        
			    ))))))
	  ;; formatiere eventuell neu hinzugekommene Zeilen
	  ;; jetzt: frage für jeden speziellen Punkt
	  (goto-char 1)
	  (re-search-forward "^\\*" nil t)   ; erste Zeile mit Outline-Überschrift-Charakter
	  (while (re-search-forward "^[^* \t]." nil t)
	  ;;    (if (y-or-n-p "Indent OP-Mode like ? ")
		  (progn
		    (beginning-of-line)
		    (re-indent)
		    (next-line 1)
		    (beginning-of-line)))
					;)
;;; 	  ;; jetzt noch indents nachholen, die durch die Baumstruktur vielleicht verloren gegangen sind
;;; 	  (goto-char 1)
;;; 	  (while (re-search-forward "^\t" nil t)
;;; 	    (re-indent))
	  ;;
	  ;; File-Sicherung und löschen des Files
	  (if (yes-or-no-p (concat "Save file to " tfilename " ? "))
	      (progn
		(write-file tfilename)
		(revert-buffer nil t)
		(kill-buffer (current-buffer))
		(find-file tfilename))
	    (if (y-or-n-p "Revert buffer? ")
		(revert-buffer nil t)))
	  )
      (error "Filename exists already with extention .op"))))


(defun to_BF ()
  (interactive)
  (let ((tfilename))
    (goto-char 1)
    ;; setzt den Filenamen um von der Extention .op auf die Extention .txt - auf diesem File
    ;; wird gearbeitet (zum Schutz des .op-Files)
    (setq tfilename (concat
		     (substring buffer-file-name 0 (string-match "\\.op$" buffer-file-name))
		     ".txt"))
    (if (not (file-exists-p tfilename))  ; wenn .txt schon existiert: Abbruch
	(progn
	  ;; zum .txt-File
	  (copy-file buffer-file-name tfilename)
	  (find-file tfilename)
	  ;; nehme alle nicht-"outline-headings" durch  zu der Überschrift dazu
	  (while (re-search-forward "^[ \t]" nil t)
	    (if (not (eobp))
		(progn
		  (backward-char 2)
		  (delete-char 1)
		  (insert-char ? 1)
		  (if (not (eobp))
		      (forward-char 1)))))
	  ;; ersetze alle reinen Freizeilen durch das Zeichen , damit diese bei der 
	  ;; Rekonstruktion wieder herstellbar sind
	  (trim-buffer)
	  (goto-char 1)
	  (while (and (re-search-forward "^$" nil t)
		      (not (eobp)))
	    (progn
	      (delete-char 1)
	      (if (not (= (point) 1))
		  (backward-char 1))
	      (insert-char ? 1)
	      (if (not (eobp))
		  (forward-char 1))))
	  ;; setze oben eine Synchronisationszeile ein, zur Kennzeichnung und um 
	  ;; BrainForest zu zwingen, die Headlines eine Stufe zu tief einzusetzen -
	  ;; die Stufe-1-Überschriften bleiben sonst nicht in Outline-Form erhalten
	  (goto-char 1)
	  (if (not (looking-at "\n*emacs OP-Mode"))
	      (insert (concat "emacs OP-Mode-File zur Synchronisation in BF: \"" buffer-file-name "\"\n")))
	  (save-buffer)
	  (kill-buffer (current-buffer)))
      (error "Filename exists already with extention .txt"))))





;; Gift Botulinum, Fa. Botox gegen Migräne
;; man ist nicht, sondern man tut so als wenn man wäre
;; es ist ein Puzzle, das muss man sich zusammenbauen aus den eigenen Bildern und denen der anderen





