



(defun mpx-msp-text-extractor (withcomments current-buff)
  "Extracts from the Microsoft Project 4.0 mpx-file the text
entries; with positive Argument: inserts comment also,
with - as Argument: Outline-Outfit without comments,
negative number as argument: Outline-Outfit with comments"
  (if (eq withcomments '-)
      (setq withcomments '(0)))
  (if (listp withcomments)
      (setq withcomments (car withcomments)))
  ;; zuerst: reduzieren des MPX-Files auf die wesentichen Zeilen: 60,70.71
  (if withcomments
      (if (> (abs withcomments) 0)
	  (occur "^60;\\|^70;\\|^71;")
	(occur "^60;\\|^70;"))
    (occur "^60;\\|^70;"))
  (switch-to-buffer "*Occur*")
  (toggle-read-only t)
  (toggle-read-only)
  (load "op-indent")
  (beginning-of-buffer)
  (kill-line)
  
  ;; Aufbau: ersten Zeile beschreibt die Textfelder, alle weiteren den Aufbau des Files, beides im .cvs-Format
  
  ;; Initialisiere die Variablen, die für die Eintragung verwendet werden
  (let ((meilenstein "") (prozentfertig "") (vorgangsname "") (anfang)(ende)
	(vorgangsnummer 0) (kontaktperson "") (unterprojekt "") (einrueckung ""))
    (goto-char (point-min))
    (while (re-search-forward ":70;" nil t) ; laufe den File ab
      ;; Projektzeilen mit "70" bearbeiten
      (progn
	
	;;
	;;
	;; Setze die Variable "Einmalige Nr." in Zeile 60 zu "Vorgangsnummer"
	(setq vorgangsnummer     (finde_Feld "Einmalige Nr."    ":60;"))
	
	;;
	;;
	(setq vorgangsname	 (finde_Feld "Name"		":60;"))
	
	;;
	;;
	;; für die Schachtelungstiefe: die Gliederungsebene
	(setq einrueckung	 (finde_Feld "Gliederungsebene"	":60;"))
	
	;;
	;;
	(setq dauer		 (finde_Feld "Dauer"		":60;"))
	;; Wenn Meilenstein, dann setze String
	(if (or (string= dauer "0d")
		(string= dauer "0t")) (setq meilenstein "[Meilenstein]  ") (setq meilenstein ""))
	
	;;
	;;
	;; für die Prozentzahlen, wieviel erledigt ist
	(setq prozentfertig	 (finde_Feld "% Abgeschlossen"	":60;"))
	
	;;
	;;
	(setq kontaktperson	 (finde_Feld "Kontaktperson"	":60;"))
	
	;;
	;;
	;; zum Ermitteln, ob hier ein Unterprojektfile eingebaut wurde
	(setq unterprojekt       (finde_Feld "Teilprojektdatei" ":60;"))
	(if unterprojekt
	    (if (not (string= unterprojekt ""))
		(setq unterprojekt (concat "|| [Unterprojekt]  "
					   unterprojekt
					   " || ")))
	  (setq unterprojekt ""))
	
	;;
	;;
	;; ermitteln des Anfangs- und des Endtermines
	(setq anfang	         (finde_Feld "Anfang"		":60;"))
	(setq ende               (finde_Feld "Ende"		":60;"))
	
	
	;;
	;; =============================================================================
	;;
	;; Aufbau des Projektfiles - Zeile für Zeile
	
	;; Vorgangsname
	(beginning-of-line)
	(kill-line t)			; alles was in der Zeile ist löschen
	(insert "\n")
	(backward-char 1)
	;;
	(if vorgangsname (insert vorgangsname))
	;;
	;; setze Unterprojekt ein
	(beginning-of-line)
	(insert unterprojekt)
	;;
	;; setze Meilenstein ein
	(beginning-of-line)
	(insert meilenstein)
	;;
	;; als Merker zur späteren Verwendung: die Einrückungstiefe als extra Punkt
	(end-of-line)
	(if einrueckung 
	    (insert "  :Einrückung:" einrueckung ":")
	  (insert "  :Einrückung:-1:"))
	
	;;---
	;; setze unter der Hauptzeile eine Beschreibungszeile ein
	(end-of-line)
	(insert "\n {")
	(if vorgangsnummer (insert "Nr." vorgangsnummer))
	;;
	;; setze Prozentzahl ein (nach dem Semikolon)
	(end-of-line)
	(if prozentfertig  (insert "; " prozentfertig " erledigt"))
	;;
	(end-of-line)
	(if anfang (insert "; Anfangstermin: " anfang))
	;;
	(end-of-line)
	(if ende (insert ", fertigzustellen bis: " ende))
	;;
	(end-of-line)
	(insert "}")
	;;
	;;

	(end-of-line)
	(if kontaktperson 
	    (if (not (or (string= kontaktperson "NV")
			 (string= kontaktperson "")))
		(insert "\n {Verantwortlich: " kontaktperson "}")))
	  
	)))				; durch alle 70er-Zeilen durch 
  
  
  
  ;; Kommentarzeilen, wenn gewünscht (d.h. 71er-Zeilen wurden im Occur oben gewünscht!)
  (goto-char (point-min))
  (while (re-search-forward ":71;" nil t)  ; jetzt über die Kommentarzeilen laufen - sofern vorhanden 
    (end-of-line)
    (re-search-backward ":71;" nil t)
    (kill-line 0)
    (delete-char 4)
    (insert "\t"))
  
  ;;;=======================================================================================
  ;;;
  ;;; Hier sollte der Occur folgende Struktur haben: 
  ;;; Hauptzeilen fangen bei Position 1 der jeweiligen Zeile an
  ;;; Die Einrücktiefe (Indentierung/Gliederungsebene) ist am Ende dieser Zeile eingetragen
  ;;; Prozessspezifizierende Merkmale sind, soweit gewünscht, nach einem Freizeichen 
  ;;;        in geschweiften Klammern in extra nachfolgenden Zeilen
  ;;; MPX-Kommentare sind nach einem TAB in einer extra Zeile
  ;;;
  
  (goto-char (point-min))
  (forward-line 1)			; die 60er-Zeile überspringen


  (let ((indent-anzahl 0) (tmp ""))
    (while (re-search-forward "^[^ \t]" nil t) ; so lange es Hauptthemenzeilen gibt
      ;; 
      ;; jetzt hole die Einrückungstiefe aus dem letzten Ausdruck in den Hauptüberschriften: sollte immer vorhanden sein
      (re-search-forward ":Einrückung:" (save-excursion (end-of-line)(point)) nil) ; :Einrückung:-1:
      (setq indent-anzahl (string-to-int (buffer-substring-no-properties (point)
									 (save-excursion (re-search-forward ":"
													    (save-excursion (end-of-line)(point))
													    nil)
											 (1- (point))))))
      ;;
      ;; indent-anzahl hat nun die Einrückungstiefe
      ;;
      
      ;; die Anzahl von * entsprechend der Indentierung eintragen
      (beginning-of-line)
      (insert-char ?* indent-anzahl)
      ;;
      ;; wenn schon zu 100% erledigt: setze noch ein "E" hintendran
      ;; (setzt hier voraus, dass die Beschreibungszeile der Hauptüberschrift folgt)
      (save-excursion
	(next-line 1)
	(if (search-forward "100%" (save-excursion (end-of-line)(point)) t)
	    (setq tmp "E")
	  (setq tmp "")))
      (insert tmp)

      (insert-OP-indents indent-anzahl)
      
      ;; jetzt: gibt es Beschreibungs- und Kommentarzeilen (erkennbar am führenden TAB)? Diese werden indentiert.
      (next-line 1)
      (beginning-of-line)
      (while (looking-at "[ \t]")
	(if (looking-at " ")		;entweder es steht auf einem Freizeichen
	    (progn
	      (insert-OP-indents indent-anzahl)
	      (delete-char 1)
	      (if (= indent-anzahl 1) (insert-char ?  1)))
	  (if (looking-at "\t")		; oder auf einem Tab
	      (progn
		(delete-char 1)		; Tab löschen
		(if (and (not (looking-at "-"))
			 (not (looking-at "==>")))
		    (insert "§§§- ")	; §§§-Zeichen als Markierung 
		  (insert "§§§"))
		(beginning-of-line)
		(insert-OP-indents indent-anzahl)
		(if (= indent-anzahl 1) (insert-char ?  1)))))
	(next-line 1)
	(beginning-of-line))

      ))

  ;; die oberste Zeile (60er) löschen
  (goto-char (point-min))
  (kill-line 1)
  (newline)

    ;;;=======================================================================================
  ;; jetzt: Nachbearbeitung des Files


  ;; zuerst: Kommentarzeilen verarbeiten
  ;;
  (goto-char (point-min))
  ;; umbrechen
  (while (re-search-forward "§§§" nil t)
    (end-of-line)
    ;; Zeilen des Kommentars wiederherstellen
    (while (re-search-backward "" (save-excursion (beginning-of-line)(point)) t)
      (replace-match "\n" nil nil)
      (backward-char 1)))
  ;; von den Begriffen :Einrückung: und §§§ reinigen
  (goto-char (point-min))
  (while (re-search-forward ":Einrückung:*.[0-9]+:" nil t)
    (replace-match "" nil nil))
  (goto-char (point-min))
  (while (re-search-forward "§§§" nil t)
    (replace-match "" nil nil))
  ;; nachsehen, welche Zeilen zu lang sind und dann eventuell umbrechen
  (goto-char (point-min))
  (end-of-line)
  ;;
  (while (not (eobp))
    (beginning-of-line)
    ;; gehe zum Zeilenende oder zu Spalte 111 -- umständlich und langsam, jedoch notwendig 
    ;; wegen eventueller Tabulatoren (1 Char = 8 Plätze)
    (while (and (not (eolp))
		(< (current-column) 111))
      (if (not (eolp))(forward-char 1)))
    (if (>= (current-column) 111)
	(if (re-search-backward "[ \t]" (save-excursion (beginning-of-line)(point)) t)
	    (newline)
	  (end-of-line)
	  (if (not (eobp)) (next-line 1)))
      ;; steht jetzt am Ende der Zeile
      (if (not (eobp)) (next-line 1))))

  ;; jetzt noch sauber indentieren
  (goto-char (point-min))
  (end-of-line)
  (re-search-forward "^\\*" nil t)
  (while (not (eobp))
    (beginning-of-line)
    (when (not (looking-at "^\\*")) (insert " ")(beginning-of-line))
    (if (not (bobp))(re-indent-for-tab-command))
    (end-of-line)
    (if (not (eobp))(next-line 1)))

  ;; füge Leerzeilen ein zwischen den einzelnen Paragraphen
  (goto-char (point-min))
  (while (re-search-forward "^\\*" nil t)
    (beginning-of-line)
    (newline)
    (end-of-line))
  
  ;; Aufräumarbeiten
  (goto-char (point-min))
  (setq truncate-lines t)
  ;; Lösche doppelte Newlines heraus
  (trim-buffer)
  (goto-char (point-min))
  (while (re-search-forward "\n\n+" nil t)
    (replace-match "\n\n" nil nil))
  ;;
  (tabify (point-min)(point-max))
  ;;
  ;; Leerzeichen einfügen an das Ende des Files
  (goto-char (point-max))
  (insert-char ?\n 5)
  ;;
  (op-mpx-rehilit)			; BUG
  (mpx-menu-bar)
  ;;
  (delete-window)
  (switch-to-buffer current-buff)
  (split-window-vertically 8)
  (switch-to-buffer "*Occur*")
  (toggle-read-only t)
  (toggle-read-only)
  (rename-buffer "*mpx*" t)
  (local-set-key "\C-c\C-c" 'op-jump-to-line)
  (if (featurep 'xemacs)
      (local-set-key [button2] 'op-jump-to-line-at-mouse-position)
    (local-set-key [mouse-2] 'op-jump-to-line-at-mouse-position))
  (local-set-key "\C-c\C-s" 'xop-sort-reference-lines)
  (local-set-key "q" 'delete-window)
  (beginning-of-buffer)
)

      

(defun insert-OP-indents (indent-anzahl)
  (if (= indent-anzahl 1)
      (insert-char ?  1)
    (insert-char ?\t
		 (- indent-anzahl
		    ;; tab-Weite modulo Anzahl
		    (floor (/ indent-anzahl tab-width))
		    1))))



;;; alter Code zum Aufheben... vielleicht noch gebraucht
;		  (goto-char save-start)
;		  (if (not (looking-at "-\\|[0-9]+\\."))
;		      (progn
;			(insert " ==> ")
;			(setq arrowinserted 5))
;		    (setq arrowinserted 0))
;		  (set-mark (point))
;		  (goto-char (+ save-end arrowinserted))
;		  ;;
;		  (insert-char ?\n 1)
;		  (backward-char 2)
;		  (beginning-of-line)
;		  (if (= (point) save-start)
;		      (force-indent)
;		    (op-indent-region))
;		  (end-of-line)
;		  (setq save-end (point))
;		  ;;
;		  ;; anstatt (fill-paragraph): Umbrechen der zu langen Textzeilen im OP-Modus
;		  (while (and (>= (point) save-start)
;			      (<= (point) save-end))
;		    (if (< (save-excursion
;			     (beginning-of-line)
;			     (save-excursion
;			       (goto-char save-end)
;			       (untabify save-start save-end)
;			       (end-of-line)
;			       (setq save-end (point)))
;			     (forward-char (min fill-column
;						(save-excursion (end-of-line) (current-column))))
;			     (if (not (looking-at "$"))
;				 (progn
;				   (forward-word 1)
;				   (if (not (looking-at "$"))
;				       (forward-char 1))))
;			     (current-column))
;			   (save-excursion
;			     (end-of-line)
;			     (current-column)))
;			(progn
;			  (beginning-of-line)
;			  (save-excursion
;			    (goto-char save-end)
;			    (untabify save-start save-end)
;			    (setq save-end (point)))
;			  (forward-char (min fill-column
;					     (save-excursion (end-of-line) (current-column))))
;			  (if (not (looking-at "$"))
;			      (progn
;				(forward-word 1)
;				(if (not (looking-at "$"))
;				    (forward-char 1))
;				(newline-and-re-indent)
;				(setq save-end (point)))))
;		      (previous-line 1)))
;		  ;; end (fill-paragraph)
;		  ;; Leerzeichen abräumen
;		  (next-line 1))
;	      (next-line 1)))))



(defun op-mpx-rehilit ()
  (interactive)
  (if window-system
      (progn
	(if (x-display-color-p)
	    (progn
	      (hilit-set-mode-patterns
	       'indented-text-mode
	       '(
					;("/\\*" "\\*/" msg-from)   ; wie C-Kommentare
		 ;; Doppelzeilenkombination für golden unterlegt
		 ("/\\*\\|\\*/" nil msg-separator)
		 ("/\\*\\|\\*/" nil msg-quote)

		 ;;	   ("->" "[ \t\r]\\|$" jargon-keyword)
		 ;; ("->" nil label)
		 ;;	   ("->" "[ \t\r]\\|$" string)

		 ;; Dreizeilenkombination für Dunkelgrün
		 ("#[^TR]" ".#\\|[\r\n]" msg-quote)
		 ("#[^TR]" ".#\\|[\r\n]" struct)
		 ("#[^TR]" ".#\\|[\r\n]" msg-quote)

		 ;; Doppelzeilenkombination für golden unterlegt
					;("#T" ".#\\|[\r\n]" msg-separator)
					;("#T" ".#\\|[\r\n]" msg-quote)
		 ("#T" ".#\\|$" jargon-keyword)

		 ("#R" ".#\\|[\r\n]" comment)

		 ("\\<\\(Info\\|INFO\\|info\\)\\>.*" nil struct)

		 ("\\[Meilenstein\\]" nil define)
		 ("|| \\[Unterprojekt\\]" " ||" error)
		 ("#include[ \t]+\"" "\"" define)
		 ("[\n\r]#end_include_statement" nil define)

		 ;; speziell für MPX-MSP
		 ("{Nr\\." "}" goldenrod)
		 ("{Verantwortlich" "}" goldenrod)

		 ("^\\(\\*E\\|\\*FW\\).*" "[\r\n]" summary-deleted) ; Erledigtes in Hauptüberschrift
		 ("^\\(\\*\\*+E\\|\\*+FW\\).*" "[\r\n]" keyword) ; Erledigtes in Unterüberschrift
								 ;(generelle Doku: in op-menus.el)
		 ("^\\(\\*+V\\|\\*+S\\).*" "[\r\n]" include)	 ; Verschobenes und ständig wiederkehrendes
		 ("^\\*+\\-+" "[\r\n]" string)			 ; *---- - Zeilen
		 ("^\\*[^*EFVDSR].*" "[\r\n]" msg-header)	 ; Hauptüberschrift ; F != FW !!!
		 ("^\\*\\*+[^*EFVDSR].*" "[\r\n]" defun)	 ; Unterüberschriften
		 ("^\\*+D" "[\r\n]" error)			 ; Dringendes
		 )))
	  (hilit-set-mode-patterns
	   'indented-text-mode
	   '(
	     ("==>" nil string)
	     ("^\\*+E.*\\|^\\*+FW.*\\|^\\*+V.*" "[\r\n]" keyword)
	     ("#[^#]" ".#\\|[\r\n]" msg-from)
	     ("\\<\\(Info\\|INFO\\|info\\)\\>.*" "[\r\n]" glob-struct)
	     ("^\\*+\\-+" "[\r\n]" msg-from)
	     ("^\\*" "[\r\n]" defun)
	     )))

	;;(setq hilit-face-check nil) ; Fonds werden ja meistens nicht extra geändert, sonst hilft auch noch C-u S-C-l
	(setq hilit-inhibit-rebinding t) ; wenn der Server nur wenige Farben hat, geht sonst manchmal Yank nicht mehr
	(setq hilit-auto-rehighlight 10000) ; Anzahl der automatisch upgedateten Zeilen bei Benutzung des X-Servers

	;; auch der hilit-mode kommt nicht mit dem (setq selective-display t) zurecht
	(setq hilit-auto-rehighlight-fallback '(20000 . 10000))
	(make-variable-buffer-local 'hilit-auto-rehighlight-fallback)

	(hilit-repaint-command t)
	)))


(defun mpx-headlines-only ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\n[ \t]*{.*}" nil t)
      (replace-match "" nil nil))))


(defun mpx-ueberschriften-abgleich ()
  "Im erzeugten *mpx*-Buffer werden alle Überschriftenzeilen
mit denen im Projektfile nach Abgleich durchsucht. Eindeutigkeit
der Zeilen ist Voraussetzung."
  (interactive)
  (let ((ptrpossv))
    (if window-system
	(require 'hilit19.variation))
    (while (re-search-forward "*+" nil t)
      (if (op-jump-to-line)
	  (progn
	    ;; Zeile existiert
	    (while (progn
		     ;; zurück zum mpx-Buffer
		     (other-window 1)
		     (beginning-of-line)
		     (save-match-data
		       (re-search-forward "*+" nil t))
		     (setq ptrpossv (buffer-substring-no-properties (point)(1+ (point))))
		     ;; zurück zum Projekt-Buffer
		     (other-window -1)
		     (beginning-of-line)
		     (save-match-data
		       (re-search-forward "*+" nil t))
		     (if (not (looking-at ptrpossv))
			 (progn
			   (recenter)
			   (other-window 1)
			   (recenter)
			   (if window-system
			       (hilit-region-set-face (save-excursion (beginning-of-line)
								      (point))
						      (save-excursion (end-of-line)
								      (point))
						      'highlight))
			   (other-window -1)
			   (if window-system
			       (hilit-region-set-face (1- (match-beginning 0))(match-end 0) 'highlight))
			   (if (if window-system
				   (y-or-n-p "Abgleichen ? [SPC=y] ")
				 (string= (read-string "Abgleichen ? (y or n): ") "y"))
			       (progn
				 (delete-char 1)
				 (insert ptrpossv)
				 (if (not (looking-at "[ \t]"))
				     (insert-char ?  1))
				 ))
			   (other-window 1)
			   (if window-system
			       (hilit-unhighlight-region (save-excursion (beginning-of-line)
									 (point))
							 (save-excursion (end-of-line)
									 (point))
							 'highlight))
			   (other-window -1)
			   (if window-system
			       (hilit-unhighlight-region (1- (match-beginning 0))(match-end 0)))))
		     ;; wenn mehr als einmal die gleiche Zeile auftaucht
		     (other-window -1)
		     (op-jump-to-line))))
	    ;(other-window -1))
	(if window-system
	    (hilit-region-set-face (save-excursion
				     (beginning-of-line)
				     (point))
				   (save-excursion
				     (end-of-line)
				     (point))
				   'highlight))
	(recenter)
	(other-window 1)
	(recenter)
	(other-window -1)
	(if (if window-system
		(y-or-n-p "Diese Zeile ist nicht im Projektfile: mit NV Markieren ? [SPC=y] ")
	      (string= (read-string
			"Diese Zeile ist nicht im Projektfile: mit NV Markieren ? [SPC=y] ") "y"))
	    (progn
	      (beginning-of-line)
	      (insert "NV: ")
	      (end-of-line))
;	  (if window-system
;	      (hilit-unhighlight-region (save-excursion
;					  (beginning-of-line)
;					  (point))
;					(save-excursion
;					  (end-of-line)
;					  (point))
;					'highlight))
	  (next-line 1))
	)
      (end-of-line))))



(defun mpx-abgleich ()
  "Gleicht *mpx*-Buffer mit Projektkontrollbuffer ab. Beide Buffer müssen in
zwei verschiedenen Rahmen stehen, Aufruf im *mpx*-Buffer"
  (interactive)
  (goto-char (point-min))
  (other-window 1)
  (show-really-everything)
  (goto-char (point-min))
  (other-window -1)
  (mpx-ueberschriften-abgleich)
  (goto-char (point-min))
  (other-window 1)
  (goto-char (point-min))
  (other-window -1)
  (mpx-headlines-only)
  (goto-char (point-min))
  (other-window 1)
  (goto-char (point-min))
  (let ((tmpbuf (current-buffer)))
    (other-window -1)
    (ediff-buffers (current-buffer) tmpbuf)))




(defun mpx-text-extractor (&optional withcomments)
  "Extracts from Microsoft Project the mpx-file at the current
cursor position the text entries; with Argument: inserts comments also"
  (interactive "P")
  (let ((find-file-not-found-hooks find-file-not-found-hooks) (current-buffer (current-buffer)))
    (add-hook 'find-file-not-found-hooks
	      (function (lambda ()
			  (error "Keinen solchen File gefunden: %s" buffer-file-name))))
    (find-file (current-longword))
    (message "MPX nach Standard MS Project 4.0/95/98")
    (mpx-msp-text-extractor withcomments current-buffer))
  )


(defun mpx-import (file &optional withcomments)
  "Extracts from Microsoft Project or CA SuperProject the mpx-file at the current
cursor position the text entries; with Argument: inserts comments also"
  (interactive "fMPX-File: \nnMit Kommentarzeilenverarbeitung (1) oder ohne (0) ?: ")
  (find-file file)
  (message "MPX nach Standard MS Project 4.0/95/98")
  (mpx-msp-text-extractor withcomments (file-name-nondirectory buffer-file-name))
  )
(define-project-mode-key  "\M-m" 'mpx-text-extractor)


(setq selective-display t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun mpx-menu-bar ()
  (defvar mpx-mode-menu-bar-map (make-sparse-keymap))
  ;;
  (define-key mpx-mode-menu-bar-map [MPX]
    (cons "MPX" (make-sparse-keymap "MPX")))
  ;;
  (define-key mpx-mode-menu-bar-map [MPX op-mpx-rehilit]
    '("Neu Einfärben" . op-mpx-rehilit))
  (define-key mpx-mode-menu-bar-map [MPX xop-sort-reference-lines]
    '("Zeilen sortieren" . xop-sort-reference-lines))
  (define-key mpx-mode-menu-bar-map [MPX op-jump-to-line]
    '("Zu Zeile in Projektkontrollfile in anderem Fenster" . op-jump-to-line))
  (define-key mpx-mode-menu-bar-map [MPX mpx-ueberschriften-abgleich]
    '("MPX Überschriftenabgleich mit Projektkontrollfile in anderem Fenster: M-x mpx-ueberschriften-abgleich" . dummy))
  (define-key mpx-mode-menu-bar-map [MPX mpx-headlines-only]
    '("MPX: nur noch Überschriften" . mpx-headlines-only))
  (define-key mpx-mode-menu-bar-map [MPX mpx-abgleich]
    '("MPX-ABGLEICH mit Projektkontrollfile in anderem Fenster: M-x mpx-abgleich" . dummy))
  ;;
  ;;  (define-key mpx-mode-menu-bar-map [MPX mpx-tescht]
  ;;	'("Test" . tescht))
  ;;
  ;;
  (define-key (current-local-map) [menu-bar] mpx-mode-menu-bar-map))



;;(defun tescht ()
;;  (interactive)
;;  (setq window-system nil)
;;    (y-or-n-p "test "))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;
;;;
;;;  Allgemeine Lib zum Bearbeiten von Semikolon-basierten .mpx-Dateien (das ausgelaufene "Microsoft Project Exchange"-Format)
;;;  SMW, 17.9.2004
;;;


;;  Hauptfunktion: finde_Feld



(defun zaehle_Semikolons_rueckwaerts ()
  (let ((Anzahl_gueltiger_Semikolons 0) (svpnt 0) (m1 0)(m2 0) (in nil))
    (save-excursion
      ;; sehe zuerst nach, ob sich der Cursor direkt im String befindet
      (setq svpnt (point))
      (beginning-of-line)
      (while (re-search-forward "\\([^\\]\"\\).*\\([^\\]\"\\)" (save-excursion (end-of-line)(point)) t)
	(backward-char 1)
	(setq m2 (point))
	(re-search-backward "[^\\]\"" (save-excursion (beginning-of-line)(point)) t)
	(setq m1 (1+ (point)))
	(if (and (<= m1 svpnt)
		 (>= m2 svpnt)) ; der Punkt liegt im String!!
	    (setq in t))
	(goto-char m2))
      (goto-char svpnt))
      (if in (progn
	       (re-search-backward "[^\\]\"" (save-excursion (beginning-of-line)(point)) t)
	       (forward-char 1)))
      ;;
      ;;
      (while (re-search-backward ";\\|\\([^\\]\"\\)" (save-excursion (beginning-of-line)(point)) t)  ; sucht rückwärts nach Semikolon oder Anführungsstrich
	(if (looking-at "[^\\]\"") ; steht auf einem Anführungsstrich
	    (progn
	      (re-search-backward "[^\\]\"" (save-excursion (beginning-of-line)(point)) t) ; suche das Pendant zum Anführungsstrich
	      (forward-char 1)
	      (re-search-backward ";" (save-excursion (beginning-of-line)(point)) t)  ; suche den Anführungsstrich vor dem Anführungsstrich
	      (if (save-excursion (forward-char 1) (not (looking-at "\"")))           ; das dem Semikolon folgende Zeichen ist kein Anführungsstrich: dann Fehler
		  (error "Fehler beim Parsen in \"zaehle_Semikolons_rueckwaerts\": beim Stringparsen des CSV-Files Anführungsstrichproblem - siehe CSV-File"))))
	(setq Anzahl_gueltiger_Semikolons (1+ Anzahl_gueltiger_Semikolons)))
      (goto-char svpnt)
      Anzahl_gueltiger_Semikolons))
;Teststring;Teststring;"Test; ; \" ;string";Teststring  xxx  Sollergebnis vom Kreuz: 4 !!!



;Zellenformat:         Test " mit einem ; im String ", mit Semikolon ; pur, mit ;; und mit ;";"
;ergibt CSV-Format:    "Test "" mit einem ; im String "", mit Semikolon ; pur, mit ;; und mit ;"";"""
;das gleiche über 3 Zellen:
;                      "Test "" mit einem ; im String "", mit Semikolon ; pur, mit ;; und mit ;"";""";"Test "" mit einem ; im String "", mit Semikolon ; pur, mit ;; und mit ;"";""";"Test "" mit einem ; im String "", mit Semikolon ; pur, mit ;; und mit ;"";"""

;und die MPX-Datei aus MS Project mit dem gleichen Sting in der Zelle:
;                      70;1;1;"Test  mit einem ; im String , mit Semikolon ; pur, mit ;; und mit ;;";1t;Nein;1;0t;;Mo 20.09.04;Mo 20.09.04;Mo 20.09.04;Mo 20.09.04;Mo 20.09.04;Mo 20.09.04;0t;0t;0ft;0%;NV;NV;NV;NV;So früh wie möglich;;NV;NV;So 19.09.04;0h;0h;0h;0,00 DM;0,00 DM;0,00 DM;0,00 DM;0,00 DM;1;Mittel;Nein;Nein;Nein;;;;;;;;;;;0,00 DM;0,00 DM;0,00 DM;0t;0t;0t;Nein;Nein;Nein;Nein;Nein;Nein;Nein;Nein;Nein;Nein;Nein;0;0;0;0;0;
;d.h. alle Mittelstrings werden im MPX-File gestrippt
 



(defun wandere_Semikolons_vorwaerts (arg)
  (interactive)
  "Setzt den Cursor um die Anzahl von Semikolons nach vorne, die im Argument angegeben werden.
Der Cursor bleibt bei dem Zeichen nach dem Semikolon stehen. Erfolgt keine Positionsveränderung,
ist das Rückgabeargument nil, kann der Cursor nur um weniger verrückt werden als im Argument angegeben, 
wird die maximal mögliche Zahl nach vorne verrückt."
  (if (and (looking-at ";") (not (eolp))) (forward-char 1))
  (let ((Anzahl_gueltiger_Semikolons 0) (pnt (point)))
    (while (> arg Anzahl_gueltiger_Semikolons)
      (progn
	(re-search-forward ";\\|\"" (save-excursion (end-of-line)(point)) t)  ; sucht vorwärts nach Semikolon oder Anführungsstrich
	(backward-char 1)
	(if (looking-at "\"") ; steht auf einem Anführungsstrich
	    (progn
	      (re-search-forward "[^\\]\"" (save-excursion (end-of-line)(point)) t) ; suche das Pendant zum Anführungsstrich
	      (backward-char 1)
	      (re-search-forward ";" (save-excursion (end-of-line)(point)) t)       ; suche den Anführungsstrich vor dem Anführungsstrich
	      (if (save-excursion (backward-char 2) (not (looking-at "\"")))       ; das dem Semikolon vorangehende Zeichen ist kein Anführungsstrich: dann Fehler
		  (error "Fehler beim Parsen in \"zaehle_Semikolons_rueckwaerts\": beim Stringparsen des CSV-Files Anführungsstrichproblem - siehe CSV-File"))))
	(setq Anzahl_gueltiger_Semikolons (1+ Anzahl_gueltiger_Semikolons))
	(forward-char 1)))
    (if (not (save-excursion (backward-char 1) (looking-at ";"))) (backward-char 1))
    (if (= (1- pnt) (point)) 
	(progn (forward-char 1)
	       (setq Anzahl_gueltiger_Semikolons nil)))
    Anzahl_gueltiger_Semikolons))




(defun finde_Feld (FeldStringDefinition BeschreibungszeilenSuchmuster)
  "Macht folgendes: Sucht nach oben mit dem Muster BeschreibungszeilenSuchmuster eine 
Zeile, sucht in dieser Zeile den Beschreibungsstring \"FeldStringDefinition\", zählt
die Feld-Stelle, an der sich dieses Feld befindet, und wandert dann in der aktuellen Zeile
an diese Feldstelle. Der Rückgabewert ist der Inhalt dieser Feldstelle.
Beispielsfile:

Beschreibungszeile;Beschreibungsfeld_1;Beschreibungsfeld_2;Beschreibungsfeld_3;Beschreibungsfeld_4
Inhaltszeile;Inhalt1;Inhalt2;Inhalt3;Inhalt4
Inhaltszeile;AndererInhalt1;AndererInhalt2;AndererInhalt3;AndererInhalt4

Wenn man jetzt z.B. auf \"AndererInhalt1\" steht und 
(finde_Feld \"Beschreibungsfeld_3\" \"Beschreibungszeile\")
aufruft ist der Rückgabewert \"AndererInhalt3\"." 
  (interactive)
  (let ((feldposition 0))
    (save-excursion
      (if (not (re-search-backward BeschreibungszeilenSuchmuster nil t))  ; wenn Beschreibungszeile nicht gefunden wird: nil
	  (progn
            (condition-case nil
	        (message (concat "Beschreibungzeile " BeschreibungszeilenSuchmuster " nicht hochwärts im File gefunden!"))
	     (error (concat "Beschreibungzeile mit Prozentzeichen-Beschreibungszeilen-Suchmuster nicht hochwärts im File gefunden!")))
	    nil)
	;; Beschreibungzeile wurde gefunden
	(beginning-of-line)
	(if (not (search-forward FeldStringDefinition nil t))  ; Muster wurde in der Beschreibungszeile nicht gefunden
	    (progn
              (condition-case nil
	          (message (concat "Suchmuster " FeldStringDefinition "in Beschreibungzeile " BeschreibungszeilenSuchmuster " nicht gefunden!"))
	        (error (concat "Beschreibungzeile mit Prozentzeichen-Beschreibungszeilen-Suchmuster nicht hochwärts im File gefunden!")))
	      nil)
	  ;; Beschreibungszeilenfeld wurde in Beschreibungszeile gefunden
	  (backward-char 1)
	  ;; Bestimme die Position des Feldes
	  (setq feldposition (zaehle_Semikolons_rueckwaerts)))))
    ;; Hier sollte nun die Feldposition bekannt sein
    (if (not (= feldposition 0))
	(progn
	  (beginning-of-line)
	  ;;
          (if (not (save-excursion (beginning-of-line)(looking-at ";")))
	      (wandere_Semikolons_vorwaerts feldposition)
	    (wandere_Semikolons_vorwaerts (1- feldposition)))
	  ;; an dieser Stelle sollte der Cursor am Anfang des Feldinhaltes, der gesucht wird, stehen.
          ;;
	  ;;
	  ;; nun: Ermittlung des Rückgabewertes:
	  ;; Um Probleme konsistent zu halten, wird hier die Funktion wandere_Semikolons_vorwaerts noch einmal genutzt
	  (let ((p1) (p2))
	    (setq p1 (point))
	    (save-excursion
	      (beginning-of-line)
	      ;;
	      (if (not (save-excursion (beginning-of-line)(looking-at ";")))
		  (wandere_Semikolons_vorwaerts (1+ feldposition))
		(wandere_Semikolons_vorwaerts feldposition))
	      ;;
	      (setq p2 (1- (point)))
	      (when (= p1 (1+ p2)) 
		(end-of-line)
		(setq p2 (point))))
            ;; wenn das Feld leer ist, ist der folgende Ausdruck ein Semikolon, sonst der String
	    (if (string= (buffer-substring-no-properties p1 p2 (current-buffer)) ";")
		""
	      (buffer-substring-no-properties p1 p2 (current-buffer)))
	    )))))
;;Beschreibungszeile;Beschreibungsfeld_1;Beschreibungsfeld_2;Beschreibungsfeld_3;Beschreibungsfeld_4
;;Inhaltszeile;Inhalt1;Inhalt2;Inhalt3;Inhalt4
;;Inhaltszeile;AndererInhalt1;AndererInhalt2;AndererInhalt3;AndererInhalt4
;Beschreibungszeile;Beschreibungsfeld_1;Beschreibungsfeld_2;Beschreibungsfeld_3;Beschreibungsfeld_4
;Inhaltszeile;Inhalt1;Inhalt2;Inhalt3;Inhalt4
;Inhaltszeile;AndererInhalt1;AndererInhalt2;AndererInhalt3;AndererInhalt4




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


