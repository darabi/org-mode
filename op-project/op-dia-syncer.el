;;; =================================================================================================
;;;
;;;  Teil des OP-Modes: Einsetzen und Synchronisation des OP-Modes mit dem XML-Abspeicherungsfile
;;;  des Programmes Dia  --  damit bei der Projektplanung eine erleichterte h,Ad(Bndige Synchronisation
;;;  zwischen dem Dia-File und dem OP-Mode-File m,Av(Bglich wird
;;;
;;;  Geschrieben 28.9.2004 als Erg,Ad(Bnzung zum OP-Mode von SMW
;;;
;;;

;;;  f,A|(Bnf Nutzerfunktionen werden hier definiert:
;;;   (dia-inserter FILE boolean)
;;;   (dia-inserter-from-file)
;;;   (dia-ID-synchronisator FILE)
;;;   (next-dia-tag)
;;;   (previous-dia-tag)

;;; =================================================================================================




(define-key (current-local-map) "\M-c\M-n\M-n" 'next-dia-tag)
(define-key (current-local-map) "\M-c\M-n\M-p" 'previous-dia-tag)
(define-key (current-local-map) "\M-c\M-n\M-s" 'dia-ID-synchronisator)
(define-key (current-local-map) "\M-c\M-n\M-i" 'dia-inserter-from-file)





(defun dia-string-searcher-forward ()
  "Sucht aus dem XML-File des Programmes Dia die nchste Textpassagen-Position heraus
und gibt diese als (evtl. mehrzeiligen) R$(1vU(Bkgabestring heraus. Wurde keine Passage
gefunden ist die Funktion nil."
  (let ((string-start 0)(string nil))
;;;
;;; <dia:object type="Standard - Text" version="0" id="O12">
;;;
    (if (re-search-forward "\<dia:object[ \t]*type[ \t]*=[ \t]*\".*Text.*\".*\".*\\id[ \t]*=[ \t]*\"O.*\".*\>" nil t) 
					; <- Eindeutige Identifikation von Text-Objekten
	;; bei den restlichen Objekten geht der Code einfach von einer Einhaltung der XML-Struktur aus
	(if (re-search-forward "\<dia:attribute[ \t]*name[ \t]*=[ \t]*\"text\"\>" nil t)
	    (if (re-search-forward "\<dia:composite[ \t]*type[ \t]*=[ \t]*\"text\"\>" nil t)
		(if (re-search-forward "\<dia:attribute[ \t]*name[ \t]*=[ \t]*\"string\"\>" nil t)
		    (if (search-forward "<dia:string>" nil t)
			(progn
			  (setq string-start (point))
			  (search-forward "</dia:string>" nil nil)
			  (goto-char (match-beginning 0))
			  (setq string (buffer-substring-no-properties (1+ string-start)   ; 1+ wegen des dia-typischen Lattenkreuzes
								       (1- (point))))))))) ; 1- wegen des dia-typischen Lattenkreuzes
      string)))


(defun dia-id-searcher-backward ()
  "Nach dem Aufruf von (dia-string-searcher-forward) steht der Cursor auf dem Text. 
Diese Funktion sucht den letzten Text-ID-Tag heraus und gibt ihn zur$(1vU(Bk."
  (let ((idstring ""))
;;;
;;; <dia:object type="Standard - Text" version="0" id="O12">
;;;
    (save-excursion
      (if (re-search-backward "\<dia:object[ \t]*type[ \t]*=[ \t]*\".*Text.*\".*\".*id[ \t]*=[ \t]*\"O.*\".*\>" nil t) 
					; <- Eindeutige Identifikation von Text-Objekten
	  ;; Schnellprogrammierung, da das mit dem Gruppierung hat nicht geklappt, warum auch immer
	  (progn
	    (end-of-line)
	    (re-search-backward "[0-9]" (save-excursion (beginning-of-line)(point)) nil) ; steht auf einem string
	    (while (looking-at "[0-9]")
	      (setq idstring (concat (buffer-substring-no-properties (point) (1+ (point))) idstring))
	      (backward-char 1))
	    ;; idstring enth,Ad(Blt hier die ID des Dia-Strings
	    ))
      idstring)))


(defun op-dia-id-searcher-backward ()
  "Gibt im OP-Mode-File die ID des Dia-Tags heraus - nil wenn nicht innerhalb der Dia-Tags.
Diese Funktion sucht den letzten Text-ID-Tag heraus und gibt ihn zur,A|(Bck."
  (let ((idstring ""))
;;;
;;; <dia id=123> Text </dia>
;;;
    (save-excursion
      (if (re-search-backward "\<dia[ \t]*id[ \t]*=.*\>" nil t)
					; <- Eindeutige Identifikation von Dia-Objekten im OP-Mode
	  ;; Schnellprogrammierung, da das mit dem Gruppierung hat nicht geklappt, warum auch immer
	  (progn
	    (re-search-forward "[0-9]" (save-excursion (end-of-line)(point)) nil) ; gibt einen Fehler wenn kein ID-Tag da ist
	    (backward-char 1)
	    (while (looking-at "[0-9]")
	      (setq idstring (concat idstring (buffer-substring-no-properties (point) (1+ (point)))))
	      (forward-char 1)))))
	    ;; idstring ent,Ad(Bllt hier die ID des Dia-Strings
    idstring))




;(defun dia-inserter (file &optional killemptylines)
;  "Setzt die Textpassagen aus dem XML-File des Programmes Dia (abgespeichert in 
;nicht komprimierter Form!) in OP-Mode-Form an die aktuelle Cursorposition ein.
;Argument FILE f$(1vd(B den Dia-File, Argument KILLEMPTYLINES steuert bei nil das 
;Einsetzen leere Zeilen von Dia zu OP-Mode zu verhindern."
;  (interactive "fDia file: ")
;  (let ((string "") (current-buff (current-buffer)) (idstring "") (tagpoint (point)))
;    (find-file file)
;    (switch-to-buffer (file-name-nondirectory file))
;    (beginning-of-buffer)
;    (if (setq string (dia-string-searcher-forward))
;	(progn
;	  ;; l,Av(Bsche wenn nicht vermieden alle Leerzeilen aus dem String
;	  (if (not killemptylines)
;	      (while (posix-string-match "^[ \t\n\r]+" string)
;		(setq string (concat (substring string 0 (match-beginning 0)) (substring string (match-end 0) nil)))))
;	  (setq idstring (dia-id-searcher-backward))
;	  ;;
;	  (switch-to-buffer current-buff)
;	  (end-of-line)
;	  (newline)
;	  (insert "* Textbereiche aus dem XML-Code des Programmes \"Dia\", File: " file "\n" )
;	  (insert (concat "\n**\t" "<dia id=" idstring ">" string "</dia>" "\n"))
;	  ;; indentiere OP-Mode-gem,Ad_(B
;	  (progn
;	    (setq tagpoint (+ 2 (point)))
;	    (search-backward "<dia" nil t)
;	      (end-of-line)
;	      (next-line 1)
;	      (while (and (not (eobp))
;			  (<= (point) tagpoint))
;		(beginning-of-line)
;		(when (not (looking-at "^\\*")) (insert " ")(beginning-of-line))
;		;;(re-indent-for-tab-command)
;		(if (not (bobp))(re-indent-for-tab-command))
;		(end-of-line)
;		(if (not (eobp))(next-line 1))))
;	  (switch-to-buffer (file-name-nondirectory file))
;	  ;;
;	  ;;
;	  (while (setq string (dia-string-searcher-forward))
;	    (setq idstring (dia-id-searcher-backward))
;	    ;; l,Av(Bsche wenn nicht vermieden alle Leerzeilen aus dem String
;	    (if (not killemptylines)
;		(while (posix-string-match "^[ \t\n\r]+" string)
;		  (setq string (concat (substring string 0 (match-beginning 0)) (substring string (match-end 0) nil)))))
;	    ;;
;	    (switch-to-buffer current-buff)
;	    (insert (concat "\n**\t" "<dia id=" idstring ">" string "</dia>" "\n"))
;	    (switch-to-buffer (file-name-nondirectory file)))
;	  (switch-to-buffer current-buff))
;      (switch-to-buffer current-buff))
;    ))



(defun dia-inserter-from-file ()
"Extracts from the dia file at the current cursor position the text entries"
  (interactive)
  (let (find-file-not-found-hooks find-file-not-found-hooks)
    (add-hook 'find-file-not-found-hooks
	      (function (lambda ()
			  (error "Keinen solchen File gefunden: %s" buffer-file-name))))
    (let ((current-buff (current-buffer)))
      (dia-inserter (current-longword)))))




(defun dia-inserter (file &optional killemptylines)
  "Setzt die ZUS,AD(BZLICHEN Textpassagen aus dem XML-File des Programmes Dia (abgespeichert in 
nicht komprimierter Form!) in OP-Mode-Form an die aktuelle Cursorposition ein.
Argument FILE f$(1vd(B den Dia-File, Argument KILLEMPTYLINES steuert bei nil das 
Einsetzen leere Zeilen von Dia zu OP-Mode zu verhindern."
  (interactive "fDia file: ")
  (let ((string "") (current-buff (current-buffer)) (idstring "") (indent-lines-counter (point)) (already nil))
    (find-file file)
    (switch-to-buffer (file-name-nondirectory file))
    (beginning-of-buffer)
    ;;
    ;; suche den ersten Sting im Dia-File
    (if (setq string (dia-string-searcher-forward))
	(progn
	  ;; l,Av(Bsche wenn nicht vermieden alle Leerzeilen aus dem String
	  (if (not killemptylines)
	      (while (posix-string-match "^[ \t\n\r]+" string)
		(setq string (concat (substring string 0 (match-beginning 0)) (substring string (match-end 0) nil)))))
	  (setq idstring (dia-id-searcher-backward))
	  ;;
	  ;; zur,A|(Bck zum OP-Mode-Buffer, string enth,Ad(Blt den Dia-String, idstring die ID
	  (switch-to-buffer current-buff)
	  (end-of-line)
	  ;;
	  ;; Haupt$(1vT(Berschrift einsetzen
	  (insert "\n* Neue Textbereiche aus dem XML-Code des Programmes \"Dia\", ")
	  (if (fboundp 'insert-date) (progn (insert-date) (insert ", ")) "")
	  (insert "File: " file "\n" )
	  ;;
	  ;;
	  ;; Dia-Inhalte einsetzen
	  (while (progn
		   ;; l,Av(Bsche wenn nicht vermieden alle Leerzeilen aus dem String
		   (if (not killemptylines)
		       (while (posix-string-match "^[ \t\n\r]+" string)
			 (setq string (concat (substring string 0 (match-beginning 0)) (substring string (match-end 0) nil)))))
		   ;;
		   ;; wenn Tag-ID im OP-Mode noch nicht vorhanden ist, dann setze den String ein
		   ;; - daher hier bestimmen, ob der ID-Tag schon da ist: Variable already
		   (save-excursion
		     (beginning-of-buffer)
		     (if (re-search-forward (concat "\<dia[ \t]*id[ \t]*=[ \t]*" idstring "[ \t]*\>") nil t) ; OP-Mode-Dia-Tag
			 (setq already t)
		       (setq already nil)))
		   
		   ;;
		   ;;

		   (debug)
		   (if (not already)
		       ;;
		       ;; wenn der Dia-Tag noch nicht vorhanden ist, f$(1vY(Be den neu gefundenen als OP-Mode-berschrift ein
		       ;;
		       (progn
			 (insert (concat "\n**\t" "<dia id=" idstring ">" string "</dia>" "\n"))
			 ;; indentiere OP-Mode-gem
			 (progn
			   (setq indent-lines-counter (count-lines (save-excursion (re-search-backward "<dia[ \t]" nil t) (point))
								   (point)))
			   (re-search-backward "<dia[ \t]" nil t)
			   (end-of-line)
			   (next-line 1)
			   ;; laufe nun die Anzahl der Zeilen nach unten ab und indentiere sie:
			   ;; die Variable indent-lines-counter ist der Zeilenzeiler
			   (setq indent-lines-counter (1- indent-lines-counter))
			   (while (and (not (eobp))
				       (<= 0 indent-lines-counter))
			     (beginning-of-line)
			     (when (not (looking-at "^\\*")) (insert " ")(beginning-of-line))
			     (if (not (bobp))(re-indent-for-tab-command))
			     (end-of-line)
			     (if (not (eobp))(next-line 1))
			     (setq indent-lines-counter (1- indent-lines-counter))
			     )
			   (previous-line 1)))
		     ;;
		     ;; und wenn er schon vorhanden ist und der Taginhalt NICHT bereinstimmt, frage nach einer Synchronisation
		     ;;
		     ;; Vergleiche den Tag-Inhalt
		     ;;     zuerst die ID im op-File suchen
		     (switch-to-buffer current-buff)
		     (goto-char (point-min))
		     (if (search-forward (concat "<dia id=" idstring ">") nil t)
			 ;; hat den Tag im op-file gefunden, daher jetzt den Text vergleichen
			 ;; (muss gefunden werden!!)
			 (progn
			   (backward-char 1)
			   (if (save-excursion
				 (search-forward string (save-excursion (re-search-forward "<dia[ \t]" nil t) (point)) t))
			       ;; hat den Text identisch gefunden, alles in Ordnung
			       t
			     ;; wenn nicht: den String zur h,Ad(Bndigen Synchronisation anbieten
			     (dia-ID-synchronisator file)
			     ))
		       
		       (error "op-dia-syncer.el: das sollte aber gefunden werden"))
		     (switch-to-buffer (file-name-nondirectory file))
		     



		     ; (if (string= string diaopsstring



		     ; wenn Tag nicht gleich: h,Ad(Bndige Synchronisation




		     ; Erg,Ad(Bnzung zur h,Ad(Bndigen Synchronisation: a->b, b->a, q f$(1vd(B quit im dia-XML-Buffer




		     )
		   
		   (switch-to-buffer (file-name-nondirectory file))
		   (if (setq string (dia-string-searcher-forward))
		       (progn
			 (setq idstring (dia-id-searcher-backward))
			 (switch-to-buffer current-buff))
		     (switch-to-buffer current-buff)
		     nil)))
	  ))))


;(dia-inserter "c:/temp/Diagramm1.dia" nil)
;(dia-inserter "c:/temp/Diagramm1.dia" t)






(defun dia-ID-synchronisator (file)
  "Sucht, wenn im OP-Mode-File an einer <dia id=.*>-Passagen stehend, den XML-File von Dia heraus
und bietet in einem extra Fenster den Tag zur hndigen Synchronisation an. 
Die Sync-Suche des Dia-Files im Argument mit Hilfe der Dia-ID-Tags."
  (interactive "fDia file: ")
  (let ((idstring "") (tmppnt (point)) (tvar 0))

    ;; nachsehen, ob man sich tatschlich innerhalb eines Dia-Tags im OP-Mode befindet
    (split-window-vertically 10)

    ;; wenn man direkt auf einem Tag steht: gehe in eine tagfreie Zone
    (if (< (setq tvar (- (point)
			 (save-excursion (search-backward "\<" nil t) (point))))
	   3)
	(backward-char tvar))
    ;; oder wenn man in dem Tag im Bereich des Wortes id= steht
    (if (<= (setq tvar (- (point)
			  (save-excursion (search-backward "<dia" nil t) (point))))
	    9)
	(backward-char tvar))
    
    (if (re-search-backward "\<dia\\|\</dia" nil t)
	(if (looking-at "\</dia") ; steht nicht zwischen zwei Tags
	    (progn
	      (goto-char tmppnt)
	      (message "not in <dia id=...> tag")
	      (delete-window)
	      (other-window 1)
	      nil)
	  (goto-char tmppnt)
	  (setq idstring (op-dia-id-searcher-backward))
	  (find-file file)
	  (beginning-of-buffer)
	  (if (re-search-forward (concat "\<dia:object[ \t]*type[ \t]*=[ \t]*\".*Text.*\".*\".*\\id[ \t]*=[ \t]*\"O" idstring ".*\".*\>") nil t)
	      (progn
		(beginning-of-line)
		(dia-string-searcher-forward) 
		(re-search-backward "\<dia:string\>#" (save-excursion (beginning-of-line)(point)) nil)
		(re-search-forward "\<dia:string\>#" (save-excursion (end-of-line)(point)) nil)
		(recenter))
	    (delete-window)
	    (other-window 1)
	    (message "no equivalent tag found")))
      (delete-window)
      (other-window 1)
      (message "Parser error: Current cursor position is not inside a <dia...> </dia> tag")
      nil)))




(defun next-dia-tag ()
  (interactive)
  (if (not (re-search-forward "\<dia[ \t]*id[ \t]*=.*\>" nil t))
					; <- Eindeutige Identifikation von Dia-Objekten im OP-Mode
      (message "no Dia tag found")))

(defun previous-dia-tag ()
  (interactive)
  (if (re-search-backward "\<dia[ \t]*id[ \t]*=.*\>" nil t)
					; <- Eindeutige Identifikation von Dia-Objekten im OP-Mode
	(re-search-forward "\<dia[ \t]*id[ \t]*=.*\>" nil t)
      (message "no Dia tag found")))





(defun dia-textline-synchronisator (file)
  "Sucht im OP-Mode-File die markierten <dia.*>-Passagen und bietet die beiden in einem extra Fenster 
zur hndigen Synchronisation an. 
Die Sync-Suche des Dia-Files im Argument mit Hilfe einer Zeilensuche - das heit mit Hilfe identischer 
Textstrings innerhalb eines Dia-Tags im Dia-File.

Dies ist *keine* zuverl,Ad(Bssige Synchronisation.
"
  ; auskommentiert, damit nicht aktiv sichtbar ;(interactive "fDia file: ")
  (let ((syncline "") (tmppnt 0) (current-buff (current-buffer)) (tpoint1 0) (tpoint2 0) (current-point (point)))
    (find-file file)
    (switch-to-buffer current-buff)
    (beginning-of-buffer)
    ;;
    ;; suche Tag
    (while (search-forward "<dia" nil t)
      (setq tmppnt (save-excursion (search-forward "</dia>" nil nil) (point)))
      ;;
      ;; suche die Textzeile zwischen den Tags einzeln ab, suche diese im Dia-File und frage nach, ob 
      ;; und in welche Richtung diese synchronisiert werden soll. 
      ;; Die Textzeilen werden _einzeln_ synchronisiert, da eventuell im OP-Mode Formatierungen ge,Ad(Bndert werden.
      ;;
      ;;
      ;; suche die Zeile im Tag
      (while (and (> tmppnt (save-excursion (end-of-line)(point)))
		  (re-search-forward "[^ \t]" 
				     (min (save-excursion (end-of-line)(point))
					  (save-excursion
					    (if (search-forward "</dia>" (save-excursion (end-of-line)(point)) t)
						(- (point) 6)
					      999999)))
				     t)
		  )
	(progn
	  (backward-char 1)
	  (setq syncline (buffer-substring-no-properties (point) (min (save-excursion (end-of-line)(point))
								      (save-excursion
									(if (search-forward "</dia>" (save-excursion (end-of-line)(point)) t)
									    (- (point) 6)
									  999999)))))
	  ;; hier sollte syncline den Inhalt zwischen den Tags enthalten - von der Zeile, in der der Cursor steht
	  ;;
	  ;;
	  
	  ;; suche im Dia-File nach dem Inhalt der Variablen "syncline"
	  (switch-to-buffer (file-name-nondirectory file))
	  (beginning-of-buffer)
	  (if (search-forward syncline nil t)
	      (progn
		;; extra Fenster mit Dia-File
		(split-window-vertically 10)
		(beginning-of-line)
		(recenter)
		(other-window 1)
		;; wieder zum Ursprungsfenster, Cursor-Position im <dia.*>-Bereich
		(split-window-vertically 10)
		(beginning-of-line)
		(recenter)
		(switch-to-buffer current-buff)
		(other-window 1)
		)
	    )
	  (switch-to-buffer current-buff)
	  (goto-char current-point)
)))))


