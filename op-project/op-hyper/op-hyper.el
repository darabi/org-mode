;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Kettenausführungen, genaueres siehe unter den Beispielen von EVAL
;   ->FILE:op-hyper.el*defun;EVAL:date
;   ->FILE:op-hyper.el*defun->;EVAL:date
;
;;;;;;;;;;;;;;;; Beispielliste aus den Kommentaren (z.T. erweitert) ;;;
;;
;; Beispiele SQL:
;;  
;   ->SQL:sqlfile.sql*startpunkt
;   ->SQL:sqlfile2.sql
;
;; ->>../op-project.el*defun
;; Beispiele FILE:
;; 
;;   ->>Wort sucht alle vorkommenden ganzen Wörter, ->Wort dieses nur in Überschriften
;;                                                    {(AUßER DIESE EXISTIERT NICHT, dann wird der Pfeil wie ein 
;;                                                     ->> behandelt !! (Grund: bessere Optik im File))
;;                                                     Dieses Verhalten, beschrieben hier in der
;;                                                     geschweifte Klammer kann durch vertauschen der
;;                                                     Funktionen FILE:func und FILE:func2 geändert werden, siehe
;;                                                     Kommentar über FILE:func2.}
;;   ->*Wort dieses nur am Anfang einer Einser-Überschrift, entsprechend ->**Wort bei einer Zweier-Überschrift
;;   ->Name* sucht in File "Name"
;; 
;   ->>FILE:../op-project.el*defun     ; mehrmals auslösen gibt jeweils das nächste Muster
;   ->>FILE:../op-project.el*defun->
;   ->>FILE:defun
;   ->>FILE:defun->
;   ->>FILE:../op-project.el*          ; Sucht nur den anderen File aus
;   ->>defun                           ; Defaultbutton: sucht im gleichen File das Wort "defun"
;   ->>defun                           ; Defaultbutton: sucht im gleichen File das Wort "defun"
;   ->>defun->                         ; dito, setzt den Cursor aber in ein anderes Fenster
;   ->FILE:../op-project.el*defun      ; mehrmals auslösen gibt jeweils das nächste Muster
;   ->FILE:../op-project.el*defun->
;   ->FILE:defun
;   ->FILE:defun->
;   ->FILE:../op-project.el*           ; Sucht nur den anderen File aus
;   ->defun                            ; Defaultbutton: sucht im gleichen File das Wort "defun"
;   ->defun                            ; Defaultbutton: sucht im gleichen File das Wort "defun"
;   ->defun->                          ; dito, setzt den Cursor aber in ein anderes Fenster
;   ->>../op-project.el*               ; sucht den File ../op-project.el heraus
;   ->../op-project.el*                ; sucht den File ../op-project heraus (-> geht hier nur, weil kein Einspring-
;                                      ;           punkt vorhanden ist, eigentlich muesste hier ->> stehen
;
;   ->../op-project.el                 ; doppelt verkürzte Form: wie oben, jedoch kann auch noch der Stern
;                                      ; weggelassen werden, da hier der File über den / erkannt wird
;   ->../op-project.el->               ; doppelt verkürzte Form: wie oben, jedoch kann auch noch der Stern
;                                      ; weggelassen werden, da hier der File über den / erkannt wird
;   ->op-consistent.el                 ; doppelt verkürzte Form: wie oben, jedoch kann auch noch der Stern
;                                      ; weggelassen werden, da hier der File über den . und Buchstabe 
;                                      ; als Verweis auf eine Extention gesucht wird
;   ->op-consistent.el->               ; doppelt verkürzte Form: wie oben, jedoch kann auch noch der Stern
;                                      ; weggelassen werden, da hier der File über den . und Buchstabe 
;                                      ; als Verweis auf eine Extention gesucht wird
;
;   ->>../op-project.el*defun->        ; in anderem File
;   ->>FILE:[ab]test                   ; alle Buttons dürfen Regexe enthalten, jedoch keine Blanks
;        atest btest ctest             ;    (bleibt bei atest und btest stehen, kennt aber kein ctest)
;   ->>FILE:"a test"->                 ; Blanks usw. sind erlaubt, doch nur in Anführungsstrichen  ("a test")
;   ->>FILE:"\* test"                  ; Achtgeben: Sterne gehören zu den Regexen, müssen daher gequotet werden
;   ->>FILE:"\* test"->
;   * test    $ test                   ; nur zum Testen
;
;   ->>A:Müller                        ; private Kurzform für ->FILE:adressen.op*"name", Adressenfile als Variable hier im Code
;
;   ->**Ueberschriftentest             ; steht ein * am Anfang des Musters, werden die Sterne nicht als Regexe interpretiert,
;                                      ;    sondern als "Uberschriftengerechtes Muster" (hier: `**	Ueberschriftentest')
;                                      ;    Verhalten: ist das Muster am Zeilenanfang zu finden, wird es auch dort genommen,
;                                      ;    ansonsten geht es durch den normalen Text.
; 
;   ->Ueberschriftentest
;   ->FILE:Ueberschriftentest
;
;**	Ueberschriftentest
;
;;
;; Beispiele FUNC:
;;  
;   ->FUNC:funcfile*startpunkt
;   ->FUNC:funcfile*Fakultät
;   ->FUNC:startpunkt
;
;;
;; Beispiele EVAL:
;;  
;   ->EVAL:xeyes
;   ->EVAL:xeyes&
;   ->EVAL:"date --version"  
;   ->EVAL:"gcc -v"
;   ->EVAL:xfig*test.fig
;   ->EVAL:"xfig -bg red"*test.fig    ; Argumente in Anführungsstrichen, Ausführungspunkt NUR vor dem 
;                                     ; ersten Blank (beim "->EVAL:xxxx"
;   ->EVAL:"xfig -bg red"*test.fig&   ; wie eben
;   ->FILE:op-hyper.el*defun;EVAL:"date --version"  
;   ->FILE:op-hyper.el*defun->;EVAL:"date --version"  
;				      ; bei Kettenargumenten: EVAL immer an den Schluß,
;                                     ;  andere Funktionen immer mit *-Zeichen. Nur je ein Buttontyp ist erlaubt.
;   ->EVAL:test.fig                   ; Defaultextention ist einprogrammiert (siehe oben)
;   ->!date                           ; Defaultbuttons
;   ->!"date --version"
;   ->!cat*op-hyper.el
;  
;;
;; Beispiele REF:
;;
;   ->REF:../op-hyper/op-hyper.el*1button      ; Muster ist hier "1button", kann aber beliebig sein
;   ->REF:../op-hyper/op-hyper.el*1button->
;   ->REF:1button                              ; auch hier: mehrmaliges Auslösen ohne Bewegen des 
;                                              ; Cursors im anderen Fenster sucht nächste Referenz
;   ->REF:1button->
;
; die alle sollten hier enden:   ->1button:FILE:defun   ; Doppelpunkt ist günstig, aber nicht Pflicht:
;                                ->1button#FILE:defun   ; geht z. B. auch
;
;
;;
;; Beispiele (siehe auch die bei REF:func) REF-R:
;;
;   ->REF-R:../op-hyper/op-hyper.el*2button->      ; Muster ist hier "2button", kann aber beliebig sein
;
;   ->REF-R:2button->
;
;   ->2button:REF-R:3button->
;
; die alle sollten hier enden:   ->3button:FILE:das_hier_gibts_nicht   
;
;; weiteres Beispiel: Loop zurück zum ersten Button wird nicht unendlich durchlaufen 
;; (ideal zum Einlesen vieler Files !!!)
;
;   ->4button:REF-R:5button->           
;        ->5button:REF-R:6button->
;             ->6button:REF-R:4button->
;
;
;; nicht gefunden wird dagegen die folgende Loop (gibt dann aber einen Emacs-Fehler)
;; (zum Debuggen empfiehlt sich REF-R:func in REF:func umzudefinieren) 
;   ->4button:REF-R:5button->           
;        ->5button:REF-R:6button->
;             ->6button:REF-R:5button->
;
;


(defvar adressenfile "~/projects/adressen.op")
(make-variable-buffer-local 'adressenfile)



(defun current-thislongword ()
;;; ACHTUNG !!! siehe auch Funktion "current-longword-quote" im File op-columns.el !!!!
;;; Sucht alle EVAL-Button-Knöpfe sauber aus (siehe Beispiele und Kommentare bei "defun EVAL:func"
;;; unten), auch mit Anführungsstrichen.
  (interactive)
  (save-excursion
    (let ((begin) (end) (temp))
      (save-excursion
	(if (re-search-backward "[ \t\r]\\|^" nil t)
	    (progn
	      (if (looking-at "[ \t\r]")
		  (forward-char 1))))
	(setq begin (point))
	(if (re-search-forward "[ \t\r]\\|$" nil t)     
	    (progn
	      (backward-char 1)
	      (if (not (looking-at "[ \t\r\n]"))
		       (forward-char 1))))
	(setq end (point)))
      (setq temp (buffer-substring-no-properties begin end))
      (if (and (string-match "\"" temp)
	       (not (string-match "\".*\"" temp)))
	  (progn
	    (goto-char end)
	    (re-search-forward "\"\\|$" nil t)
	    (if (re-search-forward "[ \t\r]\\|$" nil t)
		(progn
		  (backward-char 1)
		  (if (not (looking-at "[ \t\r\n]"))
			   (forward-char 1))))
	    (setq end (point))))
      (buffer-substring-no-properties begin end))))
	  


(defvar current-point-tmp1 0)
(make-variable-buffer-local 'current-point-tmp1)
(defvar current-buffer-tmp1 (current-buffer))
(make-variable-buffer-local 'current-point-tmp1)
(defvar called-via-OP-button nil)
(make-variable-buffer-local 'called-via-OP-button)
(defun select-hyper-func (&optional stop-recursive)
  (interactive)
  (let ((command (current-thislongword)))
    (if (string-match "->[>]?[^ \t]*\\(SQL:\\|FILE:\\|A:\\|FUNC:\\|EVAL:\\|REF:\\|REF-R\\)" command)
	(progn
	  (setq called-via-OP-button t)
	  
	  ;; ---------- SQL-Button
	  ;; suche nach ->...SQL:func und führe func im SQL-Modus aus
	  ;; erst: Suche nach "->SQL:file*evalstartpunkt"
	  (if (string-match "\\(-\>[^ \t]*SQL:\\)\\(.*\\*\\)\\([^ \t\r;]*\\)\\(;?\\)" command)
	      (progn
		(SQL:func (substring command (match-beginning 2) (- (match-end 2) 1))
			  (substring command (match-end 2) (match-beginning 4))))
	    ;; oder auch: suche nach ->SQL:file
	    (if (string-match "-\>[^ \t]*SQL:" command)
		(SQL:func (substring command (match-end 0)) nil)))
	  
	  ;; ---------- FILE-Button
	  ;; suche nach ->>...FILE:file*RegexStelle
	  (if (string-match "\\(-\>\>[^ \t]*FILE:\\)\\(.*\\*\\)\\([^ \t\r;]*\\)\\(;?\\)" command)
	      (FILE:func (substring command (match-beginning 2) (- (match-end 2) 1))
			 (concat ">" (substring command (match-end 2) (match-beginning 4))))
	    ;; oder auch: suche nach ->>FILE:RegexStelle (dann im aktuellen File)
	    (if (string-match "-\>\>[^ \t]*FILE:" command)
		(FILE:func nil (concat ">"(substring command (match-end 0))))
	      ;; suche nach ->...FILE:file*RegexStelle
	      (if (string-match "\\(-\>[^ \t]*FILE:\\)\\(.*\\*\\)\\([^ \t\r;]*\\)\\(;?\\)" command)
		  (FILE:func (substring command (match-beginning 2) (- (match-end 2) 1))
			     (substring command (match-end 2) (match-beginning 4)))
		;; oder auch: suche nach ->FILE:RegexStelle (dann im aktuellen File)
		(if (string-match "-\>[^ \t]*FILE:" command)
		    (FILE:func nil (substring command (match-end 0)))))))
	  
	  ;; ---------- A-Button (für den eigenen Adressenfile) (Kurzform für ->FILE:adressen.op*"Name"
	  ;; suche nach ->...A:RegexStelle
	  (if (string-match "-\>[^ \t]*A:" command)
	      (FILE:func adressenfile (substring command (match-end 0))))

	  ;; ---------- FUNC-Button
	  ;; suche nach ->...FUNC:funcfile*func, sehe im funcfile nach, was func machen soll
	  ;;    und führe dieses dann aus
	  (if (string-match "\\(-\>[^ \t]*FUNC:\\)\\(.*\\*\\)\\([^ \t\r;]*\\)\\(;?\\)" command)
	      (progn
		(FUNC:func (substring command (match-beginning 2) (- (match-end 2) 1))
			   (substring command (match-end 2) (match-beginning 4))))
	    ;; oder nehme dafür den Default-Funcfile: .op-navigate
	    (if (string-match "-\>[^ \t]*FUNC:" command)
		(FUNC:func nil (substring command (match-end 0)))))
	  
	  ;; ---------- EVAL-Button
	  ;; suche nach ->...EVAL:programm*file, ruft Programm auf, um damit File auszuführen
	  (if (string-match "\\(-\>[^ \t]*EVAL:\\)\\(.*\\*\\)\\([^ \t\r;]*\\)\\(;?\\)" command)
	      (progn
		(EVAL:func (substring command (match-beginning 2) (- (match-end 2) 1))
			   (substring command (match-end 2) (match-beginning 4))))
	    ;; oder nach EVAL:file, dieses sucht sich 
	    (if (string-match "-\>[^ \t]*EVAL:" command)
		(EVAL:func nil (substring command (match-end 0)))))
	  
	  ;; ---------- REF-Button
	  ;; suche nach ->...REF:file*RegexStelle
	  (if (string-match "\\(-\>[^ \t]*REF:\\)\\(.*\\*\\)\\([^ \t\r;]*\\)\\(;?\\)" command)
	      (progn
		(REF:func (substring command (match-beginning 2) (- (match-end 2) 1))
			  (substring command (match-end 2) (match-beginning 4))))
	    ;; oder auch: suche nach ->REF:RegexStelle (dann im aktuellen File)
	    (if (string-match "-\>[^ \t]*REF:" command)
		(REF:func nil (substring command (match-end 0)))))
	  
	  ;; ---------- REF-R-Button (R für rekursiv)
	  ;; suche nach ->...REF-R:file*RegexStelle: Führe neu gefundenen Button sogleich wieder aus
	  (if (not stop-recursive)  ; interaktiv gerufen
	      (progn
		;; suche Position von REF-R
		(save-excursion
		  (let ((suchende (re-search-forward "[ \t\r]\\$" nil t)) (suchanfang))
		    (backward-char 1)
		    (setq suchanfang (re-search-backward "[ \t\r]\\|^" nil t))
		    (if (search-forward "REF-R" suchende t)
			(progn
			  (search-backward "REF-R" suchanfang t)
			  (setq current-point-tmp1 (point))
			  (setq current-buffer-tmp1 (current-buffer))))))))
	  ;; wenn nicht interaktiv gerufen und nicht im selben Buffer am selben Punkt
	  (if (not (and stop-recursive
			(eq current-point-tmp1 (point))
			(eq current-buffer-tmp1 (current-buffer))))
	      (if (string-match "\\(-\>[^ \t]*REF-R:\\)\\(.*\\*\\)\\([^ \t\r;]*\\)\\(;?\\)" command)
		  (progn
		    (REF-R:func (substring command (match-beginning 2) (- (match-end 2) 1))
				(substring command (match-end 2) (match-beginning 4))))
		;; oder auch: suche nach ->REF-R:RegexStelle (dann im aktuellen File)
		(if (string-match "-\>[^ \t]*REF-R:" command)
		    (REF-R:func nil (substring command (match-end 0)))))
	    (message "Returned to current button: stop")
	    (delete-other-windows)
	    (isearch-dehighlight t)
	    (recenter)))
      
      
      ;; ---------- Default-Button 
      ;;
      ;; erstmal: ->etwashier
      ;;
      (if (string-match "^-\>" command)
	  ;; Suche nach ->!programm oder ->!programm*file  ( entspricht EVAL: )
	  (if (string-match "-\>!" command)
	      (if (string-match "\\(-\>!\\)\\(.*\\*\\)" command)
		  (progn
		    (EVAL:func (substring command (match-beginning 2) (- (match-end 2) 1))
			       (substring command (match-end 2))))
		;; oder auch: suche nach ->!programm
		(if (string-match "-\>!" command)
		    ;;(nur ->file*RegexStelle oder nur ->RegexStelle
		    (EVAL:func nil (substring command (match-end 0)))))
	    ;; oder nach ->>file*RegexStelle
	    (if (string-match "\\(-\>\>\\)\\(.*\\*\\)" command)
		(FILE:func (substring command (match-beginning 2) (- (match-end 2) 1))
			   (concat ">" (substring command (match-end 2) (match-beginning 4))))
	      ;; oder auch: suche nach ->>RegexStelle (dann im aktuellen File)
	      (if (string-match "-\>\>" command)
		  (FILE:func nil (concat ">" (substring command (match-end 0))))
		;; suche nach ->file*RegexStelle
		(if (string-match "\\(-\>\\)\\(.*\\*\\)" command)
		    (FILE:func (substring command (match-beginning 2) (- (match-end 2) 1))
			       (substring command (match-end 2) (match-beginning 4)))
		  ;; mit / im Link: daraus wird ein FILE: extrapoliert
		  (if (or (string-match "\\(-\>\\)\\(.*/.*\\)" command)
			  ;; mit . mit Nachbuchstabe im Link: daraus wird ein FILE: extrapoliert
			  (string-match "\\(-\>\\)\\(.*\\.+.*\\)" command))
		      (FILE:func (substring command (match-beginning 2) (match-end 2))
				 (substring command (match-end 2) (match-beginning 4)))
		    ;; oder auch: suche nach ->RegexStelle (dann im aktuellen File)
		    (if (string-match "-\>" command)
			(FILE:func nil (substring command (match-end 0)))))))))

;	    (if (string-match "\\(-\>\\)\\(.*\\*\\)" command)
;		(FILE:func (substring command (match-beginning 2) (- (match-end 2) 1))
;			   (substring command (match-end 2)))
;	      ;; oder auch: suche nach ->RegexStelle (dann im aktuellen File)
;	      (if (string-match "-\>" command)
;		  (FILE:func nil (substring command (match-end 0))))))
	(message "not recognised as a button")))
    (setq called-via-OP-button nil)
    ))



(defun SQL:func (file startpunkt)
  (let ((pointsv (point-min)) (current-buf (current-buffer)))
;    (if startpunkt
;	(setq startpunkt (regexp-quote startpunkt)))
    (if (get-buffer (file-name-nondirectory file))
	(switch-to-buffer (file-name-nondirectory file))
      (if (not (file-exists-p file))
	  (progn
	    (delete-other-windows)
	    (error "File not found  " file))
	(if (file-exists-p file)
	    (find-file file)
	  (if (file-exists-p (concat file ".crypt"))
	      (find-file (concat file ".crypt"))))))
    (goto-char (point-min))
    ;; wenn Startpunkt im Buffer angegeben ist,
    (if startpunkt
	(progn
	  (re-search-forward startpunkt nil t)
	  ;; dann eval ab dort -
	  (setq pointsv (point))
	  ;; bis zum "END"-statement
	  (if (re-search-forward "END" nil t)
	      (backward-char (length "END"))
	    ;; oder bis zum Ende des Files
	    (goto-char (point-max)))
	  ;; ansonsten: sowieso bis zum Ende
	  (narrow-to-region pointsv (point)))
      (goto-char (point-max)))
;;;    (SQLeval buffer)
    (sql-send-region pointsv (point))
    (widen)
    (switch-to-buffer current-buf)))
;;
;; Beispiele
;;  
;   ->SQL:sqlfile.sql*startpunkt
;   ->SQL:sqlfile2.sql
;
  




(defvar am-i-at-point 0)
(make-local-variable 'am-i-at-point)
(defun FILE:func (file regex)
  ;; Verhalten: Beim ersten Betätigen: Suche ersten Verweis. Ist schon ein Fenster offen: 
  ;; siehe nach, ob sich der Cursor bewegt hat. Wenn nicht: Suche nächsen Verweis.
  (let ((pattern regex) (current-buf (current-buffer)) (was-one-window nil) (originalpattern)
	(no-headline-search t) (setto-other-window nil) (einsplus 0))
;    (setq regex (regexp-quote regex))
    (if (one-window-p)
	(progn
	  (split-window)
	  (setq was-one-window t)))
    (other-window 1)
    ;; ist ein Pfeil am Ende des Ausdrucks von "file" ? "pattern" ist "regex" ohne "->" am Ende.
    (if file
    (if (string-match "-\>$" file)
	(progn
	  (setq file (substring file 0 -2))
	  (setq setto-other-window 1))
    (if (string-match "-$" file)
	(progn
	  (setq file (substring file 0 -1))
	  (setq setto-other-window 2)))))
    ;; --
    (if file
	(if (get-buffer (file-name-nondirectory file))
	    (progn
	      (switch-to-buffer (file-name-nondirectory file))
	      (if was-one-window
		  (setq am-i-at-point 0)))
	  (if (file-exists-p file)
	      (find-file file)
	    (if (file-exists-p (concat file ".crypt"))
		(find-file (concat file ".crypt"))
	      (delete-other-windows)
	      (error "File not found  " file)))
	  (setq am-i-at-point 0))
      (switch-to-buffer current-buf))
    ;; ist ein Pfeil am Ende des Ausdrucks ? "pattern" ist "regex" ohne "->" am Ende.
    (if (string-match "-\>$" regex)
	(setq pattern (substring regex 0 -2)))
    (if (string-match "-$" regex)
	(setq pattern (substring regex 0 -1)))
    ;; wenn am Ende des Musters ein zweiter . ist, dann streiche diesen weg
    (if (string-match ".*\\..*\\.$" regex)
	(progn
	  (setq pattern (substring regex 0 -1))
	  (setq einsplus 1)))
    ;; schaue nach, ob pattern Anführungsstriche enthält ( wie "Muster" )
    (if (string-match "^\".*\"$" pattern)
	;; wenn ja: ersten und letzen Character wegstreichen (sollten die Anführungsstriche sein)
	(setq pattern (substring pattern 1 -1))
      ;; das gleiche auch, wenn das Muster aussieht wie ->>"Muster" (für detailierte Suche) 
      (if (string-match "^>\".*\"$" pattern)
	  (setq pattern (concat ">" (substring regex 2 -1))))) ;; das ">" (von "->" + ">" = "->>") wird 
                                                               ;; einfach an das geschnittene 
					                       ;; Muster weiterverarbeitet
    (setq originalpattern pattern)
    ;;
    ;;
    ;;
    ;; wenn im Pattern ein * am Anfang steht, ist der Stern nicht als regex gemeint, sondern zum Suchen einer
    ;; Hauptüberschrift. * steht dann für eine Überschrift erster Ordnung, ** für zweite Ordnung u.s.w..
    (if (string-match "^\\*" pattern)
	(progn
	  ;;(setq pattern (substring pattern 2))
	  (string-match "\\([*]+\\)\\(.*\\)" pattern)
	  (setq pattern
		(concat
		 (regexp-quote (match-string 1 pattern))
		 ".?.?"   ; was nach den Sternen der Ueberschriften kommt (E fuer erledigt, D fuer dringend,...)
		          ; aber auch eventuell Blanks
		 ;; rechne die Anzahl der Sterne in Tabs um
		 (if (< 1 (length (match-string 1 pattern)))
		     (concat (make-string (if (< 0 (- (length (match-string 1 pattern)) 3))
					      (- (length (match-string 1 pattern)) 3)
					    0) ?	))
		   ;; oder, wenn nur ein Stern: ein Blank
		   (make-string 1 ? ))
		 ;; Muster als vollständiges Wort
		 (concat "\\<" (match-string 2 pattern) "\\>"))))
      ;; ansonsten: wenn das Muster ->> lautet, dann lasse es fast so, wie es ist, ansonsten suche nach Überschriften
      (if (string-match "^>" pattern)
	  ;; entferne das erste ">" im Muster
	  (if (string= pattern ">")
	      (setq pattern (substring pattern 1))
	    ;; suche nur ganzes Wort, keine Teilstrings !
	    (setq pattern (concat "\\<" (substring pattern 1) "\\>"))
	    (setq originalpattern pattern))
	;; mache ein Überschriften-Suchmuster daraus:
	;; ->> sucht alles, -> sucht Überschriften mit dem Muster als vollständigem Wort
	(setq originalpattern pattern)
	(if (not (string= pattern ""))
	    (progn
	      ;; Überschriftenmuster und kein Pfeil (->) im Muster vornedran
	      (setq pattern (concat "[*]+.*" "[ \t(<{[]" pattern "\\>"))
	      (setq no-headline-search nil)))))
    ;; Wenn kein Verweis auf ein anderes Fenster stattfindet ("->"), dann
    ;; ist u.U. eine Mehrfachsuche sehr nützlich. Dazu:
    ;; Wenn schon mal gesucht wurde und der Cursor im gesuchten Buffer 
    ;; nicht bewegt wurde: nicht zum Anfang zurück, sondern weitersuchen
    (if (= am-i-at-point (point))
	(forward-char 1)
      (goto-char (point-min)))
    ;; suche nach regex, aber verbiete Suche nach ->...regex (damit Knöpfe sich nicht selbst finden)
    ;;
    ;; ja ja ich weiß, der nächste Teil könnte sehr viel schöner geschrieben werden (wie so manches hier),
    ;; ich habe ihn aber so gelassen, da er sonst für meinen Geschmack unlesbar dicht geworden wäre.
    ;;
    (let ((tmp 0))
      (progn
	(message "searching...")
	(while
	    ;; wenn er das Muster "pattern" findet ohne einen Pfeil "->" vornedran
	    (if (or
		 (re-search-forward (concat "^" pattern) nil t)
		 (and
		  no-headline-search
		  (re-search-forward (concat "\\([ \t\r]\\|^\\)\\([^ \t\r]*\\)" pattern) nil t)
		  (not (string-equal "->" (buffer-substring-no-properties (match-beginning 2) (+ 2 (match-beginning 2)))))))
		(progn
		  (message " ")
		  ;; dann dieses hier
		  (re-search-backward originalpattern nil t)
		  (save-match-data
		    (forward-char 1)
		    (backward-char 1)
		    (if (fboundp 'outline-visible)
			(if (not (outline-visible))
			    (progn
			      (save-excursion
				(forward-char 1)
				(backward-char 1)
				(if (re-search-backward "[*]" nil t)
				    (if (fboundp 'show-entry-hide)
					(show-entry-hide t)
				      (if (fboundp 'show-entry)
					  (show-entry))))))))
		    (recenter)) ; Marker !!! und noch einmal gleich ein paar Zeilen weiter
		  (isearch-highlight (match-beginning 0)(+ (match-end 0) einsplus))
		  ;; mit -> am Ende des Strings: wechsle das Fenster
		  (setq am-i-at-point (point))
		  (if (not (string-match "-\>$" regex))
		      (other-window -1)
		    (if (not (string-match "-$" regex))
			(progn
			  (other-window -1)
			  (delete-window))))
		  nil)
	      ;; wenn er es nicht findet, sehe nach, ob er an diesem Punkt schon mal war.
	      (if (not (= tmp (point)))
		  ;; wenn nein, suche weiter
		  (setq tmp (point))
		;; sonst: mache den Pfeil -> zu einem Pfeil ->> und das ganze nochmal
		;;
		;;
		;;
		;; wenn er das Muster "pattern" findet ohne einen Pfeil "->" vornedran
		(if (or
		     (re-search-forward (concat "^" originalpattern) nil t)
		     (and
		      (re-search-forward (concat "\\([ \t\r]\\|^\\)\\([^ \t\r]*\\)" originalpattern) nil t)
		      (not (string-equal "->" 
					 (buffer-substring-no-properties 
					  (match-beginning 2) 
					  (+ 2 (match-beginning 2)))))))
		    (progn
		      (message " ")
		      ;; dann dieses hier
		      (re-search-backward originalpattern nil t)
		      (save-match-data
			(forward-char 1)
			(backward-char 1)
			(if (fboundp 'outline-visible)
			    (if (not (outline-visible))
				(progn
				  (save-excursion
				    (if (re-search-backward "^\\*" nil t)
					(if (fboundp 'show-entry-hide)
					    (show-entry-hide t)
					  (if (fboundp 'show-entry)
					      (show-entry))))))))
			(recenter))
		      (isearch-highlight (match-beginning 0)(+ (match-end 0) einsplus))
		      ;; mit -> am Ende des Strings: wechsle das Fenster
		      (setq am-i-at-point (point))
		      (if (not (string-match "-\>$" regex))
			  (other-window -1)
			(if (not (string-match "-$" regex))
			    (progn
			      (other-window -1)
			      (delete-window))))
		      nil)
		  ;; wenn er es nicht findet, sehe nach, ob er an diesem Punkt schon mal war.
		  (if (not (= tmp (point)))
		      ;; wenn nein, suche weiter
		      (setq tmp (point))
		    ;; sonst: mache den Pfeil -> zu einem Pfeil ->> und das ganze nochmal
		    ;;
		    ;;
		    ;;
		    ;;
		    (message "not found")
		    (message "Didn't find string or pattern")
		    (beep)
		    (delete-window)
		    (recenter)
		    (isearch-dehighlight t)
		    nil)))))))
    (if setto-other-window
	(progn
	  (other-window 1)
	  (if (= setto-other-window 2)
	      (progn
		(other-window -1)
		(delete-window)))))
))
;;
;; Beispiele
;;     ->defun  defun
;   ->FILE:../op-project.el*defun
;   ->>FILE:../op-project.el*defun->
;   ->>FILE:defun
;   ->>FILE:defun->
;





;;;
;;; Die nächste Funktion ist gut, wenn man die Pfeile -> und ->> scharf getrennt haben will:
;;; -> NUR in Überschriften, ->> überall. Dazu müssen die Funktionen FILE:func und FILE:func2 
;;; getauscht werden. 
;;;
;
;(defvar am-i-at-point 0)
;(make-local-variable 'am-i-at-point)
;(defun FILE:func2 (file regex)
;  ;; Verhalten: Beim ersten Betätigen: Suche ersten Verweis. Ist schon ein Fenster offen: 
;  ;; siehe nach, ob sich der Cursor bewegt hat. Wenn nicht: Suche nächsen Verweis.
;  (let ((pattern regex) (current-buf (current-buffer)) (was-one-window nil) (originalpattern)
;	(no-headline-search t))
;;    (setq regex (regexp-quote regex))
;    (if (one-window-p)
;	(progn
;	  (split-window)
;	  (setq was-one-window t)))
;    (other-window 1)
;    (if file
;	(if (get-buffer (file-name-nondirectory file))
;	    (progn
;	      (switch-to-buffer (file-name-nondirectory file))
;	      (if was-one-window
;		  (setq am-i-at-point 0)))
;	  (if (file-exists-p file)
;	      (find-file file)
;	    (if (file-exists-p (concat file ".crypt"))
;		(find-file (concat file ".crypt"))
;             (delete-other-windows)
;	      (error "File not found  " file))
;	  (setq am-i-at-point 0))
;      (switch-to-buffer current-buf))
;    ;; ist ein Pfeil am Ende des Ausdrucks ? "pattern" ist "regex" ohne "->" am Ende.
;    (if (string-match "-\>$" regex)
;	(setq pattern (substring regex 0 -2)))
;    (if (string-match "-$" regex)
;	(setq pattern (substring regex 0 -1)))
;    ;; schaue nach, ob pattern Anführungsstriche enthält ( wie "Muster" )
;    (if (string-match "^\".*\"$" pattern)
;	;; wenn ja: ersten und letzen Character wegstreichen (sollten die Anführungsstriche sein)
;	(setq pattern (substring pattern 1 -1))
;      ;; das gleiche auch, wenn das Muster aussieht wie ->>"Muster" (für detailierte Suche) 
;      (if (string-match "^>\".*\"$" pattern)
;	  (setq pattern (concat ">" (substring regex 2 -1))))) ;; das ">" (von "->" + ">" = "->>") wird 
;                                                               ;; einfach an das geschnittene 
;					                       ;; Muster weiterverarbeitet
;    (setq originalpattern pattern)
;    ;; wenn im Pattern ein * am Anfang steht, ist der Stern nicht als regex gemeint, sondern zum Suchen einer
;    ;; Hauptüberschrift. * steht dann für eine Überschrift erster Ordnung, ** für zweite Ordnung u.s.w..
;    (if (string-match "^\\*" pattern)
;	(progn
;	  ;;(setq pattern (substring pattern 2))
;	  (string-match "\\([*]+\\)\\(.*\\)" pattern)
;	  (setq pattern
;		(concat
;		 (regexp-quote (match-string 1 pattern))
;		 ".?.?"   ; was nach den Sternen der Ueberschriften kommt (E fuer erledigt, D fuer dringend,...)
;		          ; aber auch eventuell Blanks
;		 ;; rechne die Anzahl der Sterne in Tabs um
;		 (if (< 1 (length (match-string 1 pattern)))
;		     (concat (make-string (if (< 0 (- (length (match-string 1 pattern)) 3))
;					      (- (length (match-string 1 pattern)) 3)
;					    0) ?	))
;		   ;; oder, wenn nur ein Stern: ein Blank
;		   (make-string 1 ? ))
;		 ;; Muster als vollständiges Wort
;		 (concat "\\<" (match-string 2 pattern) "\\>"))))
;      ;; ansonsten: wenn das Muster ->> lautet, dann lasse es fast so, wie es ist, ansonsten suche nach Überschriften
;      (if (string-match "^>" pattern)
;	  ;; entferne das erste ">" im Muster
;	  (if (string= pattern ">")
;	      (setq pattern (substring pattern 1))
;	    ;; suche nur ganzes Wort, keine Teilstrings !
;	    (setq pattern (concat "\\<" (substring pattern 1) "\\>"))
;	    (setq originalpattern pattern))
;	;; mache ein Überschriften-Suchmuster daraus:
;	;; ->> sucht alles, -> sucht Überschriften mit dem Muster als vollständigem Wort
;	(setq originalpattern pattern)
;	(if (not (string= pattern ""))
;	    (progn
;	      ;; Überschriftenmuster und kein Pfeil (->) im Muster vornedran
;	      (setq pattern (concat "[*]+.*" "[ \t(<{[]" pattern "\\>"))
;	      (setq no-headline-search nil)))))
;    ;; Wenn kein Verweis auf ein anderes Fenster stattfindet ("->"), dann
;    ;; ist u.U. eine Mehrfachsuche sehr nützlich. Dazu:
;    ;; Wenn schon mal gesucht wurde und der Cursor im gesuchten Buffer 
;    ;; nicht bewegt wurde: nicht zum Anfang zurück, sondern weitersuchen
;    (if (= am-i-at-point (point))
;	(forward-char 1)
;      (goto-char (point-min)))
;    ;; suche nach regex, aber verbiete Suche nach ->...regex (damit Knöpfe sich nicht selbst finden)
;    ;;
;    ;; ja ja ich weiß, der nächste Teil könnte sehr viel schöner geschrieben werden (wie so manches hier),
;    ;; ich habe ihn aber so gelassen, da er sonst für meinen Geschmack unlesbar dicht geworden wäre.
;    ;;
;    (let ((tmp 0))
;      (progn
;	(message "searching...")
;	(while
;	    ;; wenn er das Muster "pattern" findet ohne einen Pfeil "->" vornedran
;	    (if (or
;		 (re-search-forward (concat "^" pattern) nil t)
;		 (and
;		  no-headline-search
;		  (re-search-forward (concat "\\([ \t\r]\\|^\\)\\([^ \t\r]*\\)" pattern) nil t)
;		  (not (string-equal "->" (buffer-substring-no-properties (match-beginning 2) (+ 2 (match-beginning 2)))))))
;		(progn
;		  (message " ")
;		  ;; dann dieses hier
;		  (recenter)
;		  (isearch-highlight (match-beginning 0)(match-end 0))
;		  (re-search-backward originalpattern nil t)
;		  ;; mit -> am Ende des Strings: wechsle das Fenster
;		  (setq am-i-at-point (point))
;		  (if (not (string-match "-\>$" regex))
;		      (other-window -1)
;		    (if (not (string-match "-$" regex))
;			(progn
;			  (other-window -1)
;			  (delete-window))))
;		  nil)
;	      ;; wenn er es nicht findet, sehe nach, ob er an diesem Punkt schon mal war.
;	      (if (not (= tmp (point)))
;		  ;; wenn nein, suche weiter
;		  (setq tmp (point))
;		;; sonst: Abbruch
;		(message "not found")
;		(message "no-pattern-stop")
;		(delete-window)
;		(recenter)
;		(isearch-dehighlight t)
;		nil)))))))


(defun FUNC:func (file startpunkt)
  (let ((pointsv (point)) (current-buf (current-buffer)))
;    (setq startpunkt (regexp-quote startpunkt))
    (if file
	(if (get-buffer (file-name-nondirectory file))
	    (set-buffer (file-name-nondirectory file))
	  (if (file-exists-p file)
	      (find-file file)
	    (if (file-exists-p (concat file ".crypt"))
		(find-file (concat file ".crypt")))))
      (if (file-exists-p ".op-navigate")
	  (find-file ".op-navigate")
	(error "no file \".op-navigate\" in current directory found")))
    (goto-char (point-min))
    ;; wenn Startpunkt im Buffer angegeben ist,
    (if startpunkt
	(progn
	  (re-search-forward startpunkt nil t)
	  ;; dann eval ab dort -
	  (setq pointsv (point))
	  ;; bis zum "END"-statement
	  (if (re-search-forward "END" nil t)
	      (backward-char (length "END"))
	    ;; oder bis zum Ende des Files
	    (goto-char (point-max))))
      (goto-char (point-max)))
    ;; ansonsten: sowieso bis zum Ende
    (narrow-to-region pointsv (point))
    (eval-current-buffer)
    (widen)
    (switch-to-buffer current-buf)))
;;
;; Beispiele
;;  
;   ->FUNC:funcfile*startpunkt
;   ->FUNC:funcfile*Fakultät
;   ->FUNC:startpunkt
;



(defun EVAL:func (program file)
  ;; wenn programm: führe dieses aus
  ;; ACHTUNG: Kommandozeilenargumente werden nur genommen, wenn ein Prozentzeichen auftaucht !
  (if program
      (progn
	;; schaue nach, ob da was in Anführungsstrichen herumliegt (wie "Programm")
	(if (string-match "^\".*\"$" program)
	    ;; ersten und letzen Character wegstreichen (sollten die Anführungsstriche sein)
	    (setq program (substring program 1 -1)))
	(shell-command (concat program " " file)))
    ;; wenn nicht: Suche nach Extention
    (if (string-match "^\".*\"$" file)
	;; ersten und letzen Character wegstreichen (sollten die Anführungsstriche sein)
	(setq file (substring file 1 -1)))
    (if (string-match ".tki" file)
	(shell-command (concat "tkined" " " file))
      ;; weitere Suche nach Extention...
      (if (string-match ".pl" file)
	  (shell-command (concat "perl" " " file))
	(if (string-match ".fig" file)
	    (shell-command (concat "xfig" " " file))
	  (if (string-match ".py" file)
	      (shell-command (concat "python" " " file))
	    ;; wenn bisher noch keine Extention gematcht hat, führe den File direkt aus
	    (shell-command file))))))
  ;; im Falle eines Batch-Betriebes: lösche das Fenster, daß die asynchone Ausgabe aufnimmt
  (if (string-match "&" file)
      (progn
	(other-window 1)
	(switch-to-buffer "*Asynch Shell Command*")
	(delete-window)
	(message (concat "Output stream of " file " goes to buffer \"*Asynch Shell Command*\"")))))
;;

;; Beispiele
;;  
;   ->EVAL:xeyes
;   ->EVAL:xeyes&
;   ->EVAL:"date --version"  
;   ->EVAL:xfig*test.fig
;   ->EVAL:"xfig -bg red"*test.fig    ; Argumente in Anführungsstrichen, Ausführungspunkt NUR vor dem 
;                                     ; ersten Blank (beim "->EVAL:xxxx"
;   ->EVAL:"xfig -bg red"*test.fig&   ; wie eben
;   ->FILE:op-hyper.el*defun;EVAL:"date --version"  
;				      ; bei Kettenargumenten: EVAL immer an den Schluß,
;                                     ;  andere Funktionen immer mit *-Zeichen. Nur je ein Buttontyp ist erlaubt.
;   ->EVAL:test.fig                   ; Defaultextention ist einprogrammiert (siehe oben)
;  



(defvar am-i-at-point-tmp 0)
(make-local-variable 'am-i-at-point-tmp)
(defun REF:func (file button-match)
  ;; Verhalten: Beim ersten Betätigen: Suche erstes Button-Pattern. Ist schon ein Fenster offen: 
  ;; siehe nach, ob sich der Cursor bewegt hat. Wenn nicht: Suche nächsen Verweis.
  (let ((pattern button-match) (current-buf (current-buffer)) (was-one-window nil))
;    (setq button-match (regexp-quote button-match))
    (if (one-window-p)
	(progn
	  (split-window)
	  (setq was-one-window t)))
    (other-window 1)
    (if file
	(if (get-buffer (file-name-nondirectory file))
	    (progn
	      (switch-to-buffer (file-name-nondirectory file))
	      (if was-one-window
		  (setq am-i-at-point-tmp 0)))
	  (if (file-exists-p file)
	      (find-file file)
	    (if (file-exists-p (concat file ".crypt"))
		(find-file (concat file ".crypt"))))
	  (setq am-i-at-point-tmp 0))
      (switch-to-buffer current-buf))
    ;; ist ein Pfeil am Ende des Ausdrucks ? "pattern" ist "button-match" ohne "->" am Ende.
    (if (string-match "-\>$" button-match)
	(setq pattern (substring button-match 0 -2)))
    (if (string-match "-$" button-match)
	(setq pattern (substring button-match 0 -1)))
      ;; Wenn kein Verweis auf ein anderes Fenster stattfindet ("->"), dann
      ;; ist u.U. eine Mehrfachsuche sehr nützlich. Dazu:
      ;; Wenn schon mal gesucht wurde und der Cursor im gesuchten Buffer 
      ;; nicht bewegt wurde: nicht zum Anfang zurück, sondern weitersuchen
    (if (= am-i-at-point-tmp (point))
	(forward-char 1)
      (goto-char (point-min)))
    ;; suche nach button-match, aber verbiete Suche nach ->...button-match (damit Knöpfe sich nicht selbst finden)
    (if (re-search-forward (concat "-\>" pattern) nil t)
	(progn
	  (recenter)
	  (isearch-highlight (match-beginning 0)(match-end 0))
	  (forward-char 1)
	  ;; mit -> am Ende des Strings: wechsle das Fenster
	  (setq am-i-at-point-tmp (point))
	  (if (not (string-match "-\>$" button-match))
	      (other-window -1)
	    (if (not (string-match "-$" button-match))
		(progn
		  (other-window -1)
		  (delete-window)))))
      (message "no-pattern-stop")
      (delete-window)
      (isearch-dehighlight t)
      (recenter))))
;;
;; Beispiele
;;     ->defun  defun
;   ->REF:../op-hyper/op-hyper.el*1button      ; Muster ist hier "1button", kann aber beliebig sein
;   ->REF:../op-hyper/op-hyper.el*1button->
;   ->REF:1button
;   ->REF:1button->
;
; die alle sollten hier enden:   ->1button:FILE:defun   ; Doppelpunkt ist günstig, aber nicht Pflicht:
;                                ->1button#FILE:defun   ; geht z. B. auch
;


(defvar am-i-at-point-tmp-R 0)
(make-local-variable 'am-i-at-point-tmp-R)
(defun REF-R:func (file button-match)
  ;; Verhalten: Beim ersten Betätigen: Suche erstes Button-Pattern. Ist schon ein Fenster offen: 
  ;; siehe nach, ob sich der Cursor bewegt hat. Wenn nicht: Suche nächsen Verweis.
  (let ((pattern button-match) (current-buf (current-buffer)) (was-one-window nil))
;    (setq button-match (regexp-quote button-match))
    (if (one-window-p)
	(progn
	  (split-window)
	  (setq was-one-window t)))
    (other-window 1)
    (if file
	(if (get-buffer (file-name-nondirectory file))
	    (progn
	      (switch-to-buffer (file-name-nondirectory file))
	      (if was-one-window
		  (setq am-i-at-point-tmp-R 0)))
	  (if (file-exists-p file)
	      (find-file file)
	    (if (file-exists-p (concat file ".crypt"))
		(find-file (concat file ".crypt"))))
	  (setq am-i-at-point-tmp-R 0))
      (switch-to-buffer current-buf))
    ;; ist ein Pfeil am Ende des Ausdrucks ? "pattern" ist "button-match" ohne "->" am Ende.
    (if (string-match "-\>$" button-match)
	(setq pattern (substring button-match 0 -2)))
      ;; Wenn kein Verweis auf ein anderes Fenster stattfindet ("->"), dann
      ;; ist u.U. eine Mehrfachsuche sehr nützlich. Dazu:
      ;; Wenn schon mal gesucht wurde und der Cursor im gesuchten Buffer 
      ;; nicht bewegt wurde: nicht zum Anfang zurück, sondern weitersuchen
    (if (= am-i-at-point-tmp-R (point))
	(forward-char 1)
      (goto-char (point-min)))
    ;; suche nach button-match, aber verbiete Suche nach ->...button-match (damit Knöpfe sich nicht selbst finden)
    (if (re-search-forward (concat "-\>" pattern) nil t)
	(progn
	  (recenter)
	  (isearch-highlight (match-beginning 0)(match-end 0))
	  (forward-char 1)
	  ;; mit -> am Ende des Strings: wechsle das Fenster
	  (setq am-i-at-point-tmp-R (point))
	  (if (not (string-match "-\>$" button-match))
	      (other-window -1)
	    (select-hyper-func t)))
      (message "no-pattern-stop")
      (delete-window)
      (isearch-dehighlight t)
      (recenter))))
;;
;; Beispiele (siehe auch die bei REF:func)
;;
;   ->REF-R:../op-hyper/op-hyper.el*2button->      ; Muster ist hier "2button", kann aber beliebig sein
;
;   ->REF-R:2button->
;
;   ->2button:REF-R:3button->
;
; die alle sollten hier enden:   ->3button:FILE:das_hier_gibts_nicht   
;
;
;; weiteres Beispiel: Loop vom ersten Button wird nicht unendlich durchlaufen 
;; (Ideal zum Einlesen vieler Files !!!)
;
;   ->4button:REF-R:5button->           
;        ->5button:REF-R:6button->
;             ->6button:REF-R:4button->
;
;
;; nicht gefunden wird dagegen die folgende Loop (gibt dann aber einen Emacs-Fehler)
;; (zum Debuggen empfiehlt sich REF-R:func in REF:func umzudefinieren) 
;   ->4button:REF-R:5button->           
;        ->5button:REF-R:6button->
;             ->6button:REF-R:5button->
;
;



;;;
;;; Wie der Name schon sagt: Dieses hier führt den Button aus, wenn ein Mausklick kommt.
;;;
(defun Activate-button-with-mouse (click)
  (interactive "e")
  (let* ((start (event-start click))
	 (window (car start))
	 (pos (car (cdr start))))
    (select-window window)
    (goto-char pos))
  (select-hyper-func))





;(global-set-key "\M-\C-m" 'select-hyper-func)
(define-key (current-local-map) "\M-\C-m" 'select-hyper-func)
(if xemacsp
    (define-key (current-local-map) [(shift button2)] 'Activate-button-with-mouse)
  (define-key (current-local-map) [\S-mouse-2] 'Activate-button-with-mouse))





