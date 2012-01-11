; (setq debug-on-error t)
;;;
;;;
;;;   Dieser File ist ein Teil der OP-Suite zum Organisieren großer und sehr großer Projekte
;;;   mit Hilfe des emacs und des outline-Modus (daher der Name: Outline-Project). 
;;;   Dieser File kann aber auch alleine Verwendet werden als Kalender unter verschiedenen 
;;;   Betriebssystemen, getestet unter Unix, NT, VM/CMS, MVS und VMS. Einzige Bedingung ist
;;;   ein gemeinsam verfügbares Directory.
;;;
;;;   Zeile zum Starten des Kalenders:
;;;   emacs -l ~/op-project/op-calendar/op-calendar.el -f calendar-in-extra-window -iconic &
;;;
;;;   Feiertage etc. werden nicht extra berechnet, sondern mit Hilfe eines gcal-Interfaces 
;;;   eingelesen. Dieses gcal-Interface ist im Moment nur Offline verfügbar, d.h. es muss
;;;   ein File op-calendar-holidays.el[c] erzeugt werden. Dies geschieht mit Hilfe der Funktion 
;;;   insert-gcal-holidays-in-calendar hier unten im File. Der dort einzulesende File musste 
;;;   vorher durch gcal erzeugt worden sein:
;;;   LANG=de gcal -u -n -q de -H no --date-format="%w,  %1%D%2 %M %y" --cc-holidays=de+fr+us+nl+gb 2012+1960 >tmp/gcal-output
;;;   Nach diesem File wird dann von der Funktion insert-gcal-holidays-in-calendar gefragt
;;;
;;;   Eigenschaften des OP-Kalenders:
;;;   - Vollwertiger Kalender
;;;   - persönlicher Terminplaner kombiniert mit Gruppenterminplaner
;;;   - Feiertage verschiedener Nationen werden angezeigt, aber nicht als Feiertag eingetragen
;;;   - Nationale Feiertage werden in den vier gcal-Abstufungen wiedergegeben (in den meisten 
;;;     Bundesländern, in einigen Bundesländern,...)
;;;   - Tage wie der 1. April oder Muttertag werden unter Merktagen wiedergegeben, jedoch außer auf
;;;     Wunsch nicht speziell gekennzeichnet
;;;   - halbwegs sinnvolle Optik unter allen oben aufgeführten Betriebssystemen
;;;   - voll der deutschen Sprache mächtig
;;;   - Schnellfunktion (c) im gleichen Buffer oder in einem extra Fenster (calendar-in-extra-window)
;;;
;;;   Der File op-calendar.el ist der File op-calendar-changes.el mit includetem hilit19.el
;;;
;;;
;;;   Geschrieben um Weihnachten 1997 von Dr. Stefan Walther, Rudolf-Diesel-Str. 3, 55131 Mainz
;;;
;;;
;;;
;;; command emacs -l ~/op-project/op-calendar/op-calendar -f calendar-in-extra-window -iconic &

(require 'ps-print)

;(setq diary-file "~/op-project/op-diary")
;(setq group-diary-file "~/op-project/op-group-diary")
;(setq number-of-diary-entries 8 )   ; Für wieviele Tage soll das Tagebuch ausgerechnet werden ?


(if (boundp 'op-project-calendar-loaded)  ; wenn der Kalender schon einmal geladen wurde, dann nichts
    nil


(if (not (boundp 'diary-fill-column))
    (setq diary-fill-column 82)) ;; Fill-column in den Diary-Files



(defun set-load-path ()
  "Erweitere den Pfad der Kommando-Optionen um den Platz, an dem op-calendar steht.
Dient dazu, dass der File op-calendar-holidaylist einfach im selben Directory stehen kann,
aber nicht beim Benutzer im Load-path eingetragen sein muss."
  (let ((command-line-args command-line-args))
    (while (and command-line-args
		(not (posix-string-match "[/\\]op-calendar.el" (car command-line-args))))
      (setq command-line-args (cdr command-line-args)))
    (if command-line-args
	(setq load-path
	      (if (string= (substring (expand-file-name (file-name-directory (car command-line-args))) -1) "/")
		  (cons (substring (expand-file-name (file-name-directory (car command-line-args))) 0 -1)
			load-path)
		(cons (expand-file-name (file-name-directory (car command-line-args)))
		      load-path))))))

;; aktiviere den momentanen Ladepfad
(set-load-path)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun insert-weekday-diary-entry (&optional args)
  "Setzt diary-weekday in den Kalender ein"
  (interactive)
  (let ((dayofweek)(weeknumber))
    (setq dayofweek (completing-read "Welcher Wochentag: " '(("Montag" 1)
							     ("Dienstag" 2)
							     ("Mittwoch" 3)
							     ("Donnerstag" 4)
							     ("Freitag" 5)
							     ("Samstag" 6)
							     ("Sonntag" 7))
				     nil t))
    (setq weeknumber (completing-read "in jeder wievielten Woche im Monat: " '(("1" 1)
									       ("2" 2)
									       ("3" 3)
									       ("4" 4)
									       ("5" 5)) 
				      nil t))
    (make-diary-entry
     (format "%s(diary-weekday   %s \"%s\")"
             sexp-diary-entry-symbol
	     weeknumber
	     dayofweek) args)))



(defun diary-weekday (weeknumber dayofweek)
"Eintrag, wenn DAYOFWEEK in WEEKNUMBER übereinstimmt,
z.B. jeder 2. Donnerstag im Monat."
  (let ((dayname (calendar-day-of-week date))
	(day (extract-calendar-day date))
	(weeklist nil))
    (cond ((= weeknumber 1)
	   (setq weeklist '(1 2 3 4 5 6 7)))
	  ((= weeknumber 2)
	   (setq weeklist '(8 9 10 11 12 13 14)))
	  ((= weeknumber 3)
	   (setq weeklist '(15 16 17 18 19 20 21)))
	  ((= weeknumber 4)
	   (setq weeklist '(22 23 24 25 26 27 28)))
	  ((= weeknumber 5)
	   (setq weeklist '(29 30 31))))
    (setq dayofweek (downcase dayofweek))
    (cond ((string= dayofweek "montag")
	   (setq dayofweek 1))
	  ((string= dayofweek "dienstag")
	   (setq dayofweek 2))
	  ((string= dayofweek "mittwoch")
	   (setq dayofweek 3))
	  ((string= dayofweek "donnerstag")
	   (setq dayofweek 4))
	  ((string= dayofweek "freitag")
	   (setq dayofweek 5))
	  ((string= dayofweek "samstag")
	   (setq dayofweek 6))
	  ((string= dayofweek "sonntag")
	   (setq dayofweek 7)))
    (if (and (memq day weeklist) 
	     (= dayname dayofweek))
	entry)))

(message "Kalender geladen bis Marke 0/1")

(setq european-calendar-display-form
  '((if dayname (concat dayname ", ")) day "." monthname " " year))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(if (not (boundp 'signature))
    (setq signature ""))


(setq general-holidays nil)
(setq local-holidays nil)
(setq other-holidays nil)
(setq christian-holidays nil) 
(setq hebrew-holidays nil)
;(setq islamic-holidays nil)
(setq oriental-holidays nil)
;(setq solar-holidays nil)

(setq islamic-holidays
  '((holiday-islamic
     1 1
     (format "[Islamisches Neujahrsfest %d]]"
             (let ((m displayed-month)
                   (y displayed-year))
               (increment-calendar-month m y 1)
               (extract-calendar-year
                (calendar-islamic-from-absolute
                 (calendar-absolute-from-gregorian
                  (list
                   m (calendar-last-day-of-month m y) y)))))))
    (holiday-islamic 9 1 "[[Beginn der muslimischen Fastenzeit Ramadan [Merktag]]]")
    (holiday-islamic 9 30 "[[Ende der muslimischen Fastenzeit Ramadan [Merktag]]]")))

(defvar solar-n-hemi-seasons
  '("Frühjahrs- Tag- und Nachtgleiche" "Sommersonnenwende" "Herbst-Tag- und Nachtgleiche" "Wintersonnenwende")
  "List of season changes for the northern hemisphere.")

(defvar solar-s-hemi-seasons
  '("Herbst-Tag- und Nachtgleiche" "Wintersonnenwende" "Frühjahrs- Tag- und Nachtgleiche" "Sommersonnenwende")
  "List of season changes for the southern hemisphere.")

(require 'solar)
(defun solar-sunrise-sunset-string (date)
  "String of *local* times of sunrise, sunset, and daylight on Gregorian DATE."
  (let ((l (solar-sunrise-sunset date)))
    (format
     "%s, %s at %s (%s Stunden Tageslicht)"
     (if (car l)
         (concat "Sonnenaufgang " (apply 'solar-time-string (car l)))
       "No sunrise")
     (if (car (cdr l))
         (concat "Sonnenuntergang " (apply 'solar-time-string (car (cdr l))))
       "Kein Sonnenuntergang")
     (eval calendar-location-name)
     (car (cdr (cdr l))))))

(require 'lunar)
(defun lunar-phase-name (phase)
  "Name of lunar PHASE.
0 = new moon, 1 = first quarter, 2 = full moon, 3 = last quarter."
  (cond ((= 0 phase) "Neumond")
        ((= 1 phase) "Zunehmender Halbmond")
        ((= 2 phase) "Vollmond")
        ((= 3 phase) "Abnehmender Halbmond")))



(message "Kalender geladen bis Marke 1a")



;; Lade das Diary, wenn es sich um den ersten Neuaufruf des OP-Modes handelt
;; und checke die Diary-files auf Syntaxfehler
(if (not (boundp 'op-diary-already-loaded))
    (progn
      (let ((current-buf (current-buffer)))
	;; if there is an error in the diary file
	(condition-case nil
	    (diary)
	  (error "Fehler im persönlichen Diary-File %s oder im Gruppendiaryfile %s" diary-file 
		 (if (boundp 'group-diary-file) group-diary-file "[wird nicht benutzt]")))
	(setq op-diary-already-loaded t)
	(switch-to-buffer current-buf))))



(setq mark-calendar-holidays-initially t)

(message "Kalender geladen bis Marke 1b")



;;; ========================================================================
;;; ========================================================================

(setq emacs-mainframe (selected-frame))

(defconst calendar-buffer "*Kalender*"
  "Name of the buffer used for the calendar.")

(defconst holiday-buffer "*Feier- und Merktage*"
  "Name of the buffer used for the displaying the holidays.")

(defconst fancy-diary-buffer "*Termine*"
  "Name of the buffer used for the optional fancy display of the diary.")

(defconst lunar-phases-buffer "*Mondphasen*"
  "Name of the buffer used for the lunar phases.")


(defvar calendar-setup nil
  "The frame set up of the calendar.
The choices are `one-frame' (calendar and diary together in one separate,
dedicated frame) or `two-frames' (calendar and diary in separate, dedicated
frames); with nil the current frame is used. Other value may lead to
strange results.")
(make-variable-buffer-local 'calendar-setup)


(message "Kalender geladen bis Marke 2")


(if window-system
    (progn
      (if xemacsp
	  (require 'overlay))
      (require 'hilit19.variation)
      (hilit-set-mode-patterns
       'calendar-mode
       '(("[A-Z][a-z]+ [0-9]+" nil define)	; Monat und Jahr
	 ("Mo Di Mi Do Fr Sa So" nil struct)	; Wochentage, geht davon aus, dass der Sonntag der letzte Wochentag ist
	 ))))

(if gnuemacsp
    (progn
      (require 'calendar)
      (require 'diary-lib)
      (require 'holidays)
      (require 'appt)
      ))
(if xemacsp
    (progn
      (require 'calendar)
      (require 'diary-lib)
      (load "holidays")
      (load "appt")
      ))


(message "Kalender geladen bis Marke 3")


(setq mark-sundays t)


(if (not (fboundp 'mark-all-calendar-holidays-original))
    (fset 'mark-all-calendar-holidays-original (symbol-function 'mark-calendar-holidays)))

(defun mark-calendar-holidays ()
  "Wie calendar-holidays, aber ohne die \"merktage\"-, sondern 
nur mit der \"gcal\"-feiertage\"-Liste."
  (interactive)
  (let ((calendar-holidays nil))
    (setq calendar-holidays gcal-feiertage)
    (mark-all-calendar-holidays-original)
    ))


(defun mark-all-calendar-holidays ()
  "Wie calendar-holidays, aber mit der \"merktage\"-
und der \"gcal\"-feiertage\"-Liste."
  (interactive)
  (let ((calendar-holidays nil))
    (setq calendar-holidays
	  (append general-holidays local-holidays other-holidays
		  christian-holidays hebrew-holidays islamic-holidays
		  oriental-holidays solar-holidays
		  gcal-feiertage merktage
		  ))
    (mark-all-calendar-holidays-original)
    ))


(define-key calendar-mode-map "?" 'calendar-kurzinfo)
(defun calendar-kurzinfo ()
  "Go to the info node for the calendar."
  (interactive)
  (if calendar-setup
      (progn
	(make-frame-visible emacs-mainframe)
	(select-frame emacs-mainframe)))
  (switch-to-buffer "*Kalender-Kurzanleitung*")
  (if (< emacs-major-version 20)
      (standard-display-european t))
  (insert 
   "Kurzanleitung für den Kalender-Mode:

Es gibt generell drei Stufen von Kalendern, die miteinander
zusammenspielen:
- Persönlicher Terminkalender (nicht einsehbar von anderen)
- Persönlicher einsehbarer Kalender (damit einem jemand anderes einen
  Termin eintragen kann)
- Allgemeiner Gruppenkalender (für Termine, die für alle interessant
  sind)
Mithilfe von #include-Statements (auch rekursiv) im Kalenderfile sind 
alle Kombinationen zwischen verschiedenen Zugriffstufen erlaubt.

Alle hier genannten Funktionen sind auch über die Menüs ansprechbar.

Im Kalender-Fenster:

   Fenster aus dem Weg schaffen

       Strg-x Strg-y ==> Fenster iconifizieren
       Strg-x k ==> Fensterbuffer rauswerfen (z.B. bei der
                    Feiertagsliste)
       q ==> löscht beim ersten Mal das Terminkalenderfenster, dann
             iconifiziert es den Kalender
       Strg-x Strg-c vernichtet das Gesamtprogramm

   Bewegen:

       Cursortasten ==> im Kalenderfenster anderen Tag auswählen
       f ==> einen Monat vorwärts
       b ==> einen Monat rückwärts
       .  ==> zurück zum heutigen Datum
       Strg-v ==> 3 Monate nach vorne
       Alt-v ==> 3 Monate nach hinten
       Strg-l ==> Kalender zentrieren und neu einfärben
       Strg-o ==> Wechseln zum Terminkalenderfenster
       Alt-d ==> Terminkalenderfenster nach unten scrollen
       Alt-u ==> Terminkalenderfenster nach oben scrollen

   Informationen:

       n ==> Info über den heutigen Tag und die Kalenderwoche
       Menü -> News: Aktuelle Informationen

   Terminkalender ansehen:

       d ==> Zeigt Terminkalender des bezeichneten Tages
       Strg-u [Anzahl] d zeigt soviele Tage ab dem gekennzeichneten
             Tag
       q ==> Fenster verlassen
       D ==> Terminkalnder von jemand anderem ansehen

   Termine eintragen:

       Termine für einen Tag eintragen
	   i d ==> Termin in den persönlichen Terminkalender eintragen
	   i g d ==> Termin in den Gruppenterminkalender eintragen
	   i o d ==> Termin in einen anderen Terminkalenderfile
                     eintragen
       Termine für einen Zeitraum eintragen
           - auf den Anfangstag gehen
           - Strg-Freizeichentaste drücken
           - zum letzten Tag gehen
           dann (b steht für Block):
           i b ==> Zeitraum im persönlichen Terminkalender
           i g b ==> Zeitraum im Gruppenterminkalender
           i o b ==> Zeitraum in einem fremden Terminkalender
       Regelmäßige Termine eintragen:
           i w ==> wöchentlich
           i m ==> monatlich
           i y ==> jährlich
           i c ==> zyklisch wiederkehrende Tage
           i g w, i o w usw entsprechend für Gruppenterminkalender und
               Fremdkalender

Im Tagebuch-File:

   Alt-x cde ==> sucht im persönlichen Kalender sowie im
           Gruppenkalender nach dem aktuelle Eintrag (nicht sehr gut
           programmiert...)

")
  (setq buffer-read-only t)
  (goto-char (point-min))
  ;;
  (setq my-mode-map (cons 'keymap text-mode-map))
  (local-set-key "q" '(lambda ()(interactive)(kill-buffer (current-buffer))(iconify-frame)))
  (message "q verläßt den Buffer"))


(message "Kalender geladen bis Marke 3a")


;; SMW spezial
(defvar diary-noentry-string "bisher kein Termin"
  "If \"\", no diary will be popped up if no diary entry exists.
If not empty string, this string will be shown in empty diary entries")


(defun calendar-week-number (date)
  "Gives you the number of the week of DATE in the current year"
  (let* ((mark-diary-entries-in-calendar mark-diary-entries-in-calendar)
	 (mark-diary-entries-in-calendar nil))
    (calendar-goto-date (reverse (cons  (extract-calendar-year date) '(1 1))))
    (calendar-end-of-week 1)
     ;; in calendar-end-of-week ist ein Bug: daher
      (if (> (calendar-day-number (calendar-cursor-to-date t)) 7)
  	(calendar-backward-week 1))
      ;; Kalenderwoche 1 ist immer die erste vollständige Woche im neuen Jahr!!
    (let ((offset (calendar-day-number (calendar-cursor-to-date t))))
      (if (>= offset 7) (setq offset (- offset 7)))
      (calendar-goto-date date)
      (ceiling (/ (- (calendar-day-number date) offset) 7.0)))))


(defun calendar-day-message ()
  (interactive)
  (let* ((day-number (calendar-day-number (calendar-cursor-to-date t)))
	 (datestring (calendar-date-string (calendar-cursor-to-date t)))
	 (calendarweek (calendar-week-number (calendar-cursor-to-date t)))
	 (mark-diary-entries-in-calendar mark-diary-entries-in-calendar)
	 (mark-diary-entries-in-calendar nil))
    (if window-system
	(progn
	  (hilit-repaint-command t)
	  (if mark-sundays (mark-calendar-days-named 7)))) ;; alle Sonntage rot einfärben
    (if (< day-number 20)
	(message ">> %s, der %dte Tag des Jahres in Kalenderwoche %d <<"
		 datestring day-number calendarweek)
      (message ">> %s, der %dste Tag des Jahres in Kalenderwoche %d <<"
	       datestring day-number calendarweek)
    )))

(message "Kalender geladen bis Marke 3b")


(defun calendar-day-message-dummy ()
  (calendar-day-message))



(defun holiday-extract-from-list (kallist)
  "Returns kallist if visible, returns nil 
  if it is not visible in the current calendar window."
  (let ((m displayed-month)
        (y displayed-year)
	month)
    (setq month (car (car kallist)))
    (setq year (nth 2 (car kallist)))
    ;; 
    (if (> 2 (abs (- (+ (* 12 y) m) (+ (* 12 year) month))))
	(progn
	  (increment-calendar-month m y (- 11 month))
	  (if (> m 9) (list kallist) nil)))))

(message "Kalender geladen bis Marke 3c")

;;; Marke: XEmacs
(if (not (get-buffer calendar-buffer))
    (progn
      (let ((current-buf (current-buffer)))

	(load "op-calendar/op-calendar-holidaylist")

	(setq calendar-setup nil)

	(setq calendar-day-name-array
	      ["Sonntag" "Montag" "Dienstag" "Mittwoch" "Donnerstag" "Freitag" "Samstag"])
	
	(if (or (< emacs-major-version 20) xemacsp)
	    (setq calendar-month-name-array
		  ["Januar" "Februar" "März" "April" "Mai" "Juni" "Juli" "August" "September" "Oktober" "November" "Dezember"])
	  (setq calendar-month-name-array
		["Januar" "Februar" "März" "April" "Mai" "Juni" "Juli" "August" "September" "Oktober" "November" "Dezember"]))
	(setq calendar-week-start-day 1)
	(setq number-of-diary-entries 8)	   ; für wieviele Tage soll das Tagebuch ausgerechnet werden ?
 

	;; if XEmacs
	(if xemacsp
	    (progn
	      (setq time-stamp-month-numbers
		'(("Jan" . 1) ("Feb" . 2) ("Mär" . 3) ("Apr" . 4) ("Mai" . 5) ("Jun" . 6)
		  ("Jul" . 7) ("Aug" . 8) ("Sep" . 9) ("Okt" . 10) ("Nov" . 11) ("Dez" . 12)))
	      
	      (setq time-stamp-month-full-names
		["(zero)" "Januar" "Februar" "März" "April" "Mai" "Juni"
		 "Juli" "August" "September" "Oktober" "November" "Dezember"])
	      
	      (setq time-stamp-weekday-numbers
		'(("Son" . 0) ("Mon" . 1) ("Die" . 2) ("Mit" . 3)
		  ("Don" . 4) ("Fre" . 5) ("Sam" . 6)))

	      (setq time-stamp-weekday-full-names
		["Sonntag" "Montag" "Dienstag" "Mittwoch" "Donnerstag" "Freitag" "Samstag"])
	      
	      (setq time-stamp-format "%02d.%02m.%02y // %02H:%02M:%02S %u")

	      (defvar timezone-months-assoc
		'(("JAN" .  1)("FEB" .  2)("MÄR" .  3)
		  ("APR" .  4)("MAI" .  5)("JUN" .  6)
		  ("JUL" .  7)("AUG" .  8)("SEP" .  9)
		  ("OKT" . 10)("NOV" . 11)("DEZ" . 12))
		"Alist of first three letters of a month and its numerical representation.")

	      ))







	(calendar)
	(setq calendar-time-display-form '(24-hours ":" minutes (if time-zone " (") time-zone (if time-zone ")")))
	(setq calendar-date-display-form	'(day "." month "." year))
	(european-calendar)
	(if mark-calendar-holidays-initially
	    (mark-calendar-holidays))

	(calendar-day-message)

;	(modify-syntax-entry ?ä "w")
;	(modify-syntax-entry ?Ä "w")
;	(modify-syntax-entry ?ö "w")
;	(modify-syntax-entry ?Ö "w")
;	(modify-syntax-entry ?ü "w")
;	(modify-syntax-entry ?Ü "w")
;	(modify-syntax-entry ?ß "w")

	(defun actual-week-numberf (date)
	  "Gives you the number of the week of DATE in the current year"
	  (calendar-goto-date (reverse (cons  (extract-calendar-year date) '(1 1))))
	  (calendar-end-of-week 1)
	  ;; in calendar-end-of-week ist ein Bug: daher
	  (if (> (calendar-day-number (calendar-cursor-to-date t)) 7)
	      (calendar-backward-week 1))
	  (let ((offset (- 7 (calendar-day-number (calendar-cursor-to-date t)))))
	    (calendar-goto-date date)
	    (ceiling (/ (+ (calendar-day-number date) offset) 7.0))))
	
	(setq actual-week-number (actual-week-numberf (calendar-current-date)))

	(if window-system
	    (progn
	      (hilit-repaint-command t)
	      (if mark-sundays (mark-calendar-days-named 7)))) ;; alle Sonntage rot einfärben
	(delete-window)
	(setq diary-list-include-blanks t)

	(add-hook 'diary-display-hook 'fancy-diary-display)
	(add-hook 'diary-display-hook 'appt-make-list)

	;; the next two statements are enabeling the #include "filename" statement in the diary file
	(add-hook 'list-diary-entries-hook 'include-other-diary-files)
	(add-hook 'mark-diary-entries-hook 'mark-included-diary-files)
	(add-hook 'list-diary-entries-hook 'sort-diary-entries t)
	
	(setq today-visible-calendar-hook 'calendar-mark-today)
	
	(setq appt-message-warning-time 25)  ; Wieviele Minuten vorher will ich vor einem Termin gewarnt werden ?
	(setq appt-display-duration 8)	   ; Wieviele Sekunden soll die Display-Zeile erinnern ?

	(switch-to-buffer current-buf))))


(message "Kalender geladen bis Marke 4")


(defvar original-window-config nil
  "Current windown configuration before starting the calendar with \"c\"")
(defun c (&optional ask-for-date read-window-config)
  "calls calendar and re-hilits it"
  (interactive)
  (if (not read-window-config)
      (setq original-window-config (current-window-configuration)))
  (if (frame-live-p calendar-frame) (delete-frame calendar-frame))
  (if (< emacs-major-version 20)
      (standard-display-european t))
  (setq display-time-24hr-format t)
  (setq display-time-day-and-date t)
  (setq calendar-setup nil)
  (split-window-vertically)
  (calendar-basic-noframe-setup ask-for-date)
  (switch-to-buffer calendar-buffer)
  (if (> (window-height) 9) (shrink-window (- (window-height) 9)))
  (setq pop-up-windows t)
  (setq pop-up-frame-function 'switch-to-buffer)
  (setq special-display-function 'switch-to-buffer)
  (let ((pop-up-windows nil)
	(view-diary-entries-initially nil))
    ;;
    ;; hier werden die Feiertage geladen !!!
    (require 'op-calendar-holidaylist)
    ;;
    ;(calendar-recenter)
    (if mark-calendar-holidays-initially
	(mark-calendar-holidays))
    (unless mark-diary-entries-in-calendar
      (calendar-day-message))
    (define-key calendar-mode-map "d" 'view-diary-entries-2)
    (define-key calendar-mode-map "D" 'view-other-diary-entries-2)
    ;; Umdefinition der Funktion make-diary-entry
))


  

(defun calendar-basic-noframe-setup (&optional arg)
  (interactive "P")
  (set-buffer (get-buffer-create calendar-buffer))
  (calendar-mode)
  (let* ((date (if arg
                   (calendar-read-date t)
                 (calendar-current-date)))
         (month (extract-calendar-month date))
         (year (extract-calendar-year date)))
    (increment-calendar-month month year (- calendar-offset))
    (generate-calendar-window month year)
    (if (and view-diary-entries-initially (calendar-date-is-visible-p date))
        (view-diary-entries
         (if (vectorp number-of-diary-entries)
             (aref number-of-diary-entries (calendar-day-of-week date))
           number-of-diary-entries))))
  (let* ((diary-buffer (get-file-buffer diary-file)))
    (if view-calendar-holidays-initially
	(list-calendar-holidays)))
  (run-hooks 'initial-calendar-window-hook))


(defun calendar-forward-month-center ()
  (interactive)
  (if mark-diary-entries-in-calendar
      (progn
	(setq mark-diary-entries-in-calendar nil)
	(calendar-forward-month 1)
	(setq mark-diary-entries-in-calendar t))
    (calendar-forward-month 1))
  (calendar-recenter))

(defun calendar-backward-month-center ()
  (interactive)
  (if mark-diary-entries-in-calendar
      (progn
	(setq mark-diary-entries-in-calendar nil)
	(calendar-backward-month 1)
	(setq mark-diary-entries-in-calendar t))
    (calendar-backward-month 1))
  (calendar-recenter))

(defun calendar-goto-today-rehilit ()
  (interactive)
  (calendar-goto-today)
  (if window-system
      (progn
	(hilit-repaint-command t)
	(if mark-sundays (mark-calendar-days-named 7)))) ;; alle Sonntage rot einfärben
  (calendar-recenter))

(defun calendar-recenter ()
  (interactive)
  ;; besser, aber langsamer
  ;;(calendar-backward-week 21)
  ;;(calendar-forward-week 21)
  ;; WARNUNG
  ;; im Anblick verwirrend, aber mir ist nix anderes eingefallen
  (let ((mark-diary-entries-in-calendar mark-diary-entries-in-calendar))
    (setq mark-diary-entries-in-calendar nil)
    (calendar-backward-month 12)
    (calendar-forward-month 12)
    (if window-system
	(progn
	  (hilit-repaint-command t)
	  (if mark-sundays (mark-calendar-days-named 7)))))) ;; alle Sonntage rot einfärben

(defun calendar-quit () 
  "Quits first the diary window, afterwards iconifies the calendar"
  (interactive)
  ;; falls Diary existiert, lösche mit q den Diary-Buffer,
  ;; ansonsten iconifizieren den Calender
  (if calendar-setup
      (if (get-buffer fancy-diary-buffer)
	  (progn
	    (switch-to-buffer-other-frame fancy-diary-buffer)
	    (kill-buffer (current-buffer)))
	(exit-calendar)
	(if calendar-setup (other-frame -1) (other-window -1)))
    (exit-calendar))
  (set-window-configuration original-window-config))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define-key calendar-mode-map [menu-bar edit] 'undefined)
(define-key calendar-mode-map [menu-bar search] 'undefined)
(define-key calendar-mode-map [menu-bar scroll] 'undefined)

(define-key calendar-mode-map [down-mouse-2] 'calendar-mouse-2-date-menu)
(define-key calendar-mode-map [mouse-2] 'ignore)

(defvar calendar-mouse-3-map (make-sparse-keymap "Kalender"))
(define-key calendar-mode-map [down-mouse-3] calendar-mouse-3-map)

(if xemacsp
    (define-key calendar-mode-map [(control button3)] calendar-mouse-3-map)
  (define-key calendar-mode-map [C-down-mouse-3] calendar-mouse-3-map))

(define-key calendar-mode-map [menu-bar moon]
  (cons "Phasen" (make-sparse-keymap "Phasen")))

(define-key calendar-mode-map [menu-bar moon moon]
  '("Mondphasen" . calendar-phases-of-moon))

(define-key calendar-mode-map [menu-bar diary]
  (cons "Basis" (make-sparse-keymap "Grundfunktionen")))

(define-key calendar-mode-map [menu-bar diary weekday]
  '("Wochentagstermin jede n-te Monatswoche eintragen" . insert-weekday-diary-entry))
(define-key calendar-mode-map [menu-bar diary cyc]
  '("Zyklisch wiederkehrendes Datum in Privatkalender eintragen" . insert-cyclic-diary-entry))
(define-key calendar-mode-map [menu-bar diary blk]
  '("Zeitraum in Privatkalender eintragen (ab Marke)" . insert-block-diary-entry))
(define-key calendar-mode-map [menu-bar diary ann]
  '("Jahrestag in Privatkalender eintragen" . insert-anniversary-diary-entry))
(define-key calendar-mode-map [menu-bar diary yr]
  '("Jährlichen wiederkehrenden Termin in Privatkalender eintragen" . insert-yearly-diary-entry))
(define-key calendar-mode-map [menu-bar diary mon]
  '("Monatlich wiederkehrenden Termin in Privatkalender eintragen" . insert-monthly-diary-entry))
(define-key calendar-mode-map [menu-bar diary wk]
  '("Wöchentlich wiederkehrenden Termin in Privatkalender eintragen" . insert-weekly-diary-entry))
(define-key calendar-mode-map [menu-bar diary ent]
  '("Termin in Privatterminkalender eintragen" . insert-diary-entry))
(define-key calendar-mode-map [menu-bar diary markdate]
  '("Setze Marke bei diesem Datum [auch (C-SPC)]" . calendar-set-mark))
(define-key calendar-mode-map [menu-bar diary all]
  '("Termineintragungen des private Terminkalenders" . show-all-diary-entries))
(define-key calendar-mode-map [menu-bar diary mark]
 '("Markiere alle Tage" . mark-diary-entries))
(define-key calendar-mode-map [menu-bar diary view]
  '("Termine ab heutigen Tag" . view-diary-entries))
(define-key calendar-mode-map [menu-bar diary view]
  '("Anderer Tagebuchfile" . view-other-diary-entries))

(if (< emacs-major-version 20)
    (progn
      (define-key calendar-mode-map [menu-bar holidays]
	(cons "Feiertage" (make-sparse-keymap "Feiertage")))
      (define-key calendar-mode-map [menu-bar holidays unmark]
	'("Feiertage nicht anzeigen" . calendar-unmark-rehilit))
      (define-key calendar-mode-map [menu-bar holidays all-known]
	'("Alle benamten Tage anzeigen" . mark-all-calendar-holidays))
      (define-key calendar-mode-map [menu-bar holidays mark]
	'("Feiertage anzeigen" . mark-calendar-holidays))
      (if xemacsp
	  nil
	(define-key calendar-mode-map [menu-bar holidays 1-day]
	  '("Heutige Feiertage" . calendar-cursor-holidays))))
  (define-key calendar-mode-map [menu-bar Holidays]
    (cons "Feiertage" (make-sparse-keymap "Feiertage")))
  (define-key calendar-mode-map [menu-bar Holidays unmark]
    '("Feiertage nicht anzeigen" . calendar-unmark-rehilit))
  (define-key calendar-mode-map [menu-bar Holidays all-known]
    '("Alle benamten Tage anzeigen" . mark-all-calendar-holidays))
  (define-key calendar-mode-map [menu-bar Holidays mark]
    '("Feiertage anzeigen" . mark-calendar-holidays))
  (if xemacsp
      nil
    (define-key calendar-mode-map [menu-bar Holidays 1-day]
      '("Heutige Feiertage" . calendar-cursor-holidays))))

(define-key calendar-mode-map [menu-bar goto]
  (cons "Bewegen" (make-sparse-keymap "Bewegen")))


(define-key calendar-mode-map [menu-bar goto alien]
  (cons "Terminkalender mit anderen Rechnungsweisen" (make-sparse-keymap "Terminkalender mit anderen Rechnungsweisen")))

(define-key calendar-mode-map [menu-bar goto alien heb]
  '("Hebräisches Datum eintragen" . calendar-mouse-insert-hebrew-diary-entry))
(define-key calendar-mode-map [menu-bar goto alien isl]
  '("Islamisches Datum eintragen" . calendar-mouse-insert-islamic-diary-entry))
(define-key calendar-mode-map [menu-bar goto alien french]
  '("Französisches Datum" . calendar-goto-french-date))
(define-key calendar-mode-map [menu-bar goto alien mayan]
  (cons "Maya-Datum" (make-sparse-keymap "Maya-Datum")))
(define-key calendar-mode-map [menu-bar goto alien ethiopic]
  '("Äthiopisches Datum" . calendar-goto-ethiopic-date))
(define-key calendar-mode-map [menu-bar goto alien coptic]
  '("Koptisches Datum" . calendar-goto-coptic-date))
(define-key calendar-mode-map [menu-bar goto alien chinese]
  '("Chinesisches Datum" . calendar-goto-chinese-date))
(define-key calendar-mode-map [menu-bar goto alien julian]
  '("Julianisches Kalenderdatum" . calendar-goto-julian-date))
(define-key calendar-mode-map [menu-bar goto alien islamic]
  '("Islamisches Datum" . calendar-goto-islamic-date))
(define-key calendar-mode-map [menu-bar goto alien persian]
  '("Persisches Datum" . calendar-goto-persian-date))
(define-key calendar-mode-map [menu-bar goto alien hebrew]
  '("Hebräisches Datum" . calendar-goto-hebrew-date))
(define-key calendar-mode-map [menu-bar goto alien astro]
  '("Astronomisches Datum" . calendar-goto-astro-day-number))
(define-key calendar-mode-map [menu-bar goto alien iso]
  '("ISO-Datum" . calendar-goto-iso-date))



(define-key calendar-mode-map [menu-bar goto bk-12]
  '("Ein Jahr zurück" . "4\ev"))
(define-key calendar-mode-map [menu-bar goto bk-3]
  '("3 Monate zurück" . scroll-calendar-right-three-months))
(define-key calendar-mode-map [menu-bar goto bk-1]
  '("1 Monat zurück" . scroll-calendar-right))
(define-key calendar-mode-map [menu-bar goto fwd-12]
  '("Ein Jahr weiter" . "4\C-v"))
(define-key calendar-mode-map [menu-bar goto fwd-3]
  '("3 Monate weiter" . scroll-calendar-left-three-months))
(define-key calendar-mode-map [menu-bar goto fwd-1]
  '("1 Monat weiter" . scroll-calendar-left))


(define-key calendar-mode-map [menu-bar goto gregorian]
  '("Anderes Datum" . calendar-goto-date))
(define-key calendar-mode-map [menu-bar goto gregorian-month]
  '("Anderer Monat" . calendar-other-month))
(define-key calendar-mode-map [menu-bar goto end-of-year]
  '("Zum Jahresende" . calendar-end-of-year))
(define-key calendar-mode-map [menu-bar goto beginning-of-year]
  '("Zum Jahresanfang" . calendar-beginning-of-year))
(define-key calendar-mode-map [menu-bar goto end-of-month]
  '("Zum Monatsende" . calendar-end-of-month))
(define-key calendar-mode-map [menu-bar goto beginning-of-month]
  '("Zum Monatsbeginn" . calendar-beginning-of-month))
(define-key calendar-mode-map [menu-bar goto end-of-week]
  '("Zum Ende der Woche" . calendar-end-of-week))
(define-key calendar-mode-map [menu-bar goto beginning-of-week]
  '("Zum Anfang der Woche" . calendar-beginning-of-week))
(define-key calendar-mode-map [menu-bar goto today]
  '("Zum heutigen Datum" . calendar-goto-today))


(define-key calendar-mode-map [menu-bar goto alien mayan prev-rnd]
  '("Previous Round" . calendar-previous-calendar-round-date))
(define-key calendar-mode-map [menu-bar goto alien mayan nxt-rnd]
  '("Next Round" . calendar-next-calendar-round-date))
(define-key calendar-mode-map [menu-bar goto alien mayan prev-haab]
  '("Previous Haab" . calendar-previous-haab-date))
(define-key calendar-mode-map [menu-bar goto alien mayan next-haab]
  '("Next Haab" . calendar-next-haab-date))
(define-key calendar-mode-map [menu-bar goto alien mayan prev-tzol]
  '("Previous Tzolkin" . calendar-previous-tzolkin-date))
(define-key calendar-mode-map [menu-bar goto alien mayan next-tzol]
  '("Next Tzolkin" . calendar-next-tzolkin-date))



(defun calendar-mouse-2-date-menu (event)
  "Pop up menu for Mouse-2 for selected date in the calendar window."
  (interactive "e")
  (let* ((date (calendar-event-to-date t))
         (selection
          (x-popup-menu
           event
           (list (calendar-date-string date t nil)
                 (list
                  ""
                  '("Termineintragungen" . calendar-mouse-view-diary-entries)
                  '("Neuen Termin im privaten Terminkalender eintragen" . calendar-mouse-insert-diary-entry)
                  '("Neuen Termin im persönlichen Terminkalender eintragen" . calendar-mouse-insert-personal-diary-entry)
                  '("Neuen Termin im Gruppenterminkalender eintragen" . calendar-mouse-insert-group-diary-entry)
                  '("Tag markieren" . calendar-mouse-set-mark)
                  '("Ferientage" . calendar-mouse-holidays)
                  '("Sonnenaufgang/Sonnenuntergang" . calendar-mouse-sunrise/sunset)
                  '("Anderer Kalender" . calendar-mouse-print-dates)
                  '("Anderer Terminfile"
                    . calendar-mouse-view-other-diary-entries)
                  )))))
    (and selection (call-interactively selection))))

(define-key calendar-mouse-3-map [exit-calendar]
  '("Kalender verlassen" . exit-calendar))
(define-key calendar-mouse-3-map [show-diary]
  '("Terminkalender" . show-all-diary-entries))
(define-key calendar-mouse-3-map [lunar-phases]
  '("Mondphasen" . calendar-phases-of-moon))
(define-key calendar-mouse-3-map [unmark]
  '("Markierung löschen" . calendar-unmark))
(define-key calendar-mouse-3-map [mark-holidays]
  '("Feiertage kennzeichnen" . mark-calendar-holidays))
(define-key calendar-mouse-3-map [list-holidays]
  '("Feiertage auflisten" . list-calendar-holidays))
(define-key calendar-mouse-3-map [mark-diary-entries]
  '("Termintage kennzeichnen" . mark-diary-entries))
(define-key calendar-mouse-3-map [scroll-backward]
  '("Drei Monate rückwärts" . scroll-calendar-right-three-months))
(define-key calendar-mouse-3-map [scroll-forward]
  '("Drei Monate vorwärts" . scroll-calendar-left-three-months))


(defun calendar-mouse-insert-personal-diary-entry ()
  "Insert diary entry for mouse-selected date."
  (interactive)
  (let ((tmp diary-file))
    (setq diary-file personal-diary-file)
    (save-excursion
      (calendar-mouse-goto-date (calendar-event-to-date))
      (insert-diary-entry nil))
    (setq diary-file tmp)))

(defun calendar-mouse-insert-group-diary-entry ()
  "Insert diary entry for mouse-selected date."
  (interactive)
  (let ((tmp diary-file))
    (setq diary-file group-diary-file)
    (save-excursion
      (calendar-mouse-goto-date (calendar-event-to-date))
      (insert-diary-entry nil))
    (setq diary-file tmp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





(define-key calendar-mode-map " " 'calendar-scroll-diary-down) ; Blank-Taste
(define-key calendar-mode-map [menu-bar goto calendar-scroll-down-diary-1]
  '("Scolle Terminkalenderfenster noch unten" . calendar-scroll-diary-down))
(define-key calendar-mode-map "\C-?" 'calendar-scroll-diary-up) ; delete-Taste
(define-key calendar-mode-map [menu-bar goto calendar-scroll-up-diary-1]
  '("Scolle Terminkalenderfenster noch oben" . calendar-scroll-diary-up))



(define-key calendar-mode-map "\M-d" 'calendar-scroll-diary-down)
(define-key calendar-mode-map [menu-bar goto scroll-down-diary]
  '("Blättere Terminkalenderfenster noch unten" . calendar-scroll-diary-down))
(defun calendar-scroll-diary-down () 
  "Scrolls the diary window"
  (interactive)
  (let ((calbuf (current-buffer)) dum)
    (if (get-buffer fancy-diary-buffer)
	(progn
	  (if calendar-setup
	      (switch-to-buffer-other-frame fancy-diary-buffer)
	    (switch-to-buffer-other-frame fancy-diary-buffer))
	  (condition-case dum
	      (scroll-up 5)
	    (error "End of buffer"))
	  (if calendar-setup
	      (switch-to-buffer-other-frame calbuf)
	    (switch-to-buffer-other-frame calbuf))))))

(define-key calendar-mode-map "\M-u" 'calendar-scroll-diary-up)
(define-key calendar-mode-map [menu-bar goto scroll-up-diary]
  '("Blättere Terminkalenderfenster nach oben" . calendar-scroll-diary-up))
(defun calendar-scroll-diary-up () 
  "Scrolls the diary window"
  (interactive)
  (let ((calbuf (current-buffer)) dum)
    (if (get-buffer fancy-diary-buffer)
	(progn
	  (if calendar-setup
	      (switch-to-buffer-other-frame fancy-diary-buffer)
	    (switch-to-buffer-other-frame fancy-diary-buffer))
	  (condition-case dum
	      (scroll-down 5)
	    (error "End of buffer"))
	  (if calendar-setup
	      (switch-to-buffer-other-frame calbuf)
	    (switch-to-buffer-other-frame calbuf))))))


(define-key calendar-mode-map "\C-o" 'change-focus)
(define-key calendar-mode-map [menu-bar goto change-focus]
  '("Ins andere Fenster" . change-focus))
(defun change-focus ()
  (interactive)
  (if calendar-setup
      (raise-frame (next-frame nil 'visible))
    (other-window 1)))









(message "Kalender geladen bis Marke 5")


(define-key calendar-mode-map "." 'calendar-goto-today-rehilit)
(define-key calendar-mode-map [menu-bar goto today]
  '("Zum heutigen Datum" . calendar-goto-today-rehilit))
(define-key calendar-mode-map "b" 'calendar-backward-month-center)
(define-key calendar-mode-map [menu-bar goto bk-1]
  '("1 Monat zurück" . calendar-backward-month-center))
(define-key calendar-mode-map "f" 'calendar-forward-month-center)
(define-key calendar-mode-map [menu-bar goto fwd-1]
  '("1 Monat vorwärts" . calendar-forward-month-center))
(define-key calendar-mode-map "\C-l" 'calendar-recenter)
(define-key calendar-mode-map [menu-bar goto recenter]
  '("Kalender neu zeichnen" . calendar-recenter))
(define-key calendar-mode-map "n" 'calendar-day-message)
(define-key calendar-mode-map [menu-bar goto daymess]
  '("Info über den Tag" . calendar-day-message))
(define-key calendar-mode-map "d" 'view-diary-entries-2)
(define-key calendar-mode-map "D" 'view-other-diary-entries-2)
(define-key calendar-mode-map "q" 'calendar-quit)
(if (not xemacsp)
    (define-key menu-bar-file-menu [quit-cal]
      '("Kalender iconifizieren" . calendar-quit)))
(define-key calendar-mode-map "c" 'mark-all-calendar-holidays)
(define-key calendar-mode-map [menu-bar goto change-diary-entry]
  '("Terminkalendereintrag suchen [M-x cde]" . change-diary-entry))



;; Mode-Line
(setq calendar-mode-line-format
  (list
   (substitute-command-keys "\\<calendar-mode-map>\\[scroll-calendar-left]")
   "Kalender"
   (substitute-command-keys "\\<calendar-mode-map>|\\[calendar-kurzinfo]=Info|\\[calendar-other-month]=Andere|\\[calendar-goto-today-rehilit]=Heute|")
   '(calendar-date-string (calendar-current-date) nil)
   (substitute-command-keys "\\<calendar-mode-map>\\[scroll-calendar-right]")))



;;; auto-fill-function neu definieren
(defun newline-and-tab ()
  (interactive)
  (if (not (<= (current-column) (current-fill-column)))
      (insert "\n\t\t")))


;; ================================================
;;
;; Modifizieren des Eintragens der Tagebucheinträge
;;
;;
(defvar calendar-date-display-form-tmp nil)

(defadvice insert-diary-entry (before set-date-substring activate)
  (setq calendar-date-display-form-tmp calendar-date-display-form)
  (setq calendar-date-display-form	'(day " " monthname " " year))
  )
(defadvice insert-diary-entry (after activate-auto-fill-function activate)
  (make-local-variable 'auto-fill-function)
  (setq auto-fill-function 'newline-and-tab)
  (set-fill-column diary-fill-column) ;; an den PS-Mode angeglichen
  (setq calendar-date-display-form calendar-date-display-form-tmp)
  )

(defadvice insert-block-diary-entry (after activate-auto-fill-function activate)
  (make-local-variable 'auto-fill-function)
  (setq auto-fill-function 'newline-and-tab)
  (set-fill-column diary-fill-column) ;; an den PS-Mode angeglichen
  )
(defadvice insert-cyclic-diary-entry (after activate-auto-fill-function activate)
  (make-local-variable 'auto-fill-function)
  (setq auto-fill-function 'newline-and-tab)
  (set-fill-column diary-fill-column) ;; an den PS-Mode angeglichen
  )
(defadvice insert-anniversary-diary-entry (after activate-auto-fill-function activate)
  (make-local-variable 'auto-fill-function)
  (setq auto-fill-function 'newline-and-tab)
  (set-fill-column diary-fill-column) ;; an den PS-Mode angeglichen
  )
(defadvice insert-yearly-diary-entry (after activate-auto-fill-function activate)
  (make-local-variable 'auto-fill-function)
  (setq auto-fill-function 'newline-and-tab)
  (set-fill-column diary-fill-column) ;; an den PS-Mode angeglichen
  )
(defadvice insert-monthly-diary-entry (after activate-auto-fill-function activate)
  (make-local-variable 'auto-fill-function)
  (setq auto-fill-function 'newline-and-tab)
  (set-fill-column diary-fill-column) ;; an den PS-Mode angeglichen
  )
(defadvice insert-weekly-diary-entry (after activate-auto-fill-function activate)
  (make-local-variable 'auto-fill-function)
  (setq auto-fill-function 'newline-and-tab)
  (set-fill-column diary-fill-column) ;; an den PS-Mode angeglichen
  )
(defadvice insert-weekday-diary-entry (after activate-auto-fill-function activate)
  (make-local-variable 'auto-fill-function)
  (setq auto-fill-function 'newline-and-tab)
  (set-fill-column diary-fill-column) ;; an den PS-Mode angeglichen
  )
;;
;;
;; ================================================
;;





;;;
;;; Für den Persönlichen, von aussen eintragbaren kalender: 
;;; Prefig i p setzt in den perönlichen, von aussen eintragbaren ein, 
;;; was sonst in den eigenen eigesetzt wird
;;;
;;; i p c		insert-cyclic-diary-entry
;;; i p b		insert-block-diary-entry
;;; i p a		insert-anniversary-diary-entry
;;; i p y		insert-yearly-diary-entry
;;; i p m		insert-monthly-diary-entry
;;; i p w		insert-weekly-diary-entry
;;; i p d		insert-diary-entry
;;;

(define-key calendar-mode-map "ipd" 'insert-personal-diary)
(defun insert-personal-diary (&optional args) 
  (interactive "P")
  (let ((tmp diary-file))
    (setq diary-file personal-diary-file)
    (insert-diary-entry args)
    (if (not (or (string-match user-login-name (file-name-nondirectory buffer-file-name))
		 (string-match user-real-login-name (file-name-nondirectory buffer-file-name))))
	(save-excursion
	  (insert "  <" signature ">")))
;    (save-excursion
;      (insert (concat "\n\t\t<File: " (file-name-nondirectory buffer-file-name) ">")))
    (setq diary-file tmp)))
(define-key calendar-mode-map "ipb" 'insert-personal-diary-block)
(defun insert-personal-diary-block (&optional args) 
  (interactive "P")
  (let ((tmp diary-file))
    (setq diary-file personal-diary-file)
    (insert-block-diary-entry args)
    (if (not (or (string-match user-login-name (file-name-nondirectory buffer-file-name))
		 (string-match user-real-login-name (file-name-nondirectory buffer-file-name))))
	(save-excursion
	  (insert "  <" signature ">")))
;    (save-excursion
;      (insert (concat "\n\t\t<File: " (file-name-nondirectory buffer-file-name) ">")))
    (setq diary-file tmp)))
(define-key calendar-mode-map "ipc" 'insert-personal-cyclic-diary)
(defun insert-personal-cyclic-diary (&optional args) 
  (interactive "P")
  (let ((tmp diary-file))
    (setq diary-file personal-diary-file)
    (insert-cyclic-diary-entry args)
    (if (not (or (string-match user-login-name (file-name-nondirectory buffer-file-name))
		 (string-match user-real-login-name (file-name-nondirectory buffer-file-name))))
	(save-excursion
	  (insert "  <" signature ">")))
;    (save-excursion
;      (insert (concat "\n\t\t<File: " (file-name-nondirectory buffer-file-name) ">")))
    (setq diary-file tmp)))
(define-key calendar-mode-map "ipa" 'insert-personal-anniversary-entry)
(defun insert-personal-anniversary-entry (&optional args)
  (interactive "P")
  (let ((tmp diary-file))
    (setq diary-file personal-diary-file)
    (insert-anniversary-diary-entry args)
    (if (not (or (string-match user-login-name (file-name-nondirectory buffer-file-name))
		 (string-match user-real-login-name (file-name-nondirectory buffer-file-name))))
	(save-excursion
	  (insert "  <" signature ">")))
;    (save-excursion
;      (insert (concat "\n\t\t<File: " (file-name-nondirectory buffer-file-name) ">")))
    (setq diary-file tmp)))
(define-key calendar-mode-map "ipy" 'insert-personal-yearly-diary-entry)
(defun insert-personal-yearly-diary-entry (&optional args)
  (interactive "P")
  (let ((tmp diary-file))
    (setq diary-file personal-diary-file)
    (insert-yearly-diary-entry args)
    (if (not (or (string-match user-login-name (file-name-nondirectory buffer-file-name))
		 (string-match user-real-login-name (file-name-nondirectory buffer-file-name))))
	(save-excursion
	  (insert "  <" signature ">")))
;    (save-excursion
;      (insert (concat "\n\t\t<File: " (file-name-nondirectory buffer-file-name) ">")))
    (setq diary-file tmp)))
(define-key calendar-mode-map "ipm" 'insert-personal-monthly-diary-entry)
(defun insert-personal-monthly-diary-entry (&optional args)
  (interactive "P")
  (let ((tmp diary-file))
    (setq diary-file personal-diary-file)
    (insert-monthly-diary-entry args)
    (if (not (or (string-match user-login-name (file-name-nondirectory buffer-file-name))
		 (string-match user-real-login-name (file-name-nondirectory buffer-file-name))))
	(save-excursion
	  (insert "  <" signature ">")))
;    (save-excursion
;      (insert (concat "\n\t\t<File: " (file-name-nondirectory buffer-file-name) ">")))
    (setq diary-file tmp)))
(define-key calendar-mode-map "ipw" 'insert-personal-weekly-diary-entry)
(defun insert-personal-weekly-diary-entry (&optional args)
  (interactive "P")
  (let ((tmp diary-file))
    (setq diary-file personal-diary-file)
    (insert-weekly-diary-entry args)
    (if (not (or (string-match user-login-name (file-name-nondirectory buffer-file-name))
		 (string-match user-real-login-name (file-name-nondirectory buffer-file-name))))
	(save-excursion
	  (insert "  <" signature ">")))
;    (save-excursion
;      (insert (concat "\n\t\t<File: " (file-name-nondirectory buffer-file-name) ">")))
    (setq diary-file tmp)))
(define-key calendar-mode-map "ipr" 'insert-personal-weekday-diary-entry)
(defun insert-personal-weekday-diary-entry (&optional args)
  (interactive "P")
  (let ((tmp diary-file))
    (setq diary-file personal-diary-file)
    (insert-weekday-diary-entry args)
    (if (not (or (string-match user-login-name (file-name-nondirectory buffer-file-name))
		 (string-match user-real-login-name (file-name-nondirectory buffer-file-name))))
	(save-excursion
	  (insert "  <" signature ">")))
;    (save-excursion
;      (insert (concat "\n\t\t<File: " (file-name-nondirectory buffer-file-name) ">")))
    (setq diary-file tmp)))
;;; den Gruppenkalenderfile ansehen (wie s normal)
(define-key calendar-mode-map "p" 'show-personal-entries)
(defun show-personal-entries ()
  (interactive)
  (let ((tmp diary-file))
    (setq diary-file personal-diary-file)
    (show-all-diary-entries)
    (setq diary-file tmp)))

(defadvice show-all-diary-entries (after show-really-everything-c activate)
  (show-really-everything-c))


;;;
;;; Für den Gruppenkalender: Prefig i g setzt in den Gruppenkalender ein, was sonst in den eigenen eigesetzt wird
;;;
;;; i g c		insert-cyclic-diary-entry
;;; i g b		insert-block-diary-entry
;;; i g a		insert-anniversary-diary-entry
;;; i g y		insert-yearly-diary-entry
;;; i g m		insert-monthly-diary-entry
;;; i g w		insert-weekly-diary-entry
;;; i g d		insert-diary-entry
;;;


(define-key calendar-mode-map "igd" 'insert-group-diary)
(defun insert-group-diary (&optional args) 
  (interactive "P")
  (let ((tmp diary-file))
    (setq diary-file group-diary-file)
    (insert-diary-entry args)
    (if (not (or (string-match user-login-name (file-name-nondirectory buffer-file-name))
		 (string-match user-real-login-name (file-name-nondirectory buffer-file-name))))
	(save-excursion
	  (insert "  <" signature ">")))
;    (save-excursion
;      (insert "\n\t\t<Gruppenterminkalenderfile>"))
    (setq diary-file tmp)))
(define-key calendar-mode-map "igb" 'insert-group-diary-block)
(defun insert-group-diary-block (&optional args) 
  (interactive "P")
  (let ((tmp diary-file))
    (setq diary-file group-diary-file)
    (insert-block-diary-entry args)
    (if (not (or (string-match user-login-name (file-name-nondirectory buffer-file-name))
		 (string-match user-real-login-name (file-name-nondirectory buffer-file-name))))
	(save-excursion
	  (insert "  <" signature ">")))
;    (save-excursion
;      (insert "\n\t\t<Gruppenterminkalenderfile>"))
    (setq diary-file tmp)))
(define-key calendar-mode-map "igc" 'insert-group-cyclic-diary)
(defun insert-group-cyclic-diary (&optional args) 
  (interactive "P")
  (let ((tmp diary-file))
    (setq diary-file group-diary-file)
    (insert-cyclic-diary-entry args)
    (if (not (or (string-match user-login-name (file-name-nondirectory buffer-file-name))
		 (string-match user-real-login-name (file-name-nondirectory buffer-file-name))))
	(save-excursion
	  (insert "  <" signature ">")))
;    (save-excursion
;      (insert "\n\t\t<Gruppenterminkalenderfile>"))
    (setq diary-file tmp)))
(define-key calendar-mode-map "iga" 'insert-group-anniversary-entry)
(defun insert-group-anniversary-entry (&optional args)
  (interactive "P")
  (let ((tmp diary-file))
    (setq diary-file group-diary-file)
    (insert-anniversary-diary-entry args)
    (if (not (or (string-match user-login-name (file-name-nondirectory buffer-file-name))
		 (string-match user-real-login-name (file-name-nondirectory buffer-file-name))))
	(save-excursion
	  (insert "  <" signature ">")))
;    (save-excursion
;      (insert "\n\t\t<Gruppenterminkalenderfile>"))
    (setq diary-file tmp)))
(define-key calendar-mode-map "igy" 'insert-group-yearly-diary-entry)
(defun insert-group-yearly-diary-entry (&optional args)
  (interactive "P")
  (let ((tmp diary-file))
    (setq diary-file group-diary-file)
    (insert-yearly-diary-entry args)
    (if (not (or (string-match user-login-name (file-name-nondirectory buffer-file-name))
		 (string-match user-real-login-name (file-name-nondirectory buffer-file-name))))
	(save-excursion
	  (insert "  <" signature ">")))
;    (save-excursion
;      (insert "\n\t\t<Gruppenterminkalenderfile>"))
    (setq diary-file tmp)))
(define-key calendar-mode-map "igm" 'insert-group-monthly-diary-entry)
(defun insert-group-monthly-diary-entry (&optional args)
  (interactive "P")
  (let ((tmp diary-file))
    (setq diary-file group-diary-file)
    (insert-monthly-diary-entry args)
    (if (not (or (string-match user-login-name (file-name-nondirectory buffer-file-name))
		 (string-match user-real-login-name (file-name-nondirectory buffer-file-name))))
	(save-excursion
	  (insert "  <" signature ">")))
;    (save-excursion
;      (insert "\n\t\t<Gruppenterminkalenderfile>"))
    (setq diary-file tmp)))
(define-key calendar-mode-map "igw" 'insert-group-weekly-diary-entry)
(defun insert-group-weekly-diary-entry (&optional args)
  (interactive "P")
  (let ((tmp diary-file))
    (setq diary-file group-diary-file)
    (insert-weekly-diary-entry args)
    (if (not (or (string-match user-login-name (file-name-nondirectory buffer-file-name))
		 (string-match user-real-login-name (file-name-nondirectory buffer-file-name))))
	(save-excursion
	  (insert "  <" signature ">")))
;    (save-excursion
;      (insert "\n\t<Gruppenterminkalenderfile>"))
    (setq diary-file tmp)))
(define-key calendar-mode-map "igr" 'insert-group-weekday-diary-entry)
(defun insert-group-weekday-diary-entry (&optional args)
  (interactive "P")
  (let ((tmp diary-file))
    (setq diary-file group-diary-file)
    (insert-weekday-diary-entry args)
    (if (not (or (string-match user-login-name (file-name-nondirectory buffer-file-name))
		 (string-match user-real-login-name (file-name-nondirectory buffer-file-name))))
	(save-excursion
	  (insert "  <" signature ">")))
;    (save-excursion
;      (insert "\n\t\t<Gruppenterminkalenderfile>"))
    (setq diary-file tmp)))
;;; den Gruppenkalenderfile ansehen (wie s normal)
(define-key calendar-mode-map "g" 'show-group-entries)
(defun show-group-entries ()
  (interactive)
  (let ((tmp diary-file))
    (setq diary-file group-diary-file)
    (show-all-diary-entries)
    (setq diary-file tmp)))
;;; nur den Gruppenkalender, ohne den eigenen
(define-key calendar-mode-map "e" 'view-group-calendar-only)
(defun view-group-calendar-only (&optional args) 
  (interactive "P")
  (let ((tmp diary-file))
    (setq diary-file group-diary-file)
    (view-diary-entries (if (not args) 1 args))
    (calendar-day-message)
    (setq diary-file tmp)))
;;; nur den eigenen, nicht den Gruppenkalender
;;; (programmiertes Function-Overloading in LISP)
(define-key calendar-mode-map "r" 'show-own-calendar-without-includes)
(defun show-own-calendar-without-includes (&optional args)
  (interactive "P")
  (fset 'dummy-func (symbol-function 'include-other-diary-files))
  (unwind-protect
      (progn
	(fset 'include-other-diary-files 'dummy)
	(view-diary-entries (if (not args) 1 args))
	(calendar-day-message))
    (fset 'include-other-diary-files (symbol-function 'dummy-func))))
(defun dummy (&optional args)
  ;;  '(lambda()(message "not including \"#include\" files")))
  (message "not including \"#include\" files"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Für andere Terminkalender


(define-key calendar-mode-map "iod" 'insert-other-diary)
(defun insert-other-diary (other-diary-file &optional args) 
  (interactive "fTerminkalenderfile: \nPnonmarking")
  (let ((tmp diary-file))
    (setq diary-file other-diary-file)
    (insert-diary-entry args)
    (save-excursion
      (insert "  <" signature ">"))
    (save-excursion
      (insert (concat "\n\t\t<File: " (file-name-nondirectory buffer-file-name) ">")))
    (setq diary-file tmp)))
(define-key calendar-mode-map "iob" 'insert-other-diary-block)
(defun insert-other-diary-block (other-diary-file &optional args) 
  (interactive "fTerminkalenderfile: \nPnonmarking")
  (let ((tmp diary-file))
    (setq diary-file other-diary-file)
    (insert-block-diary-entry args)
    (save-excursion
      (insert "  <" signature ">"))
    (save-excursion
      (insert (concat "\n\t\t<File: " (file-name-nondirectory buffer-file-name) ">")))
    (setq diary-file tmp)))
(define-key calendar-mode-map "ioc" 'insert-other-cyclic-diary)
(defun insert-other-cyclic-diary (other-diary-file &optional args) 
  (interactive "fTerminkalenderfile: \nPnonmarking")
  (let ((tmp diary-file))
    (setq diary-file other-diary-file)
    (insert-cyclic-diary-entry args)
    (save-excursion
      (insert "  <" signature ">"))
    (save-excursion
      (insert (concat "\n\t\t<File: " (file-name-nondirectory buffer-file-name) ">")))
    (setq diary-file tmp)))
(define-key calendar-mode-map "ioa" 'insert-other-anniversary-entry)
(defun insert-other-anniversary-entry (other-diary-file &optional args)
  (interactive "fTerminkalenderfile: \nPnonmarking")
  (let ((tmp diary-file))
    (setq diary-file other-diary-file)
    (insert-anniversary-diary-entry args)
    (save-excursion
      (insert "  <" signature ">"))
    (save-excursion
      (insert (concat "\n\t\t<File: " (file-name-nondirectory buffer-file-name) ">")))
    (setq diary-file tmp)))
(define-key calendar-mode-map "ioy" 'insert-other-yearly-diary-entry)
(defun insert-other-yearly-diary-entry (other-diary-file &optional args)
  (interactive "fTerminkalenderfile: \nPnonmarking")
  (let ((tmp diary-file))
    (setq diary-file other-diary-file)
    (insert-yearly-diary-entry args)
    (save-excursion
      (insert "  <" signature ">"))
    (save-excursion
      (insert (concat "\n\t\t<File: " (file-name-nondirectory buffer-file-name) ">")))
    (setq diary-file tmp)))
(define-key calendar-mode-map "iom" 'insert-other-monthly-diary-entry)
(defun insert-other-monthly-diary-entry (other-diary-file &optional args)
  (interactive "fTerminkalenderfile: \nPnonmarking")
  (let ((tmp diary-file))
    (setq diary-file other-diary-file)
    (insert-monthly-diary-entry args)
    (save-excursion
      (insert "  <" signature ">"))
    (save-excursion
      (insert (concat "\n\t\t<File: " (file-name-nondirectory buffer-file-name) ">")))
    (setq diary-file tmp)))
(define-key calendar-mode-map "iow" 'insert-other-weekly-diary-entry)
(defun insert-other-weekly-diary-entry (other-diary-file &optional args)
  (interactive "fTerminkalenderfile: \nPnonmarking")
  (let ((tmp diary-file))
    (setq diary-file other-diary-file)
    (insert-weekly-diary-entry args)
    (save-excursion
      (insert "  <" signature ">"))
    (save-excursion
      (insert (concat "\n\t\t<File: " (file-name-nondirectory buffer-file-name) ">")))
    (setq diary-file tmp)))
(define-key calendar-mode-map "ior" 'insert-other-weekday-diary-entry)
(defun insert-other-weekday-diary-entry (other-diary-file &optional args)
  (interactive "fTerminkalenderfile: \nPnonmarking")
  (let ((tmp diary-file))
    (setq diary-file other-diary-file)
    (insert-weekday-diary-entry args)
    (save-excursion
      (insert "  <" signature ">"))
    (save-excursion
      (insert (concat "\n\t\t<File: " (file-name-nondirectory buffer-file-name) ">")))
    (setq diary-file tmp)))






;; neue Funktion im privaten Kalender

(define-key calendar-mode-map "ir" 'insert-weekday-diary-entry)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;










(define-key calendar-mode-map [menu-bar moon sun]
  '("Sonnenaufgang/Sonnenuntergang   (S)" . (lambda()(interactive)(calendar-sunrise-sunset))))
(define-key calendar-mode-map [menu-bar tools latex]
  '("Präpariere LaTeX-Buffer" . calendar-mouse-cal-tex-menu))
(define-key calendar-mode-map [menu-bar diary view]
  '("Zeige Terminkalender von anderem File" . view-other-diary-entries-2))
(define-key calendar-mode-map [menu-bar diary show]
  '("Zeige Terminkalender" . view-diary-entries-2))
(define-key calendar-mode-map [menu-bar diary mark]
 '("Markieren der Tage mit Terminen" . mark-diary-entries))
(define-key calendar-mode-map [menu-bar diary unmark]
 '("Unmarkieren der Tage mit Terminen" . calendar-unmark-rehilit))
(define-key calendar-mode-map "u"   'calendar-unmark-rehilit)
(define-key calendar-mode-map [menu-bar Holidays unmark]
  '("Markierung löschen" . calendar-unmark-rehilit))
(define-key calendar-mode-map [menu-bar diary daymessage]
  '("Tagesorientierung" . calendar-day-message))
(define-key calendar-mode-map "P" 'ps-print-diary-entries)
(define-key calendar-mode-map [menu-bar diary ps-print-diary-entries]
  '("PS-Druck des anderen Fensters" . ps-print-diary-entries))
(defun ps-print-diary-entries ()
  (interactive)
  (if (featurep 'ps-print)
      (if (y-or-n-p "Buffer in anderem Fenster drucken? ")
	  (if calendar-setup
	      (progn
		(raise-frame (next-frame nil 'visible))
		(let ((current-buf (current-buffer)))
		  (switch-to-buffer-other-frame fancy-diary-buffer)
		  (set-buffer-modified-p nil)
		  (ps-print-buffer-with-faces)
		  (raise-frame (previous-frame nil 'visible))
		  (switch-to-buffer-other-frame current-buf)))
	    (other-window 1)
	    (set-buffer-modified-p nil)
	    (ps-print-buffer-with-faces)
	    (other-window -1)))
    (error "ps-print not yet loaded")))


(message "Kalender geladen bis Marke 6")


(defun calendar-unmark-rehilit ()
  (interactive)
  (calendar-unmark)
  (if window-system
      (progn
	(hilit-repaint-command t)
	(if mark-sundays (mark-calendar-days-named 7))))) ;; alle Sonntage rot einfärben


(define-key calendar-mode-map [menu-bar group]
  (cons "Termine" (make-sparse-keymap "Terminkalender")))
;;
(define-key calendar-mode-map [menu-bar group group]
  (cons "Gruppenterminkalender" (make-sparse-keymap "Gruppenterminkalender")))

(define-key calendar-mode-map [menu-bar group group insert-anniv]
  '("Jahrestag in Gruppentermin eintragen" . insert-group-anniversary-entry))
(define-key calendar-mode-map [menu-bar group group insert-yearly]
  '("Jährlichen Gruppentermin eintragen" . insert-group-yearly-diary-entry))
(define-key calendar-mode-map [menu-bar group group insert-monthly]
  '("Monatlichen Gruppentermin eintragen" . insert-group-monthly-diary-entry))
(define-key calendar-mode-map [menu-bar group group insert-weekly]
  '("Wöchentlichen Gruppentermin eintragen" . insert-group-weekly-diary-entry))
(define-key calendar-mode-map [menu-bar group group insert-weekday]
  '("Wochentagstermin jede n-te Monatswoche eintragen" . insert-group-weekday-diary-entry))
(define-key calendar-mode-map [menu-bar group group insert-cyclic]
  '("Zyklischen Gruppentermin eintragen" . insert-group-cyclic-diary))
(define-key calendar-mode-map [menu-bar group group insert-block]
  '("Zeitraum (ab Marke) als Gruppenterminin eintragen" . insert-group-diary-block))
(define-key calendar-mode-map [menu-bar group group insert-group]
  '("Gruppentermin eintragen" . insert-group-diary))

(define-key calendar-mode-map [menu-bar group group headline]
  '("*** Gruppenterminkalenderfunktionen: ***" . calendar-day-message-dummy))

(define-key calendar-mode-map [menu-bar group other]
  (cons "Fremde Terminkalender" (make-sparse-keymap "Fremde Terminkalender")))

(define-key calendar-mode-map [menu-bar group other insert-other-anniv]
  '("Jahrestag in anderen File eintragen" . insert-other-anniversary-entry))
(define-key calendar-mode-map [menu-bar group other insert-other-yearly]
  '("Jährlichen Termin in anderen File eintragen" . insert-other-yearly-diary-entry))
(define-key calendar-mode-map [menu-bar group other insert-other-monthly]
  '("Monatlichen Termin in anderen File eintragen" . insert-other-monthly-diary-entry))
(define-key calendar-mode-map [menu-bar group other insert-other-weekly]
  '("Wöchentlichen Termin in anderen File eintragen" . insert-other-weekly-diary-entry))
(define-key calendar-mode-map [menu-bar group other insert-weekday]
  '("Wochentagstermin jede n-te Monatswoche eintragen" . insert-other-weekday-diary-entry))
(define-key calendar-mode-map [menu-bar group other insert-other-cyclic]
  '("Zyklischen Termine in anderen File eintragen" . insert-other-cyclic-diary))
(define-key calendar-mode-map [menu-bar group other insert-other-block]
  '("Zeitraum (ab Marke) in anderen File eintragen" . insert-other-diary-block))
(define-key calendar-mode-map [menu-bar group other insert-other-diary]
  '("Termin in anderen File eintragen" . insert-other-diary))

(define-key calendar-mode-map [menu-bar group other headline]
  '("*** Terminkalenderfunktionen Fremdkalender: ***" . calendar-day-message-dummy))

(define-key calendar-mode-map [menu-bar group personal]
  (cons "Persönlicher Terminkalender" (make-sparse-keymap "Persönliche Terminkalender")))

(define-key calendar-mode-map [menu-bar group personal insert-personal-anniv]
  '("Jahrestag in persönlichen Kalender eintragen" . insert-personal-anniversary-entry))
(define-key calendar-mode-map [menu-bar group personal insert-personal-yearly]
  '("Jährlichen Termin in persönlichen Kalender eintragen" . insert-personal-yearly-diary-entry))
(define-key calendar-mode-map [menu-bar group personal insert-personal-monthly]
  '("Monatlichen Termin in persönlichen Kalender eintragen" . insert-personal-monthly-diary-entry))
(define-key calendar-mode-map [menu-bar group personal insert-personal-weekly]
  '("Wöchentlichen Termin in persönlichen Kalender eintragen" . insert-personal-weekly-diary-entry))
(define-key calendar-mode-map [menu-bar group personal insert-weekday]
  '("Wochentagstermin jede n-te Monatswoche eintragen" . insert-personal-weekday-diary-entry))
(define-key calendar-mode-map [menu-bar group personal insert-personal-cyclic]
  '("Zyklischen Termine in persönlichen Kalender eintragen" . insert-personal-cyclic-diary))
(define-key calendar-mode-map [menu-bar group personal insert-personal-block]
  '("Zeitraum (ab Marke) in persönlichen Kalender eintragen" . insert-personal-diary-block))
(define-key calendar-mode-map [menu-bar group personal insert-personal-diary]
  '("Termin in persönlichen Kalender eintragen" . insert-personal-diary))

(define-key calendar-mode-map [menu-bar group personal headline]
  '("*** Terminkalenderfunktionen persönlicher Kalender: ***" . calendar-day-message-dummy))


(define-key calendar-mode-map [menu-bar group privat]
  (cons "Privater Terminkalender" (make-sparse-keymap "Privater Terminkalender")))

(define-key calendar-mode-map [menu-bar group privat ann]
  '("Jahrestag in Privatkalender eintragen" . insert-anniversary-diary-entry))
(define-key calendar-mode-map [menu-bar group privat yr]
  '("Jährlichen wiederkehrenden Termin in Privatkalender eintragen" . insert-yearly-diary-entry))
(define-key calendar-mode-map [menu-bar group privat mon]
  '("Monatlich wiederkehrenden Termin in Privatkalender eintragen" . insert-monthly-diary-entry))
(define-key calendar-mode-map [menu-bar group privat wk]
  '("Wöchentlich wiederkehrenden Termin in Privatkalender eintragen" . insert-weekly-diary-entry))
(define-key calendar-mode-map [menu-bar group privat insert-weekday]
  '("Wochentagstermin jede n-te Monatswoche eintragen" . insert-weekday-diary-entry))
(define-key calendar-mode-map [menu-bar group privat cyc]
  '("Zyklisch wiederkehrendes Datum in Privatkalender eintragen" . insert-cyclic-diary-entry))
(define-key calendar-mode-map [menu-bar group privat blk]
  '("Zeitraum in Privatkalender eintragen (ab Marke)" . insert-block-diary-entry))
(define-key calendar-mode-map [menu-bar group privat ent]
  '("Termin in Privatkalender eintragen". insert-diary-entry))

(define-key calendar-mode-map [menu-bar group privat headline]
  '("*** Terminkalenderfunktionen Privatkalender: ***" . calendar-day-message-dummy))


(define-key calendar-mode-map [menu-bar group markdate]
  '("Setze Marke bei diesem Datum [auch (C-SPC)]" . calendar-set-mark))

(define-key calendar-mode-map [menu-bar group show-all-3]
  '("Termineinträge im Gruppenkalender" . show-group-entries))
(define-key calendar-mode-map [menu-bar group show-all-2]
  '("Termineinträge in personenzugeordneten Kalender" . show-personal-entries))
(define-key calendar-mode-map [menu-bar group show-all-1]
  '("Termineinträge des privaten Kalenders" . show-all-diary-entries))

(define-key calendar-mode-map [menu-bar group show-personal]
  '("Kalender ohne #include-Files" . show-own-calendar-without-includes))
(define-key calendar-mode-map [menu-bar group show-group]
  '("Terminkalender des Gruppenterminkalenders" . view-group-calendar-only))
(define-key calendar-mode-map [menu-bar group view-others]
  '("Zeige anderen Terminkalender" . view-other-diary-entries-2))
(define-key calendar-mode-map [menu-bar group view-diary]
  '("Zeige privaten Terminkalender" . view-diary-entries-2))




(message "Kalender geladen bis Marke 7")



(defun ask-for-calendar-date ()
  (let ((tag "1") (monat) (jahr))
    (setq tag (read-string "Tag: "))
    (setq monat (read-string "Monat: "))
    (setq jahr (read-string "Jahr: "))
    (list (string-to-number monat) (string-to-number tag) (string-to-number jahr))))
(defun d (&optional arg)
  "Short for calling the diary in this buffer"
  (interactive "P")
  (let ((calendar-setup calendar-setup))
    (setq calendar-setup nil)
  (let* ((end-of-namestring (string-match "\\.crypt\\|$" diary-file))
	 (save-string-tmp (match-end 0))
	 (diary-buffer-name (substring diary-file save-string-tmp end-of-namestring))
	 (switch-to-buffer diary-buffer-name)
	 (kill-buffer diary-file))
;;;
;;; Von diary-lib.el:
;;;(defun diary (&optional arg)
;;;  "Generate the diary window for ARG days starting with the current date.
;;; If no argument is provided, the number of days of diary entries is governed
;;; by the variable `number-of-diary-entries'.	This function is suitable for
;;;execution in a `.emacs' file."
;;;  (interactive "P")
    (if (or (not arg) (>= arg 0))
	(diary arg)
      (let ((d-file (substitute-in-file-name diary-file))
	    (date (ask-for-calendar-date)) (arg (- arg)))
	(if (and d-file (file-exists-p d-file))
	    (if (file-readable-p d-file)
		(list-diary-entries
		 date
		 (cond
		  (arg (prefix-numeric-value arg))
		  ((vectorp number-of-diary-entries)
		   (aref number-of-diary-entries (calendar-day-of-week date)))
		  (t number-of-diary-entries)))
	      (error "Your diary file is not readable!"))
	  (error "You don't have a diary file!")))
;      (if calendar-setup
;	  (kill-buffer calendar-buffer))
      ))))


(defun change-diary-entry ()
  "In den \"Fancy Diary Entries\" auf eine Position gehen, dann stellt es den Cursor an den
entsprechenden Eintrag in dem File DIARY-FILE"
  (interactive)
  (let ((begin) (end) (tmp-substring) (current-buf (current-buffer)))
    (save-excursion
      (beginning-of-line)
      (re-search-forward "[^ \t\r]\\|$" nil t)
      (backward-char 1)
      (setq begin (point))
      (end-of-line)
      (re-search-backward "[^ \t\r]\\|^" nil t)
      (forward-char 1)
      (setq end (point))
      (forward-char 1)
      (setq tmp-substring (buffer-substring-no-properties begin end)))
    ;;
    ;; gefährlich und noch nicht ausgereift
    (let ((current-buf (current-buffer)))
      (if (not (get-buffer "*scratch*"))
	  (progn
	    (make-frame-visible emacs-mainframe)
	    (select-frame emacs-mainframe)
	    (switch-to-buffer "*scratch*"))
	(if calendar-setup
	    (switch-to-buffer-other-frame "*scratch*")
	  (switch-to-buffer "*scratch*")))
      (if (boundp 'diary-file)
	  (progn
	    (find-file diary-file)
	    (goto-char (point-max))
	    (if (not (search-backward tmp-substring nil t))
		(progn
		  (message "Keinen IDENTISCHEN Eintrag im durchsuchten File gefunden: %s" diary-file)
		  (switch-to-buffer current-buf))
	      (show-really-everything-c)
	      (error "Eintrag gefunden"))))
	(if (boundp 'personal-diary-file)
	    (progn
	      (find-file personal-diary-file)
	      (goto-char (point-max))
	      (if (not (search-backward tmp-substring nil t))
		  (progn
		    (message "Keinen IDENTISCHEN Eintrag in den durchsuchten Files gefunden: %s %s" diary-file personal-diary-file)
		    (switch-to-buffer current-buf))
		(show-really-everything-c)
		(error "Eintrag gefunden"))))
	(if (boundp 'group-diary-file)
	    (progn
	      (find-file group-diary-file)
	      (goto-char (point-max))
	      (if (not (search-backward tmp-substring nil t))
		  (progn
		    (message "Keinen IDENTISCHEN Eintrag in den durchsuchten Files gefunden: %s %s %s" 
			     diary-file personal-diary-file group-diary-file)
		    (switch-to-buffer current-buf))
		(show-really-everything-c)
		(error "Eintrag gefunden")))))))

(defun show-really-everything-c ()
  (interactive)
  (let ((before-change-functions nil)(after-change-functions nil))
  (save-excursion
    (let ((already-modified (buffer-modified-p)))
      (widen)
      (goto-char (point-min))
      (while (search-forward "\r" nil t)
	(replace-match "\n" nil t))
      (setq selective-display nil)
      (set-buffer-modified-p already-modified)))))



(defun cde ()
  "Ruft change-diary-entry"
  (interactive)
  (change-diary-entry))





;;;
;;; Funktion herauskopiert aus
;;; diary-lib.el    66330  Emacs-Lisp	/usr/share/emacs/20.3/lisp/calendar/diary-lib.el
;;;
(if (>= emacs-major-version 20)
(defun diary-mail-entries (&optional ndays)
  "Send a mail message showing diary entries for next NDAYS days.
If no prefix argument is given, NDAYS is set to `diary-mail-days'.

You can call `diary-mail-entries' every night using an at/cron job.
For example, this script will run the program at 2am daily.  Since
`emacs -batch' does not load your `.emacs' file, you must ensure that
all relevant variables are set, as done here.

#!/bin/sh
# diary-rem.sh -- repeatedly run the Emacs diary-reminder
emacs -batch \\
-eval \"(setq diary-mail-days 3 \\
             european-calendar-style t \\
             diary-mail-addr \\\"user@host.name\\\" )\" \\
-l diary-lib -f diary-mail-entries 
at -f diary-rem.sh 0200 tomorrow

You may have to tweak the syntax of the `at' command to suit your
system.  Alternatively, you can specify a cron entry:
0 1 * * * diary-rem.sh
to run it every morning at 1am."
  (interactive "p")
  (let ((text nil)
	;; Use the fancy-diary-display as it doesn't hide rest of
	;; diary file with ^M characters.  It also looks nicer.
	(diary-display-hook 'fancy-diary-display))	
    (if (not current-prefix-arg)
	(setq ndays diary-mail-days))
    (calendar)
;; SMW ff
    (view-diary-entries-2 ndays)
;;  (set-buffer "*Fancy Diary Entries*")
    (set-buffer "*Termine*")
;; SMW
    (setq text (buffer-substring (point-min) (point-max)))

    ;; Now send text as a mail message.
    (mail)
    (mail-to)
    (insert diary-mail-addr)
    (mail-subject)
    (insert "Diary entries generated ")
    (insert (format-time-string "%a %d %b %Y" (current-time)))
    (mail-text)
    (insert text)
    (mail-send-and-exit nil)
    (exit-calendar)))
)






(if (< emacs-major-version 20)
;;;
;;; Hier gibt es so etwas wie einen Bug (keinen richtigen):
;;; Die Orginalfunktion hat ein "kill-buffer" drin, was dazu führt, daß, wenn der includete Buffer nicht
;;; abgespeichert ist, man gefragt wird, ob man diesen nicht rauswerfen will.
;;;
;;; Funktion herauskopiert aus
;;; diary-lib.el    61701  Emacs-Lisp	/usr/local/gnu/share/emacs/19.31/lisp/diary-lib.el
;;;
    (defun include-other-diary-files ()
      "Include the diary entries from other diary files with those of diary-file.
This function is suitable for use in `list-diary-entries-hook';
it enables you to use shared diary files together with your own.
The files included are specified in the diaryfile by lines of this form:
        #include \"filename\"
This is recursive; that is, #include directives in diary files thus included
are obeyed.  You can change the `#include' to some other string by
changing the variable `diary-include-string'."
      (goto-char (point-min))
      (if (buffer-file-name (get-buffer diary-file))
	  (if (buffer-modified-p (find-buffer-visiting diary-file))
	      (save-buffer (find-buffer-visiting diary-file))))
      (while (re-search-forward
	      (concat
	       "\\(\\`\\|\^M\\|\n\\)"
	       (regexp-quote diary-include-string)
	       " \"\\([^\"]*\\)\"")
	      nil t)
	(let ((diary-file (substitute-in-file-name
			   (buffer-substring-no-properties
			    (match-beginning 2) (match-end 2))))
	      ;; SMW ff
	      (diary-list-include-blanks t)  
	      ;; SMW
	      (list-diary-entries-hook 'include-other-diary-files)
	      (diary-display-hook 'ignore)
	      (diary-hook nil))
	  (if (file-exists-p diary-file)
	      (if (file-readable-p diary-file)
		  (unwind-protect
		      (progn
			(setq diary-entries-list
			      (append diary-entries-list
				      (list-diary-entries original-date number))))
		    ;; SMW ff
		    (if (buffer-file-name (get-buffer diary-file))
			(if (buffer-modified-p (find-buffer-visiting diary-file))
			    (save-buffer (find-buffer-visiting diary-file))))
		    (kill-buffer (find-buffer-visiting diary-file)))
		;; SMW
		(beep)
		(message "Can't read included diary file %s" diary-file)
		(sleep-for 2))
	    (beep)
	    (message "Can't find included diary file %s" diary-file)
	    (sleep-for 2))))
      (goto-char (point-min)))
;;;
;;; Funktion herauskopiert aus
;;; diary-lib.el    66330  Emacs-Lisp	/usr/share/emacs/20.3/lisp/calendar/diary-lib.el
;;;
  (defun include-other-diary-files ()
    "Include the diary entries from other diary files with those of diary-file.
This function is suitable for use in `list-diary-entries-hook';
it enables you to use shared diary files together with your own.
The files included are specified in the diaryfile by lines of this form:
        #include \"filename\"
This is recursive; that is, #include directives in diary files thus included
are obeyed.  You can change the `#include' to some other string by
changing the variable `diary-include-string'."
    (goto-char (point-min))
					; SMW ff
    (if (buffer-file-name (get-buffer diary-file))
	(if (buffer-modified-p (find-buffer-visiting diary-file))
	    (save-buffer (find-buffer-visiting diary-file))))
					; SMW
    (while (re-search-forward
	    (concat
	     "\\(\\`\\|\^M\\|\n\\)"
	     (regexp-quote diary-include-string)
	     " \"\\([^\"]*\\)\"")
	    nil t)
      (let ((diary-file (substitute-in-file-name
			 (buffer-substring-no-properties
			  (match-beginning 2) (match-end 2))))
	    (diary-list-include-blanks nil)
	    ;; SMW ff
	    (diary-list-include-blanks t)  
	    ;; SMW
	    (list-diary-entries-hook 'include-other-diary-files)
	    (diary-display-hook 'ignore)
	    (diary-hook nil))
	(if (file-exists-p diary-file)
	    (if (file-readable-p diary-file)
		(unwind-protect
		    (setq diary-entries-list
			  (append diary-entries-list
				  (list-diary-entries original-date number)))
		  ;; SMW ff
		  (if (buffer-file-name (get-buffer diary-file))
		      (if (buffer-modified-p (find-buffer-visiting diary-file))
			  (save-buffer (find-buffer-visiting diary-file))))
		  ;; SMW
		  (kill-buffer (find-buffer-visiting diary-file)))
	      (beep)
	      (message "Can't read included diary file %s" diary-file)
	      (sleep-for 2))
	  (beep)
	  (message "Can't find included diary file %s" diary-file)
	  (sleep-for 2))))
    (goto-char (point-min)))
  )


(defun remove-double-list-entries (a)
  ;;  ;; entferne doppelte "diary-noentry-string"-Elemente -- nicht vollständig getestet
  ;; Testversion:
  ;;  (setq a (list (list 1 2) (list 3 4) (list 1 2) (list 3 4) (list 3 4)(list 3 2)))
  ;;  (setq num 0)
  ;;  (while (< num 4)
  ;;    (setq num (1+ num))
  ;;    (if (member (nth num a) (nthcdr num a))
  ;;	(progn
  ;;	  (setq b (list (nth num a)))
  ;;	  (setq a (append b (delete (nth num a) a)))))
  ;;    (nthcdr 1 a))
  (let ((num 0))
    (while (< num (1- (length a)))
      (if (member (nth num a) (nthcdr num a))
	  (progn
	    (setq b (list (nth num a)))
	    (setq a (append (delete (nth num a) a) b))))
      (setq num (1+ num))))
  a)






(message "Kalender geladen bis Marke 8")


(if (< emacs-major-version 20)
;;;
;;; Funktion herauskopiert aus
;;; diary-lib.el    61701  Emacs-Lisp	/usr/local/gnu/share/emacs/19.31/lisp/diary-lib.el
;;;
    (defun list-diary-entries (date number)
      "Create and display a buffer containing the relevant lines in diary-file.
The arguments are DATE and NUMBER; the entries selected are those
for NUMBER days starting with date DATE.  The other entries are hidden
using selective display.

Returns a list of all relevant diary entries found, if any, in order by date.
The list entries have the form ((month day year) string).  If the variable
`diary-list-include-blanks' is t, this list includes a dummy diary entry
\(consisting of the empty string) for a date with no diary entries.

After the list is prepared, the hooks `nongregorian-diary-listing-hook',
`list-diary-entries-hook', `diary-display-hook', and `diary-hook' are run.
These hooks have the following distinct roles:

    `nongregorian-diary-listing-hook' can cull dates from the diary
        and each included file.  Usually used for Hebrew or Islamic
        diary entries in files.  Applied to *each* file.

    `list-diary-entries-hook' adds or manipulates diary entries from
        external sources.  Used, for example, to include diary entries
        from other files or to sort the diary entries.  Invoked *once* only,
        before the display hook is run.

    `diary-display-hook' does the actual display of information.  If this is
        nil, simple-diary-display will be used.  Use add-hook to set this to
        fancy-diary-display, if desired.  If you want no diary display, use
        add-hook to set this to ignore.

    `diary-hook' is run last.  This can be used for an appointment
        notification function."

      (if (< 0 number)
	  (let* ((original-date date);; save for possible use in the hooks
		 (old-diary-syntax-table)
		 (diary-entries-list)
		 (date-string (calendar-date-string date))
		 (d-file (substitute-in-file-name diary-file)))
       
	    (message "Preparing diary...")
	    (save-excursion
	      (let ((diary-buffer (find-buffer-visiting d-file)))
		(set-buffer (if diary-buffer
				diary-buffer
			      (find-file-noselect d-file t))))
	      (setq selective-display t)
	      (setq selective-display-ellipses nil)
	      (setq old-diary-syntax-table (syntax-table))
	      (set-syntax-table diary-syntax-table)
	      (unwind-protect
		  (let ((buffer-read-only nil)
			(diary-modified (buffer-modified-p))
			(mark (regexp-quote diary-nonmarking-symbol)))
		    (goto-char (1- (point-max)))
		    (if (not (looking-at "\^M\\|\n"))
			(progn
			  (forward-char 1)
			  (insert-string "\^M")))
		    (goto-char (point-min))
		    (if (not (looking-at "\^M\\|\n"))
			(insert-string "\^M"))
		    (subst-char-in-region (point-min) (point-max) ?\n ?\^M t)
		    (calendar-for-loop i from 1 to number do
				       (let ((d diary-date-forms)
					     (month (extract-calendar-month date))
					     (day (extract-calendar-day date))
					     (year (extract-calendar-year date))
					     (entry-found (list-sexp-diary-entries date)))
;;;; debug: bis hierher Datumsvorbereitung
;;;; jetzt: Suche
					 (while d
					   (let*
					       ((date-form (if (equal (car (car d)) 'backup)
							       (cdr (car d))
							     (car d)))
						(backup (equal (car (car d)) 'backup))
						(dayname
						 (concat
						  (calendar-day-name date) "\\|"
						  (substring (calendar-day-name date) 0 3) ".?"))
						(monthname
						 (concat
						  "\\*\\|"
						  (calendar-month-name month) "\\|"
						  (substring (calendar-month-name month) 0 3) ".?"))
						(month (concat "\\*\\|0*" (int-to-string month)))
						(day (concat "\\*\\|0*" (int-to-string day)))
						(year
						 (concat
						  "\\*\\|0*" (int-to-string year)
						  (if abbreviated-calendar-year
						      (concat "\\|" (int-to-string (% year 100)))
						    "")))
						(regexp
						 (concat
						  "\\(\\`\\|\^M\\|\n\\)" mark "?\\("
						  (mapconcat 'eval date-form "\\)\\(")
						  "\\)"))
						(case-fold-search t))
					     (goto-char (point-min))
					     (while (re-search-forward regexp nil t)
					       (if backup (re-search-backward "\\<" nil t))
					       (if (and (or (char-equal (preceding-char) ?\^M)
							    (char-equal (preceding-char) ?\n))
							(not (looking-at " \\|\^I")))
						   ;;  Diary entry that consists only of date.
						   (backward-char 1)
						 ;; Found a nonempty diary entry--make it visible and
						 ;; add it to the list.
						 (setq entry-found t)
						 (let ((entry-start (point))
						       (date-start))
						   (re-search-backward "\^M\\|\n\\|\\`")
						   (setq date-start (point))
						   (re-search-forward "\^M\\|\n" nil t 2)
						   (while (looking-at " \\|\^I")
						     (re-search-forward "\^M\\|\n" nil t))
						   (backward-char 1)
						   (subst-char-in-region date-start
									 (point) ?\^M ?\n t)
						   (add-to-diary-list
						    date
						    (buffer-substring-no-properties
						     entry-start (point)))))))
					   (setq d (cdr d)))
;;;; debug: Vorbereiten der Anzeige
					 (or entry-found
					     (not diary-list-include-blanks)
					     (setq diary-entries-list 
						   (nconc diary-entries-list
							  (list (list date diary-noentry-string))))) ;SMW
;;;; Dies letzte Zeile war eine Änderung von mir

					 (setq date
					       (calendar-gregorian-from-absolute
						(1+ (calendar-absolute-from-gregorian date))))
					 (setq entry-found nil)))
		    (set-buffer-modified-p diary-modified))
		(set-syntax-table old-diary-syntax-table))
	      (goto-char (point-min))
	      (run-hooks 'nongregorian-diary-listing-hook
			 'list-diary-entries-hook)
	      (if diary-display-hook
		  (run-hooks 'diary-display-hook)
		(simple-diary-display))
	      (run-hooks 'diary-hook)
	      diary-entries-list))))
;;;
;;; Funktion herauskopiert aus
;;; diary-lib.el    66330  Emacs-Lisp	/usr/share/emacs/20.3/lisp/calendar/diary-lib.el
;;;
  (defun list-diary-entries (date number)
    "Create and display a buffer containing the relevant lines in diary-file.
The arguments are DATE and NUMBER; the entries selected are those
for NUMBER days starting with date DATE.  The other entries are hidden
using selective display.

Returns a list of all relevant diary entries found, if any, in order by date.
The list entries have the form ((month day year) string specifier) where
\(month day year) is the date of the entry, string is the entry text, and
specifier is the applicability.  If the variable `diary-list-include-blanks'
is t, this list includes a dummy diary entry consisting of the empty string)
for a date with no diary entries.

After the list is prepared, the hooks `nongregorian-diary-listing-hook',
`list-diary-entries-hook', `diary-display-hook', and `diary-hook' are run.
These hooks have the following distinct roles:

    `nongregorian-diary-listing-hook' can cull dates from the diary
        and each included file.  Usually used for Hebrew or Islamic
        diary entries in files.  Applied to *each* file.

    `list-diary-entries-hook' adds or manipulates diary entries from
        external sources.  Used, for example, to include diary entries
        from other files or to sort the diary entries.  Invoked *once* only,
        before the display hook is run.

    `diary-display-hook' does the actual display of information.  If this is
        nil, simple-diary-display will be used.  Use add-hook to set this to
        fancy-diary-display, if desired.  If you want no diary display, use
        add-hook to set this to ignore.

    `diary-hook' is run last.  This can be used for an appointment
        notification function."

    (if (< 0 number)
	(let* ((original-date date);; save for possible use in the hooks
	       (old-diary-syntax-table)
	       (diary-entries-list)
	       (date-string (calendar-date-string date))
	       (d-file (substitute-in-file-name diary-file)))
	  (message "Preparing diary...")
	  (save-excursion
	    (let ((diary-buffer (find-buffer-visiting d-file)))
	      (if (not diary-buffer)
		  (set-buffer (find-file-noselect d-file t))
		(set-buffer diary-buffer)
		(if (buffer-file-name (get-buffer diary-file)) ;SMW
		    (or (verify-visited-file-modtime diary-buffer)
			(revert-buffer t t)))
		))
	    (setq selective-display t)
	    (setq selective-display-ellipses nil)
	    (setq old-diary-syntax-table (syntax-table))
	    (set-syntax-table diary-syntax-table)
	    (unwind-protect
		(let ((buffer-read-only nil)
		      (diary-modified (buffer-modified-p))
		      (mark (regexp-quote diary-nonmarking-symbol)))
		  (goto-char (1- (point-max)))
		  (if (not (looking-at "\^M\\|\n"))
		      (progn
			(forward-char 1)
			(insert-string "\^M")))
		  (goto-char (point-min))
		  (if (not (looking-at "\^M\\|\n"))
		      (insert-string "\^M"))
		  (subst-char-in-region (point-min) (point-max) ?\n ?\^M t)
		  (calendar-for-loop i from 1 to number do
				     (let ((d diary-date-forms)
					   (month (extract-calendar-month date))
					   (day (extract-calendar-day date))
					   (year (extract-calendar-year date))
					   (entry-found (list-sexp-diary-entries date)))
				       (while d
					 (let*
					     ((date-form (if (equal (car (car d)) 'backup)
							     (cdr (car d))
							   (car d)))
					      (backup (equal (car (car d)) 'backup))
					      (dayname
					       (concat
						(calendar-day-name date) "\\|"
						(substring (calendar-day-name date) 0 3) ".?"))
					      (monthname
					       (concat
						"\\*\\|"
						(calendar-month-name month) "\\|"
						(substring (calendar-month-name month) 0 3) ".?"))
					      (month (concat "\\*\\|0*" (int-to-string month)))
					      (day (concat "\\*\\|0*" (int-to-string day)))
					      (year
					       (concat
						"\\*\\|0*" (int-to-string year)
						(if abbreviated-calendar-year
						    (concat "\\|" (int-to-string (% year 100)))
						  "")))
					      (regexp
					       (concat
						"\\(\\`\\|\^M\\|\n\\)" mark "?\\("
						(mapconcat 'eval date-form "\\)\\(")
						"\\)"))
					      (case-fold-search t))
					   (goto-char (point-min))
					   (while (re-search-forward regexp nil t)
					     (if backup (re-search-backward "\\<" nil t))
					     (if (and (or (char-equal (preceding-char) ?\^M)
							  (char-equal (preceding-char) ?\n))
						      (not (looking-at " \\|\^I")))
						 ;;  Diary entry that consists only of date.
						 (backward-char 1)
					       ;; Found a nonempty diary entry--make it visible and
					       ;; add it to the list.
					       (setq entry-found t)
					       (let ((entry-start (point))
						     (date-start))
						 (re-search-backward "\^M\\|\n\\|\\`")
						 (setq date-start (point))
						 (re-search-forward "\^M\\|\n" nil t 2)
						 (while (looking-at " \\|\^I")
						   (re-search-forward "\^M\\|\n" nil t))
						 (backward-char 1)
						 (subst-char-in-region date-start
								       (point) ?\^M ?\n t)
						 (add-to-diary-list
						  date
						  (buffer-substring-no-properties
						   entry-start (point))
						  (buffer-substring-no-properties
						   (1+ date-start) (1- entry-start)))))))
					 (setq d (cdr d)))
;;;; debug: Vorbereiten der Anzeige
				       (or entry-found
					   (not diary-list-include-blanks)
					   (setq diary-entries-list 
						 (nconc diary-entries-list
							(list (list date diary-noentry-string))))) ;SMW
;;;; Dies letzte Zeile war eine Änderung von mir
				       (setq date
					     (calendar-gregorian-from-absolute
					      (1+ (calendar-absolute-from-gregorian date))))
				       (setq entry-found nil)))
		  (set-buffer-modified-p diary-modified))
	      (set-syntax-table old-diary-syntax-table))
	    (goto-char (point-min))
	    (run-hooks 'nongregorian-diary-listing-hook
		       'list-diary-entries-hook)
	    (if diary-display-hook
		(run-hooks 'diary-display-hook)
	      (simple-diary-display))
	    (run-hooks 'diary-hook)
	    diary-entries-list))))
  )


(message "Kalender geladen bis Marke 9")


(require 'cal-x)

(setq calendar-frame-parameters
  '((name . "Kalender") (minibuffer . t) (height . 10) (width . 80)
    (unsplittable . t) (vertical-scroll-bars . nil)))

(defun calendar-in-extra-window (&optional arg)
  "Start calendar and diary in separate, dedicated frames."
  (interactive "p")
  (if arg (if (= arg 1) (setq arg nil)))
  (if (not window-system)
      (calendar-basic-setup arg)
    (if (< emacs-major-version 20)
	(standard-display-european t))
    (setq display-time-24hr-format t)
    (setq display-time-day-and-date t)
    (setq calendar-setup 'calendar-in-extra-window)
    (if (frame-live-p calendar-frame) (delete-frame calendar-frame))
    ;;(if (frame-live-p diary-frame) (delete-frame diary-frame))
    (add-hook 'diary-hook 'change-diary-frame-position)
    (let ((pop-up-windows nil)
          (view-diary-entries-initially nil)
          (special-display-buffer-names nil))
      ;;
      (setq pop-up-frame-function
	    (function (lambda ()
			(make-frame pop-up-frame-alist))))
      (setq special-display-function 'special-display-popup-frame)
      ;;
      (save-window-excursion
        (save-excursion (calendar-basic-setup arg))
        (setq calendar-frame (make-frame calendar-frame-parameters))
        (run-hooks 'calendar-after-frame-setup-hooks)
        (select-frame calendar-frame)
        (if (eq 'icon (cdr (assoc 'visibility
                                  (frame-parameters calendar-frame))))
            (iconify-or-deiconify-frame))
        (display-buffer calendar-buffer)
        (set-window-dedicated-p (selected-window) 'calendar)
	;;
	;;
	;; hier werden die Feiertage geladen !!!
	(require 'op-calendar-holidaylist)
	;;
	;;
	(calendar-recenter)
	(setq calendar-setup 'calendar-in-extra-window)
	(if mark-calendar-holidays-initially
	    (mark-calendar-holidays))
	(calendar-day-message)
	(if mark-sundays (mark-calendar-days-named 7)) ;; alle Sonntage rot einfärben
	(define-key calendar-mode-map "d" 'view-diary-entries-2)
	(define-key calendar-mode-map "D" 'view-other-diary-entries-2)
        (run-hooks 'calendar-after-frame-setup-hooks)))))
(defun change-diary-frame-position ()
  (let ((calendar-frame-x-position (cdr (assoc 'left (frame-parameters (selected-frame)))))
	(calendar-frame-y-position (cdr (assoc 'top (frame-parameters (selected-frame)))))
	(calendar-frame-scroll-bar-width 2) 
	(calendar-frame-width (cdr (assoc 'width (frame-parameters (selected-frame)))))
	(diary-frame-width 2)
	(scroll-bar-width 2))
    (if (listp calendar-frame-x-position)
	(setq calendar-frame-x-position (nth 1 calendar-frame-x-position)))
    (if (listp calendar-frame-y-position)
	(setq calendar-frame-y-position (nth 1 calendar-frame-y-position)))
    (switch-to-buffer-other-frame fancy-diary-buffer)
    ;;
    ;; Suche die Zeile mit den ===== - Zeichen und bestimme die Länge
    ;; Setze dann daraus die Breite des anzuzeigenden Rahmens fest
    (goto-char (point-min))
    (if (search-forward "=============" nil t)
	(progn
	  (let ((tmp (save-excursion (beginning-of-line)(point))))
	    (end-of-line)
	    (setq diary-frame-width (1+ (- (point) tmp)))
	    (forward-char 1))))
    ;;
    ;;
    ;; setzt die Rahmenposition des diary-Fensters
    ;; in Abhängigkeit vom jeweiligen Fenstersystem
    ;;
    ;(require 'backquote)
    ;;
    ;;(setq scroll-bar-width 
    ;;	 (cdr (assoc 'scroll-bar-width (frame-parameters (selected-frame)))))
    (setq diary-frame-width (max (- calendar-frame-width scroll-bar-width) diary-frame-width))
    (cond ((equal window-system 'x)
	   (modify-frame-parameters (selected-frame)
				    `((name . "Eingetragene Termine") ; ` damin das Komma in der Liste evaluiert wird
				      (vertical-scroll-bars . t) 
				      (width . ,diary-frame-width) ; , zum evaluieren des Characters
				      (height . 22))))
	  ;; (- calendar-frame-width calendar-frame-scroll-bar-width)))))
	  ((equal window-system 'win32)
	   (modify-frame-parameters (selected-frame) 
				    `((name . "Eingetragene Termine") 
				      (vertical-scroll-bars . t) 
				      (width . ,diary-frame-width) ; , zum evaluieren des Characters
				      (height . 22)))))
    (set-frame-position (selected-frame) 
			calendar-frame-x-position 
			(+ calendar-frame-y-position 200))
    (switch-to-buffer-other-frame calendar-buffer)
    ))



(message "Kalender geladen bis Marke 10")


(defun view-diary-entries-2 (&optional args)
  "Documentation see view-diary-entries, but some features more 
for natural behavior"
  (interactive "P")
  (let ((current-frame (selected-frame))
	(current-buf (current-buffer))
	(calendar-setup-from-calendar-buffer calendar-setup))
    ;; sehe nach, ob sich einer der diary-files geändert hat
    ;;     (diese Zeilen nutzen den Seiteneffekt von find-file-noselect, dass dieses auf das
    ;;     aktuelle Synchronisation des Files und des zugehörigen Buffers sieht)
    (if (boundp 'group-diary-file) (find-file-noselect group-diary-file))
    (if (boundp 'diary-file) (find-file-noselect diary-file))
    ;;
    ;;
    ;; falls calendar-setup nil: mache eventuell ein zweites Fenster und wechsle dorthin
    (if (not calendar-setup)
	(progn
	  (setq pop-up-windows t)
	  (setq pop-up-frame-function 'switch-to-buffer)
	  (setq special-display-function 'switch-to-buffer)
	  (setq diary-hook (delq 'change-diary-frame-position diary-hook))
	  (if (one-window-p)
	      (split-window (selected-window) 8))  ;; 8 = Kalender-Höhe
	  ))
    (view-diary-entries (if (not args) 1 args))
    (let ((calbuf (current-buffer)))
      ;;
      (set-buffer (find-file-noselect group-diary-file))
      (let ((modp (buffer-modified-p)))
	(save-excursion
	  (goto-char (point-min))
	  (replace-regexp "\r" "\n"))
	(set-buffer-modified-p modp))
      ;;
      (set-buffer (find-file-noselect diary-file))
      (let ((modp (buffer-modified-p)))
	(save-excursion
	  (goto-char (point-min))
	  (replace-regexp "\r" "\n"))
	(set-buffer-modified-p modp))
      ;;

      (if window-system
	  (progn
	    (hilit-add-pattern 
	     "^Montag\\|^Dienstag\\|^Mittwoch\\|^Donnerstag\\|^Freitag\\|^Samstag\\|^Sonntag"
	     "^=+$" 'struct)
	    (hilit-repaint-command t)))
		  
      (if calendar-setup-from-calendar-buffer
	  (progn
	    (switch-to-buffer-other-frame fancy-diary-buffer)
	    (setq calendar-setup calendar-setup-from-calendar-buffer)
	    (calendar-set-mode-line "Eingetragene Termine")
	    (remove-strange-noentry-entries)
	    (local-set-key "\C-o" 'change-focus)
	    (if window-system
		(hilit-repaint-command t))
	    (switch-to-buffer-other-frame calendar-buffer))
	(switch-to-buffer calendar-buffer)
	(other-window 1)
	(switch-to-buffer fancy-diary-buffer)
	(calendar-set-mode-line "Eingetragene Termine")
	(remove-strange-noentry-entries)
	(setq calendar-setup calendar-setup-from-calendar-buffer)
	(local-set-key "\C-o" 'change-focus)
	(if window-system
	    (hilit-repaint-command t))
	(other-window -1)))
    (calendar-day-message)))


(defun view-other-diary-entries-2 (filename &optional args)
  (interactive "fTerminkalenderfile: \npAnzahl der auzuzeigenden Tage [1]: ")
  (let ((current-frame (selected-frame))
	(current-buf (current-buffer))
	(diary-file filename)
	(calendar-setup-from-calendar-buffer calendar-setup))
    ;; sehe nach, ob sich einer der diary-files geändert hat
    ;;     (diese Zeilen nutzen den Seiteneffekt von find-file-noselect, dass dieses auf das
    ;;     aktuelle Synchronisation des Files und des zugehörigen Buffers sieht)
    (if (boundp 'group-diary-file) (find-file-noselect group-diary-file))
    (if (boundp 'diary-file) (find-file-noselect diary-file))
    ;;

    ;;
    ;; falls calendar-setup nil: mache eventuell ein zweites Fenster und wechsle dorthin
    ;;
    (if (not calendar-setup)
	(progn
	  (setq pop-up-windows t)
	  (setq pop-up-frame-function 'switch-to-buffer)
	  (setq special-display-function 'switch-to-buffer)
	  (setq diary-hook (delq 'change-diary-frame-position diary-hook))
	  (if (one-window-p)
	      (split-window (selected-window) 8))  ;; 8 = Kalender-Höhe
	  ))
    
    (view-diary-entries (if (not args) 1 args))

    (let ((calbuf (current-buffer)))
      
      (set-buffer (find-file-noselect group-diary-file))
      (let ((modp (buffer-modified-p)))
	(save-excursion
	  (goto-char (point-min))
	  (replace-regexp "\r" "\n"))
	(set-buffer-modified-p modp)
	(setq calendar-setup calendar-setup-from-calendar-buffer))
      (set-buffer (find-file-noselect diary-file))
      (let ((modp (buffer-modified-p)))
	(save-excursion
	  (goto-char (point-min))
	  (replace-regexp "\r" "\n"))
	(set-buffer-modified-p modp)
	(setq calendar-setup calendar-setup-from-calendar-buffer))
      (if calendar-setup
	  (progn
	    (switch-to-buffer-other-frame fancy-diary-buffer)
	    (calendar-set-mode-line "Eingetragene Termine")
	    (remove-strange-noentry-entries)
	    (local-set-key "\C-o" 'change-focus)
	    (if window-system
		(hilit-repaint-command t))
	    (switch-to-buffer-other-frame calendar-buffer))
	(switch-to-buffer calendar-buffer)
	(other-window 1)
	(switch-to-buffer fancy-diary-buffer)
	(calendar-set-mode-line "Eingetragene Termine")
	(remove-strange-noentry-entries)
	(local-set-key "\C-o" 'change-focus)
	(if window-system
	    (hilit-repaint-command t))
	(other-window -1)))
    (calendar-day-message)))

(defun remove-strange-noentry-entries ()
  "Bugkorrektur auf der optischen Seite, nicht schön, aber ohne 
extrem großen Aufwand nicht anders zu machen
(unter bestimmten Umständen taucht manchmal ein dirary-noentry-string 
zuviel im Kalender auf. Dieser wird in dieser Subroutine entfernt.)"
	    (save-excursion
	      (toggle-read-only t)
	      (toggle-read-only)
	      (goto-char (point-min))
	      (let ((tmppnt 0))
		(while (search-forward diary-noentry-string nil t)
		  (goto-char (match-beginning 0))
		  (setq tmppnt (match-end 0))
		  (backward-char 7)
		  (if (not (looking-at "======"))
		      (progn
			(forward-line 1)
			(beginning-of-line)
			(kill-line 1))
		    (goto-char tmppnt)
		    (next-line 1)
		    (if (not (looking-at "^$"))
			(progn
			  (previous-line 1)
			  (beginning-of-line)
			  (kill-line 1))))))
	      (toggle-read-only t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Neuigkeitenbufferfenster

(define-key calendar-mode-map [menu-bar news]
  (cons "Neues" (make-sparse-keymap "Nachrichten")))

(define-key calendar-mode-map [menu-bar news news-infofile]
  '("Allgemeine Nachrichten" . view-information-file))

(defun view-information-file ()
  (interactive)
  (find-file-noselect information-file)
  (let ((pop-up-windows nil))
    (display-buffer (get-file-buffer information-file))
    (set-buffer (get-file-buffer information-file))
    (calendar-set-mode-line "Nachrichten")
    ;;
    (setq buffer-read-only t)
    (goto-char (point-min))
    ;;
    (setq my-mode-map (cons 'keymap text-mode-map))
    (local-set-key "q" '(lambda ()(interactive)(kill-buffer (current-buffer))(iconify-frame)))
    (message "q verläßt den Buffer")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; gefährlich
(define-key (current-global-map) "\C-x\C-y" 'iconify-frame)
(if (not xemacsp)
    (define-key menu-bar-tools-menu [iconify]
      '("Fenster iconifizieren" . iconify-frame)))



(message "Kalender geladen bis Marke 11")

;;;================================================================================
;;;================================================================================
;;;================================================================================
;;;================================================================================
;;;================================================================================

;
;
; Funktionen zum Erstellen des Kalenders aus dem gcal-Ausgabefile
;
;



;;; Zwei noch per Hand nachgetragene Termine in der Liste: Siebenschläfer und Johannisnacht
;)(holiday-extract-from-list '((11 20 1960) "Totensonntag (DE) [Merktag]")
;)(holiday-extract-from-list '((12 24 1960) "Heiligabend (Std) [Merktag]")
;)(holiday-extract-from-list '((12 31 1960) "Silvester (Std) [Merktag]")
;)
;(holiday-fixed 7 27 "Siebenschläfer [Merktag]")
;(holiday-fixed 6 21 "Johannisnacht [+\"Fete de la musique\"] [Merktag]")
;))

(defun insert-gcal-holidays-in-calendar (infile)
  "Macht aus einem gcal-Output-File einen emacs-Holiday-File,
  der dann durch ein eval bzw. load ausgeführt werden kann.
  der Ausgabefile ist unter \"op-project/op-calendar-holidaylist.el\"
  festŽim Code eingetragen."
  (interactive "FFilename of gcal output file: ")
  (find-file infile)
  (goto-char (point-max))
  (insert-char ?\n 3)
  (goto-char (point-min))
  ;;
  (let (mark1 mark2 feiertagsname tag monat jahr lokalfeiertag 
	      kallist-a kallist-b kallist-c nichtdeutscherfeiertag)
    ;; while-loop über alle Kalender aller Jahre
    (while (search-forward "Ewige Feiertagsliste:" nil t)
      (forward-line 2)
      (beginning-of-line)
      ;;
      ;; while-loop über die Zeilen der Feiertage der einzelnen Jahre
      (while (not (looking-at "^$"))
	;; Anfang Extraktion des Formates in eine Liste
	(beginning-of-line)
	(setq mark1 (point)) ; mark1 = Anfang der Zeile
	(re-search-forward "\\( Mo, \\| Di, \\| Mi, \\| Do, \\| Fr, \\| Sa, \\| So, \\)" 
			   (save-excursion (end-of-line)(point))
			   nil) ; Suche: Mo, Di,... - bei Fehler: breche ab
	(goto-char (match-beginning 0))
	;;
	;;
	;; zum Zeichen vor dem Wochentag
	;;
	(backward-char 1) ; Zeichen vor Mo, Di, ...
	(save-excursion
	  (backward-char)
	  ;; extrahiere den Feiertagsnamen
	  (re-search-backward "[^ \t]" nil nil)
	  (forward-char 1)
	  (setq feiertagsname (regexp-quote (buffer-substring-no-properties mark1 (point)))))
	  ;;
	  ;;
	  ;; extraire die Datumstrings
	  ;;
	(save-excursion
	  (search-forward "," nil nil)
	  (forward-word 1)
	  (backward-word 1)
	  (setq mark2 (point))
	  (setq tag (string-to-number (current-longword-quote-2)))
	  (forward-word 2)
	  (backward-char 1)
	  (setq monat (string-to-number (current-longword-quote-2)))
	  (forward-word 2)
	  (backward-char 1)
	  (setq jahr (string-to-number (current-longword-quote-2)))
	  )
	;;
	;;
	;; Suche danach, in welchem Land dieser Tag Feiertag ist -
	;; wenn er kein deutscher oder Standardfeiertag ist, dann setzte
	;; den Tag auf Kategorie "Merktag".
	;; Ebenso wird auf "Merktag" gesetzt, wenn das gcal-Zeichen 
	;; ein "-" ist
	;;
	(save-excursion
	  (beginning-of-line)
	  (re-search-forward "(\\(.*\\))" nil nil)
	  (backward-char 2)
	  (if (not (or (string= (current-longword-quote-2) "(DE)")
		       (string= (current-longword-quote-2) "(Std)")))
	      (setq nichtdeutscherfeiertag t)
	    (setq nichtdeutscherfeiertag nil)))
	;; interpretiere den String vor dem Wochentag für ausländische Tage
	(cond ((looking-at "-")
	       (setq lokalfeiertag "[Besonderer Tag im Ausland]"))
	      ((looking-at "#")  
	       (setq lokalfeiertag "[Feiertag im Ausland (im Großteil des Landes)]"))
	      ((looking-at "\\*")
	       (setq lokalfeiertag "[Feiertag im Ausland (in einem Teil des Landes)]"))
	      ((looking-at "\\+")
	       (setq lokalfeiertag "[Feiertag im Ausland]")))
	;;
	;;
	;; interpretiere den String vor dem Wochentag bei deutschen Feiertagen
	(cond ((or (looking-at "-")
		   nichtdeutscherfeiertag)
	       (if nichtdeutscherfeiertag
		   ;; Auslandsfeiertagsliste
		   (setq kallist-c (nconc kallist-c 
					   (list (concat "((" monat " " tag " " jahr ") \""
							 feiertagsname 
							 " [" lokalfeiertag "]\")\n"))))
		 ;; Inlandsmerktagsliste
		 (setq lokalfeiertag "Merktag")
		 (setq kallist-a (nconc kallist-a 
					 (list (concat "((" monat " " tag " " jahr ") \""
						       feiertagsname 
						       " [" lokalfeiertag "]\")\n"))))))
	      ((looking-at "#")
	       (if (not nichtdeutscherfeiertag)(setq lokalfeiertag "Feiertag in den meisten Bundesländern"))
	       (setq kallist-b (nconc kallist-b 
				       (list (concat "((" monat " " tag " " jahr ") \""
						     feiertagsname 
						     " [" lokalfeiertag "]\")\n")))))
	      ((looking-at "\\*")
	       (if (not nichtdeutscherfeiertag)(setq lokalfeiertag "Feiertag in einigen Bundesländern"))
	       (setq kallist-b (nconc kallist-b 
				       (list (concat "((" monat " " tag " " jahr ") \""
						     feiertagsname 
						     " [" lokalfeiertag "]\")\n")))))
	      ((looking-at "\\+")
	       (if (not nichtdeutscherfeiertag)(setq lokalfeiertag "Gesetzlicher Feiertag"))
	       (setq kallist-b (nconc kallist-b 
				       (list (concat "((" monat " " tag " " jahr ") \""
						     feiertagsname 
						     " [" lokalfeiertag "]\")\n"))))))
	;;
	;;
	;; neue Position für die while-loop
	(beginning-of-line)
	(forward-line 1)
	;;(if (looking-at "St. ")(debug))
	))
    ;;  ; einfügen vornadran
    (goto-char (point-min))
    (replace-regexp "\\(^\\)" "\\1;")
    ;; hänge an den File ein Stück gültigen Elisp-Code, 
    ;; der den Feiertagen des Emacs-Calenders entspricht
    (goto-char (point-max))
    ;;
    ;;
    ;;
    ;; setze den zu evaluierenden Code ein
    ;;
    (insert "\n\n\n\50setq merktage\n'\50")
    (while (car kallist-a)
      (insert "\50holiday-extract-from-list '" (car kallist-a) "\51")
      (setq kallist-a (cdr kallist-a)))
    ;;
    ;;
    ;; nachträglich "per Hand" eingetragene Tage
    ;;
    (insert "\n\50holiday-fixed 4 30 \"zum 1. Mai: Wallpurgisnacht [Merktag]\"\51")
    (insert "\n\50holiday-fixed 6 27 \"Siebenschläfer [Merktag]\"\51")
    (insert "\n\50holiday-fixed 6 21 \"Johannisnacht [+\\\"Fete de la musique\\\"] [Merktag]\"\51")
    ;;
    (insert "\n\51\51\n")
    ;;
    ;;
    ;;
    ;;
    (insert "\n\n\n\50setq gcal-feiertage\n'\50")
    (while (car kallist-b)
      (insert "\50holiday-extract-from-list '" (car kallist-b) "\51")
      (setq kallist-b (cdr kallist-b)))
    (insert "\51\51\n")
    ;;
    (insert "\n\n\n\50setq auslandsfeiertage\n'\50")
    (while (car kallist-c)
      (insert "\50holiday-extract-from-list '" (car kallist-c) "\51")
      (setq kallist-c (cdr kallist-c)))
    (insert "\51\51\n")
    ;;
    ;;
    ;;
    ;; zu guter letzt noch: die Kalender-Liste bekommt noch die Tage dazu,
    ;; die nicht markiert werden sollen, aber angezeigt werden sollen
    ;; (siehe Feiertagskategorie "Merktag")
    (insert "\n\n\n\50setq calendar-holidays
	    \50append calendar-holidays gcal-feiertage merktage auslandsfeiertage\51\51\n\n")
    ;;
    ;;
    (insert "\n\n\n\50provide 'op-calendar-holidaylist\51\n\n\n")
    ;;
    ;;
    ;; ändere leicht die grammatische Struktur
    (goto-char (point-min))
    (replace-string " Tag (" "-Tag (")
    ;;
    ;;
    ;; compiliere den File zum Laden
    (write-file "~walther/op-project/op-calendar-holidaylist.el")
    (byte-compile-file "~walther/op-project/op-calendar-holidaylist.el")
    ))



(message "Kalender geladen bis Marke 12")


(defun current-longword-quote-2 ()
  (let ((begin) (end))
    (save-excursion
      (if (re-search-backward "[ \t\r]\\|^" nil t)
	      (progn
		(if (looking-at "[ \t\r]")
		    (forward-char 1))))
      (setq begin (point))
      (if (re-search-forward "[ \t\r]\\|$" nil t)
	  (progn
	    (backward-char 1)
	    (if (not (looking-at "[ \t\r]"))
		(forward-char 1))))
      (setq end (point)))
    (regexp-quote (buffer-substring-no-properties begin end))))



(message "Kalender geladen bis Marke 13")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Das hier passt mir die Tabs beim Diary-Einsetzen an meinen Stil
;; (Leerzeichen vornedran und alle Diary-Blöcke zusammen)
;; im diary-File an (aus diary-ins.el)
;;
;;; gefährlich !!x		
(defun make-diary-entry (string &optional nonmarking file)
  "Insert a diary entry STRING which may be NONMARKING in FILE.
If omitted, NONMARKING defaults to nil and FILE defaults to diary-file."
  (find-file-other-window
   (substitute-in-file-name (if file file diary-file)))
  (save-excursion
    (goto-char (point-min))
    (replace-regexp "\r" "\n"))
  (goto-char (point-max))
  (if (string-match "(diary-block" string)
      (progn
	(if (re-search-backward "(diary-block" nil t)
	    (progn
	      (end-of-line)
	      (re-search-forward "^[ \t]*$" nil t)
	      (insert "\n")))))
  (if (string-match "(diary-cyclic" string)
      (progn
	(if (re-search-backward "(diary-cyclic" nil t)
	    (progn
	      (end-of-line)
	      (re-search-forward "^[ \t]*$" nil t)
	      (insert "\n")))))
  (if (string-match "(diary-anniversary" string)
      (progn
	(if (re-search-backward "(diary-anniversary" nil t)
	    (progn
	      (end-of-line)
	      (re-search-forward "^[ \t]*$" nil t)
	      (insert "\n")))))
  (if (string-match "(diary-weekday" string)
      (progn
	(if (re-search-backward "(diary-anniversary" nil t)
	    (progn
	      (end-of-line)
	      (re-search-forward "^[ \t]*$" nil t)
	      (insert "\n")))))
  (if (string-match "Sonntag\\|Montag\\|Dienstag\\|Mittwoch\\|Donnerstag\\|Freitag\\|Samstag" string)
      (insert
       (if (bolp) "" "\n")
       (if nonmarking diary-nonmarking-symbol "")
       string "\n\t\n\n")
    (if (string-match " \\* " string)
	(insert
	 (if (bolp) "" "\n")
	 (if nonmarking diary-nonmarking-symbol "")
	 string "x\t\n\n")
      (insert
       (if (bolp) "" "\n")
       (if nonmarking diary-nonmarking-symbol "")
       string ".\t\n\n")))
  (backward-char 2)
  (iso-accents-mode t))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Korrekturen zu appt.el:

;; 3 x auf Deutsch, 1 x Bug-Korrektur für mehrzeilige Anzeigen


;; Deutsch 1
(defun appt-check ()
  "Check for an appointment and update the mode line.
Note: the time must be the first thing in the line in the diary
for a warning to be issued.

The format of the time can be either 24 hour or am/pm.
Example: 

               02/23/89
                 18:00 Dinner
            
              Thursday
                11:45am Lunch meeting.

Appointments are checked every `appt-display-interval' minutes.
The following variables control appointment notification:

`appt-issue-message'
	If t, the diary buffer is checked for appointments.

`appt-message-warning-time'
	Variable used to determine if appointment message
	should be displayed.

`appt-audible'
	Variable used to determine if appointment is audible.
	Default is t.

`appt-visible'
	Variable used to determine if appointment message should be
	displayed in the mini-buffer.  Default is t.

`appt-msg-window'
	Variable used to determine if appointment message
	should temporarily appear in another window.  Mutually exclusive
	to `appt-visible'.

`appt-display-duration'
	The number of seconds an appointment message
	is displayed in another window.

`appt-disp-window-function'
    	Function called to display appointment window.  You can customize
	appt.el by setting this variable to a function different from the
	one provided with this package.
  
`appt-delete-window-function'
    	Function called to remove appointment window and buffer.  You can
	customize appt.el by setting this variable to a function different
	from the one provided with this package."

  (let* ((min-to-app -1)
	 (new-time "")
	 (prev-appt-mode-string appt-mode-string)
	 (prev-appt-display-count (or appt-display-count 0))
	 ;; Non-nil means do a full check for pending appointments
	 ;; and display in whatever ways the user has selected.
	 ;; When no appointment is being displayed,
	 ;; we always do a full check.
	 (full-check
	  (or (not appt-now-displayed)
	      ;; This is true every appt-display-interval minutes.
	      (= 0 (mod prev-appt-display-count appt-display-interval))))
	 ;; Non-nil means only update the interval displayed in the mode line.
	 (mode-line-only
	  (and (not full-check) appt-now-displayed)))

    (when (or full-check mode-line-only)
      (save-excursion

	;; Get the current time and convert it to minutes
	;; from midnight. ie. 12:01am = 1, midnight = 0.

	(let* ((now (decode-time))
	       (cur-hour (nth 2 now))
	       (cur-min (nth 1 now))
	       (cur-comp-time (+ (* cur-hour 60) cur-min)))

	  ;; At the first check in any given day, update our 
	  ;; appointments to today's list.

	  (if (or (null appt-prev-comp-time)
		  (< cur-comp-time appt-prev-comp-time))
	      (condition-case nil
		  (progn
		    (if (and view-diary-entries-initially appt-display-diary)
			(diary)
		      (let ((diary-display-hook 'appt-make-list))
			(diary))))
		(error nil)))
	  (setq appt-prev-comp-time cur-comp-time)

	  (setq appt-mode-string nil)
	  (setq appt-display-count nil)

	  ;; If there are entries in the list, and the
	  ;; user wants a message issued,
	  ;; get the first time off of the list
	  ;; and calculate the number of minutes until the appointment.

	  (if (and appt-issue-message appt-time-msg-list)
	      (let ((appt-comp-time (car (car (car appt-time-msg-list)))))
		(setq min-to-app (- appt-comp-time cur-comp-time))

		(while (and appt-time-msg-list 
			    (< appt-comp-time cur-comp-time))
		  (setq appt-time-msg-list (cdr appt-time-msg-list)) 
		  (if appt-time-msg-list
		      (setq appt-comp-time 
			    (car (car (car appt-time-msg-list))))))

		;; If we have an appointment between midnight and
		;; 'appt-message-warning-time' minutes after midnight,
		;; we must begin to issue a message before midnight.
		;; Midnight is considered 0 minutes and 11:59pm is
		;; 1439 minutes. Therefore we must recalculate the minutes
		;; to appointment variable. It is equal to the number of 
		;; minutes before midnight plus the number of 
		;; minutes after midnight our appointment is.

		(if (and (< appt-comp-time appt-message-warning-time)
			 (> (+ cur-comp-time appt-message-warning-time)
			    appt-max-time))
		    (setq min-to-app (+ (- (1+ appt-max-time) cur-comp-time))
			  appt-comp-time))

		;; issue warning if the appointment time is 
		;; within appt-message-warning time

		(when (and (<= min-to-app appt-message-warning-time)
			   (>= min-to-app 0))
		  (setq appt-now-displayed t)
		  (setq appt-display-count
			(1+ prev-appt-display-count))
		  (unless mode-line-only
		    (if appt-msg-window
			(progn
			  (setq new-time (calendar-date-string (calendar-current-date)))  ;SMW
					;(format-time-string "%a %b %e "  
					;		    (current-time)))
			  (funcall
			   appt-disp-window-function
			   min-to-app new-time
			   (car (cdr (car appt-time-msg-list))))

			  (run-at-time
			   (format "%d sec" appt-display-duration)
			   nil
			   appt-delete-window-function))
			      ;;; else

		      (if appt-visible
			  (message "%s" 
				   (car (cdr (car appt-time-msg-list)))))

		      (if appt-audible
			  (beep 1))))

		  (when appt-display-mode-line
		    (setq appt-mode-string
			  (concat  " Termin in " min-to-app " min. ")))

		  ;; When an appointment is reached,
		  ;; delete it from the list.
		  ;; Reset the count to 0 in case we display another
		  ;; appointment on the next cycle.
		  (if (= min-to-app 0)
		      (setq appt-time-msg-list
			    (cdr appt-time-msg-list)
			    appt-display-count nil)))))

	  ;; If we have changed the mode line string,
	  ;; redisplay all mode lines.
	  (and appt-display-mode-line
	       (not (equal appt-mode-string
			   prev-appt-mode-string))
	       (progn
		 (force-mode-line-update t)
		 ;; If the string now has a notification,
		 ;; redisplay right now.
		 (if appt-mode-string
		     (sit-for 0)))))))))


;; Deutsch 2
;; Display appointment message in a separate buffer.
(defun appt-disp-window (min-to-app new-time appt-msg)
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

    (if (cdr (assq 'unsplittable (frame-parameters)))
	;; In an unsplittable frame, use something somewhere else.
	(display-buffer appt-disp-buf)
      (unless (or (special-display-p (buffer-name appt-disp-buf))
		  (same-window-p (buffer-name appt-disp-buf)))
	;; By default, split the bottom window and use the lower part.
	(appt-select-lowest-window)
	(split-window))
      (pop-to-buffer appt-disp-buf))
    (setq mode-line-format 
	  (concat "-------------------- Termin in "
		  min-to-app " Minuten. " new-time " %-"))
    (erase-buffer)
    (insert-string appt-msg)
    (shrink-window-if-larger-than-buffer (get-buffer-window appt-disp-buf t))
    (set-buffer-modified-p nil)
    (raise-frame (selected-frame))
    (select-window this-window)
    (if appt-audible
	(beep 1))))


;; Deutsch 3
;   (if window-system
;       (progn
;         (make-local-hook 'activate-menubar-hook)
;         (add-hook 'activate-menubar-hook 'cal-menu-update nil t)))
(defun cal-menu-update ()
  ;; Update the holiday part of calendar menu bar for the current display.
  (condition-case nil
      (if (eq major-mode 'calendar-mode)
          (let ((l))
            (calendar-for-loop;; Show 11 years--5 before, 5 after year of
                   ;; middle month
             i from (- displayed-year 5) to (+ displayed-year 5) do
             (setq l (cons (vector (format "des Jahres %s" i)
                                   (list (list 'lambda 'nil '(interactive)
                                               (list 'list-holidays i i)))
                                   t)
                           l)))
;SMW             (setq l (cons ["Feiertage markieren           (u)" mark-calendar-holidays t]
;SMW                           (cons ["Feiertagsmarkierung löschen" calendar-unmark t]
;SMW                                 (cons ["--" '("--") t] l))))
            (easy-menu-change '("Holidays") "Feiertage" (nreverse l)) ;SMW
;SMW            (define-key calendar-mode-map [menu-bar Holidays separator]
;SMW              '("--")) ;SMW
            (define-key calendar-mode-map [menu-bar Holidays today]
                `(,(format "Für heute (%s)"
                           (calendar-date-string (calendar-current-date) t t))
                  . cal-menu-today-holidays))
            (let ((title
                   (let ((m1 displayed-month)
                         (y1 displayed-year)
                         (m2 displayed-month)
                         (y2 displayed-year))
                     (increment-calendar-month m1 y1 -1)
                     (increment-calendar-month m2 y2 1)
                     (if (= y1 y2)
                         (format "%s-%s, %d"
                                 (calendar-month-name m1 3)
                                 (calendar-month-name m2 3)
                                 y2)
                       (format "%s, %d-%s, %d"
                               (calendar-month-name m1 3)
                               y1
                               (calendar-month-name m2 3)
                               y2)))))
              (define-key  calendar-mode-map [menu-bar Holidays 3-month]
                `(,(format "Für das aktuelle Fenster (%s)" title)
                  . list-calendar-holidays)))
            (let ((date (calendar-cursor-to-date)))
              (if date
                  (define-key calendar-mode-map [menu-bar Holidays 1-day]
                    `(,(format "Für Cursor-Datum %s"
                               (calendar-date-string date t t))
                      . calendar-cursor-holidays))))))
    ;; Try to avoid entering infinite beep mode in case of errors.
    (error (ding))))
(defun calendar-cursor-holidays ()
  "Find holidays for the date specified by the cursor in the calendar window."
  (interactive)
  (message "Checking holidays...")
  (let* ((date (calendar-cursor-to-date t))
         (date-string (calendar-date-string date))
         (holiday-list (check-calendar-holidays date))
         (holiday-string (mapconcat 'identity holiday-list ";  "))
         (msg (format "%s:  %s" date-string holiday-string)))
    (if (not holiday-list)
        (message "Keine Feiertage bekannt für %s" date-string)
      (if (<= (length msg) (frame-width))
          (message "%s" msg)
        (set-buffer (get-buffer-create holiday-buffer))
        (setq buffer-read-only nil)
        (calendar-set-mode-line date-string)
        (erase-buffer)
        (insert (mapconcat 'identity holiday-list "\n"))
        (goto-char (point-min))
        (set-buffer-modified-p nil)
        (setq buffer-read-only t)
        (display-buffer holiday-buffer)
        (message "Checking holidays...done")))))



;; Bug-Fix: auch mehrzeilige Korrekturen
(defun appt-make-list ()
  ;; We have something to do if the range of dates that the diary is
  ;; considering includes the current date.
  (let ((calendar-setup calendar-setup))
    (if (and (not (calendar-date-compare
		   (list (calendar-current-date))
		   (list original-date)))
	     (calendar-date-compare
	      (list (calendar-current-date))
	      (list (calendar-gregorian-from-absolute
		     (+ (calendar-absolute-from-gregorian original-date)
			number)))))
	(save-excursion
	  ;; Clear the appointments list, then fill it in from the diary.
	  (setq appt-time-msg-list nil)
	  (if diary-entries-list

	      ;; Cycle through the entry-list (diary-entries-list)
	      ;; looking for entries beginning with a time. If 
	      ;; the entry begins with a time, add it to the
	      ;; appt-time-msg-list. Then sort the list.

	      (let ((entry-list diary-entries-list)
		    (new-time-string ""))
		;; Skip diary entries for dates before today.
		(while (and entry-list
			    (calendar-date-compare
			     (car entry-list) (list (calendar-current-date))))
		  (setq entry-list (cdr entry-list)))
		;; Parse the entries for today.
		(while (and entry-list 
			    (calendar-date-equal 
			     (calendar-current-date) (car (car entry-list))))
		  (let ((time-string (substring (prin1-to-string 
						 (cadr (car entry-list))) 1 -1)))

		    (while (string-match
			    "[0-9]?[0-9]:[0-9][0-9]\\(am\\|pm\\)?\\(.*\n\\)*.*" ;SMW
			    time-string)
		      (let* ((appt-time-string (substring time-string
							  (match-beginning 0)
							  (match-end 0))))

			(if (< (match-end 0) (length time-string))
			    (setq new-time-string (substring time-string 
							     (+ (match-end 0) 1)
							     nil))
			  (setq new-time-string ""))

			(string-match "[0-9]?[0-9]:[0-9][0-9]\\(am\\|pm\\)?"
				      time-string)

			(let* ((appt-time (list (appt-convert-time 
						 (substring time-string
							    (match-beginning 0)
							    (match-end 0)))))
			       (time-msg (cons appt-time
					       (list appt-time-string))))
			  (setq time-string new-time-string)
			  (setq appt-time-msg-list (append appt-time-msg-list
							   (list time-msg)))))))
		  (setq entry-list (cdr entry-list)))))
	  (setq appt-time-msg-list (appt-sort-list appt-time-msg-list))

	  ;; Get the current time and convert it to minutes
	  ;; from midnight. ie. 12:01am = 1, midnight = 0,
	  ;; so that the elements in the list
	  ;; that are earlier than the present time can
	  ;; be removed.

	  (let* ((now (decode-time))
		 (cur-hour (nth 2 now))
		 (cur-min (nth 1 now))
		 (cur-comp-time (+ (* cur-hour 60) cur-min))
		 (appt-comp-time (car (car (car appt-time-msg-list)))))

	    (while (and appt-time-msg-list (< appt-comp-time cur-comp-time))
	      (setq appt-time-msg-list (cdr appt-time-msg-list)) 
	      (if appt-time-msg-list
		  (setq appt-comp-time (car (car (car appt-time-msg-list)))))))))))
  




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(message "Benutzerdefiniertes")


(setq appt-message-warning-time 15 ) ; Wieviele Minuten vorher will ich vor einem Termin gewarnt werden ?
(setq appt-display-duration 8 )      ; Wieviele Sekunden soll die Display-Zeile erinnern ?


(message "Kalender vollständig geladen")


(setq op-project-calendar-loaded t))   ; kommt ganz von vorne vom if am Anfang !!!

(provide 'op-calendar)


