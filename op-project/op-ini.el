;;
;;
;;  This file will be called at the end from project mode for your personal 
;;  wishes (SMW)
;;

;; Für das persönliche Logbuch: Logbuch-Mode
;;(setq hilit-auto-rehighlight 200) ; bei langsamen X-Server: Anzahl der zu aktualisierenden Zeilen

;;;
;;; enable horizontal-scolling if point is not visible
;;;
;;;(setq post-command-hook 'hscroll-point-visible)


;; nehme eine der beiden nächsten Funktionen: font-lock-fontify-buffer refärbt
;; bei jedem ausklappen, kann man aber auch händig (c-s-l) machen
;; (mit hilit-repaint-command-interactive)
;(defun hilit-repaint-command (&optional var)
;  (interactive)
;  (save-excursion
;    (font-lock-fontify-buffer)))
(defun hilit-repaint-command (&optional var)
  (interactive)
  t)



(if (< emacs-major-version 20)
    (standard-display-european 1))

(setq display-time-24hr-format t)
(setq display-time-day-and-date t)

;;(setq ps-header-lines 2)
;;(set ps-spool-duplex nil)
;;(setq ps-lpr-command "lpr")
;;(setq ps-lpr-switches nil)

;;(setq ps-print-header nil) ; kein Header beim Druck
;;(setq ps-spool-duplex t)


(setq display-time-string-forms
  '((if display-time-day-and-date
	(format "%s.,%s.%s%s,KW%s  "

		(cdr
		 (assoc
		  dayname
		  '(("Mon" . "Mo") ("Tue" . "Di") ("Wed" . "Mi") ("Thu" . "Do")
		    ("Fri" . "Fr") ("Sat" . "Sa") ("Sun" . "So"))))

		day 

		(cdr
		 (assoc
		  monthname
		  '(("Jan" . "Jan.") ("Feb" . "Feb.") ("Mar" . "März ") ("Apr" . "Apr.")
		    ("May" . "Mai ") ("Jun" . "Jun.") ("Jul" . "Jul.") ("Aug" . "Aug.")
		    ("Sep" . "Sep.") ("Oct" . "Okt.") ("Nov" . "Nov.") ("Dec" . "Dez."))))

		year

		actual-week-number

		)
     
      "")
    (format "%s:%s%s  SMW"
	    (if display-time-24hr-format 24-hours 12-hours)
	    minutes
	    (if display-time-24hr-format "" am-pm))
    (if mail "  Mail" "")))



;;;    "*A list of expressions governing display of the time in the mode line.
;;;  This expression is a list of expressions that can involve the keywords
;;;  `load', `day', `month', and `year', `12-hours', `24-hours', `minutes',
;;;  `seconds', all numbers in string form, and `monthname', `dayname', `am-pm',
;;;  and `time-zone' all alphabetic strings, and `mail' a true/nil value.
;;;
;;;  For example, the form
;;;
;;;    '((substring year -2) \"/\" month \"/\" day
;;;      \" \" 24-hours \":\" minutes \":\" seconds
;;;      (if time-zone \" (\") time-zone (if time-zone \")\")
;;;      (if mail \" Mail\" \"\"))
;;;
;;;  would give mode line times like `94/12/30 21:07:48 (UTC)'.")



(display-time)

(column-number-mode 1)

(setq transient-mark-mode nil)

(setq truncate-lines t)

(setq fill-column 99)


(setq use-logbuch-projectdoc-buffer t)   ; Zu einen neuen Buffer (*logbuch)(=nil) 
                                         ; oder zum Buffer  logbuch-projectdoc-buffer  springen (=t) ?
;(setq logbuch-projectdoc-buffer "~/Projekte/SAP/Projektkontrolle.cvs.op")
(setq logbuch-projectdoc-buffer "~/tmp.op")
(setq current-logbuch-file "~/Projekte/Taetigkeitsberichte/Taetigkeitsbericht.txt") 
                                         ; an diesen File sollen die *logbuch*-Buffer Logbuch-Kommentare angehängt werden

(logbuch-alarm-on 90)                ; alle 90 Minuten Nachfragen

(setq number-of-diary-entries 8 )    ; Für wieviele Tage soll das Tagebuch ausgerechnet werden ?
(setq appt-message-warning-time 25 ) ; Wieviele Minuten vorher will ich vor einem Termin gewarnt werden ?
(setq appt-display-duration 8 )      ; Wieviele Sekunden soll die Display-Zeile erinnern ?

;(automatic-change-mode t)           ; Alle Labels automatisch aktualisieren 
;(automatic-change-mode)     	     ;(nil macht es aber nicht aus -- geht nur durch toggeln)


; Anzeige fuer C-c C-g
(setq column-ruler
  (concat "0    5   10        20        30        40        50        60        70        80        90        100       110       120       130       140       150       160       170       180       190       200       210       220\n"
	  "[    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |\n")
  )


; Anzeige fuer C-c C-g

(setq logbuch-trenner-anfang
  "<++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++>")
(setq logbuch-trenner-ende "")

(setq perlIsCallableFromEmacs nil)

(setq signature "SMW")

