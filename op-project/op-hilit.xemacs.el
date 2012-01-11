;;;
;;;
;;;
;;;   Achtung: zum Debuggen:
;;;   Variablen setzen langt nicht - ab besten debuggen in op-project.el (nach Löschen des .elc-Files),
;;;   dann übernehmen. Und nach jeder Änderung muss dann der File komplett durch Hochrufen eines
;;;   .op-Files neu geladen werden.
;;;
;;;   Grundsätzlich werden die Muster in der Reihenfolge des Erscheinens verarbeitet und beendet, wenn
;;;   das erste Muster trifft [so scheint es wohl zu sein...]
;;;
;;;







;;
;; Stichworte aus font-lock.el
;;


;;; laden der Faces
(require 'custom)
(require 'cus-edit)
(require 'font-lock)

;;
;; mit der Einstellung "lazy shot" im Options->Highlighting->Lazy Shot funktioniert der op-mode 
;; im Highlighting nicht
;;
(progn (lazy-shot-mode -1) 
       (customize-set-variable (quote lazy-shot-mode) lazy-shot-mode) 
       (redraw-modeline))


(defgroup gnus-visual nil
  "Options controling the visual fluff."
  :group 'gnus
  :group 'faces)


(defface gnus-group-news-1-face
  '((((class color)
      (background dark))
     (:foreground "PaleTurquoise" :bold t))
    (((class color)
      (background light))
     (:foreground "DarkGreen" :bold t))
    (t
     ()))
  "Level 1 newsgroup face.")

(defface gnus-group-news-1-empty-face
  '((((class color)
      (background dark))
     (:foreground "PaleTurquoise"))
    (((class color)
      (background light))
     (:foreground "DarkGreen"))
    (t
     ()))
  "Level 1 empty newsgroup face.")

(defface gnus-group-news-2-face
  '((((class color)
      (background dark))
     (:foreground "turquoise" :bold t))
    (((class color)
      (background light))
     (:foreground "CadetBlue4" :bold t))
    (t
     ()))
  "Level 2 newsgroup face.")

(defface gnus-group-news-2-empty-face
  '((((class color)
      (background dark))
     (:foreground "turquoise"))
    (((class color)
      (background light))
     (:foreground "CadetBlue4"))
    (t
     ()))
  "Level 2 empty newsgroup face.")

(defface gnus-group-news-3-face
  '((((class color)
      (background dark))
     (:bold t))
    (((class color)
      (background light))
     (:bold t))
    (t
     ()))
  "Level 3 newsgroup face.")

(defface gnus-group-news-3-empty-face
  '((((class color)
      (background dark))
     ())
    (((class color)
      (background light))
     ())
    (t
     ()))
  "Level 3 empty newsgroup face.")

(defface gnus-group-news-4-face
  '((((class color)
      (background dark))
     (:bold t))
    (((class color)
      (background light))
     (:bold t))
    (t
     ()))
  "Level 4 newsgroup face.")

(defface gnus-group-news-4-empty-face
  '((((class color)
      (background dark))
     ())
    (((class color)
      (background light))
     ())
    (t
     ()))
  "Level 4 empty newsgroup face.")

(defface gnus-group-news-5-face
  '((((class color)
      (background dark))
     (:bold t))
    (((class color)
      (background light))
     (:bold t))
    (t
     ()))
  "Level 5 newsgroup face.")

(defface gnus-group-news-5-empty-face
  '((((class color)
      (background dark))
     ())
    (((class color)
      (background light))
     ())
    (t
     ()))
  "Level 5 empty newsgroup face.")

(defface gnus-group-news-6-face
  '((((class color)
      (background dark))
     (:bold t))
    (((class color)
      (background light))
     (:bold t))
    (t
     ()))
  "Level 6 newsgroup face.")

(defface gnus-group-news-6-empty-face
  '((((class color)
      (background dark))
     ())
    (((class color)
      (background light))
     ())
    (t
     ()))
  "Level 6 empty newsgroup face.")

(defface gnus-group-news-low-face
  '((((class color)
      (background dark))
     (:foreground "DarkTurquoise" :bold t))
    (((class color)
      (background light))
     (:foreground "DarkGreen" :bold t))
    (t
     ()))
  "Low level newsgroup face.")

(defface gnus-group-news-low-empty-face
  '((((class color)
      (background dark))
     (:foreground "DarkTurquoise"))
    (((class color)
      (background light))
     (:foreground "DarkGreen"))
    (t
     ()))
  "Low level empty newsgroup face.")

(defface gnus-group-mail-1-face
  '((((class color)
      (background dark))
     (:foreground "aquamarine1" :bold t))
    (((class color)
      (background light))
     (:foreground "DeepPink3" :bold t))
    (t
     (:bold t)))
  "Level 1 mailgroup face.")

(defface gnus-group-mail-1-empty-face
  '((((class color)
      (background dark))
     (:foreground "aquamarine1"))
    (((class color)
      (background light))
     (:foreground "DeepPink3"))
    (t
     (:italic t :bold t)))
  "Level 1 empty mailgroup face.")

(defface gnus-group-mail-2-face
  '((((class color)
      (background dark))
     (:foreground "aquamarine2" :bold t))
    (((class color)
      (background light))
     (:foreground "HotPink3" :bold t))
    (t
     (:bold t)))
  "Level 2 mailgroup face.")

(defface gnus-group-mail-2-empty-face
  '((((class color)
      (background dark))
     (:foreground "aquamarine2"))
    (((class color)
      (background light))
     (:foreground "HotPink3"))
    (t
     (:bold t)))
  "Level 2 empty mailgroup face.")

(defface gnus-group-mail-3-face
  '((((class color)
      (background dark))
     (:foreground "aquamarine3" :bold t))
    (((class color)
      (background light))
     (:foreground "magenta4" :bold t))
    (t
     (:bold t)))
  "Level 3 mailgroup face.")

(defface gnus-group-mail-3-empty-face
  '((((class color)
      (background dark))
     (:foreground "aquamarine3"))
    (((class color)
      (background light))
     (:foreground "magenta4"))
    (t
     ()))
  "Level 3 empty mailgroup face.")

(defface gnus-group-mail-low-face
  '((((class color)
      (background dark))
     (:foreground "aquamarine4" :bold t))
    (((class color)
      (background light))
     (:foreground "DeepPink4" :bold t))
    (t
     (:bold t)))
  "Low level mailgroup face.")

(defface gnus-group-mail-low-empty-face
  '((((class color)
      (background dark))
     (:foreground "aquamarine4"))
    (((class color)
      (background light))
     (:foreground "DeepPink4"))
    (t
     (:bold t)))
  "Low level empty mailgroup face.")

;; Summary mode faces.

(defface gnus-summary-selected-face '((t
				       (:underline t)))
  "Face used for selected articles.")

(defface gnus-summary-cancelled-face
  '((((class color))
     (:foreground "yellow" :background "black")))
  "Face used for cancelled articles.")

(defface gnus-summary-high-ticked-face
  '((((class color)
      (background dark))
     (:foreground "pink" :bold t))
    (((class color)
      (background light))
     (:foreground "firebrick" :bold t))
    (t
     (:bold t)))
  "Face used for high interest ticked articles.")

(defface gnus-summary-low-ticked-face
  '((((class color)
      (background dark))
     (:foreground "pink" :italic t))
    (((class color)
      (background light))
     (:foreground "firebrick" :italic t))
    (t
     (:italic t)))
  "Face used for low interest ticked articles.")

(defface gnus-summary-normal-ticked-face
  '((((class color)
      (background dark))
     (:foreground "pink"))
    (((class color)
      (background light))
     (:foreground "firebrick"))
    (t
     ()))
  "Face used for normal interest ticked articles.")

(defface gnus-summary-high-ancient-face
  '((((class color)
      (background dark))
     (:foreground "SkyBlue" :bold t))
    (((class color)
      (background light))
     (:foreground "RoyalBlue" :bold t))
    (t
     (:bold t)))
  "Face used for high interest ancient articles.")

(defface gnus-summary-low-ancient-face
  '((((class color)
      (background dark))
     (:foreground "SkyBlue" :italic t))
    (((class color)
      (background light))
     (:foreground "RoyalBlue" :italic t))
    (t
     (:italic t)))
  "Face used for low interest ancient articles.")

(defface gnus-summary-normal-ancient-face
  '((((class color)
      (background dark))
     (:foreground "SkyBlue"))
    (((class color)
      (background light))
     (:foreground "RoyalBlue"))
    (t
     ()))
  "Face used for normal interest ancient articles.")

(defface gnus-summary-high-unread-face
  '((t
     (:bold t)))
  "Face used for high interest unread articles.")

(defface gnus-summary-low-unread-face
  '((t
     (:italic t)))
  "Face used for low interest unread articles.")

(defface gnus-summary-normal-unread-face
  '((t
     ()))
  "Face used for normal interest unread articles.")

(defface gnus-summary-high-read-face
  '((((class color)
      (background dark))
     (:foreground "PaleGreen"
		  :bold t))
    (((class color)
      (background light))
     (:foreground "DarkGreen"
		  :bold t))
    (t
     (:bold t)))
  "Face used for high interest read articles.")

(defface gnus-summary-low-read-face
  '((((class color)
      (background dark))
     (:foreground "PaleGreen"
		  :italic t))
    (((class color)
      (background light))
     (:foreground "DarkGreen"
		  :italic t))
    (t
     (:italic t)))
  "Face used for low interest read articles.")

(defface gnus-summary-normal-read-face
  '((((class color)
      (background dark))
     (:foreground "PaleGreen"))
    (((class color)
      (background light))
     (:foreground "DarkGreen"))
    (t
     ()))
  "Face used for normal interest read articles.")

(defface gnus-splash-face
  '((((class color)
      (background dark))
     (:foreground "Brown"))
    (((class color)
      (background light))
     (:foreground "Brown"))
    (t
     ()))
  "Face of the splash screen.")

(defface message-header-to-face
  '((((class color)
      (background dark))
     (:foreground "green2" :bold t))
    (((class color)
      (background light))
     (:foreground "MidnightBlue" :bold t))
    (t
     (:bold t :italic t)))
  "Face used for displaying From headers."
  :group 'message-faces)

(defface message-header-cc-face
  '((((class color)
      (background dark))
     (:foreground "green4" :bold t))
    (((class color)
      (background light))
     (:foreground "MidnightBlue"))
    (t
     (:bold t)))
  "Face used for displaying Cc headers."
  :group 'message-faces)

(defface message-header-subject-face
  '((((class color)
      (background dark))
     (:foreground "green3"))
    (((class color)
      (background light))
     (:foreground "navy blue" :bold t))
    (t
     (:bold t)))
  "Face used for displaying subject headers."
  :group 'message-faces)

(defface message-header-newsgroups-face
  '((((class color)
      (background dark))
     (:foreground "yellow" :bold t :italic t))
    (((class color)
      (background light))
     (:foreground "blue4" :bold t :italic t))
    (t
     (:bold t :italic t)))
  "Face used for displaying newsgroups headers."
  :group 'message-faces)

(defface message-header-other-face
  '((((class color)
      (background dark))
     (:foreground "#b00000"))
    (((class color)
      (background light))
     (:foreground "steel blue"))
    (t
     (:bold t :italic t)))
  "Face used for displaying newsgroups headers."
  :group 'message-faces)

(defface message-header-name-face
  '((((class color)
      (background dark))
     (:foreground "DarkGreen"))
    (((class color)
      (background light))
     (:foreground "cornflower blue"))
    (t
     (:bold t)))
  "Face used for displaying header names."
  :group 'message-faces)

(defface message-header-xheader-face
  '((((class color)
      (background dark))
     (:foreground "blue"))
    (((class color)
      (background light))
     (:foreground "blue"))
    (t
     (:bold t)))
  "Face used for displaying X-Header headers."
  :group 'message-faces)

(defface message-separator-face
  '((((class color)
      (background dark))
     (:foreground "blue3"))
    (((class color)
      (background light))
     (:foreground "brown"))
    (t
     (:bold t)))
  "Face used for displaying the separator."
  :group 'message-faces)

(defface message-cited-text-face
  '((((class color)
      (background dark))
     (:foreground "red"))
    (((class color)
      (background light))
     (:foreground "red"))
    (t
     (:bold t)))
  "Face used for displaying cited text names."
  :group 'message-faces)

(defface message-mml-face
  '((((class color)
      (background dark))
     (:foreground "DarkGreen"))
    (((class color)
      (background light))
     (:foreground "DarkGreen"))
    (t
     (:bold t)))
  "Face used for displaying MML."
  :group 'message-faces)

(if window-system
    (progn
      (if (x-display-color-p)
	  (progn
;;	    (hilit-set-mode-patterns
;;	     'logbuch-mode
	    (setq op-font-lock-keywords
	      (list


	       '("^\\(\\*E\\|\\*FW\\).*[\r\n]" . widget-button-pressed-face)       ; Erledigtes in Hauptüberschrift
	       '("^\\(\\*\\*+E\\|\\*+FW\\).*[\r\n]" . gnus-group-news-2-empty-face); Erledigtes in Unterüberschrift
					                                           ; (generelle Doku: in op-menus.el)
	       '("^\\(\\*+V\\|\\*+S\\).*[\r\n]" . gnus-group-mail-3-face)          ; Verschobenes und ständig 
					                                           ;              wiederkehrendes
	       '("^\\*+[ \t]+#.*[\r\n]" . cvs-need-action-face)	    		   ; Schablonen-Färbung

	       '("^\\*+\\-.*[\r\n]" . gnus-group-news-2-face)			   ; *---- - Zeilen
	       '("^\\*+\\+.*[\r\n]" . message-header-to-face)			   ; *++++ - Zeilen
	       '("^\\*+|.*[\r\n]" . custom-invalid-face)			   ; *|||| - Zeilen
	       '("^\\*+=.*[\r\n]" . display-time-mail-balloon-enhance-face)	   ; *==== - Zeilen
	       '("^\\*[^*+=-EFVDSR].*[\r\n]" . gnus-summary-high-ticked-face)      ; Hauptüberschrift ; F != FW !!!
	       '("^\\*\\*[^*+=-EFVDSR].*[\r\n]" . gnus-summary-high-unread-face)             ; **-Unterüberschrift ; F != FW !!!
	       '("^\\*\\*\\*+[^*+=-EFVDSR].*[\r\n]" . message-header-to-face)  ; Unterüberschriften
	;;     '("^\\*\\*\\*+[^*+=-EFVDSR].*[\r\n]" . font-latex-bold-face)        ; Unterüberschriften
	       '("^\\*+D.*[\r\n]" . font-lock-warning-face)			   ; Dringendes
	       ;'("^\\*+IB.*[\r\n]"  . dired-face-socket)                          ; lila schwach: in Bearbeitung (noch BUG)
	       '("^\\*+IB.*[\r\n]" . custom-set-face)                              ; blau schwach: in Bearbeitung

	       ;; speziell für MPX-MSP
	       '("{Nr\\..*}" . gnus-group-news-low-empty-face)                     ; goldenrot wäre besser
	       '("{Verantwortlich.*}" . gnus-group-news-low-empty-face)
	       '("{Dauer.*}" . message-header-newsgroups-face)


	       ;; Doppelzeilenkombination für golden unterlegt -> xemacs: rot grau unterlegt
	       '("#T.*\\(#\\|[\r\n]\\)" . modeline-mousable)
	       '("#R.*\\(#\\|[\r\n]\\)" . custom-comment-tag-face)

	       ;; eigentlich gewünscht: dunkelgrün
	       '("#[^TR].*\\(#\\|[\r\n]\\)" . dired-face-socket)


	       '("\\<\\(Info\\|INFO\\|info\\)\\>.*\\(\\|[\r\n]\\)" . custom-button-face)    ; bold schwarz
	          ;;glob-struct)
	       '("\\[Meilenstein\\]" . gnus-group-news-1-face) ; grün bold
	       '("|| \\[Unterprojekt\\].*||" . font-lock-warning-face)             ; rot bold
	       '("#include[ \t]+\".*\"" . gnus-group-news-1-face)
	       '("[\r\n]#end_include_statement" . gnus-group-news-1-face)

	       ;;	   ("->" "[ \t\r]\\|$" jargon-keyword)
	       ;; ("->" nil label)
	       ;;("/\\*" "\\*/" msg-from)   ; wie C-Kommentare
	       ;; Doppelzeilenkombination für golden unterlegt
	       '("/\\*\\.*\\*/" . yellow)

	       '("\\<\\(Info\\|INFO\\|info\\)\\>.*\\(\\|[\r\n]\\)" . custom-button-face)    ; bold schwarz
	          ;;glob-struct)
	       '("\\[Meilenstein\\]" . gnus-group-news-1-face) ; grün bold
	       '("|| \\[Unterprojekt\\].*||" . font-lock-warning-face)             ; rot bold
	       '("#include[ \t]+\".*\"" . gnus-group-news-1-face)
	       '("[\r\n]#end_include_statement" . gnus-group-news-1-face)

	       ;;	   ("->" "[ \t\r]\\|$" jargon-keyword)
	       ;; ("->" nil label)
	       ;;	   ("->" "[ \t\r]\\|$" string)

	       
	       ))))))



;;
;; mache ein anderes hilit-repaint-command auf control-shift-l = control L
;;
(if xemacsp
    (progn
      (defun hilit-repaint-command-interactive (&optional var)
	(interactive)
	(save-excursion
	  (font-lock-fontify-buffer)))

      (defun hilit-repaint-command (&optional var)
	(interactive)
	(if (fboundp 'font-lock-fontify-buffer)
	    (save-excursion
	      (font-lock-fontify-buffer)))
	t)

      (defun hilit-repaint-command-region ()
	(interactive)
	(if (fboundp 'font-lock-fontify-region)
	    (save-excursion
	      (font-lock-fontify-region (region-beginning) (region-end)))))))
;;
;; nur zum merken
;; die frühere Version, sehr viel langsamer und massiver
;;; verändertes hilit-repaint-command für den XEmacs
;      (progn (require (quote font-lock)) 
;	     (if (and (integerp font-lock-maximum-decoration) 
;		      (= 2 font-lock-maximum-decoration)) 
;		 nil 
;	       (customize-set-variable (quote font-lock-maximum-decoration) 2) 
;      (font-lock-recompute-variables)
;	       ))
;      (progn (require (quote font-lock)) 
;	     (if (or (eq font-lock-maximum-decoration t) 
;		     (and (integerp font-lock-maximum-decoration) 
;			  (>= font-lock-maximum-decoration 3))) 
;		 nil 
;	       (customize-set-variable (quote font-lock-maximum-decoration) t) 
;	       (font-lock-recompute-variables)))
;      )
;  )
(define-key outline-minor-mode-map [(control L)] 'hilit-repaint-command-interactive)
(define-key outline-minor-mode-map [(control meta L)] 'hilit-repaint-command-region)



(setq font-lock-keywords op-font-lock-keywords )
(put 'logbuch-mode 'font-lock-defaults '(op-font-lock-keywords))


;	(hilit-set-mode-patterns
;	 'logbuch-mode
;	 '(
;	   ("==>" nil string)
;	   ("^\\*+E.*\\|^\\*+FW.*\\|^\\*+V.*" "[\r\n]" keyword)
;	   ("#[^#]" ".#\\|[\r\n]" msg-from)
;	   ("\\<\\(Info\\|INFO\\|info\\)\\>.*" "[\r\n]" glob-struct)
;	   ("^\\*+\\-+" "[\r\n]" msg-from)
;	   ("^\\*" "[\r\n]" defun)
;	   )))

;      ;;(setq hilit-face-check nil) ; Fonds werden ja meistens nicht extra geändert, sonst hilft auch noch C-u S-C-l    
;      (setq hilit-inhibit-rebinding t) ; wenn der Server nur wenige Farben hat, geht sonst manchmal Yank nicht mehr
;      (setq hilit-auto-rehighlight 10000) ; Anzahl der automatisch upgedateten Zeilen bei Benutzung des X-Servers

;      ;; auch der hilit-mode kommt nicht mit dem (setq selective-display t) zurecht
;      (setq hilit-auto-rehighlight-fallback '(20000 . 10000))
;      (make-variable-buffer-local 'hilit-auto-rehighlight-fallback)

;      (hilit-repaint-command t)
;      ))
;;(";.*" nil comment)
;;("#|" "|#" comment)
;;("%.*$" nil comment)
;;("Vorgänger" nil crosref)
;;("==>" nil summary-seen)
;; ("==>" nil mgs-separator)

;
; alle Definitionen oben aus lisp_dir: hilit19.el
;
; error rot
; jargon-keyword  ;golden
; string ; grau
; comment ist im Prinzip nocht fast nicht genutzt
; decl schwaches blau
; dired-marked: schwaches lila
; jargon-keyword rot unterstrichen
; struct ganz schwarz
; summary-Xed blaß sehr hellgrün
; goldenrod golden
; summary-deleted laues rot
; glob-struct dunkelrosa
; summary-current  Balken in blau
; summary-unread schwaches blau
; define bold grün
; msg-from violett
; glob-struct rosa
;
