;;
;; LEICHT VARIIERTER CODE TAUCHT BEI DER FUNKTION mpx-msp-text-extractor NOCHMAL AUF !!!
;;
(if window-system
    (progn
      (if (x-display-color-p)
	  (progn
	    (hilit-set-mode-patterns
	     'logbuch-mode
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
	       ;;glob-struct)
	       ("\\[Meilenstein\\]" nil define)
	       ("|| \\[Unterprojekt\\]" " ||" error)
	       ("#include[ \t]+\"" "\"" define)
	       ("[\n\r]#end_include_statement" nil define)
	       
	       ;; speziell für MPX-MSP
	       ("{Nr\\." "}" goldenrod)
	       ("{Verantwortlich" "}" goldenrod)
	       
	       ("^\\(\\*E\\|\\*FW\\).*" "[\r\n]" summary-deleted) ; Erledigtes in Hauptüberschrift
	       ("^\\(\\*\\*+E\\|\\*+FW\\).*" "[\r\n]" keyword)     ; Erledigtes in Unterüberschrift
					                           ;(generelle Doku: in op-menus.el)
	       ("^\\(\\*+V\\|\\*+S\\).*" "[\r\n]" include)         ; Verschobenes und ständig 
					                           ;              wiederkehrendes
	       ("^\\*+\\-+" "[\r\n]" string)			   ; *---- - Zeilen
	       ("^\\*+\\++" "[\r\n]" string)			   ; *++++ - Zeilen
	       ("^\\*[^*EFVDSR].*" "[\r\n]" msg-header)	           ; Hauptüberschrift ; F != FW !!!
	       ("^\\*\\*+[^*EFVDSR].*" "[\r\n]" defun)		   ; Unterüberschriften
	       ("^\\*+D" "[\r\n]" error)			   ; Dringendes
	       )))
	(hilit-set-mode-patterns
	 'logbuch-mode
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
      ))
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

