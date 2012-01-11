;;;
;;; Indentierendes
;;;
;
;;; Die nächsten Zeilen sind
;;; wegen ernster Probleme aus text-mode.el herausgenommen worden.
;;; War: (defun indented-text-mode () ...)
;;;
(defun indented-text-mode-without-varkill ()
  "Major mode for editing text with indented paragraphs.
In this mode, paragraphs are delimited only by blank lines.
You can thus get the benefit of adaptive filling
 (see the variable `adaptive-fill-mode').
\\{indented-text-mode-map}
Turning on `indented-text-mode' calls the value of the variable
`text-mode-hook', if that value is non-nil."
;;  (interactive)
;;  (kill-all-local-variables)
;;  (use-local-map text-mode-map)  ; wirft alle fremden Tastenkombinationen raus
  (define-abbrev-table 'text-mode-abbrev-table ())
  (setq local-abbrev-table text-mode-abbrev-table)
  (set-syntax-table text-mode-syntax-table)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'indent-relative-maybe)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
;;  (use-local-map indented-text-mode-map) ; macht Tastatur und den Menü-Balken zu Schrott
  (setq mode-name "OP Indented Text")
  (setq major-mode 'indented-text-mode)
  (run-hooks 'text-mode-hook 'indented-text-mode-hook)
  ) 
(load "text-mode")
(indented-text-mode-without-varkill)


(defvar toggle-re-indent-place nil)
(make-local-variable 'toggle-re-indent-place)
(defvar toggle-re-indent-place_emptyHeadlines nil)
(make-local-variable 'toggle-re-indent-place_emptyHeadlines)
(defvar am-i-in-the-same-line 1)
(make-local-variable 'am-i-in-the-same-line)
(defvar am-i-in-the-same-line_emptyHeadlines 1)
(make-local-variable 'am-i-in-the-same-line_emptyHeadlines)

;;; braucht das Laden des indented-text-mode, in diesem Code hier
;;; aufgerufen als indented-text-mode-without-varkill (s.o.)

(defun re-indent-for-tab-command ()
  "Kompliziertes Indent für Outline- oder Project-Mode des Emacs.
-- unter einer Outline-Überschrift: Cursor nach der Anzahl der Sterne setzen
-- unter Freizeichen: an das Ende der Freizeichen (ignoriert dazwischen-
   liegende Newlines)
-- bei nur Newlines: kein Indent
-- Buchstaben am Zeilenanfang: normale Indentierung
-- Zeichen am Zeilenanfang: Indentierung nach dem ersten Leerraum
-- Pfeil (==>) in der da überliegenden Zeile: Einsetzen von vier Blanks
-- Minuszeichen (-) in der darüberliegenden Zeile: Einsetzen von 2 Blanks

-- Toggeln der Cursor-Positionen bei mehrmaligem Tippen zwischen
   Indentierung und Zeilenende
-- automatisches Tabulatorisieren der Leerzeichen (tabify)

Diese Funktion schaltet automatisch den indented-text-mode mit ein,
siehe auch unter der Dokumentation dieser Funktion"
;war: unter einer Outline-Überschrift: Cursor unter erste Schrift nach Sternen
;;;
;;; Ist die darüberliegende Zeile eine Zeile mit Buchstaben am Anfang ?
  (let ((before-change-functions nil)(after-change-functions nil))

  (if (not (fboundp 'this-is-column-mode))
      (defun this-is-column-mode (dum) nil))  ; gefährlich, Effekt noch nicht ganz abgeschätzt
	   
  (if (not (this-is-column-mode 'dum))
      (progn
	(let ((one-up-is-headline 0)(one-up-is-arrowline 0)(this-is-a-headline nil)(one-up-is-intendetforarrow-line 0))
	  (if (looking-at "[ \t]*$")
	      (setq am-i-in-the-same-line -1))
	  (save-excursion
	    (previous-line 1)
	    (beginning-of-line)
	    (setq point-save1 (point))
	    (if (not (looking-at "$"))
		(re-search-forward "^[^ \t]" nil t))
	    (setq point-save2 (- (point) 1)))
	  (if (= point-save1 point-save2)
	      (setq one-up-is-headline 1)
;;;
;;; Wenn keine Headline: Ist die darüberliegende Zeile eine Zeile mit einem Pfeil (==>) am Anfang ?
	    (save-excursion
	      (previous-line 1)
	      (beginning-of-line)
	      (if (looking-at "^[ \t]+==>")
		  (setq one-up-is-arrowline 1)))
;;; oder ist die darüberliegende Zeile eine Zeile mit einem Minus (-) am Anfang ?
	    (save-excursion
	      (previous-line 1)
	      (beginning-of-line)
	      (if (looking-at "^[ \t]*-")
		  (setq one-up-is-arrowline 2))))
;;;
;;; Falls in der gleichen Zeile: Toggeln zwischen sinnvollem Punkt und Ende der Zeile
	  (save-excursion
	    (beginning-of-line)
	    (if (= am-i-in-the-same-line (point))
		(setq toggle-re-indent-place (not toggle-re-indent-place))
	      (setq toggle-re-indent-place nil)
	      (setq am-i-in-the-same-line (point))))
	  (if toggle-re-indent-place
	      (end-of-line)
	    (let ((point-save1 (point))(point-save2)(save-position-for-short)(save-for-short))
	      (beginning-of-line)
;;;
;;; Ist die aktuelle Zeile eine Zeile mit Sonderzeichen am Anfang ?
;;	      (if (looking-at "[][!@#$\%^&\*()_+=|\\~`{};:'\"?/>.<,-]")
	      (if (looking-at "[][@$\%^&\*)_+|!\\~};:/>.,]") ; ohne '=', '#' und '-' sowie 
;;					                      ; verschiedene Sonderzeichen
;;	      (if (looking-at "[\*#]")
		  (progn
;;;
;;; Wenn ja: sind wir auf einem ^*-Pattern (Outline-Überschrift) ?
		    (if (looking-at "^\\*")
			;; Wenn ja: indent gemäß der Anzahl der Sterne
			(setq this-is-a-headline t))
;;; sonst indent-for-tab-command nach dem ersten Leerzeichen
		    (beginning-of-line)
		    (save-excursion
		      (setq save-position-for-short (point))
		      (re-search-forward "[ \t]+" nil t)
		      (beginning-of-line)
		      (if (= save-position-for-short (point))
			  (setq save-for-short t)))
		    (if save-for-short
			(re-search-forward "[ \t]+" nil t) ; --- here
		      (end-of-line)
		      (indent-for-tab-command)))
;;;
;;; Wenn nein, dann {
;;;
;;; Ist die aktuelle Zeile eine Leerzeile ? Wenn nein, dann alle Leerzeichen vornedran loeschen
		(if (not (and (looking-at "$")
			      (= one-up-is-headline 0)))
		    (progn
		      (beginning-of-line)
		      (setq point-save1 (point))
		      (re-search-forward "[^ \t]\\|$\\|^[^ \t]" nil t)

		      (if (not (= point-save1 (point)))
			  (progn
			    (backward-char 1)
			    (kill-line 0)))))))
;;;
;;; Wenn dieses eine Überschriftenzeile ist: setze zusätzliche Tabs ein
;;;
	    (if this-is-a-headline
		(progn
		  (save-excursion
		    (beginning-of-line)
		    (setq point-save1 (point))
		    (while (looking-at "\\*")
		      (forward-char 1))
		    (setq point-save2 (point))
		    ;; suche ersten Blank oder Ende der Zeile
		    (while (not (looking-at "[ \t]\\|$"))
		      (forward-char 1))
		    (if (>= (current-column) tab-width)
			(setq point-save2 (- point-save2 (/ (current-column) tab-width))))
		    (if (looking-at "[ \t]*$")
			(progn
			  (if (= (- (- point-save2 point-save1) 1) 0)
			      (insert-char ?  1)
			    (insert-char ?	(- (- point-save2 point-save1) 1)))
			  (if (not (looking-at "$"))
			      (kill-region (point) (save-excursion (end-of-line)(point)))))))
		  (if (or (save-excursion (beginning-of-line) (looking-at "^\\*+\\w*[ \t\n\r]*$"))
			  (save-excursion (beginning-of-line) (looking-at "[ \t\n\r]*$")))
		      (progn
			(if (= am-i-in-the-same-line_emptyHeadlines (save-excursion (beginning-of-line) (point)))
			    (setq toggle-re-indent-place_emptyHeadlines (not toggle-re-indent-place_emptyHeadlines))
			  (setq toggle-re-indent-place_emptyHeadlines t)
			  (save-excursion (beginning-of-line) (setq am-i-in-the-same-line_emptyHeadlines (point))))
			(if toggle-re-indent-place_emptyHeadlines
			    (end-of-line)
			  (beginning-of-line)
			  (re-search-forward "\\*+" nil t)))))
;;;
;;; Wenn die vorhergehende Zeile eine Überschrift ist, dann indentieren nach dem ersten Space,
;;; sonst normales indent-for-tab-command
	      (if (= one-up-is-headline 0)
		  (progn
;;;		  
;;; Falls die darueberliegende Zeile eine Zeile mit Pfeil (==>) ist (und sie nicht selbst einen
;;; Pfeil am Anfang hat), setze 4 Blanks ein, sonst normales Indent
		    (if (or (= one-up-is-arrowline 0)
			    (looking-at "==>")
			    (looking-at "-"))
			(progn
			  (indent-for-tab-command)
;;; Stelle erst einmal fest, ob die darüberliegende Zeile mit einem Blank anfängt: Voraussetzung für
;;; das Löschen von Zeichen, um alle Pfeile untereinander liegen zu haben
			  (save-excursion
			    (previous-line 1)
			    (backward-char 1)
			    (if (and (looking-at " ")
				     (not (save-excursion (beginning-of-line)(looking-at "  "))))
				(setq one-up-is-intendetforarrow-line 1)))
			  (if (and (looking-at "==>")
				   (= one-up-is-arrowline 0)
				   (= one-up-is-intendetforarrow-line 1))
			      (progn
				(if (not (looking-at "^  \\|^"))
				    (backward-delete-char-untabify 1))
				(if (not (looking-at "^  \\|^"))
				    (backward-delete-char-untabify 1))
				(if (not (looking-at "^  \\|^"))
				    (backward-delete-char-untabify 1))
				(if (not (looking-at "^  \\|^"))
				    (backward-delete-char-untabify 1))))
			  (if (and (looking-at "-")
				   (= one-up-is-arrowline 0)
				   (= one-up-is-intendetforarrow-line 1))
			      (progn
				(if (not (looking-at "^  \\|^"))
				    (backward-delete-char-untabify 1))
				(if (not (looking-at "^  \\|^"))
				    (backward-delete-char-untabify 1)))))
		      (indent-for-tab-command)
		      (if (= one-up-is-arrowline 1)
			  (insert-char ?  4))
		      (if (= one-up-is-arrowline 2)
			  (insert-char ?  2))))
		(previous-line 1)
		(beginning-of-line)
		(setq point-save1 (point))
		(while (looking-at "\\*")
		  (forward-char 1))
		(setq point-save2 (point))
		(next-line 1)
		(beginning-of-line)
		(insert-char ?	(- (- point-save2 point-save1) 1))
		(indent-for-tab-command)
		;; wenn obendrüber eine Headline mit nur einem Stern ist, landet man hier
		;; -- dann einfach zwei Blanks einsetzen
		(if (looking-at "^") ; am Zeilenanfang
		    (if (save-excursion (previous-line 1)
					(beginning-of-line)
					(looking-at "\\(\*[^*]\\)\\|-"))
			(insert "  ")))
		)))))
    (insert-char ?  tab-width))))
;;; }
;;;
;;; aufrufende Funktion ist hier
;;;
(defun re-indent ()
  "Kompliziertes Indent für Outline- oder Project-Mode des Emacs.
-- unter einer Outline-Überschrift: Cursor nach der Anzahl der Sterne setzen
-- unter Freizeichen: an das Ende der Freizeichen (ignoriert dazwischen-
   liegende Newlines)
-- bei nur Newlines: kein Indent
-- Buchstaben am Zeilenanfang: normale Indentierung
-- Zeichen am Zeilenanfang: Indentierung nach dem ersten Leerraum
-- Pfeil (==>) in der da überliegenden Zeile: Einsetzen von vier Blanks
-- Minuszeichen (-) in der darüberliegenden Zeile: Einsetzen von 2 Blanks

-- Toggeln der Cursor-Positionen bei mehrmaligem Tippen zwischen
   Indentierung und Zeilenende
-- Beim ersten Drücken am Zeilenende einer Zeile: nur indentieren
-- automatisches Tabulatorisieren der Leerzeichen (tabify)"
  (interactive)
  ;; wenn am Zeilenende: schau nach, ob sich die Indentierung durch zweimaliges drücken ändert
  (if (not
       (save-excursion
	 (beginning-of-line)
	 (or (looking-at "^\\*+\\w*[ \t\n\r]*$")
	     (looking-at "[ \t\n\r]*$"))))
      (if (looking-at "$")
	  (let ((p (point)))
	    (re-indent-for-tab-command)
	    (end-of-line)
	    ;; wenn nein, dann wieder an den Zeilenanfang, sonst so lassen
	    (if (= p (point))
		(re-indent-for-tab-command)))
	(re-indent-for-tab-command))
    (re-indent-for-tab-command)))

(define-key (current-local-map) "\C-i" 're-indent)


(defun newline-and-re-indent ()
  (interactive)
  (let ((before-change-functions nil)(after-change-functions nil))
;;
;; die nächsten beiden Zeilen ab emacs 19.29
;;
    (if (<= (current-column) (current-fill-column))
	nil
      ;;
      (if ;(and 
	  (not (looking-at "^"))
	  ;    (looking-at "$"))
	  (save-excursion
	    (progn
	      (backward-char 1)
	      (while (looking-at "[ \t]+$")
		(backward-char 1))
	      (if (not (looking-at "^"))
		  (progn
		    (forward-char 1)
		    ;(fixup-whitespace) ; kann man, muss man aber nicht 
		                        ;(die letzen Whitespaces der umgebrochenen Zeile werden verändert)
		    (re-search-backward "[ \t]" nil t)
		    (forward-char 1)
		    (insert-char ?\n 1))
		))))
      (setq toggle-re-indent-place nil)
      (save-excursion
	  (re-indent-for-tab-command)
	  (re-indent-for-tab-command)))))

(make-local-variable 'auto-fill-function)
(setq auto-fill-function 'newline-and-re-indent)
;;
;; Vor emacs 19.29:
;;(define-key (current-local-map) "\C-m" 'newline-and-re-indent)



(defun op-indent-region (&optional rb re)
  "Indentiert eine komplette Region mit re-indent-for-tab-command nach den Regeln
des OP-Werzeugs"
  (interactive)
  (let ((region-beginning (region-beginning))
	(region-end (region-end))
	(lastline (+ 1 (count-lines (point-min) (region-end)))))
    (save-excursion
      (goto-char region-beginning)
      (while (<= (count-lines (point-min) (point))
		 lastline)
	(if (looking-at "^$")
	    (progn
	      (re-indent-for-tab-command)
	      (next-line 1))
	  (end-of-line)
	  (force-indent)
	  (re-indent)
	  (next-line 1))))))

(defun op-softindent-region ()
  "Indentiert eine komplette Region mit re-indent-for-tab-command nach den Regeln
des OP-Werzeugs. Nimmt allerdings Rücksicht auf eingelesene Files, die in der 
Regel nicht den Indentierungen des OP-Modes unterliegen."
  (interactive)
  (let ((region-beginning (region-beginning))(region-end (region-end))
	(lastline (+ 1 (count-lines (point-min) (region-end)))))
    (save-excursion
      (goto-char region-beginning)
      (while (<= (count-lines (point-min) (point))
		 lastline)
	(beginning-of-line)
	(re-indent-for-tab-command)
	(next-line 1)))))


(defun force-indent ()
  (interactive)
  (beginning-of-line)
  (if (looking-at "^$")
      nil
    (let ((before-change-functions nil)(after-change-functions nil)(am-i-in-the-same-line 0))
      (if (re-search-forward "\\*+[A-Za-z0-9]*\\([ \t]\\|$\\)" (save-excursion (end-of-line)(point)) t)
	  (progn
	    (if (looking-at "$") (insert-char ?  1))
	    (backward-char 1)
	    (insert-char ?\n 1)
	    (previous-line 1)
	    (re-indent)
	    (re-search-forward "[\n]\\([ \t]*\\)" (save-excursion (next-line 1)(end-of-line)(- (point) 1)) t)
	    (replace-match "" nil nil)
	    (re-indent))
	(beginning-of-line)
	(insert-char ?  3)
	(insert-char ?\n 1)
	(previous-line 1)
	(re-indent)
	(re-search-forward "[\n]\\([ \t]*\\)" (save-excursion (next-line 1)(end-of-line)(- (point) 1)) t)
	(replace-match "" nil nil)
	(end-of-line)
	(re-indent)
	(end-of-line)))))
(define-key (current-local-map) "\M-;" 'force-indent)




(defun indent-like-last-headline (&optional arg)
  "Indent the same level like the headline before. With ARG, indent one level more"
  (interactive "P") 
  (let ((headline)(before-change-functions nil)(after-change-functions nil))
    (save-excursion
      (outline-back-to-heading)
      (setq heading (buffer-substring (point) (re-search-forward "\\*+" nil t))))
    (beginning-of-line)
    (insert-string heading)
    (if arg
	(progn
	  (if (= (car arg) 4)
	      (progn
		(beginning-of-line)
		(insert-string "*")))
	  (if (> (car arg) 4)
	      (progn
		(beginning-of-line)
		(if (looking-at "\\*\\*")
		    (delete-char 1))))))
    (re-indent)
    (re-indent)
    (re-indent)
    ))
(define-key (current-local-map) "\C-c\C-i" 'indent-like-last-headline)







(defun op-fill-paragraph-region ()
  (interactive)
  (save-excursion
    (let ((tmp-region-end nil))
      (setq tmp-region-end (save-excursion 
			     (goto-char (if (< (region-beginning)(region-end))(region-end)(region-beginning)))
			     (end-of-line)
			     (point)))
      (goto-char (if (< (region-beginning)(region-end))(region-beginning)(region-end)))
      (beginning-of-line)
      (newline)
      (previous-line 1)
      (save-match-data
	(re-indent))
      (beginning-of-line)
      (narrow-to-region (point) (1+ tmp-region-end))
      (fill-paragraph-or-region nil)
      (goto-char (point-min))
      (kill-line)
      (goto-char (point-max))
      (delete-char -1)
      (widen)
)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;
;;; schalte die auto-fill-function aus für das newline, da die beiden Funktionen sich überschneiden
(define-key (current-local-map) "\C-m" '(lambda()(interactive)(let((auto-fill-function nil))(newline))))

(if xemacsp
    (define-key (current-local-map) [(meta space)] 'fixup-whitespace)
  (define-key (current-local-map) [M-SPC] 'fixup-whitespace))

(define-key (current-local-map) "\M-q" 'op-fill-paragraph-region)

(define-key (current-local-map) "\M-\C-i" '(lambda()(interactive)(insert-char ?\C-i 1)))

(provide 'op-indent)

