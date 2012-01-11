



(defun project-schedule (&optional numberofdays startdate includingdiaryfile)
  "Macht einen Projektkalender aus allen sichbaren Zeilen, die 
mit einem #T anfangen. Auch mehrere Zeilen werden akzeptiert und 
hintereinander abgearbeitet.

Beispiel:

* Projekt 1

**	Unterprojekt 1
	(ein von-bis)
     #T 1.1.1997 - 3.1.1997

**	Unterprojekt 2
	(ein konkretes Datum)
		    #T 2.1.1997

**	Unterprojekt 3
	(nochmal ein konkretes Datum)
#T 3.1.1997

* Projekt 2

**	Unterprojekt 1
	(zweimal ein von-bis)
     #T vom 1.1.1997 bis zum 3.1.1997
     #T 8.1.1997 - 11.1.1997   "
  (interactive "P")
  ;; mache momentanen Projekt-Buffer als temporären zum project-to-diary-file
  (if (not numberofdays)
      (setq numberofdays (read-string "Anzahl der Tage [7]: " numberofdays)))
  (if (string= "" numberofdays)
      (setq numberofdays "7"))
  (if (not startdate)
      (setq startdate (read-string "Startdatum (deutsches Datumsformat bitte, z.B. 30.4.1997) [Default: heute]: ")))
  (if (string= "" startdate)
      (setq startdate (concat (number-to-string (nth 1 (calendar-current-date))) "."
			      (number-to-string (nth 0 (calendar-current-date))) "."
			      (number-to-string (nth 2 (calendar-current-date))))))
  (if (or (string= "" includingdiaryfile)
	  (string= "n" includingdiaryfile)
	  (string= "N" includingdiaryfile)
	  (not includingdiaryfile))
      (setq includingdiaryfile nil)
    (setq includingdiaryfile t))
  (goto-char (point-min))
  (let ((project-to-diary-file (concat "*" (buffer-name) "*"))
	(current-buf (current-buffer))(point)(pointsv (point-min))
	(diary-file diary-file)
	(calendar-date-display-form calendar-date-display-form))
    (if (get-buffer project-to-diary-file) 
	(kill-buffer project-to-diary-file))
    (setq calendar-date-display-form '(day " " monthname " " year))
    (get-buffer-create project-to-diary-file)
    ;; Buffer aufräumen
    (while (re-search-forward "\n[ \t]*#T" nil t)
      ;; alle Terminzeilen rüberkopieren
      (backward-char 2)
      (append-to-buffer project-to-diary-file (point) (save-excursion (end-of-line) (point)))
      ;; alle dazugehörigen Überschriften rüberkopieren
      ;; - 
      ;; setze die Variable op-current-headline-string
      (schedule-headlines nil t)
      ;; kopiere diese, d.h. jeweils die aktuellen Überschriften, in den File
      (if calendar-setup
	  (switch-to-buffer-other-frame project-to-diary-file)
	(switch-to-buffer project-to-diary-file))
      
      ;test(switch-to-buffer project-to-diary-file)
      (insert-char ?\n 1)
      (beginning-of-line)
      (setq point (point))
      (insert op-current-headline-string)
      (indent-rigidly point (point) tab-width)
      (switch-to-buffer current-buf))
    (switch-to-buffer project-to-diary-file)	      
    (goto-char (point-min))
    (insert-char ?\n 1)
    (save-excursion
      (replace-regexp "\r.*$" ""))
    (setq truncate-lines t)
    (let ((day) (month) (year) (daybis) (monthbis) (yearbis))
      (while (re-search-forward "[ \t]*\\([0-9]?[0-9]\\)\\.\\([0-9]?[0-9]\\)\\.\\([0-9]?[0-9]?[0-9][0-9]\\)" 
				nil t)
	
	(if (save-excursion
	      (save-match-data
		(re-search-backward "\n[ \t]*#T" (save-excursion (beginning-of-line) (backward-char 1) (point)) t)))
	    (progn
	      (setq day (match-string 1))
	      (setq month (match-string 2))
	      (setq year (match-string 3))
	      (if (re-search-forward "\\([0-9]?[0-9]\\)\\.\\([0-9][0-9]?\\)\\.\\([0-9]?[0-9]?[0-9][0-9]\\)" 
				     (save-excursion (end-of-line) (point))
				     t)
		  (progn
		    (setq daybis (match-string 1))
		    (setq monthbis (match-string 2))
		    (setq yearbis (match-string 3))
		    (beginning-of-line)
		    (insert (concat "\n%%(diary-block  " day " " month " " year " " daybis " " monthbis " " yearbis ")  \n"))
		    (kill-line 2)
		    (backward-char 1))  
		(beginning-of-line)
		(insert-char ?\n 1)
		(insert (concat (calendar-date-string 
				 (list (string-to-int month) 
				       (string-to-int day) 
				       (string-to-int year)) t t) 
				"\n"))
		(kill-line 2)
		(backward-char 1)))))
      ;;
      ;; nächster Abschnitt: mache die Diary-Eintraege
      ;;
      (if includingdiaryfile
	  (progn
	    (end-of-buffer)
	    (insert-char ?\n 1)
	    (if (file-readable-p diary-file)
		(progn
		  (find-file diary-file)
		  (append-to-buffer project-to-diary-file (point-min) (point-max))
		  (bury-buffer)))
	    (beginning-of-buffer)
	    ))

      (schedule-include-other-diary-files)
      (setq diary-file project-to-diary-file)
      ;; Trickreiches Überladen der Funktion (find-buffer-visiting), die in der Funktion diary gerufen wird
      ;; (flet git es nicht in elisp)
      ;;
      (fset 'dummy-func (symbol-function 'find-buffer-visiting))
      (condition-case nil
	  (unwind-protect
	      (progn
		(fset 'find-buffer-visiting (symbol-function 'substitute-in-file-name))
		(goto-char (point-min))
		(string-match "\\([0-9]?[0-9]\\)\\.\\([0-9]?[0-9]\\)\\.\\([0-9]?[0-9]?[0-9][0-9]\\)" startdate)
		(setq day (substring startdate (match-beginning 1) (match-end 1)))
		(setq month (substring startdate (match-beginning 2) (match-end 2)))
		(setq year (substring startdate (match-beginning 3) (match-end 3)))
		(list-diary-entries
		 (list (string-to-int month) 
		       (string-to-int day) 
		       (string-to-int year))
		 (string-to-number numberofdays)))
	    (fset 'find-buffer-visiting (symbol-function 'dummy-func)))
	(error "Fehler bei der Erstellung des Projektkalenders")))
    (switch-to-buffer project-to-diary-file)
    (kill-buffer nil)
    (split-window-vertically)
    (other-window -1)
    (setq truncate-lines t)
    (other-window 1)
    (switch-to-buffer current-buf)
    (delete-window)))





(defun teschtprojectdiaryfile ()
  (let ((project-to-diary-file (concat "*" (buffer-name) "*"))
	(current-buf (current-buffer))(point)(pointsv (point-min))
	(diary-file diary-file))
    (setq diary-file project-to-diary-file)
    (fset 'dummy-func (symbol-function 'find-buffer-visiting))
    (unwind-protect
	(progn
	  (fset 'find-buffer-visiting (symbol-function 'substitute-in-file-name))
	  (goto-char (point-min))
	  (list-diary-entries (list 1 1 1997) 3))
      (fset 'find-buffer-visiting (symbol-function 'dummy-func)))
    (other-window -1)
    (setq truncate-lines t)
    (other-window 1)
    (switch-to-buffer current-buf)))




(defun schedule-headlines (&optional leiste setheadlinestring)
  "Inserts either the current headlines (without argument) or a
column ruler momentarily above current line, till next keystroke.
The ruler is defined by the value of column-ruler.
The key typed is executed unless it is SPC."
  (interactive "P")
  (let ((selective-display-sv selective-display))
    (if leiste
	(progn
	  (let ((truncate-lines-sv truncate-lines))
	    (setq truncate-lines t)
	    (momentary-string-display
	     column-ruler (save-excursion (beginning-of-line) (point))
	     nil "Type SPC or any command to erase ruler.")
	    (setq truncate-lines truncate-lines-sv)))
      (let ((headlines-string "------------------------------------------------------\n")
	    (truncate-lines-sv truncate-lines) (mark1) (mark2) (mark3))
	(save-excursion
	  (if (not (looking-at "^[*]"))
	      (progn
		(beginning-of-line)
		(setq mark1 (point))
		(outline-previous-visible-heading 1)
		(setq mark2 (point))
		(goto-char mark1)
		(end-of-line)
		(setq mark3 (point))
		(goto-char mark1)
		;; kann man mit reinnehmen, muss man aber nicht...
		(if (not (re-search-forward "^[ \t]+==>" mark3 t))
		    (progn
		      (goto-char mark1)
		      (if (re-search-backward "^[ \t]+==>" mark2 t)
			  (setq headlines-string (concat (current-line-x) "\n" headlines-string)))))
		(outline-previous-visible-heading 1)
		(setq headlines-string (concat (current-line-x) "\n" headlines-string))))
	  (while (not (looking-at "^[*][^*]"))
	    (setq selective-display nil)
	    (outline-up-heading 1)
	    (setq headlines-string (concat (current-line-x) "\n" headlines-string))))
	(setq headlines-string (concat "\nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				       headlines-string))
	(setq truncate-lines t)
	(setq selective-display selective-display-sv)
	(if setheadlinestring
	    (setq op-current-headline-string headlines-string)
	  (momentary-string-display headlines-string (save-excursion (beginning-of-line) (point))
				    nil "Type SPC or any command to erase ruler."))
	(setq truncate-lines truncate-lines-sv)))))



(define-key calendar-mode-map "z" '(lambda (&optional nixda) 
				     (interactive "s [Return] fuer einen Projektterminplan aus dem Buffer obenan: " )
				     (let ((tmp (calendar-cursor-to-date)))
				       (other-window -1)
				       (project-schedule
					nil
					(concat (number-to-string (nth 1 tmp)) "."
						(number-to-string (nth 0 tmp)) "."
						(number-to-string (nth 2 tmp)))))
				     (other-window 1)))

(defun schedule-include-other-diary-files ()
  (goto-char (point-min))
  (while (re-search-forward
          (concat
           "\\(\\`\\|\^M\\|\n\\)"
           (regexp-quote diary-include-string)
           " \"\\([^\"]*\\)\"")
          nil t)
    (let ((diary-file (substitute-in-file-name
                       (buffer-substring-no-properties
                        (match-beginning 2) (match-end 2)))))
      (if (file-exists-p diary-file)
	  (if (file-readable-p diary-file)
	      (progn
		(beginning-of-line)
		(kill-line)
		(insert-file-contents diary-file))
            (beep)
            (message "Can't read included diary file %s" diary-file)
            (sleep-for 2))
        (beep)
        (message "Can't find included diary file %s" diary-file)
        (sleep-for 2))))
    (goto-char (point-min)))
