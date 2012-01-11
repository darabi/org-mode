;;;Zwischennotiz
;;;
;;; LOGPRG-Buffer, wenn die variable logprg-projectdoc-buffer allgemein nicht definiert ist,
;;; ansonsten wird der in dieser Variablen beschriebene Buffer an das tageslicht geholt.
;;; Aus diesem Buffer kann man direkt raus mit "q", wenn der Cursor an Position 1 steht
;;;
;; Vorschläge zum Eintrag in den .emacs-File:
;;	(load-library "logbuch-mode")
;;	(logbuch-alarm-on 60) ; einmal pro Stunde
;;
;; When you are tired of LOGBUCHding use "M-x logbuch-alarm-off"
;;
;; Alternately, use "M-x logbuch" whenever you want to log something.
;;
;; Für das persönliche Logbuch: logbuch-mode
;; (defvar use-logbuch-projectdoc-buffer t)   ;; Zu einen neuen Buffer (=nil) oder zum Buffer  logbuch-projectdoc-buffer  springen (=t) ?
;; (defvar logbuch-projectdoc-buffer "~/projects/myprojects.op.crypt")
;; (defvar current-logbuch-file "~/projects/Wochenbericht.txt")	 ;; an diesen File sollen die Logbuch-Kommentare angehängt werden
;;
;; (logbuch-alarm-on 90) ;; alle 90 Minuten Nachfragen
;;
;;



(provide 'logbuch)

(if (not (boundp 'use-logbuch-projectdoc-buffer))
    (setq use-logbuch-projectdoc-buffer nil))

(defun logbuch-mode ()
  "Major mode for editing text to be sent to logbuch.
Like Text Mode but with these additional commands:
C-c C-c	 logbuch-send-and-exit
C-c C-r	 logbuch-send-region"
  (interactive)
  (setq logbuch-map (copy-keymap (current-local-map)))
  (use-local-map logbuch-map)
  (define-key logbuch-map "\C-c\C-c"   'logbuch-send-and-exit)
  (define-key logbuch-map "\C-c\C-r"   'logbuch-send-region)
  (define-key logbuch-map "\C-c\C-s"   'logbuch-set-document-buffer)
  (if (not use-logbuch-projectdoc-buffer)
      (define-key logbuch-map "\M-\C-i"  'tab-to-tab-stop))
;  (define-key (current-local-map) "\C-c\C-l\C-c"   'logbuch-send-and-exit)
;  (define-key (current-local-map) "\C-c\C-l\C-r"   'logbuch-send-region)
;  (define-key (current-local-map) "\C-c\C-l\C-s"   'logbuch-set-document-buffer)
;  (if (not use-logbuch-projectdoc-buffer)
;      (define-key (current-local-map) "\M-\C-i"  'tab-to-tab-stop))
  (setq major-mode 'logbuch-mode)
  (setq mode-name "Logbuch")
  (setq buffer-offer-save t))


(defun logbuch-set-document-buffer ()
  (interactive)
  (setq logbuch-projectdoc-buffer (current-buffer))
  (message "This buffer is now set as the \"logbuch-projectdoc-buffer\" "))


(defun logbuch-send-and-exit (arg)
  "Send message like logbuch-send-buffer, then, if no errors, exit from logbuch buffer.
Prefix arg means don't delete this window."
  (interactive "P")
  (if (not (boundp 'current-logbuch-buffer))
      (setq current-logbuch-buffer (current-buffer))
    (if (eq (current-buffer) current-logbuch-buffer)
	(setq current-logbuch-buffer (current-buffer))))
  (logbuch-send-buffer)
  (if (not use-logbuch-projectdoc-buffer)
      (progn
	(bury-buffer (current-buffer))
	(if (and (not arg)
		 (not (one-window-p)))
	    (delete-window)
	  (switch-to-buffer current-logbuch-buffer)))))


(defun logbuch-send-buffer ()
  "Send the message in the current buffer to logbuch."
  (message "Sending...")
  (if (and (string= (prin1-to-string (current-buffer)) "#<buffer *logbuch*>")
	   (not (buffer-modified-p)))
      (message "(No changes need to be saved)")
    (if (eq (current-buffer) current-logbuch-buffer)
	(setq current-logbuch-buffer (current-buffer)))
    (logbuch-send-buffer-sub)
    (if (not use-logbuch-projectdoc-buffer)
	(progn
	  (set-buffer-modified-p nil)
	  (delete-auto-save-file-if-necessary)))
    (switch-to-buffer current-logbuch-buffer)
    (message "Sending done")))


(defun logbuch-send-buffer-sub ()
  (if (and use-logbuch-projectdoc-buffer
	   (not (eq (current-buffer) current-logbuch-buffer)))
      (if (string-match "\\.crypt" logbuch-projectdoc-buffer)
	  (progn
	    (save-buffer-crypted)
	    (bury-buffer-smartly))
	(save-buffer)
	(bury-buffer-smartly))
    (if (not (eq (current-buffer) current-logbuch-buffer))
	(progn
	  (goto-char (point-min))
	  (insert "\n>>>  ")(insert-date-in-brackets)(insert "	")(insert logbuch-trenner-anfang)(insert-char ?\n 1)
	  (goto-char (point-max))
	  (insert-char ?\n 1)(insert logbuch-trenner-ende)
	  (insert-char ?\n 1)(insert-char ?  50)(insert signature)(insert-char ?\n 4)
	  (write-region (point-min) (point-max) current-logbuch-file t))
      (if (y-or-n-p (concat "Are you sure to save the whole buffer in logbuch-file  " current-logbuch-file " ? "))
	  (progn
	    (write-region (concat "\n>>>  " (calendar-date-string (calendar-current-date)) "	"
				  logbuch-trenner-anfang "\n") t current-logbuch-file t)
	    (write-region (point-min) (point-max) current-logbuch-file t)
	    (write-region (concat "\n" logbuch-trenner-ende
				  "\n														    "
				  signature  "\n\n\n\n") t current-logbuch-file t)
	    (message "Sending...done"))
	(message "Sending aborted")))
    (switch-to-buffer "*logbuch*")
    (kill-buffer "*logbuch*")))


(defun logbuch-send-region-sub (region-begin region-end)
  (if (and use-logbuch-projectdoc-buffer
	   (not (eq (current-buffer) current-logbuch-buffer)))
      (if (string-match "\\.crypt" logbuch-projectdoc-buffer)
	  (progn
	    (save-buffer-crypted)
	    (bury-buffer-smartly))
	(save-buffer)
	(bury-buffer-smartly))
    (if (> region-begin region-end)
	(let ((tmp region-end))
	  (setq region-end region-begin)
	  (setq region-begin tmp)))
    (if (not (eq (current-buffer) current-logbuch-buffer))
	(progn
	  (goto-char region-begin)
	  (insert "\n>>>  ")(insert-date-in-brackets)(insert "	")(insert logbuch-trenner-anfang)(insert-char ?\n 2)
	  (setq region-end (+ region-begin
			      5
			      (length (calendar-date-string (calendar-current-date)))
			      1
			      (length logbuch-trenner-anfang)
			      2
			      ))
	  (goto-char region-end)
	  (insert-char ?\n 1)(insert logbuch-trenner-ende)
	  (insert-char ?\n 1)(insert-char ?  109)(insert signature)(insert-char ?\n 4)
	  (write-region region-begin (point) current-logbuch-file t))
      (write-region (concat "\n>>>  " (calendar-date-string (calendar-current-date)) "	"
			    logbuch-trenner-anfang "\n") t current-logbuch-file t)
      (write-region region-begin region-end current-logbuch-file t)
      (write-region (concat "\n" logbuch-trenner-ende
			    "\n														    "
			    signature  "\n\n\n\n") t current-logbuch-file t))
    (switch-to-buffer "*logbuch*")
    (kill-buffer "*logbuch*")))



(defun logbuch-send-buffer-crypted-sub () ; noch in Arbeit
  (if (and use-logbuch-projectdoc-buffer
	   (not (eq (current-buffer) current-logbuch-buffer)))
      (if (string-match "\\.crypt" logbuch-projectdoc-buffer)
	  (progn
	    (save-buffer-crypted)
	    (bury-buffer-smartly))
	(save-buffer)
	(bury-buffer-smartly))
    (if (string-match "\\.crypt" current-logbuch-file)
	(progn
	  (if (not (eq (current-buffer) current-logbuch-buffer))
	      (progn
		(goto-char (point-min))
		(insert "\n>>>  ")(insert-date-in-brackets)(insert "	")(insert logbuch-trenner-anfang)(insert-char ?\n 1)
		(goto-char (point-max))
		(insert-char ?\n 1)(insert logbuch-trenner-ende)
		(insert-char ?\n 1)(insert-char ?  50)(insert signature)(insert-char ?\n 4)
		(write-region (point-min) (point-max) current-logbuch-file t))
	    (if (y-or-n-p (concat "Are you sure to save the whole buffer in logbuch-file  " current-logbuch-file " ? "))
		(progn
		  (write-region (concat "\n>>>  " (calendar-date-string (calendar-current-date)) "	"
					logbuch-trenner-anfang "\n") t current-logbuch-file t)
		  (write-region (point-min) (point-max) current-logbuch-file t)
		  (write-region (concat "\n" logbuch-trenner-ende
					"\n														    "
					signature  "\n\n\n\n") t current-logbuch-file t)
		  (message "Sending...done"))
	      (message "Sending aborted")))
	  (switch-to-buffer "*logbuch*")
	  (kill-buffer "*logbuch*")))))


(defun logbuch-send-region-crypted-sub (region-begin region-end) ; noch in Arbeit
  (if (and use-logbuch-projectdoc-buffer
	   (not (eq (current-buffer) current-logbuch-buffer)))
      (if (string-match "\\.crypt" logbuch-projectdoc-buffer)
	  (progn
	    (save-buffer-crypted)
	    (bury-buffer-smartly))
	(save-buffer)
	(bury-buffer-smartly))
    (if (> region-begin region-end)
	(let ((tmp region-end))
	  (setq region-end region-begin)
	  (setq region-begin tmp)))
    (if (not (eq (current-buffer) current-logbuch-buffer))
	(progn
	  (goto-char region-begin)
	  (insert "\n>>>  ")(insert-date-in-brackets)(insert "	")(insert logbuch-trenner-anfang)(insert-char ?\n 2)
	  (setq region-end (+ region-begin
			      5
			      (length (calendar-date-string (calendar-current-date)))
			      1
			      (length logbuch-trenner-anfang)
			      2
			      ))
	  (goto-char region-end)
	  (insert-char ?\n 1)(insert logbuch-trenner-ende)
	  (insert-char ?\n 1)(insert-char ?  109)(insert signature)(insert-char ?\n 4)
	  (write-region region-begin (point) current-logbuch-file t))
      (write-region (concat "\n>>>  " (calendar-date-string (calendar-current-date)) "	"
			    logbuch-trenner-anfang "\n") t current-logbuch-file t)
      (write-region region-begin region-end current-logbuch-file t)
      (write-region (concat "\n" logbuch-trenner-ende
			    "\n														    "
			    signature  "\n\n\n\n") t current-logbuch-file t))
    (switch-to-buffer "*logbuch*")
    (kill-buffer "*logbuch*")))


(defvar logbuch-buffer nil
  "Der aktuelle Logbuch-Buffer, in den die Logs eingetragen werden")
(defun logbuch (&optional noerase)
  "Edit a message to be sent.  Argument means resume editing (don't erase).
Returns with message buffer selected; value t if message freshly initialized.
While editing message, type C-c C-c to send the message and exit.

If logbuch-setup-hook is bound, its value is called with no arguments
after the message is initialized. "
  (interactive "P")
;;; sehe nach, ob der Pointer schon im logbuch-projectdoc-buffer steht, wenn nein, dann definiere den current-buffer
;;; als den current-logbuch-buffer:
  ;; wenn logbuch-projectdoc-buffer benutzt wird
  (if use-logbuch-projectdoc-buffer
      ;; wenn gecryptet:
      (if (string-match "\\.crypt" logbuch-projectdoc-buffer)
	  ;; dann hänge an den Namen des Files .crypt an und vergleiche die beiden Namen
	  ;; (sieht umständlich aus, aber der emacs expandiert im afs die Filenamen nicht immer auf die gleiche Weise)
	  (if (not (string= (expand-file-name (file-name-nondirectory (concat (buffer-file-name (current-buffer)) ".crypt")))
			    (expand-file-name (file-name-nondirectory logbuch-projectdoc-buffer))))
	      ;; wenn nicht gleich, dann
	      (setq current-logbuch-buffer (current-buffer)))
	;; wenn nicht gecryptet und nicht gleich:
	(if (not (string= (expand-file-name (file-name-nondirectory (buffer-file-name (current-buffer))))
			  (expand-file-name (file-name-nondirectory logbuch-projectdoc-buffer))))
	    ;; dann
	    (setq current-logbuch-buffer (current-buffer))))
    ;; wenn logbuch-projectdoc-buffer nicht benutzt wird
    (if (not (string= (prin1-to-string (current-buffer)) "#<buffer *logbuch*>"))
	(setq current-logbuch-buffer (current-buffer))))
;;; das nächste verhält sich nur so, wenn die Variable logbuch-projectdoc-buffer allgemein definiert ist, sonst wie ein LOGBUCH-buffer
  (if use-logbuch-projectdoc-buffer
      (progn
	(if (string-match "\\.crypt" logbuch-projectdoc-buffer)
	    (find-file-crypted logbuch-projectdoc-buffer)
	  (find-file logbuch-projectdoc-buffer))
	(setq logbuch-buffer (current-buffer))
	(message "Dieser Buffer ist nun ein Logbuch-Buffer: Sichern und in den Hintergrund mit	\C-c \C-c"))
    (switch-to-buffer "*logbuch*")
    (setq logbuch-buffer (current-buffer))
    (setq default-directory (expand-file-name "~/"))
    (auto-save-mode auto-save-default)
    (logbuch-mode)
    ;; naechste Zeile loescht alle lokalen Variablen
    (indented-text-mode)
    (and (not noerase)
	 (or (not (buffer-modified-p))
	     (y-or-n-p "Unsent message being composed; erase it? "))
	 (progn (erase-buffer)
		(set-buffer-modified-p nil)
		(run-hooks 'logbuch-setup-hook)
		t))))


(defun logbuch-other-window (&optional noerase)
  "Like `logbuch' command, but display logbuch buffer in another window."
  (interactive "P")
  (let ((pop-up-windows t))
    (pop-to-buffer "*logbuch*"))
  (logbuch noerase))


;; run every interval & at initial call to logbuch-alarm-on
(defun logbuch-alarm-filter ()
  (if (or (and use-logbuch-projectdoc-buffer
	       (not (eq (current-buffer) (file-name-nondirectory logbuch-projectdoc-buffer))))
	  (and (not use-logbuch-projectdoc-buffer)
	       (not (eq (current-buffer) (file-name-nondirectory current-logbuch-file))))
	  (not (eq (current-buffer) logbuch-buffer)))
      (progn
	(message "3")
	(ding)
	(sleep-for 1)
	(message "2")
	(ding)
	(sleep-for 1)
	(message "1")
	(ding)
	(sleep-for 1)
	;; abarbeiten der Events, die während des sleep-for's gekommen sind	
	(while (input-pending-p)
	  (insert (quotet-insert (read-char))))
	(logbuch)
	(message "Report-buffer: send and store with C-c C-c after you have typed something")
	)))


(defvar logbuch-zeitobjekt nil)

;; Starte LOGBUCH in eimen regelmäßigem Intervall
;;
(defvar logbuch-abfrageintervall 90
  "Der Abfrageabstand des Logbuch-Zeitbuffers im Minuten, globale Variable")
(defun logbuch-alarm-on (interval)
  "Turn the Emacs logbuch alarm on.  The alarm goes off every INTERVAL minutes
and you will be switched to the logbuch buffer automatically.
Use logbuch-alarm-off to stop this behaviour."
  (interactive "nEnter logbuch alarm interval (in minutes): ")
  (setq logbuch-abfrageintervall interval)
  (logbuch-alarm-off)
  (setq logbuch-zeitobjekt (run-with-timer
			    ;; convert minutes -> seconds for wakeup
			    (* 60.0 interval) (* 60.0 interval)
			    'logbuch-alarm-filter)))


;; Turn LOGBUCH alarm off
;;
(defun logbuch-alarm-off ()
  "Turn the Emacs logbuch alarm off."
  (interactive)
  (if logbuch-zeitobjekt
      (progn
	(cancel-timer logbuch-zeitobjekt)
	(setq logbuch-zeitobjekt nil)))
  (cancel-function-timers 'logbuch-alarm-filter))



(defun logbuch-send-region (region-begin region-end)
  "Send the message in the current region to logbuch."
  (interactive "r")
  (if (not (boundp 'current-logbuch-buffer))
      (setq current-logbuch-buffer (current-buffer))
    (if (eq (current-buffer) current-logbuch-buffer)
	(setq current-logbuch-buffer (current-buffer))))
  (message "Sending...")
  (logbuch-send-region-sub region-begin region-end)
  (switch-to-buffer current-logbuch-buffer)
  (message "Sending...done"))

