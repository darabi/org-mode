;;;Zwischennotiz
;;;
;;; LOGPRG-Buffer, wenn die variable logprg-projectdoc-buffer allgemein nicht definiert ist,
;;; ansonsten wird der in dieser Variablen beschriebene Buffer an das tageslicht geholt.
;;; Aus diesem Buffer kann man direkt raus mit "q", wenn der Cursor an Position 1 steht
;;;
;; Suggested addition to .emacs:
;;	(load-library "logbuch-mode")
;;	(logbuch-alarm-on 60) ; once an hour
;;
;; When you are tired of LOGBUCHding use "M-x logbuch-alarm-off"
;;
;; Alternately, use "M-x logbuch" whenever you want to log something.
;;
;; Für das persönliche Logbuch: Logbuch-Mode
;; (defvar use-logbuch-projectdoc-buffer t)   ;; Zu einen neuen Buffer (=nil) oder zum Buffer  logbuch-projectdoc-buffer  springen (=t) ?
;; (defvar logbuch-projectdoc-buffer "~/projects/myprojects.op.crypt")
;; (defvar current-logbuch-file "~/projects/Wochenbericht.txt")	 ;; an diesen File sollen die Logbuch-Kommentare angehängt werden
;;
;; (logbuch-alarm-on 90) ;; alle 90 Minuten Nachfragen






(provide 'logbuch)

(if (not (boundp 'use-logbuch-projectdoc-buffer))
    (setq use-logbuch-projectdoc-buffer nil))

(defvar send-logbuch-function 'logbuch-send-buffer-sub
  "Function to call to send the current buffer as logbuch.")

(defvar logbuch-mode-map nil)

(defun logbuch-mode ()
  "Major mode for editing text to be sent to logbuch.
Like Text Mode but with these additional commands:
C-c C-c	 logbuch-send-and-exit
C-c C-r	 logbuch-send-region"
  (interactive)
  (indented-text-mode)
  (use-local-map logbuch-mode-map)
  (setq major-mode 'logbuch-mode)
  (setq mode-name "Logbuch")
  (setq buffer-offer-save t))


(if logbuch-mode-map
    nil
  (setq logbuch-mode-map (make-sparse-keymap))
  (define-key logbuch-mode-map "\C-c\C-c" 'logbuch-send-and-exit)
  (define-key logbuch-mode-map "\C-c\C-r" 'logbuch-send-region)
  (define-key logbuch-mode-map "\C-c\C-s" 'logbuch-set-document-buffer)
  (define-key logbuch-mode-map "\M-\C-i"  'tab-to-tab-stop))



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
    (funcall send-logbuch-function)
    (if (not use-logbuch-projectdoc-buffer)
	(progn
	  (set-buffer-modified-p nil)
	  (delete-auto-save-file-if-necessary)))
    (switch-to-buffer current-logbuch-buffer)))


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

;  (let ((tembuf (generate-new-buffer " logbuch temp"))
;	(logbuchbuf (current-buffer)))
;    (unwind-protect
;	(call-process-region region-begin region-end
;			     (if (boundp 'logbuch-program)
;				 logbuch-program
;			       "~/op-project/op-logbuch/logbuch")
;			     nil tembuf nil)
;      (kill-buffer tembuf))))


(defun logbuch (&optional noerase)
  "Edit a message to be sent.  Argument means resume editing (don't erase).
Returns with message buffer selected; value t if message freshly initialized.
While editing message, type C-c C-c to send the message and exit.

\\{logbuch-mode-map}

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
			    (expand-file-name (file-name-nondirectory  logbuch-projectdoc-buffer))))
	      ;; wenn nicht gleich, dann
	      (setq current-logbuch-buffer (current-buffer)))
	;; wenn nicht gecryptet und nicht gleich:
	(if (not (string= (expand-file-name (file-name-nondirectory (buffer-file-name (current-buffer))))
			  (expand-file-name (file-name-nondirectory  logbuch-projectdoc-buffer))))
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
	(message "Dieser Buffer ist nun ein Logbuch-Buffer: Sichern und in den Hintergrund mit	\C-c \C-c"))
    (switch-to-buffer "*logbuch*")
    (setq default-directory (expand-file-name "~/"))
    (auto-save-mode auto-save-default)
    (logbuch-mode)
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


;;;
;;; Alarm interface
;;;

(defvar logbuch-alarm-on-p nil)		; t if alarm is on
(defvar logbuch-alarm-process nil)

;; run when logbuch-alarm-process is killed
(defun logbuch-alarm-sentinel (proc reason)
  (or (eq (process-status proc) 'run)
      (setq logbuch-alarm-on-p nil)
      (message "logbuch alarm off")))

;; run every interval & at initial call to logbuch-alarm-on
(defun logbuch-alarm-filter (proc string)
  (if logbuch-alarm-on-p
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
	  (insert (char-to-string (read-char))))
	(set-marker (process-mark proc) (point))
	(message "report-buffer: send and store with C-c C-c")
	(logbuch))
    (setq logbuch-alarm-on-p t)))



;; Set alarm to call logbuch every so often
;;
(defun logbuch-alarm-on (interval)
  "Turn the Emacs logbuch alarm on.  The alarm goes off every INTERVAL minutes
and you will be switched to the logbuch buffer automatically.
Use logbuch-alarm-off to stop this behaviour."
  (interactive "nEnter logbuch alarm interval (in minutes): ")
  (let ((live (and logbuch-alarm-process
		   (eq (process-status logbuch-alarm-process) 'run))))
    (if (not live)
	(progn
	  (setq logbuch-alarm-on-p nil)
	  (if logbuch-alarm-process
	      (delete-process logbuch-alarm-process))
	  (let ((process-connection-type nil))
	    (setq logbuch-alarm-process
		  (start-process "logbuch-alarm" nil
				 (concat exec-directory "wakeup")
				 ; convert minutes -> seconds for wakeup
				 (int-to-string (* 60 interval)))))
	  (process-kill-without-query logbuch-alarm-process)
	  (set-process-sentinel logbuch-alarm-process 'logbuch-alarm-sentinel)
	  (set-process-filter logbuch-alarm-process 'logbuch-alarm-filter)))))


;; Turn LOGBUCH alarm off
;;
(defun logbuch-alarm-off ()
  "Turn the Emacs logbuch alarm off."
  (interactive)
  (if logbuch-alarm-on-p (kill-process logbuch-alarm-process)))


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

