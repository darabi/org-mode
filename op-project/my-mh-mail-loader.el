

;;; ********************************************************************************
;;;  Mode-Ladendes für emacs 19 und emacs 20
;;; ********************************************************************************


;;; noch nicht getestet -- normale Mail-Funktionen
;(load "~/op-project/mail-emacs-19/mh/mailabbrev")
;(load "~/op-project/mail-emacs-19/mh/mabbrev-menu")
(load "~/op-project/mail-emacs-19/mh/mh-menus")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Mailcrypt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(setq load-path (cons (expand-file-name "~/op-project/mail-emacs-19/mh/mailcrypt") load-path))
(load "mc-toplev.el")



(defvar newmailwasthere nil)
(defadvice mh-inc-folder (before newmailwasthere activate)
  (if (and (file-exists-p (concat "/var/spool/mail/" user-real-login-name))
	   (not (= 0 (nth 7 (file-attributes 
			     (concat "/var/spool/mail/" user-real-login-name))))))
      (setq newmailwasthere t)
    (setq newmailwasthere nil)))



(defvar mh-first-new-message nil)
(add-hook 'mh-inc-folder-hook
	  (function
	   (lambda()
	     (if mh-first-new-message
		 (setq mh-first-new-message mh-first-msg-num))
	     (mc-install-read-mode)
	     (mc-install-write-mode)
	     (column-number-mode 1)
	     ;;
	     ;; BBDB aktualisieren
	     (setq transient-mark-mode nil)
	     (if newmailwasthere
		 (progn
		   (setq mh-first-new-message (mh-get-msg-num t))
		   ;(if (fboundp 'bbdb)
		   ;    (mh-insert-mailaddresses-in-bbdb))
		   ))
	     ;;
	     (if (< emacs-major-version 20)
		 (standard-display-european t)
	       (standard-display-european t t))
	     ;;
	     ;;
	     ;; begin hilit19
	     (cond (window-system
		    (require 'hilit19)
		    (let* ((csubject-patterns '(("^\\[.+\\]$" nil msg-subject)))
			   (header-patterns '(("^Subject:.*$" nil msg-subject)
					      ("^From:.*$" nil msg-from)
					      ("^--text follows this line--$"
					       nil msg-separator)
					      ("^[A-Za-z][A-Za-z0-9-]+:" nil msg-header)
					      ))
			   (body-patterns '(("^\\(In article\\|[ \t]*\\w*[]<>}|]\\).*$"
					     nil msg-quote)))
			   (message-patterns (append ;;csubject-patterns
					      header-patterns
					      body-patterns))
			   )
		      (hilit-set-mode-patterns 'msg-header header-patterns)
		      (hilit-set-mode-patterns 'msg-body body-patterns)
; 		      (hilit-set-mode-patterns 'mime/viewer-mode
; 					       message-patterns
; 					       'hilit-rehighlight-message)
		      )
; 		    (add-hook 'mime-viewer/content-header-filter-hook
; 			      (lambda ()
; 				(if (not (eq mime::preview/original-major-mode
; 					     'gnus-original-article-mode))
; 				    (hilit-rehighlight-buffer-quietly)
; 				  )))
; 		    (add-hook ' mime-viewer/plain-text-preview-hook
; 			      (lambda ()
; 				(if (not (eq mime::preview/original-major-mode
; 					     'gnus-original-article-mode))
; 				    (hilit-rehighlight-buffer-quietly)
; 				  )))
		    ))
	     ;; end hilit19
	     ;;
	     (if (get-buffer "*BBDB*")
		 (progn
		   (switch-to-buffer-other-window "*BBDB*")
		   (delete-window)))
	     (if newmailwasthere
		 (mh-goto-msg mh-first-new-message t nil))
	     ;;
	     t t)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Sonstiges für die Vollinstallation von mh
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; mh-e wird benötigt, damit die mh-Funktionen zur Verfügung stehen (emacs-Defaultpfad)
(require 'mh-e)

(load "mh-comp.el")
(load "mh-funcs")
(load "mh-pick")
(load "mh-seq")

(define-key mh-folder-mode-map "L" 'mh-print-msg)
(define-key mh-folder-mode-map "l" '(lambda () (interactive)
				      (mh-show)
				      (other-window 1)
				      (ps-print-buffer-with-faces) 
				      (other-window -1)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Mime-Mode und "Tools for Mime" (tm) bzw. semi (tm second generation)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;(load "mh-mime")
(load "mail-emacs-19/mh/mm/metamail")


(if (< emacs-major-version 20) ; solange nehme bitte tm mit allen fortführenden Modulen
    (progn
      (setq load-path (cons (expand-file-name "~/op-project/mail-emacs-19/mh/tm-7.106/tm") load-path))
      (setq load-path (cons (expand-file-name "~/op-project/mail-emacs-19/mh/tm-7.106/tl") load-path))
      (setq load-path (cons (expand-file-name "~/op-project/mail-emacs-19/mh/tm-7.106/emu") load-path))
      (setq load-path (cons (expand-file-name "~/op-project/mail-emacs-19/mh/tm-7.106/mu") load-path))
      (setq load-path (cons (expand-file-name "~/op-project/mail-emacs-19/mh/tm-7.106/mel") load-path))
      (setq load-path (cons (expand-file-name "~/op-project/mail-emacs-19/mh/tm-7.106/tm-mh-e") load-path))
      (setq load-path (cons (expand-file-name "~/op-project/mail-emacs-19/mh/tm-7.106/tm-gnus") load-path))
      (setq load-path (cons (expand-file-name "~/op-project/mail-emacs-19/mh/tm-7.106/tm-mail") load-path))
      (setq load-path (cons (expand-file-name "~/op-project/mail-emacs-19/mh/tm-7.106/tm-vm") load-path))
      (setq load-path (cons (expand-file-name "~/op-project/mail-emacs-19/mh/tm-7.106/gnus-mime") load-path))
      (setq load-path (cons (expand-file-name "~/op-project/mail-emacs-19/mh/tm-7.106/tm/methods") load-path))

      (load "mail-emacs-19/mh/mime-setup")

      ;; identisch kopiert aus dem eben geladenen tm-mh-e.el, aber ohne das habe ich die Umlaute unter
      ;; emacs 20 nicht zum laufen bekommen
      ;; -- und für eine genauere Analyse der Kompatibilität fehlt mir im Moment die Zeit
      (defun mh-display-msg (msg-num folder &optional show-buffer mode)
	(or mode
	    (setq mode tm-mh-e/automatic-mime-preview)
	    )
	;; Display message NUMBER of FOLDER.
	;; Sets the current buffer to the show buffer.
	(set-buffer folder)
	(or show-buffer
	    (setq show-buffer mh-show-buffer))
	;; Bind variables in folder buffer in case they are local
	(let ((msg-filename (mh-msg-filename msg-num)))
	  (if (not (file-exists-p msg-filename))
	      (error "Message %d does not exist" msg-num))
	  (set-buffer show-buffer)
	  (cond ((not (equal msg-filename buffer-file-name))
		 ;; Buffer does not yet contain message.
		 (clear-visited-file-modtime)
		 (unlock-buffer)
		 (setq buffer-file-name nil)	; no locking during setup
		 (setq buffer-read-only nil)
		 (erase-buffer)
		 (if mode
		     (let* ((aname (concat "article-" folder))
			    (abuf (get-buffer aname))
			    )
		       (if abuf
			   (progn
			     (set-buffer abuf)
			     (setq buffer-read-only nil)
			     (erase-buffer)
			     )
			 (setq abuf (get-buffer-create aname))
			 (set-buffer abuf)
			 )
		       (as-binary-input-file
			(insert-file-contents msg-filename)
			;; (goto-char (point-min))
			(while (re-search-forward "\r$" nil t)
			  (replace-match "")
			  )
			)
		       (set-buffer-modified-p nil)
		       (setq buffer-read-only t)
		       (setq buffer-file-name msg-filename)
		       (mh-show-mode)
		       (mime/viewer-mode nil nil nil
					 aname (concat "show-" folder))
		       (goto-char (point-min))
		       )
		   (let ((clean-message-header mh-clean-message-header)
			 (invisible-headers mh-invisible-headers)
			 (visible-headers mh-visible-headers)
			 )
		     ;; 1995/9/21
		     ;;   modified by ARIURA <ariura@cc.tuat.ac.jp>
		     ;;   to support mhl.
		     (if mhl-formfile
			 (mh-exec-lib-cmd-output "mhl" "-nobell" "-noclear"
						 (if (stringp mhl-formfile)
						     (list "-form" mhl-formfile))
						 msg-filename)
		       (insert-file-contents msg-filename))
		     ;; end
		     (goto-char (point-min))
		     (cond (clean-message-header
			    (mh-clean-msg-header (point-min)
						 invisible-headers
						 visible-headers)
			    (goto-char (point-min)))
			   (t
			    (mh-start-of-uncleaned-message)))
		     (if tm-mh-e/decode-encoded-word
			 (mime/decode-message-header)
		       )
		     (set-buffer-modified-p nil)
		     (setq buffer-read-only t)
		     (setq buffer-file-name msg-filename)
		     (mh-show-mode)
		     ))
		 (or (eq buffer-undo-list t)	;don't save undo info for prev msgs
		     (setq buffer-undo-list nil))
		 ;; Added by itokon (02/19/96)
		 (setq buffer-file-name msg-filename)
		 ;;
		 (set-mark nil)
		 (setq mode-line-buffer-identification
		       (list (format mh-show-buffer-mode-line-buffer-id
				     folder msg-num)))
		 (set-buffer folder)
		 (setq mh-showing-with-headers nil)))))
      )
  ;; emacs 20
  (load "mail-emacs-20/mh-mime-setup") ; for semi (tm second generation)
  )


(setq mime-editor/split-message t)
;; "*Split large message if it is non-nil. [tm-edit.el]")

(setq mime-editor/message-default-max-lines 222222) ; 16M with 72 characters
;; "*Default maximum lines of a message. [tm-edit.el]")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  semi-definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setq mime-edit-split-message nil)
(autoload 'turn-on-mime-edit "mime-edit"
           "Minor mode for editing MIME message." t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Super-Cite setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'supercite)
(add-hook 'mail-citation-hook 'sc-cite-original)

;(setq mh-yank-from-start-of-msg nil) ; Zitiert ohne den Mail-Header

(setq sc-mail-warn-if-non-rfc822-p nil) ; Warnung bei nichtkonformen Header
(setq sc-nuke-mail-headers 'all) ; Header der Mail nicht replizieren

(setq sc-cite-region-limit 500)
(setq sc-cite-blank-lines-p t)
;(setq sc-blank-lines-after-headers nil)

(setq sc-citation-leader "  ")  ; String vor dem Zitieren
(setq sc-citation-delimiter ">") ; Nach dem Namen etc benutzter String: "Stefan>"
(setq sc-citation-separator " ") ; nach dem sc-citation-delimiter benutzter String

(setq sc-auto-fill-region-p nil) ;  Auffüllen des Textes (autofill)
(setq sc-fixup-whitespace-p nil) ; Kein Löschen von Leerzeilen vor dem Zitieren

(setq sc-preferred-attribution-list '("sc-lastchoice" "x-attribution" "initials" "firstname" "lastname" "emailname"))

(setq sc-reference-tag-string "--> ") ; "wer hat was gesagt"-String

(setq sc-confirm-always-p t) ; frage iinning-of-lininning-of-lininning-of-lininning-of-lininning-of-linmmer nach
(setq sc-electric-references-p nil)
(setq sc-nested-citation-p nil)
(setq sc-preferred-header-style 4)
;(autoload 'sc-cite-original "supercite" "Supercite 3.1" t)

;; GUT: Funktion sc-insert-citation (C-c C-p i)


(defun mh-cite-msg (nested autofill)
  (interactive "sCite nested ? [n] (y or n)  \nsAutofill citation ? [n] (y or n)  ")
  (if (looking-at "\n\n")
      (if (save-excursion
	    (forward-char 2)
	    (bolp))
	  (forward-char 2)))
  (beginning-of-line)
  ;; die nächsten lästige Zeilen (;x) wegen eines Bugs im SC
  ;; (falls Leerzeichen vornedran sind, stimmt das "nested citation" nicht ganz
  (insert-char ?- 1) ;x
  (insert-char ?\n 1) ;x
  (let ((sc-auto-fill-region-p nil)(transient-mark-mode-sv transient-mark-mode))
    (setq transient-mark-mode t)
    (if (string= autofill "y")
	(setq sc-auto-fill-region-p t))
    (if (string= nested "y")
	(progn
	  (let ((sc-nested-citation-p t) (sc-confirm-always-p nil) 
		(sc-preferred-header-style 5)
		(sc-citation-leader "") (sc-cite-blank-lines-p nil))
	    (mh-yank-cur-msg)))
      (mh-yank-cur-msg))
    (save-excursion ;x
      (previous-line 3) ;x
      (beginning-of-line)
      (if (looking-at (concat "-\n" sc-reference-tag-string)) ;x
	  (delete-char 1)))
    (setq transient-mark-mode transient-mark-mode-sv)
    (message "C-c C-p C-y cites Person"))) ;x

(define-key mh-letter-mode-map "\C-c\C-y" 'mh-cite-msg)


(define-key sc-mode-map "\C-y" 'sc-insert-citation-1)

(defun sc-insert-citation-1 (&optional evenif)
  (interactive "P")
  (sc-insert-citation evenif)
  (forward-char (length (sc-mail-field "sc-citation"))))



(defun sc-header-attributed-writes ()
  "\"<sc-attribution>\" == <sc-author> <address> writes:
Treats these fields in a similar manner to `sc-header-on-said'."
  (let ((sc-mumble "")
	(whofrom (sc-whofrom)))
    (if whofrom
	(insert sc-reference-tag-string
		(sc-hdr "\"" (sc-mail-field "sc-attribution") "\" == ")
		(sc-hdr ""   (sc-mail-field "sc-author") " ")
		(or (sc-hdr "<" (sc-mail-field "sc-from-address") ">" t)
		    (sc-hdr "<" (sc-mail-field "sc-reply-address") ">"  t)
		    "")
		"\n"))))

(defun sc-mail-cleanup-blank-lines ()
  "Leave some blank lines after original mail headers are nuked.
The number of lines left is specified by `sc-blank-lines-after-headers'."
  (if sc-blank-lines-after-headers
      (save-restriction
	(widen)
	(skip-chars-backward " \t\n")
	(forward-line 1)
	;; SMW gelöscht: (delete-blank-lines)
	(save-excursion ; anstatt dessen
	  (while (re-search-forward "\n[ \t]*\n[ \t]*\n+" nil t) 
	    (replace-match "\n\n" nil nil)))
	(beginning-of-line)
	(if (looking-at "[ \t]*$")
	    (delete-region (regi-pos 'bol) (regi-pos 'bonl)))
	(insert-char ?\n sc-blank-lines-after-headers)))
  nil)



;; zusätzlich

(load "mail-emacs-19/mh/e-mh-alias") ; wird durch bbdb obsolet
;;(load "mail-emacs-19/mh/mailalias")

(load "mail-emacs-19/mh/fmailutils")
(add-hook 'mail-send-hook 'mail-add-fcc-related-headers)
(load "mail-emacs-19/mh/mail-reorder-headers")

(load "mail-emacs-19/mh/mail-verify")

(load "mail-emacs-19/mh/mh-crypt")


;; nicht schlecht, aber nicht ganz ausgereift (load-library "~walther/programme/mh/mh-scraps.el")

;(load "mh/yankorig")
;(setq mail-setup-hook
;      '(lambda () (load "yankorig")))



(defun transpose-longwords-point-and-mark ()
  "Vertauscht Wörter in Listen oder Kommalisten zwischen Punkt und Marke,
z.B. Email-Listen wie   walther@uni-mainz, walther@edv.klinik.uni-mainz.de"
  (interactive)
  (let ((p1)(p2)(p3)(tmp))
    (save-excursion
      (if (not mark-active)
	  (error "no mark set")
	(if (= (region-end) (point))
	    (exchange-point-and-mark))
	(if mark-active (region-end)(point-max))
	(re-search-backward "^\\|[ \t]" nil t)
	(if (not (looking-at "^"))
	    (forward-char 1))
	(setq p1 (point))
	(re-search-forward "[ \t,]\\|$" nil t)
	(if (not (looking-at "$"))
	    (backward-char 1))
	(copy-region-as-kill p1 (point))
	(kill-region p1 (point))
	(setq p2 (point))

	(exchange-point-and-mark)
	(re-search-backward "^\\|[ \t]" nil t)
	(if (not (looking-at "^"))
	    (forward-char 1))
	(setq p1 (point))
	(re-search-forward "[ \t,]\\|$" nil t)
	(if (not (looking-at "$"))
	    (backward-char 1))
	(kill-region p1 (point))
	(yank 2)
	(goto-char p2)
	(yank 0)))))


(define-key global-map "\M-T" 'transpose-longwords-point-and-mark)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;







;;; ********************************************************************************
;;;  Persönliche Einstellungen
;;; ********************************************************************************

(if (< emacs-major-version 20)
    (setq mh-letter-mode-hook
	  (list
	   (function
	    (lambda ()
	      (mime-mode)
	      (make-local-variable 'mail-header-separator)
	      (setq mail-header-separator "--------")
	      (setq fill-column 72)
	      (load "op-indent")
	      ))))
  (add-hook 'mh-letter-mode-hook
	    (function
	     (lambda ()
	       ;(turn-on-mime-edit)
	       (make-local-variable 'mail-header-separator)
	       (setq mail-header-separator "--------")
	       (setq fill-column 72)
	      (load "op-indent")
	      )))
  (add-hook 'mh-before-send-letter-hook 'mime-edit-maybe-translate)
  )




(setq mh-signature-file-name "~/.signature")
(setq signature-separator "\n")

(setq mh-comp-formfile "components")

(setq mh-repl-formfile "replcomps")






;;; ********************************************************************************
;;;  Sendendes
;;; ********************************************************************************


(defun mh-mail-region ()
  (interactive)
  (copy-region-as-kill (if mark-active (region-beginning)(point-min))
		       (if mark-active (region-end)(point-max)))
  (mh-smail-other-window)
  (insert-char ?\n 3)
  (yank)
  (goto-char (prog1 (mark t)
	       (set-marker (mark-marker) (point) (current-buffer))))
  (backward-char 2))

  
(defun mh-mail-to-cursorposition (cc subject)
  (interactive (list
		(mh-read-address "Cc: ")
		(read-string "Subject: ")))
  (mh-send (current-longword) cc subject))


(defadvice mh-send-letter (before untabify-buffer activate)
  (untabify (point-min) (point-max)))






;;; ********************************************************************************
;;;  Ansehendes
;;; ********************************************************************************


(defun mh-thread-scan (&optional initiators-name)
  "Invokes the thread scan program in the current buffer.
With arg asks for thread initiator to show these threads only."
  (interactive "P")
  (if (mh-outstanding-commands-p)
      (if (or mh-do-not-confirm
              (y-or-n-p
	       "Process outstanding deletes and refiles (or lose them)? "))
	  (mh-execute-commands)))
  (message "Scanning folder...")
  (let ((buffer-read-only-flag buffer-read-only)
	(linenr (count-lines (point-min)(point)))
	(current-msg (mh-find-msg-get-num mh-first-msg-num))
	(tmp))
    (if buffer-read-only
	(progn
	  (toggle-read-only nil)
	  (setq buffer-read-only-flag t)))
    (if (not initiators-name)
	(call-process-region (point-min)(point-max) "~/Mail/scan.pl" t t t (buffer-name))
      (call-process-region (point-min)(point-max) "~/Mail/scan.pl" 
			   t t t
			   (buffer-name)
			   "-from" (setq tmp (read-string "Regex of person: "))
			   "-to" tmp))
    (if (< emacs-major-version 20)
	(standard-display-european t)
      (standard-display-european t t))
    (toggle-read-only buffer-read-only-flag)
    ;; find email where you have started from
    (mh-goto-msg current-msg nil t))
  (message "Scanning folder...done"))

(defun mh-scan-different (&optional initiators-name)
  "Invokes the thread scan program in the current buffer.
With arg asks for thread initiator to show these threads only."
  (interactive "P")
  (if (mh-outstanding-commands-p)
      (if (or mh-do-not-confirm
              (y-or-n-p
	       "Process outstanding deletes and refiles (or lose them)? "))
	  (mh-execute-commands)))
  (message "Scanning folder...")
  (let ((buffer-read-only-flag buffer-read-only)
	(linenr (count-lines (point-min)(point)))
	(current-msg (mh-find-msg-get-num -1)))
    (if buffer-read-only
	(progn
	  (toggle-read-only nil)
	  (setq buffer-read-only-flag t)))
    (if (not initiators-name)
	(call-process-region (point-min)(point-max) "~/Mail/scan.pl" t t t (buffer-name) "-nothread")
      (call-process-region (point-min)(point-max) "~/Mail/scan.pl" 
			   t t t
			   (buffer-name)
			   " -nothread "
			   "-from " (read-string "Name of initiator: ")))
    (if (< emacs-major-version 20)
	(standard-display-european t)
      (standard-display-european t t))
    (toggle-read-only buffer-read-only-flag)
    ;; find email where you have started from
    (mh-goto-msg current-msg nil t))
  (message "Scanning folder...done"))


(define-key mh-folder-mode-map [?\M-R] 'mh-rescan-folder)
(define-key mh-folder-mode-map "\M-R" 'mh-thread-scan)



;; "bug" fixes

;; new, because old function was not working anymore with a threaded mail buffer
;; (easier and speed is about the same !!)
(defun mh-goto-msg (number &optional no-error-if-no-message dont-show)
  "Position the cursor at message NUMBER.
Optional non-nil second argument means return nil instead of
signaling an error if message does not exist; in this case,
the cursor is positioned near where the message would have been.
Non-nil third argument means not to show the message."
  (interactive "NGo to message: ")
  (let ((current-msg (mh-find-msg-get-num mh-first-msg-num)))
    (goto-char (point-min))
    (if (re-search-forward (concat "^[ \t]*" number) nil t)
	(if (eq (mh-find-msg-get-num -1) number)
	    (or dont-show
		(mh-maybe-show number)
		t)
	  (if (not no-error-if-no-message)
	      (message "No message %d" number))
	  (goto-char (point-min))
	  (re-search-forward (concat "^[ \t]*" current-msg) nil t))
      (goto-char (point-min))
      (if (re-search-forward (concat "#" number) nil t)
	  (message "duplicate message")
	(if (not no-error-if-no-message)
	    (message "No message %d" number))
	(goto-char (point-min))
	(re-search-forward (concat "^[ \t]*" current-msg) nil t)))))


;; new, because old function was not working anymore with a threaded mail buffer
;; new version is more independend for different scan programs of mh
(defun mh-delete-line (lines)
  ;; Delete version of kill-line.
  (delete-region (save-excursion 
		   (beginning-of-line)
		   (point)) (progn (forward-line lines) (point))))

(defun mh-delete-duplicates ()
  "Deletes duplicate mails recognised by the threades mail program"
  (interactive)
  (mh-thread-scan)
  (goto-char (point-min))
  (let ((counter 0))
    (while (re-search-forward "duplicate.*#" nil t)
      (forward-word 3)
      (backward-char 1)
      (save-excursion
	(mh-goto-msg (string-to-int (current-word)))
	(mh-delete-msg (string-to-int (current-word))))
      (setq counter (1+ counter))
      (goto-char (point-min))
      (next-line counter)
      (beginning-of-line))))


;; real bug fix, but not from e-mh, but from the repl program (has had no Umlaute)
(defun mh-reply (message &optional includep)
  "Reply to MESSAGE (default: current message).
If optional prefix argument INCLUDEP provided, then include the message
in the reply using filter `mhl.reply' in your MH directory.
Prompts for type of addresses to reply to:
   from    sender only,
   to      sender and primary recipients,
   cc/all  sender and all recipients.
If the file named by `mh-repl-formfile' exists, it is used as a skeleton
for the reply.  See also documentation for `\\[mh-send]' function."
  (interactive (list (mh-get-msg-num t) current-prefix-arg))
  (let ((minibuffer-help-form
	 "from => Sender only\nto => Sender and primary recipients\ncc or all => Sender and all recipients"))
    (let ((reply-to (or mh-reply-default-reply-to
			(completing-read "Reply to whom: "
					 '(("from") ("to") ("cc") ("all"))
					 nil
					 t)))
	  (folder mh-current-folder)
	  (show-buffer mh-show-buffer)
	  (config (current-window-configuration))
	  (SMWtemp)) ; SMW
      (message "Composing a reply...")
      (mh-show)
      (other-window 1)
      (setq SMWtemp (mh-get-header-field "Subject:"))
      (other-window -1)
      (mh-exec-cmd "repl" "-build" "-noquery" "-nodraftfolder"
	     (if (stringp mh-repl-formfile) ;must be string, but we're paranoid
		 (list "-form" mh-repl-formfile))
	     mh-current-folder message
	     (cond ((or (equal reply-to "from") (equal reply-to ""))
		    '("-nocc" "all"))
		   ((equal reply-to "to")
		    '("-cc" "to"))
		   ((or (equal reply-to "cc") (equal reply-to "all"))
		    '("-cc" "all" "-nocc" "me")))
	     (if includep
		 '("-filter" "mhl.reply")))
      (let ((draft (mh-read-draft "reply"
				  (expand-file-name "reply" mh-user-path)
				  t)))
	;; SMW ff.
	(goto-char (point-min))
	(if (re-search-forward "^Subject:" nil t)
	    (if (search-forward "Re:")
		(progn
		  (kill-line)
		  (insert (concat " " SMWtemp)))))
	;; SMW


	(delete-other-windows)
	(save-buffer)
	(let ((to (mh-get-header-field "To:"))
	      (subject (mh-get-header-field "Subject:"))
	      (cc (mh-get-header-field "Cc:")))
	  (goto-char (point-min))
	  (mh-goto-header-end 1)
	  (or includep
	      (mh-in-show-buffer (show-buffer)
		(mh-display-msg message folder)))
	  (mh-add-msgs-to-seq message 'answered t)
	  (message "Composing a reply...done")
	  (mh-compose-and-send-mail draft "" folder message to subject cc
				    mh-note-repl "Replied:" config))))))


;;; ********************************************************************************


(defvar mh-note-interpreted "I"
  "String whose first character is used to notate mimeinterpreted or uudecoded messages.")

(define-key mh-folder-mode-map "\C-i" 'mh-interpret-mail)


(defun mh-interpret-mail ()
  (interactive)
  (mh-show)
  (setq msg (mh-get-msg-num t))
  (if (< emacs-major-version 20)
      ;; emacs 19 braucht tm
      (if (fboundp 'tm-mh-e/toggle-decoding-mode)
	  (progn
	    (tm-mh-e/toggle-decoding-mode t)
	    (tm-mh-e/toggle-decoding-mode nil)))
    ;; emacs 20 braucht semi
    (if (fboundp 'emh-toggle-decoding-mode)
	(progn
	  (emh-toggle-decoding-mode t)
	  (emh-toggle-decoding-mode nil))))
  (save-excursion
    (pop-to-buffer "show-+inbox") ; von der +inbox zum Buffer show-+inbox
    (setq transient-mark-mode nil)
    (goto-char (point-min))
    (if (re-search-forward "^begin [0-9][0-9][0-9] " nil t)
	;; then its a uuencoded mail
	(call-interactively 'uudecode-region)
      ;; otherwise interpret it as a mime-mail
      (mark-whole-buffer)
      (call-interactively 'mime-interpret-region)))
  (pop-to-buffer "+inbox")
  (mh-notate msg mh-note-interpreted mh-cmd-note)
  (if (< emacs-major-version 20)
      ;; emacs 19 braucht tm
      (if (fboundp 'tm-mh-e/toggle-decoding-mode)
	  (progn
	    (tm-mh-e/toggle-decoding-mode t)
	    (tm-mh-e/toggle-decoding-mode nil)))
    ;; emacs 20 braucht semi
    (if (fboundp 'emh-toggle-decoding-mode)
	(progn
	  (emh-toggle-decoding-mode t)
	  (emh-toggle-decoding-mode nil)))))



;;; ******************************

(defun mime-interpret-region (directory mailsolassen 
					write-or-interpret 
					&optional 
					region-beginning region-end viewmode buffer nodisplay)
  "Interpret a body part of a MIME message with metamail in current
buffer.  1st argument specifies the directory to write files to, 2nd
argument leaves mail unchanged if t, pointing to written file
otherwise, optional 3t and 4th argument is the current region,
optional 5th argument VIEWMODE specifies the value of the
EMACS_VIEW_MODE environment variable (defaulted to 1).  Optional 6th
argument BUFFER specifies a buffer to be filled (nil means current),
argument NODISPLAY non-nil means buffer is not redisplayed as output
is inserted."
  (interactive "DDirectory to write files in mail to: \ncLeave mail unchanged ? (y or n) \
\ncInterpret mail (i) or write content to file (w) ? ")
  (if (string= (substring directory -1) "/") 
      (setq directory (substring directory 0 -1))) ; streiche / am Ende: nur UNIX Systeme
  (let ((metamail-program-name "env")
	(metamail-switches metamail-switches)
	(message-text "Done"))
    ;;(debug)
    (if (eq write-or-interpret 105)
	(progn
	  (setq metamail-switches 
		(mapcar 'eval '( (concat "METAMAIL_TMPDIR=" (expand-file-name directory "/")) 
				 "metamail" 
				 "-x" "-d" "-z")))
	  (setq mailsolassen 121))
      (setq metamail-switches 
	    (mapcar 'eval '( (concat "METAMAIL_TMPDIR=" (expand-file-name directory "/")) 
			     "metamail"
			     "-x" "-d" "-z" "-w"))))
    (if (eq mailsolassen 121) (setq mailsolassen t) (setq mailsolassen nil)) ; wenn Character = "y" = 121
    (condition-case conds
	(progn
	  (metamail-region
	   (if mark-active (region-beginning)(point-min))
	   (if mark-active (region-end)(point-max))
	   viewmode buffer nodisplay)
	  (save-excursion
	    (goto-char (point-min))
	    (if (re-search-forward "^Command failed: " nil t)
	      (progn
		(setq mailsolassen t)
		(setq message-text "Undefinite error - please look at your mail (maybe just kill show-+inbox buffer)")))))
      (setq mailsolassen t)
      (setq message-text "Undefinite error - please look at your mail (maybe just kill show-+inbox buffer)"))
    (while (re-search-forward "^Wrote file .*$" nil t)
	(progn  
	  (if window-system
	      (progn
		(require 'hilit19)
		(hilit-region-set-face (match-beginning 0)(match-end 0) 'highlight)))
	  (forward-char 1)
	  (save-match-data
	    (if (y-or-n-p "Rename file at \"Wrote file...\" (same as M-x rename-file-at-cursorposition) ? ")
		(if mailsolassen
		    (call-interactively 'rename-file-at-cursorposition-2)
		  (call-interactively 'rename-file-at-cursorposition))))
	  (if window-system
	      (hilit-unhighlight-region (match-beginning 0)(match-end 0)))))
    (if mailsolassen
	(revert-buffer t t t)
      (save-buffer))
    (message message-text)))


(defun rename-file-at-cursorposition-2 (newname)
  (interactive "FNew Name: ")
  (setq buffer-read-only nil)
  (let ((filebeforestring (current-longword))
	(filereplacedstring (expand-file-name newname)))
    (if (file-exists-p newname)
	(if (y-or-n-p "Overwrite existing file ? ")
	    (rename-file filebeforestring filereplacedstring t)
	  (rename-file filebeforestring filereplacedstring nil)))
    (re-search-backward "^\\|[ \t\r]" nil t)
    (search-forward filebeforestring nil t)
    (replace-match filereplacedstring nil t)))


(defun rename-file-at-cursorposition (newname)
  (interactive "FNew Name: ")
  (setq buffer-read-only nil)
  (let ((filebeforestring (current-longword))
	(filereplacedstring (expand-file-name newname)))
    (if (file-exists-p newname)
	(if (y-or-n-p "Overwrite existing file ? ")
	    (rename-file filebeforestring filereplacedstring t)
	  (rename-file filebeforestring filereplacedstring nil)))
    (re-search-backward "^\\|[ \t\r]" nil t)
    (search-forward filebeforestring nil t)
    (replace-match filereplacedstring nil t)
    (if (y-or-n-p "Replace filename in the whole buffer ? ")
	(progn
	  (save-excursion
	    (goto-char (point-min))
	    (while (search-forward filebeforestring nil t)
	      (replace-match filereplacedstring nil t)))))))
  
(defun current-longword ()
;;; ACHTUNG !!! siehe auch Funktion "current-longword-quote" im File op-columns.el !!!!
  (interactive)
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
    (buffer-substring-no-properties begin end)))

(defun uudecode-region (directory &optional solassen beg end buffer nodisplay)
  (interactive "DDirectory to write file to: \ncLeave input unchanged ? (y or n) ")
  (if (not (string= (substring directory -1) "/"))
      (setq directory (concat directory "/"))) ; addiere / am Ende: nur UNIX Systeme
  (if (eq solassen 110) (setq solassen t) (setq solassen nil)) ; wenn Character = "n" = 110
  (let ((default-directory directory) uuname)
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "^begin [0-9][0-9][0-9] " nil t)
	  (setq uuname (current-longword))))
    (if (file-exists-p (concat directory uuname))
	(if (not (y-or-n-p "Overwrite existing file ? "))
	    (error "uudecoding aborted")))
    (condition-case conds
	(if (not (= (call-process-region 
		     (if mark-active (region-beginning)(point-min))
		     (if mark-active (region-end)(point-max))
		     "uudecode" 
		     solassen
		     buffer
		     (not nodisplay))
		    0))
	    (revert-buffer t t t))
      (revert-buffer t t t)
      (message "Undefinite error - please look at your mail (maybe just kill show-+inbox buffer)"))
    (if solassen
	(progn
	  (insert "\n\nWrote file at " directory uuname)
	  (backward-word 1)
	  (if (y-or-n-p "Rename file at \"Wrote file...\" (same as M-x rename-file-at-cursorposition) ? ")
	      (call-interactively 'rename-file-at-cursorposition-2)
	    (call-interactively 'rename-file-at-cursorposition)))
      (message (concat "Wrote file at " directory uuname)))))



;;; ********************************************************************************
;;;  Umlautiges  ---  siehe auch noch op-main.el
;;; ********************************************************************************



(defun replace-umlaute (dummy)
; needs perl to be installed
  (interactive "sHit return to REPLACE Umlaute (ü->ue e.g.) in current buffer/region")
  (save-excursion
    (goto-char (point-min))
    (let ((before-change-functions nil)(after-change-functions nil))
      (insert-char ?\n  1))
    (backward-char 1)
    (shell-command-on-region
     (point-min)
     (point-max)
    "perl -ne \"\\$/=undef;\\$_=<>;%convert=(\\\"ä\\\",\\\"ae\\\",\\\"ö\\\",\\\"oe\\\",\\\"ü\\\",\\\"ue\\\",\\\"Ä\\\",\\\"Ae\\\",\\\"Ö\\\",\\\"Oe\\\",\\\"Ü\\\",\\\"Ue\\\",\\\"ß\\\",\\\"ss\\\");s/([\\\\200-\\\\377])/\\$convert{\\$1}/g;print\" " t)
    ))
; $/ = undef; $_ = <>;
; %convert = ("ä","ae","ö","oe","ü","ue","Ä","AE","Ö","OE","Ü","UE");
; s/([\200-\377])/$convert{$1}/g;
; print;
;    "perl -e \"print \\\"Hallo Stefan\\\" \" ")))


(defun regenerate-umlaute (dummy)
; needs perl to be installed
  (interactive "sHit return to REPLACE Umlaute in current buffer")
  (save-excursion
    (goto-char (point-min))
    (let ((before-change-functions nil)(after-change-functions nil))
    (insert-char ?\n  1))
    (backward-char 1)
    (save-excursion
;;; unerklärter Effekt (BUG?) in shell-command-on-region in diesem Kontext: SCHNEIDET DIE ERSTE ZEILE WEG !!!
;;; daher: erst Einfügen einer Leerzeile
      (shell-command-on-region
       (point-min)
       (point-max)
       "perl -ne \"\\$/=undef;\\$_=<>;%convert=(\\\"=C4\\\",\\\"Ä\\\",\\\"=D6\\\",\\\"Ö\\\",\\\"=DC\\\",\\\"Ü\\\",\\\"=E4\\\",\\\"ä\\\",\\\"=F6\\\",\\\"ö\\\",\\\"=FC\\\",\\\"ü\\\",\\\"=DF\\\",\\\"ß\\\",\\\"=20\\\",\\\" \\\");s/(=C4|=D6|=DC|=E4|=F6|=FC|=DF|=20)/\\$convert{\\$1}/g;print\" " t)
      (goto-char (point-min))
      (insert-char ?\n 1)
      (shell-command-on-region
       (point-min)
       (point-max)
       "perl -ne \"\\$/=undef;\\$_=<>;%convert=(\\\"=3DC4\\\",\\\"Ä\\\",\\\"=3DD6\\\",\\\"Ö\\\",\\\"=3DDC\\\",\\\"Ü\\\",\\\"=3DE4\\\",\\\"ä\\\",\\\"=3DF6\\\",\\\"ö\\\",\\\"=3DFC\\\",\\\"ü\\\",\\\"=3DDF\\\",\\\"ß\\\",\\\"=3D20\\\",\\\" \\\");s/(
=3DC4|=3DD6|=3DDC|=3DE4|=3DF6|=3DFC|=3DDF|=3D20)/\\$convert{\\$1}/g;print\" " t)
      (goto-char (point-min))
      (insert-char ?\n 1)
      (shell-command-on-region
       (point-min)
       (point-max)
       "perl -ne \"\\$/=undef;\\$_=<>;%convert=(\\\"=3D\\\",\\\"=\\\");s/(=3D)/\\$convert{\\$1}/g;print\" " t)
      (goto-char (point-min))
      (replace-string "=\n" "")
      )))
; $/ = undef; $_ = <>;
; %convert = ("=C4","Ä","=D6","Ö","=DC","Ü","=E4","ä","=F6","ö","=FC","ü","=DF","ß");
; s/(=..)/$convert{$1}/g;
; print;
;    "perl -e \"print \\\"Hallo Stefan\\\" \" ")))



;> > Guten Morgen Stefan,
;> >
;> > ich komme nachher mal vorbei um Dir ein
;> > Geburtstagsst=3DE4ndchen zu bringen.
;> > Solltest Du heute vormittag schon verplant=3D20
;> > sein, dann sag mir doch kurz Bescheid,=3D20
;> > wann es Dir besser pa=3DDFt.
;> >
;> > Gru=3DDF Christine
;>=20
;> Hallo Christine,
;>=20
;>   von 10-12 bin ich in einer Besprechung: Wie sieht es danach bei Dir
;> aus ?
;>=20
;> Viele Gruesse, bis nacher
;>=20
;> Stefan




(defun mh-umlaut-korrektur ()
  (interactive)
  (regenerate-umlaute "")
;  (replace-umlaute "")
  )


(defvar mh-note-umlaute-konversion "u"
  "String whose first character is used to notate mimeinterpreted or uudecoded messages.")

(define-key mh-folder-mode-map [?\C-Q] 'mh-mail-umlaut-korrektur)

(defun mh-mail-umlaut-korrektur ()
  (interactive)
  (mh-show)
  (setq msg (mh-get-msg-num t))
  (save-excursion
    (pop-to-buffer "show-+inbox") ; von der +inbox zum Buffer show-+inbox
    (setq transient-mark-mode nil)
    (goto-char (point-min))
    (let (buffer-read-only-sv buffer-read-only)
      (setq buffer-read-only nil)
      (mh-umlaut-korrektur)
      (save-buffer)
      (setq buffer-read-only nil)))
  (pop-to-buffer "+inbox")
  (mh-show)
  (mh-notate msg mh-note-umlaute-konversion mh-cmd-note))

(define-key mh-folder-mode-map  [?\C-\S-Q] '(lambda() (interactive)
					      (other-window 1)
					      (if (< emacs-major-version 20)
						  (standard-display-european t)
						(standard-display-european t t))
					      (other-window -1)))




;;; ********************************************************************************
;;;  mh-Erweiterndes
;;; ********************************************************************************




;;;     Creating useful default folder for refiling via mh-default-folder-for-message-function

(defun my-mh-folder-from-address ()
  "Determine folder name from address.
     Takes the address in the From: header field, and returns its corresponding
     alias from the user's personal aliases file. Returns `nil' if the address
     was not found."
  (require 'rfc822)                         ; for the rfc822 functions
  (search-forward-regexp "^From: \\(.*\\)") ; grab header field contents
  (save-excursion                     ; save state
    (let ((addr (car (rfc822-addresses  ; get address
		      (buffer-substring (match-beginning 1)
					(match-end 1)))))
	  (buffer (get-buffer-create " *temp*")) ; set local variables
	  folder)
      (set-buffer buffer)             ; jump to temporary buffer
      (unwind-protect                 ; run kill-buffer when done
	  (progn                      ; function grouping construct
                 (insert-file-contents (expand-file-name "aliases"
                                                         mh-user-path))
                 (goto-char (point-min))   ; grab aliases file and go to start
                 (setq folder
                       ;; Search for the given address, even commented-out
                       ;; addresses are found!
                       ;; The function search-forward-regexp sets values that are
                       ;; later used by match-beginning and match-end.
                       (if (search-forward-regexp (format "^;*\\(.*\\):.*%s"
                                                          addr) nil t)
                           ;; NOTE WELL: this is what the return value looks like.
                           ;; You can modify the format string to match your own
                           ;; Mail hierarchy.
                           (format "+%s" (buffer-substring (match-beginning 1)
                                                           (match-end 1))))))
	(kill-buffer buffer))          ; get rid of our temporary buffer
      folder)))                        ; function's return value

(setq mh-default-folder-for-message-function 'my-mh-folder-from-address)





;;;====================================================================================================
;;;====================================================================================================
;;;====================================================================================================



;;; Dinge für den Fax-Server

(defvar fax-formular "~/Mail/fax.txt")

(defun insert-fax-date ()
  (interactive)
  (let ((before-change-functions nil)(after-change-functions nil))
  (let ((calendar-date-display-form  '(day "." month "." year)))
    (insert (calendar-date-string (calendar-current-date))))))

(defun fax ()
  (interactive)
  (re-search-backward mail-header-separator)
  (forward-line 1)
  (insert-file fax-formular)
  (search-forward "[datum]" nil t)
  (backward-kill-word 1)
  (backward-delete-char-untabify 1)
  (insert-fax-date)
  (forward-line 3)
  (set-fill-column 100000)
  (recenter)
  (message "Automatische Zeilenumbrüche werden vom Fax-Server nach Abgang der Mail übernommen"))




;;;====================================================================================================
;;;====================================================================================================
;;;====================================================================================================




;;; Dinge für den Kalender

(defun mh-c ()
  "Combines the c calendar with mh Email"
  (interactive)
  (let ((tmp-mh-original-window-config (current-window-configuration)))
    (other-window 1)
    (split-window-vertically)
    (other-window 1)
    (setq original-window-config tmp-mh-original-window-config)
    (c nil t)))

;(if (not (get-buffer "*Kalender*"))
;    (load "op-calendar/op-calendar"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  BBDB setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; warum auch immer, zum Debuggen gibt es jetzt gerade keine Zeit: 
;; Das hier läuft nur, wenn vorher mh gestartet wurde. Daher (und weil es
;; sowieso Zeit spart, da dann alles auf einmal weg ist):

(load "mh-e")

;;
;; emacs19 kann nicht mit bbdb-2.0 ohne itimer-Funktion, daher für 19 auskommentiert
;;



(if (>= emacs-major-version 20)
    (progn
      (setq load-path (cons (expand-file-name "~/op-project/mail-emacs-20/bbdb-2.00.02") load-path))

      (autoload 'bbdb         "bbdb-com" "Insidious Big Brother Database" t)
      (autoload 'bbdb-name    "bbdb-com" "Insidious Big Brother Database" t)
      (autoload 'bbdb-company "bbdb-com" "Insidious Big Brother Database" t)
      (autoload 'bbdb-net     "bbdb-com" "Insidious Big Brother Database" t)
      (autoload 'bbdb-notes   "bbdb-com" "Insidious Big Brother Database" t)
      ;;(autoload 'bbdb-insinuate-vm       "bbdb-vm"    "Hook BBDB into VM")
      ;;(autoload 'bbdb-insinuate-rmail    "bbdb-rmail" "Hook BBDB into RMAIL")
      (autoload 'bbdb-insinuate-mh       "bbdb-mhe"   "Hook BBDB into MH-E")
      (autoload 'bbdb-insinuate-gnus     "bbdb-gnus"  "Hook BBDB into GNUS")
      (autoload 'bbdb-insinuate-sendmail "bbdb"       "Hook BBDB into sendmail")

      (load "bbdb")
      (load "bbdb-mhe")
      (load "bbdb-migrate")
      (load "bbdb-com")
      (load "bbdb-ftp")
      (load "bbdb-gnus")
      (load "bbdb-hooks")
      (load "bbdb-mew")
      (load "bbdb-sc")
      (load "bbdb-snarf")
      (load "bbdb-w3")
      (load "bbdb-whois")
      (if (>= emacs-major-version 20)
	  (bbdb-initialize 'gnus 'mh-e 'sendmail 'sc 'w3))

      (setq bbdb/mail-auto-create-p t)
      (setq bbdb-completion-type nil) ;'primary-or-name)
      (setq bbdb-notice-auto-save-file t)
      ;;(setq bbdb-user-mail-names "walther@.*uni-mainz.de")
      (setq bbdb-user-mail-names "Dieses.Feature.will.ich.nicht.haben")
      (setq bbdb-always-add-addresses t) ;; händig mit mh-insert-mailaddresses-in-bbdb
      
      (setq bbdb-new-nets-always-primary t)
      (setq bbdb-default-area-code 06131)
      (setq bbdb-north-american-phone-numbers-p nil)
      (setq bbdb-message-caching-enabled nil)
      (setq bbdb-change-hook 'bbdb-timestamp-hook)
      (setq bbdb-create-hook 'bbdb-creation-date-hook)
      (setq bbdb-elided-display t)
      (setq bbdb-pop-up-target-lines 13)

      (setq bbdb-use-pop-up nil)
      (setq bbdb-pop-up-target-lines 1) ; leider Ärgernis im semi: weniger als 5 geht hier nicht
      (setq bbdb-send-mail-style 'mh)
      ;; unbd nun noch die letzte bbdb-Frage vermeiden
      (setq bbdb-offer-save 1)


      (defun bbdb/mh-annotate-sender (string)
	"Add a line to the end of the Notes field of the BBDB record
corresponding to the sender of this message."
	(interactive (list (if bbdb-readonly-p
			       (error "The Insidious Big Brother Database is read-only.")
			     (read-string "Comments: "))))
	(mh-show)
	(let ((b (current-buffer))
	      (p (point)))
	  (set-buffer mh-show-buffer)
	  (if (bbdb/mh-update-record t)
	      (bbdb-annotate-notes (bbdb/mh-update-record t) string))
	  (set-buffer b)
	  (goto-char p)))

      
      ;;(setq tm-bbdb/use-mail-extr nil)
      ;;  "*If non-nil, `mail-extract-address-components' is used.
      ;; Otherwise `tm-bbdb/extract-address-components' overrides it.")
      
      ;;(setq tm-bbdb/auto-create-p t)
      ;;  "*If t, create new BBDB records automatically.
      ;; If function, then it is called with no arguments to decide whether an
      ;; entry should be automatically creaded.
      ;;
      ;; tm-bbdb uses this variable instead of `bbdb/mail-auto-create-p' or
      ;; `bbdb/news-auto-create-p' unless other tm-MUA overrides it.")
      
      (setq tm-bbdb/delete-empty-window t)
      ;;  "*If non-nil, delete empty BBDB window.
      ;; All bbdb-MUAs but bbdb-gnus display BBDB window even if it is empty.
      ;; If you prefer behavior of bbdb-gnus, set this variable to t.
      
      
      ;; bbdb stellte für emh keine name-completion ein
      (defadvice emh-forward (before mh-bbdb-forward act)
	(interactive (list (bbdb-read-addresses-with-completion "To: ")
			   (bbdb-read-addresses-with-completion "Cc: ")
			   (if current-prefix-arg
			       (mh-read-seq-default "Forward" t)
			     (mh-get-msg-num t)))))
      
      (defun mh-insert-mailaddresses-in-bbdb ()
	(interactive)
	(mh-scan-different)
	(mh-show)
	(if (< emacs-major-version 20)
	    ;; emacs 19 braucht tm
	    (if (fboundp 'tm-mh-e/toggle-decoding-mode)
		(progn
		  (tm-mh-e/toggle-decoding-mode t)
		  (tm-mh-e/toggle-decoding-mode nil)))
	  ;; emacs 20 braucht semi
	  (if (fboundp 'emh-toggle-decoding-mode)
	      (progn
		(emh-toggle-decoding-mode t)
		(emh-toggle-decoding-mode nil))))

	(let ((bbdb-always-add-addresses bbdb-always-add-addresses)
	      (bbdb-new-nets-always-primary bbdb-new-nets-always-primary)
	      (bbdb-quiet-about-name-mismatches bbdb-quiet-about-name-mismatches)
	      (bbdb-use-alternate-names bbdb-use-alternate-names)
	      (current-buf (current-buffer))
	      addrs)
	  (setq bbdb-always-add-addresses t)
	  (setq bbdb-new-nets-always-primary t)
	  (setq bbdb-quiet-about-name-mismatches t)
	  (setq bbdb-use-alternate-names t)
	  (while (not (eobp))
	    (progn
	      ;;---
	      (bbdb/mh-annotate-sender (current-line))
	      (other-window 1)
	      (save-excursion
		(message (concat "Scanning and updating BBDB..." (prin1-to-string (current-buffer))))
		(setq addrs
			(append
			 (save-excursion
			   (bbdb-split (or (bbdb-extract-field-value "from") "") ","))
			 (save-excursion
			   (bbdb-split (or (bbdb-extract-field-value "to") "") ","))
			 (save-excursion
			   (bbdb-split (or (bbdb-extract-field-value "cc") "") ","))
			 (save-excursion
			   (bbdb-split (or (bbdb-extract-field-value "bcc") "") ","))))
		(let ((rest addrs)
		      (records '())
		      record)
		  (while rest
		    (setq record (bbdb-annotate-message-sender (car rest) t t nil))
		    (if record (setq records (cons record records)))
		    (setq rest (cdr rest)))
		  (setq records (sort records '(lambda (x y) (bbdb-record-lessp x y))))
					;(bbdb-display-records records)
		  )))
	      ;;---
	      ;; (bbdb/mh-annotate-sender (current-line))
	      ;;(bbdb-show-all-recipients)
	      ;;(bbdb-bury-buffer)
	      ;(get-buffer current-buf)
	      (other-window -1)
	      (next-line 1)
	      (not (eobp))
	      ))
	(let ((current-buf (current-buffer)))
	  (switch-to-buffer ".bbdb")
	  (save-buffer)
	  (switch-to-buffer current-buf))
	;; save last saved number in a file .bbdb.+buffername
	(mh-last-msg)
	(beginning-of-line)
	(if (looking-at "\\([0-9]+\\)")
	    (progn
	      (write-region (match-beginning 0) (match-end 0) 
			    (concat ".bbdb." (buffer-name)) nil)))
	(message "...done, last analysed number written to .bbdb.<buffername>"))
      


      (defun current-line ()
	(interactive)
	(let ((begin) (end))
	  (save-excursion
	    (beginning-of-line)
	    (setq begin (point))
	    (end-of-line)
	    (setq end (point)))
	  (buffer-substring-no-properties begin end)))
      
      
      
      (defun mh-mail-region-bbdb ()
	"Mails current region using the BBDB"
	(interactive)
	(copy-region-as-kill (if mark-active (region-beginning)(point-min))
			     (if mark-active (region-end)(point-max)))
	(call-interactively 'bbdb-net)
	(insert-char ?\n 3)
	(yank)
	(goto-char (prog1 (mark t)
		     (set-marker (mark-marker) (point) (current-buffer))))
	(backward-char 2))
      
      (fset 'mail-region-bbdb 'mh-mail-region-bbdb)
      (fset 'mail-region 'mh-mail-region)

      ;; mh-Aufruf
      (define-key mh-folder-mode-map "b" 'bbdb-net)


      ;; und das hier, weil mich die Vorgabe nervt
      (define-key bbdb-mode-map [(meta o)]  'bbdb-insert-new-field)
      (define-key bbdb-mode-map [(control o)]  'other-window)

      (define-key bbdb-mode-map [(g)]  '(lambda() (interactive)
					  "Sucht schnell den Eintrag der gesuchten Mail aus dem mh-Folder.
Key g für mh-goto-message aus der bbdb heraus"
					  (re-search-forward "[^ \t]")
					  (let ((tmp (string-to-number (current-word))))
					    (other-window 1)
					    (mh-goto-msg tmp))))

)) ; end if bbdb



;;
;; Tasten-Definitionen
;;

(define-key mh-folder-mode-map "k" 'mh-c)
(define-key mh-folder-mode-map "\C-o" 'other-window)
(define-key mh-folder-mode-map "\C-f" 'mh-write-msg-to-file)



(setq mh-summary-heigh 8) ; Höhe des MH-show+-inbox-Bildes

