


(setq crypt-encryption-type 'des)
(setq crypt-confirm-password t)
(setq crypt-encrypted-disable-auto-save nil)
(setq crypt-encoded-disable-auto-save nil)
(require 'crypt++)


(defun find-file-crypted (filename &optional force)
  "Edit file FILENAME.
Switch to a buffer visiting file FILENAME *without* extention .crypt,
creating one if none already exists.
  Filters under certain conditions Logbuch-Buffer-Files, prefix argument
forces loading in case of refusion."
  (interactive "FFind file: ")
  ;;
  ;; Überprüfung, ob das crypt-Kommando auf diesem Rechner ist, wenn nein, dann Fehlermeldung
  (switch-to-buffer "*ASYNC*")
  (shell-command (concat " test -n \"`type -path "
			 (prin1-to-string crypt-encryption-type)
			 "`\" && echo yes") t)
  (if (not (looking-at "yes"))
      (progn
	(kill-buffer (current-buffer))
	(error "crypt command not on this machine"))
    (kill-buffer (current-buffer)))
  ;;
  ;;
  (string-match "\\(.*/\\)*" (prin1-to-string filename t))
  (let* ((save-string-tmp (match-end 0))
	 (end-of-namestring (string-match "\\.crypt" (prin1-to-string filename t)))
	 (new-buffer-name (substring (prin1-to-string filename t) save-string-tmp end-of-namestring)))
    (if (get-buffer new-buffer-name)
	(switch-to-buffer new-buffer-name)
      (if (get-buffer (substring (prin1-to-string filename t) save-string-tmp))
	  (error "Buffer error: Only shaddow buffer %s is there: Please correct (i.g. by deleting this buffer ?)" filename)
	(if (and (boundp 'logbuch-zeitobjekt)
		 (boundp 'logbuch-projectdoc-buffer)
		 (boundp 'logbuch-buffer))
	    ;; angeforderter File ist nicht der Logbuch-File
	    (if (not (string= (expand-file-name (file-name-nondirectory filename))
			      (expand-file-name (file-name-nondirectory logbuch-projectdoc-buffer))))
		(switch-to-buffer (find-file-noselect filename))
	      ;; falls doch: der Logbuch-buffer war noch nicht aufgerufen (logbuch-buffer==nil)
	      (if (not logbuch-buffer)
		  (switch-to-buffer (find-file-noselect filename))
		;; --- und hier wird ignoriert falls kein Prefix-Argument <> 1 vorliegt
		(if (not (string= (prin1-to-string (prefix-numeric-value force))
				  "1"))
		    (switch-to-buffer (find-file-noselect filename)))
		))
	  (switch-to-buffer (find-file-noselect filename)))))))



(defun save-buffer-crypted ()
  (interactive)
  (if (buffer-modified-p)
      
      ;; Dieser Absatz: Wechselwirkung aus dem File op-includer.el (Nachprüfen, ob alle #include-Statements ausgelagert sind)
      (save-excursion
	;;
	;; Überprüfung, ob das crypt-Kommando auf diesem Rechner ist, wenn nein, dann Fehlermeldung
	(switch-to-buffer "*ASYNC*")
	(shell-command (concat " test -n \"`type -path "
			       (prin1-to-string crypt-encryption-type)
			       "`\" && echo yes") t)
	(if (not (looking-at "yes"))
	    (progn
	      (kill-buffer (current-buffer))
	      (error "crypt command not on this machine"))
	  (kill-buffer (current-buffer)))
	;;
	;;
	(goto-char (point-min))
	(if (search-forward "end_include_statement" nil t)
	    (if (not
		 (y-or-n-p "Sie haben registriert, dass es noch nicht ausgelagerte #include-Files in diesem File gibt? "))
		(error "Speichervorgang abgebrochen")))
	(let* ((current-buf (prin1-to-string (current-buffer) t))
	       (newname (concat current-buf ".crypt")))
	  (switch-to-buffer newname t)
	  (toggle-read-only -1)
	  (erase-buffer)
	  (switch-to-buffer current-buf)
	  (widen)
	  (copy-to-buffer newname (point-min) (point-max))
	  (switch-to-buffer newname t)
	  (goto-char (point-min))
	  (replace-regexp "\r" "\n")
	  (crypt-encrypted-mode 1)
	  (auto-save-mode 0)
	  (if (file-exists-p (concat newname "~"))
	      (copy-file (concat newname "~") (concat newname ".bak") t))
	  (message "This might take a short while...")
	  (write-file newname)
	  (auto-save-mode 1)
	  (toggle-read-only 1)
	  (switch-to-buffer current-buf)
	  (set-buffer-modified-p nil)))))



(defun write-file-crypted (name &optional noninteractive)
;;;
;;; if you are changing something here: keep attention and look at the file
;;; op-project.crypt.el
;;;
  (interactive
   (list (if buffer-file-name
	     (read-file-name "Write file (without .crypt): "
			     nil nil nil nil)
	   (read-file-name "Write file (without .crypt): "
			   (cdr (assq 'default-directory
				      (buffer-local-variables)))
			   nil nil (buffer-name)))))
  (require 'op-crypt "op-crypt")
  ;;
  ;; Überprüfung, ob das crypt-Kommando auf diesem Rechner ist, wenn nein, dann Fehlermeldung
  (switch-to-buffer "*ASYNC*")
  (shell-command (concat " test -n \"`type -path "
			 (prin1-to-string crypt-encryption-type)
			 "`\" && echo yes") t)
  (if (not (looking-at "yes"))
      (progn
	(kill-buffer (current-buffer))
	(error "crypt command not on this machine"))
    (kill-buffer (current-buffer)))
  ;;
  ;;
  ;; Dieser Absatz: Wechselwirkung aus dem File op-includer.el (Nachprüfen, ob alle #include-Statements ausgelagert sind)
  (save-excursion
    (goto-char (point-min))
    (if (search-forward "end_include_statement" nil t)
	(if (not
	     (y-or-n-p "Sie haben registriert, dass es noch nicht ausgelagerte #include-Files in diesem File gibt? "))
	  (error "Speichervorgang abgebrochen")))
    (let* ((newname (concat name ".crypt")) (current-buf (current-buffer)) (position (point))
	   (fnndn (file-name-nondirectory name)) (fnndnn (file-name-nondirectory newname)))
      (if (and (not (string= (buffer-name) fnndn))
	       (get-buffer fnndn))
	  (if (y-or-n-p "buffer alredy exists: overwrite ? ")
	      (progn
		(kill-buffer fnndn)
		(if (get-buffer fnndnn)
		    (kill-buffer fnndnn)))
	    (error "canceling function \"write-file-crypted\"")))
      (switch-to-buffer fnndnn t)
      (erase-buffer)
      (switch-to-buffer current-buf)
      (widen)
      (if (file-exists-p (concat newname "~"))
	  (copy-file (concat newname "~") (concat newname ".bak") t))
      (copy-to-buffer fnndnn (point-min) (point-max))
      (switch-to-buffer fnndnn t)
      (goto-char (point-min))
      (replace-regexp "\r" "\n")
      (crypt-encrypted-mode 1)
      (auto-save-mode 0)
      (write-file newname)
      (auto-save-mode 1)
      (if (not (string= (buffer-name current-buf) fnndn))
	  (progn
	    (let* ((bfn (buffer-file-name)) (bftn nil))
	      (if (fboundp 'buffer-file-truename)
		  (setq bftn (buffer-file-truename)))
	      (switch-to-buffer current-buf)
	      (set-buffer-modified-p nil)
	      (copy-to-buffer fnndn (point-min) (point-max))
	      (switch-to-buffer fnndn)
	      (set-buffer-modified-p nil)
	      (setq buffer-file-name
		    (substring bfn 0 (string-match "\\.crypt$" bfn)))
	      (if bftn
		  (setq buffer-file-truename
			(substring bftn 0 (string-match "\\.crypt$" bftn)))))
	    ;; Lösche den Orginal-Schattenbuffer (falls da)
	    (if (get-buffer (concat (buffer-name current-buf) ".crypt"))
		(kill-buffer (concat (buffer-name current-buf) ".crypt")))
	    ;; Lösche den Orginal-Buffer
	    (kill-buffer current-buf)
	    (goto-char position))
	(let ((bfn (buffer-file-name)) (bftn nil))
	  (if (fboundp 'buffer-file-truename)
	      (setq bftn (buffer-file-truename)))
	  (switch-to-buffer current-buf)
	  (set-buffer-modified-p nil)
	  (setq buffer-file-name
		(substring bfn 0 (string-match "\\.crypt$" bfn)))
	  (if bftn
	      (setq buffer-file-truename
		    (substring bftn 0 (string-match "\\.crypt$" bftn))))))
      (setq use-keymap-for-crypted-files t)
      (if (not noninteractive)
	  (progn
	    (project-mode)
	    (if window-system
		(hilit-repaint-command t)))))))



(defun write-region-crypted (name crypted &optional kill)
  (interactive
   (list
    (let ((transient-mark-mode transient-mark-mode))
      (setq transient-mark-mode t)
      (exchange-point-and-mark)
      (exchange-point-and-mark)
      (if buffer-file-name
	  (read-file-name "Write region to file (without extention .crypt): "
			  nil nil nil nil)
	(read-file-name "Write region to file (without extention .crypt): "
			(cdr (assq 'default-directory
				   (buffer-local-variables)))
			nil nil (buffer-name))))
    (yes-or-no-p "Do you want to write the file crypted ?	" )))
  ;;
  ;; Überprüfung, ob das crypt-Kommando auf diesem Rechner ist, wenn nein, dann Fehlermeldung
  (switch-to-buffer "*ASYNC*")
  (shell-command (concat " test -n \"`type -path "
			 (prin1-to-string crypt-encryption-type)
			 "`\" && echo yes") t)
  (if (not (looking-at "yes"))
      (progn
	(kill-buffer (current-buffer))
	(error "crypt command not on this machine"))
    (kill-buffer (current-buffer)))
  ;;
  ;;
  (if xemacsp (setq mark-active (mark)))
  (if kill
      (kill-region (if mark-active (region-beginning)(point-min))
		   (if mark-active (region-end)(point-max)))
    (copy-region-as-kill (if mark-active (region-beginning)(point-min))
			 (if mark-active (region-end)(point-max))))
  (save-excursion
    (switch-to-buffer (generate-new-buffer "tmp"))
    (yank)
    (if crypted
	(progn
	  (write-file-crypted name)
	  (kill-buffer-crypted))
      (crypt-encrypted-mode 0)
      (write-file name)
      (kill-buffer (current-buffer)))))

      
(defun kill-buffer-crypted (&optional dummy)
  (interactive "sTo kill current buffer please hit [Return]")
  (let ((current-buf (current-buffer)))
    (if (boundp 'logbuch-buffer)
	(if (eq current-buf logbuch-buffer)
	    (setq logbuch-buffer nil)))
    (if (get-buffer (concat (prin1-to-string current-buf t) ".crypt"))
	(kill-buffer (concat (prin1-to-string current-buf t) ".crypt")))
    (kill-buffer current-buf)))


(defun ask-for-crypt-save (&optional yesorno)
  (interactive "sDo you want to save the file crypted ? (yes or no) [no]: ")
  (if (string= yesorno "yes")
      (progn
	(save-buffer-crypted)
	(message (concat "Maybe there is still an uncrypted file --- be careful !" )))
    (crypt-encrypted-mode 0)
    (save-buffer-with-control)))


(defun ask-for-crypt-write (name crypted)
  (interactive
   (list (if buffer-file-name
	     (read-file-name "Write to file (without extention .crypt): "
				 nil nil nil nil)
	   (read-file-name "Write to file (without extention .crypt): "
			   (cdr (assq 'default-directory
				      (buffer-local-variables)))
			   nil nil (buffer-name)))
	 (yes-or-no-p "Do you want to write the file crypted ?	" )))
  (if crypted
      (progn
	(write-file-crypted name)
	(setq use-keymap-for-crypted-files t))
    (crypt-encrypted-mode 0)
    (write-file name)
    (setq use-keymap-for-crypted-files nil)))


(defun recover-op-file (file)
  "Visit file FILE, but get contents from its last auto-save file."
  (interactive "FRecover current OP file [ please type return or ^G ]: ")
  (setq file (expand-file-name file))
  (if use-keymap-for-crypted-files
      (setq filecrypt (concat (file-relative-name file) ".crypt"))
    (setq filecrypt (file-relative-name file)))
  (if (auto-save-file-name-p (file-name-nondirectory filecrypt))
      (error "%s is an auto-save file" filecrypt))
  (let ((file-name (let ((buffer-file-name filecrypt))
		     ;; hier so gemacht wegen eines Bugs mit dem AFS-File-System
		     (make-auto-save-file-name))))
    (cond ((if (file-exists-p filecrypt)
	       (not (file-newer-than-file-p file-name filecrypt))
	     (not (file-exists-p file-name)))
	   (error "Auto-save file %s not current or not newer than current file" file-name))
	  ((save-window-excursion
	     (if (not (eq system-type 'vax-vms))
		 (with-output-to-temp-buffer "*Directory*"
		   (buffer-disable-undo standard-output)
		   (call-process "ls" nil standard-output nil
				 (if (file-symlink-p file) "-lL" "-l")
				 file file-name)))
	     (yes-or-no-p (format "Recover auto save file %s? " file-name)))
	   (switch-to-buffer (file-name-nondirectory file))
	   (let ((buffer-read-only nil))
	     (erase-buffer)
	     (insert-file-contents file-name t)
	     ;;
	     ;; falls der File noch den auto-file-name-Namen trägt: ändere den zurück
	     (if (auto-save-file-name-p buffer-file-name)
		 (progn
		   (string-match "^#\\(.*\\)#$" buffer-file-name)
		   (setq buffer-file-name (match-string 1))))
	     ;;
	     (if window-system
		 (hilit-repaint-command t)))
	   (after-find-file nil nil t))
	  (t (error "Recover-file cancelled.")))))



;---------------------------------------------------------------------------------------------------

;;;
;;; Aktivieren der jeweiligen BUFFER-lokalen Keymap durch eine Minor-Mode-List
;;;
;;; ist die Variable use-keymap-for-crypted-files t, wird eine zusätzliche Keymap aktiviert. Damit kann
;;; erreichen, daß eine Key-Belegung Buffer-Lokal ist: use-keymap-for-crypted-files muß Buffer-Lokal sein.
;;; Hier wird das dafür verwendet, daß gekryptete und ungekryptete Files unterschiedliche Speichermecha-
;;; nismen verwenden. Dieser Code hier wäre ohne die Hilfe von Matthias Kapffer nicht zum Laufen gekommen.
;;;

(local-set-key "\C-x\C-w" 'ask-for-crypt-write)
(local-set-key "\C-x\C-s" 'save-buffer-with-control)

(defvar project-mode-crypted-keymap (make-sparse-keymap))
(define-key project-mode-crypted-keymap "\C-x\C-s" 'save-buffer-crypted)
(define-key project-mode-crypted-keymap "\C-xk" 'kill-buffer-crypted)
(define-key project-mode-crypted-keymap "\C-x\C-f" 'find-file-crypted)

(setq minor-mode-map-alist
      (cons
       (cons 'use-keymap-for-crypted-files project-mode-crypted-keymap)
       minor-mode-map-alist))


(provide 'op-crypt)


