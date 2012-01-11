
(defun filename ()
  (let ((begin) (end))
    (save-excursion
      (if (re-search-backward "\"" nil t)
	  (forward-char 1)
	(beginning-of-line))
      (setq begin (point))
      (if (re-search-forward "\"" nil t)
	  (backward-char 1))
      (setq end (point)))
    (buffer-substring-no-properties begin end)))

(defun load-files-at-include-statements (&optional cvs)
  "Schleife über load-file-at-include-statement von Anfang an"
  (interactive "P")
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "#include" nil t)
	(progn
	  (backward-word 1)
	  (backward-char 1)
	  (while (looking-at "#include")
	    (load-file-at-include-statement cvs)
	    (forward-word 1)
	    (forward-char 1)
	    ;;
	    (if (re-search-forward "#include" nil t)
		(progn
		  (backward-word 1)
		  (backward-char 1)))))
      (if window-system
	  (hilit-repaint-command t)))))

(defun load-file-at-include-statement (&optional cvs)
  (interactive "P")
  (let (filename)
    (save-excursion
      (if (looking-at "#include")
	  (progn
	    (forward-word 2)
	    (setq filename (filename)))
	(beginning-of-line)
	(search-forward "\"" (save-excursion (end-of-line) (point)) t)
	(setq filename (filename))))
    (load-file-at-include-statement-sub filename cvs)))

(defun load-file-at-include-statement-sub (filename &optional cvs)
  (let ((before-change-functions nil)(after-change-functions nil)
	(insert-buf) (current-buf (current-buffer)) ediff-result-buffer)
    (save-excursion
      (if (file-exists-p filename)
	  (progn
	    (split-window-vertically 4)
	    (switch-to-buffer "*THIS_FILE_WILL_BE_INCLUDED*")
	    (insert (concat "\n\t" filename))
	    ;; wenn der einzuladende Fileauch ein #include-Statment enthält, dann VERWERFE IHN 
	    ;; mit dem Durchgeben einer Nachricht
	    (goto-char (point-min))
	    (other-window 1)
	    (re-search-forward "[ \t\r\n]" nil t)
	    (backward-char 1)
	    (if cvs
		(progn

		  ;;
		  ;;
		  ;; Die nächsten Zeilen ahben es in sich, nur Rainer hat das so gelöst:
		  ;; ediff-buffers läuft normalerweise weiter, wird hier nur durch 
		  ;; recursive-edit ausgebremst
		  ;;
		  (save-excursion
		    (let ((control-buffer (ediff-.op-to-.cvs.op-file filename)))
		      (set-buffer control-buffer)
		      (local-set-key "q" 'exit-recursive-edit)
		      (recursive-edit)
		      (ediff-quit nil)
		      (setq ediff-result-buffer (current-buffer))))
		  (switch-to-buffer ediff-result-buffer))

	      (find-file filename))
	    (if (search-forward "#include" nil t)
		(progn
		  (ding)(ding)
		  (message (concat "Der File " filename " enthält selbst ein #inlcude: Laden verweigert, da rekursiver Zugriff nicht verfügbar"))
		  (sleep-for 2))
	      (setq insert-buf (current-buffer))
	      (switch-to-buffer current-buf)
	      (end-of-line)
	      (insert-char ?\n 1)
	      (save-excursion
		(insert "\r#end_include_statement"))
	      (insert-buffer insert-buf)
	      (re-search-forward "[\r\n]#end_include_statement" nil t)
	      (switch-to-buffer insert-buf)
	      (set-buffer-modified-p nil)
	      (kill-buffer insert-buf)
	      (switch-to-buffer current-buf)
	      (other-window -1)
	      )
	    (if (not cvs)
		(delete-window))
	    (kill-buffer "*THIS_FILE_WILL_BE_INCLUDED*")
	    (switch-to-buffer current-buf)
	    (message (concat "Der File " filename " wurde eingefügt")))
	(ding)(ding)
	(message (concat "Der File " filename " konnte nicht gefunden werden: nicht eingefügt"))
	(sleep-for 2))))
  (if window-system
      (hilit-repaint-command t)))


(defun save-buffer-with-control ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (search-forward "end_include_statement" nil t)
	(if (y-or-n-p "Sie haben registriert, dass es noch nicht ausgelagerte #include-Files in diesem File gibt? ")
	    (save-buffer)
	  (message "Speichervorgang abgebrochen"))
      (save-buffer))))


(defun extract-and-save-includes (&optional cvs)
  (interactive)
  (let ((before-change-functions nil)(after-change-functions nil) (tmpvar))
    (message "This will take a while --- please wait")
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "#include" nil t)
	  (progn
	    (backward-word 1)
	    (backward-char 1)
	    (let ((insert-buf) (filename) (current-buf (current-buffer)) (mark1) (mark2) (was-a-crypted-file nil))
	      (while (looking-at "#include")

		;; Syntaxcheck
		(save-excursion
		  (if (looking-at "#include")
		      (search-forward "#include" nil t))
		  (if (search-forward "#include" nil t)
		      (setq tmpvar (point))
		    (setq tmpvar (point-max))))
		(save-excursion
		  (if (or (not (search-forward "end_include_statement" nil t))
			  (< tmpvar (point)))
		      (progn
			(ding)(ding)(ding)
			(error "FATALES SPEICHERPROBLEM: Nicht alle #include-Statements sind abgeschlossen. Bitte Label \"\nend_include_statement\"  korrekt einsetzen oder entsprechende Zeilen entfernen !!!")
			(sleep-for 3))))
		(forward-word 2)
		(setq filename (filename))

		(if (or (file-exists-p filename)
			(and (not (file-exists-p filename))
			     (if (y-or-n-p "Bitte bestätigen: Der File wird NEU angelegt, er existiert noch nicht"))))
		    (progn
		      (if (string-match "\\.crypt" filename)
			  (progn
			    (setq was-a-crypted-file t)
			    (setq filename (substring filename 0 (string-match "\\.crypt" filename)))))
		      (re-search-forward "[ \t\r\n]" nil t)
		      (setq mark1 (point))
		      (if (re-search-forward "[\r\n]#end_include_statement" nil t)
			  (progn
			    (forward-char 1)
			    (setq mark2 (point))
			    (re-search-backward "[\r\n]#end_include_statement" nil t)
			    (kill-region mark2 (point))
			    (kill-region mark1 (point))
			    (switch-to-buffer "*save-include*")
			    (yank)
			    (goto-char (point-min))
			    (replace-regexp "\r" "\n")
			    (goto-char (point-min))

			    ;; File herausschreiben, sichern, falls nicht möglich
			    (if (file-writable-p filename)
				(if was-a-crypted-file
				    (progn
				      (write-file-crypted filename t)
				      ;; für gecryptete files im Moment eigentlich nicht supported
				      ;;(if cvs
				      ;;(make-.cvs.op-copy-from-current (buffer-file-name))))
				      )
				  (write-file filename)
				  (if cvs
				      (make-.cvs.op-copy-from-current)))
			      (ding)(ding)
			      (message (concat "File " filename " ist nicht gesichert worden: keine Schreibberechtigung!"))
			      (sleep-for 2)
			      (while (string-match "\/" filename)
				(setq filename (replace-match "%" t t filename)))
			      (setq filename (concat "Buffer-Sicherung_von_" filename))
			      (save-excursion 
				(let ((savebuffer (create-file-buffer filename)))
				  (copy-to-buffer savebuffer (point-min)(point-max))
				  (set-buffer savebuffer)
				  (setq buffer-file-name filename)
				  (set-buffer-modified-p t))))

			    (setq was-a-crypted-file nil)
			    (kill-buffer (current-buffer))
			    (switch-to-buffer current-buf)
			    (if (re-search-forward "#include" nil t)
				(progn
				  (backward-word 1)
				  (backward-char 1))))
			(error "FATAL: FEHLENDES END-LABEL für #include !!! Bitte UNDO aufrufen oder Label \"\nend_include_statement\"  korrekt einsetzen!!!")
			(sleep-for 3)))))
	      (if (search-forward "#include" nil t)
		  (progn
		    (ding)(ding)(ding)
		    (message "FATAL: SPEICHERPROBLEM: Nicht alle #include-Statements sind ABGESCHLOSSEN. Bitte Label \"\nend_include_statement\"  korrekt einsetzen oder entsprechende Zeilen entfernen !!!")
		    (sleep-for 3)))
	      )))
;;;    (save-buffer)
      )))



(defun extract-and-save-this-include (&optional cvs)
  (interactive)
  (let ((before-change-functions nil)(after-change-functions nil) (tmpvar)
	(insert-buf) (filename) (current-buf (current-buffer)) (mark1) (mark2) (was-a-crypted-file nil))
    (save-excursion
      (message "This will take a while --- please wait")
      (if (looking-at "#include")
	  (progn
	    (forward-word 2)
	    (setq filename (filename)))
	(beginning-of-line)
	(search-forward "\"" (save-excursion (end-of-line) (point)) t)
	(setq filename (filename))
	(beginning-of-line))
      
      ;; Syntaxcheck
      (save-excursion
	(if (looking-at "#include")
	    (search-forward "#include" nil t))
	(if (search-forward "#include" nil t)
	    (setq tmpvar (point))
	  (setq tmpvar (point-max))))
      (save-excursion
	(if (or (not (search-forward "end_include_statement" nil t))
		(< tmpvar (point)))
	    (progn
	      (ding)(ding)(ding)
	      (error "FATALES SPEICHERPROBLEM: Nicht alle #include-Statements sind abgeschlossen. Bitte Label \"\nend_include_statement\"  korrekt einsetzen oder entsprechende Zeilen entfernen !!!")
	      (sleep-for 3))))

      (forward-word 2)
      (setq filename (filename))
      (if (or (file-exists-p filename)
	      (and (not (file-exists-p filename))
		   (if (y-or-n-p "Bitte bestätigen: Der File wird NEU angelegt, er existiert noch nicht"))))
	  (progn
	    (if (string-match "\\.crypt" filename)
		(progn
		  (setq was-a-crypted-file t)
		  (setq filename (substring filename 0 (string-match "\\.crypt" filename)))))
	    (re-search-forward "[ \t\r\n]" nil t)
	    (setq mark1 (point))
	    (if (re-search-forward "[\r\n]#end_include_statement" nil t)
		(progn
		  (forward-char 1)
		  (setq mark2 (point))
		  (re-search-backward "[\r\n]#end_include_statement" nil t)
		  (kill-region mark2 (point))
		  (kill-region mark1 (point))
		  (switch-to-buffer "*save-include*")
		  (yank)
		  (goto-char (point-min))
		  (replace-regexp "\r" "\n")
		  (goto-char (point-min))
		  ;; File herausschreiben, sichern, falls nicht möglich
		  (if (file-writable-p filename)
		      (if was-a-crypted-file
			  (progn
			    (write-file-crypted filename t)
			    ;; für gecryptete files im Moment eigentlich nicht supported
			    ;;(if cvs
			    ;;(make-.cvs.op-copy-from-current (buffer-file-name))))
			    )
			(write-file filename)
			(if cvs
			    (make-.cvs.op-copy-from-current)))
		    (ding)(ding)
		    (message (concat "File " filename " ist nicht gesichert worden: keine Schreibberechtigung!"))
		    (sleep-for 2)
		    (while (string-match "\/" filename)
		      (setq filename (replace-match "%" t t filename)))
		    (setq filename (concat "Buffer-Sicherung_von_" filename))
		    (save-excursion 
		      (let ((savebuffer (create-file-buffer filename)))
			(copy-to-buffer savebuffer (point-min)(point-max))
			(set-buffer savebuffer)
			(setq buffer-file-name filename)
			(set-buffer-modified-p t))))
		  
		  (setq was-a-crypted-file nil)
		  (kill-buffer (current-buffer))
		  (switch-to-buffer current-buf))
	      (error "FATAL: FEHLENDES END-LABEL für #include !!! Bitte UNDO aufrufen oder Label \"\nend_include_statement\"  korrekt einsetzen!!!")
	      (sleep-for 3)))))))



(defun hide-end-of-include-statments ()
  (let ((already-modified (buffer-modified-p)))
    (save-excursion
      (goto-char (point-min))
      (replace-regexp "\n#end_include_statement" "\r#end_include_statement"))
    (set-buffer-modified-p already-modified)))


(define-project-mode-key "\M-i" 'load-files-at-include-statements)
(define-project-mode-key "\M-z" 'extract-and-save-includes)




;;; --------------------------------------------------------------------------------
;;; --------------------------------------------------------------------------------
;;; --------------------------------------------------------------------------------




(defun kill-region-insert-include (name crypted)
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
  (let ((before-change-functions nil)(after-change-functions nil))
    (write-region-crypted name crypted t)
    ;; an den Anfang der Region
    (if xemacsp (setq mark-active (mark)))
    (goto-char (if mark-active 
		   (if (< (region-beginning)(region-end))
		       (region-beginning)
		     (region-end))
		 (point-min)))
    ;; Einsetzen des #include
    (insert (concat "\n#include \"" name (if crypted ".crypt") "\"\n"))
    (backward-char 1)
    (if window-system
	(hilit-repaint-command t))))


(defun kill-region-insert-include-and-mail (name crypted)
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
  (kill-region-insert-include name crypted)
  ;; send mail
  (message "Sending Mail")
  (sleep-for 1)
  (let ((tmpposition)(before-change-functions nil)(after-change-functions nil))
    (copy-region-as-kill (save-excursion (beginning-of-line)(setq tmpposition (point))) (point))
    (mh-smail-other-window)
    (insert-char ?\n 1)
    (save-excursion
      (insert-char ?\n 1)
      (yank)
      (insert-char ?\n 2))))


