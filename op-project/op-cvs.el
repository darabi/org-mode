
;
; der nächste File ist für einen komplizierten Kontrollfluss, um zu gewährleisten,
; dass in einem File eigene Kommentare ein können, die geheim sind, zusammen mit
; mit anderen upzudatenden Informationen, die andere brauchen und die zugänglich
; sein müssen.
;
; Der aktuelle #include-File wird herausgeschrieben in das cvs-Directory,
; und zusätzlich mit dem Namen .cvs.op im selben Directory aufbewahrt.
; D.h. zwei Files sind im cvs-Dir, einer ohne Extention .cvs, einer mit, der
; nicht eingecheckt, der andere als ein Mitglied der cvs-Familie. Der Fluss
; ist daraus wie folgt:
;
; - .op -FIle wird ausgelagert
; - .op-File wird kopiert als .cvs.op, nachdem aus dem .op-File alle Kommentare gestrippt worden sind
; - .cvs.op-File wird cvs-mässig aktualisiert, so dass eine neue Version hier steht
; - automatisches Ediff zwischen
;	   dem .op-File, alte Version, mit geheimen Kommentaren
;	   und dem .cvs.op-File, der diese nicht enthält, aber upgedated wurde
;   wobei der File .op im Buffer steht, damit er gekrypted gewesen sein kann
; - zurückschreiben der erneuerten Version MIT den eigenen Kommentaren
;


(defalias 'checkin-for-cvs 'make-.cvs.op-copy)

(defun make-.cvs.op-copy-from-current()
  (interactive)
  "Creates from current file .op a file with extention .cvs.op, which has been
stripped from C++ like comments"
  (let ((current-buf (current-buffer)) filebuffer
	(before-change-functions nil)(after-change-functions nil)
	(find-file-hooks nil)(find-file-not-found-hooks nil)(auto-mode-alist nil))
    (find-file (make-.cvs.op-name buffer-file-name))
    (make-variable-buffer-local 'before-change-functions)
    (setq before-change-functions nil)
    (make-variable-buffer-local 'after-change-functions)
    (setq after-change-functions nil)
    (setq filebuffer (current-buffer))
    (switch-to-buffer current-buf)
    (copy-to-buffer filebuffer (point-min) (point-max))
    (switch-to-buffer filebuffer)
    ;; entferne alle C-Kommentare
    (setq mark-active nil)
    (goto-char (point-min))
    (remove-/*-comments 1)
    ;; entferne alle //-Kommentare
    (goto-char (point-min))
    (replace-regexp "\n[ \t]*//.*" "")
    (goto-char (point-min))
    (replace-regexp "//.*" "")
    (save-buffer)
    (kill-buffer (current-buffer))))


(defun ediff-.op-to-.cvs.op-file (filename)
  (interactive "f")
  (let ((filename-cvs) (filename-nocvs) current-frame)
    ;; Finde den zugehörigen Filenamen zum Paar .cvs.op --- .op
    (if (string-match ".cvs" filename)
	(progn
	  (setq filename-nocvs (make-.op-name filename))
	  (setq filename-cvs filename))
      (setq filename-nocvs filename)
      (setq filename-cvs (make-.cvs.op-name filename)))

    ;;
    ;; erstmal das eventuelle .crypt aus filename entfernen
    ;; setzt voraus, dass NUR DER FILE OHNE CVS GECRYPTED SEIN KANN !!!
    ;;
    (string-match "\\(.*/\\)*" (prin1-to-string filename-nocvs t))
    (let* ((save-string-tmp (match-end 0))
	   (end-of-namestring (string-match "\\.crypt" (prin1-to-string filename-nocvs t)))
	   (filename-nocryptextention
	    (substring (prin1-to-string filename-nocvs t) save-string-tmp end-of-namestring)))
      (if (get-buffer filename-cvs)
	  (setq cvs-buffer (get-buffer (file-name-nondirectory filename-cvs)))
	(find-file filename-cvs)
	(setq cvs-buffer (current-buffer)))
      (show-really-everything)
      (if (get-buffer (file-name-nondirectory filename-nocryptextention))
	  (setq nocvs-buffer (get-buffer (file-name-nondirectory filename-nocryptextention)))
	(find-file filename-nocvs)
	(setq nocvs-buffer (current-buffer)))
      (show-really-everything)

      ;;
      ;; mache ein ediff auf die Buffer
      ;;
      (ediff-buffers nocvs-buffer cvs-buffer))))
      ;;
      ;; und lösche den .cvs-buffer -- durch verwenden des entsprechenden hooks
      ;;

; reset
; (setq ediff-quit-hook (list 'ediff-cleanup-mess))
(add-hook 'ediff-quit-hook
	  (function (lambda()
		      (if (get-buffer (file-name-nondirectory (make-.cvs.op-name buffer-file-name)))
			  (progn
			    (select-window (get-buffer-window
					    (file-name-nondirectory (make-.cvs.op-name buffer-file-name))))
			    (kill-buffer (current-buffer))
			    (delete-window))))) t)


(defun write-this-include-for-cvs()
  " Der aktuelle #include-File wird herausgeschrieben in das cvs-Directory,
  und zusätzlich mit dem Namen .cvs.op im selben Directory aufbewahrt.
  D.h. zwei Files sind im cvs-Dir, einer ohne Extention .cvs, einer mit, der
  andere nicht eingecheckt, der andere als ein Mitglied der cvs-Familie. Der Fluss
  (write-op-to-cvs) ist daraus wie folgt:

  - .op -FIle wird ausgelagert
  - .op-File wird kopiert als .cvs.op, nachdem aus dem .op-File alle Kommentare
    gestrippt worden sind.
  - danach ist der .cvs.op-File cvs-mässig zu aktualisieren, so dass eine neue
    Version im cvs steht."
  (interactive)
  (extract-and-save-this-include t))


(defun write-includes-for-cvs()
  "Loop über write-this-include-for-cvs über alle #includes"
  (interactive)
  (extract-and-save-includes t))

(defun load-this-include-for-cvs()
  " - .Nachdem der cvs.op-File cvs-mässig aktualisiert wurde, so dass eine neue Version
     im cvs steht steht, macht load-op-cvs
  - automatisches Ediff zwischen
	   dem .op-File, alte Version, mit geheimen Kommentaren
	   und dem .cvs.op-File, der diese nicht enthält, aber upgedated wurde
    wobei der File .op im Buffer steht, damit er gekrypted gewesen sein kann
  - zurückschreiben der erneuerten Version MIT den eigenen Kommentaren"
  (interactive)
  (load-file-at-include-statement t))

(defun load-includes-for-cvs()
  "Loop über load-this-include-for-cvs über alle #includes"
  (interactive)
  (load-files-at-include-statements t))



(defun make-.cvs.op-name(oldfilename)
  "Gibt z.B. den Namen name.op.crypt als name.cvs.op.crypt zurück"
  (let ((stringstart (string-match "\\(\.op\\)" (prin1-to-string oldfilename t))))
    (concat (substring oldfilename 0 stringstart) ".cvs" (substring oldfilename stringstart))))

(defun make-.op-name(oldfilename)
  "Gibt z.B. den Namen name.cvs.op.crypt als name.op.crypt zurück"
  (let ((stringstart (string-match "\\(\.cvs.op\\)" (prin1-to-string oldfilename t))))

  (concat (substring oldfilename 0 stringstart) ".op" (substring oldfilename (match-end 0)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Anschluß an pcl-cvs


(defun cvs-checkout (wohin directory)
  (interactive "DWohin ? \nDCheckout von: ")
  (if (getenv "CVSROOT")
      (shell-command (concat "cd "
			     wohin
			     "; cvs checkout "
			     directory))
    (shell-command (concat "cd "
			   wohin
			   "; export "
			   (prin1-to-string (read-minibuffer "Wert von CVSROOT ? "))
			   "; "
			   "cvs checkout "
			   directory))))


(defun cvs-commit (directory)
  (interactive "DVon wo aus ? ")
  (if (getenv "CVSROOT")
      (shell-command (concat "cd "
			     directory
			     "; cvs commit "
			     directory))
    (shell-command (concat "cd "
			   directory
			   "; export "
			   (prin1-to-string (read-minibuffer "Wert von CVSROOT ? "))
			   "; "
			   "; cvs commit "
			   directory))))


