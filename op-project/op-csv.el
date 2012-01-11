
;;;
;;;
;;; Kurzzusammenlegung der CSV-Modes für den OP-Mode, die Files selbst werden nicht
;;; weiters verändert (außer dem Einbau der XEmacs-Kompatibilität in csv-mode.el)
;;; SMW, 12.10.2004
;;;
;;;


(require 'csv)


(defun csv-importer (file)
  "Imports from the fig-file at FILE the text entries" 
  (interactive "fxfig figure file: ")
  (let ((current-buff (current-buffer)))
    (find-file file)
    (message "CSV format")
    (beginning-of-buffer)
    (while (search-forward ";" nil t)
      (backward-char 2)
      (if (not (looking-at "\\\\"))
	  (progn
	    (forward-char 1)
	    (delete-char 1)
	    (insert-char ?, 1))
	(forward-char 1)))
    (set-buffer-modified-p nil (current-buffer))
    (csv-sub-test)
    (switch-to-buffer current-buff)
))


(defun csv-sub-test ()
  "Test routine -- don't care."
  (let* ((b (current-buffer))
	 (tb (switch-to-buffer "*csv*")))
    (switch-to-buffer-other-window tb)
    (beginning-of-buffer)
    (csv-insert-contents (csv-parse-buffer b))
    (kill-buffer b)))


