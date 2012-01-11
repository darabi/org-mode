

(defun uglify-op-project ()
  (interactive)
  (strip-lisp-comments)
  (change-defun-names))


(defun strip-lisp-comments ()
  (goto-char (point-max))
  (debug)
  (while (re-search-backward ";" nil t)
    (if (not (re-search-forward "\"" (save-excursion (re-search-forward "$" nil t)(point)) t))
	(kill-line)
      (previous-line 1)))
  (goto-char (point-min))
  (while (re-search-forward "^;" nil t)
    (backward-char 1)
    (kill-line)))

;; (perform-replace  (concat "'" tmpdefun " ") (concat "'" newname " ") nil nil nil)



(defun change-defun-names ()
  (goto-char (point-min))
  (let ((tmpdefun "") (tmp-line-number 0) (newname ""))
    (while (re-search-forward "(defun [^b][^ ]" nil t) (backward-char 2) ; für das nicht-"b"
      (re-search-forward "[a-zA-Z]" nil nil)
      ;; tmpdefun ist der Name der defun
      (setq tmpdefun (current-word (current-thislongword)))
      ;; wenn der Name weder dummy noch test ist, dann...
      (if (not (or (string= tmpdefun "dummy")
		   (string= tmpdefun "test")))
	  (progn
	    ;; Grundname byte-compiled-function_xxx
	    (setq newname (concat "byte-compiled-function_" (number-to-string (random))))
	    ;; Zeile speichern, um dann wieder zurück gehen zu können
	    (setq tmp-line-number (count-lines (point-min) (point)))
	    ;; an den Anfang, ersetze die Funktionsnamen
	    ;; - bei Funktionsaufrufen
	    (goto-char (point-min))
	    (perform-replace  (concat "(" tmpdefun " ") (concat "(" newname " ") nil nil nil)
;	    (while (re-search-forward  (concat "(" tmpdefun " ") nil t)
;	      (replace-match           (concat "(" newname  " ")))
	    ;;
	    (goto-char (point-min))
	    (perform-replace  (concat "(" tmpdefun ")") (concat "(" newname ")") nil nil nil)
;	    (while (re-search-forward  (concat "(" tmpdefun ")") nil t)
;	      (replace-match           (concat "(" newname  ")")))
	    ;; - bei Funktionsersetzungen
	    ;;   - steht nicht auf make-variable-buffer-local
	    (goto-char (point-min))
	    (while (re-search-forward (concat "'" tmpdefun " ") nil t)
	      (if (not (save-excursion (save-match-data (re-search-backward " '" nil t)
							(backward-word 1))
				       (looking-at "local")))
		  (replace-match      (concat "'" newname  " "))))
	    ;;
	    (goto-char (point-min))
	    (while (re-search-forward (concat "'" tmpdefun ")") nil t)
	      (if (not (save-excursion (save-match-data (re-search-backward " '" nil t)
							(backward-word 1))
				       (looking-at "local")))
		  (replace-match      (concat "'" newname  ")"))))
	    ;;
	    ;; Ausgangs-Funktion verändern
	    (goto-char (point-min))
	    (perform-replace  (concat "(defun " tmpdefun " ") (concat "(defun " newname " ") nil nil nil)
;	    (while (re-search-forward (concat "(defun " tmpdefun " ") nil t)
;	      (replace-match          (concat "(defun " newname  " ")))
	    (goto-char (point-min)))))))


;; noch zwei Fehler: 
;; die Funktion "outline-minor-mode" muss händig zurück ersetzt werden
;; und die Keymap stimmt nicht (auch outline-minor-mode), d.h. u.U. alles weglassen, was outline-xxx verfälscht

