;;;
;;; Automatic-change-mode (acm):
;;; Einfachst-mögliche Idee für eine Datenkonsistenz in einem File
;;;
;
;
;
;;  zum Test: hier jeweils etwas dem Wort "[]test", "+test", "<>test" und an dem Wort "->test" ändern !
;;  (Bei normalem Text: Achtgeben mit Kommatas !)
;;
;;   []test
;;                              <>test
;;	[]test 	      #
;;                              <>test  
;;	[]test		#
;;                              <>test
;;    []tt     test
;;                              <>test
;;	   []test
;;
;; ->test   ->test
;;  +test   +test
;;
;;  zum anderen Test: Am Wort "link-test" sollte sich immer etwas aendern, nach dem Prozentzeichen
;;     jedoch nur noch an denen mit einem gleichen Ende.
;;     (siehe Dokumentation zu op-hyper.el)
;;
;;	 ->link-test%mit_einem_ende
;;	 ->link-test%mit_einem_ende
;;
;;	 ->link-test%mit_einem_anderen_ende
;;
;



(defvar acm-original-word nil)
(make-variable-buffer-local 'acm-original-word)
(defvar acm-original-word-begin 1)
(make-variable-buffer-local 'acm-original-word-begin)
(defvar acm-original-word-end 1)
(make-variable-buffer-local 'acm-original-word-end)
(defvar acm-original-word-endtype "")
(make-variable-buffer-local 'acm-original-word-endtype)

(make-variable-buffer-local 'before-change-functions)
(make-variable-buffer-local 'after-change-functions)
(setq before-change-functions '(acm-original-word-set))
(setq after-change-functions '(acm-insert-everywhere))


(defun automatic-change-mode (&optional on)
  "Toggles the automatic-change-mode. With arg, switches it on. No possibility to
switch it off by argument."
  (interactive)
  (if (and before-change-functions
	   after-change-functions
	   (not on))
      (progn
	(setq before-change-functions nil)
	(setq after-change-functions nil)
	(message "automatic-change-mode toggled to off"))
    (setq before-change-functions '(acm-original-word-set))
    (setq after-change-functions '(acm-insert-everywhere))
    (if on
	(message "automatic-change-mode switched to on")
    (message "automatic-change-mode toggled to on"))))


(defun acm-original-word-set (a b)
  ;; suche, ob vorne an dem Wort ein <- oder ein -> steht (siehe op-hyper.el)
  (let ((tmp (current-longword-quote-withmarks)))
    (if (and (< 2 (length tmp))	 ; Wortlänge sollte größer als nur der Pfeil sein
	     (not (= (point) acm-original-word-begin))  ; und wenn an dem Pfeil was gemacht wird, dann reagiere darauf nicht mehr
	     (not (= (point) (+ acm-original-word-begin 1))))
	(if (or (string= (substring tmp 0 2) "[]")
		(string= (substring tmp 0 2) "<>")
		(string= (substring tmp 0 2) "->")
		(string= (substring (regexp-quote tmp) 0 2) "\\+"))
	    (setq acm-original-word (regexp-quote tmp))
	  (setq acm-original-word nil))
      (if (and (< 1 (length tmp))	 ; Wortlänge sollte größer als nur ein Plus sein
	       (not (= (point) acm-original-word-begin)))  ; und wenn an dem Plus was gemacht wird, dann reagiere darauf nicht mehr
 	  (if (string= (substring tmp 0 1) "+")
	      (setq acm-original-word (regexp-quote tmp))
	    (setq acm-original-word nil))
	(setq acm-original-word nil)))))



(defun acm-insert-everywhere (a b deleted)
  (if acm-original-word
      (let ((p1 a)(p2 b)(del deleted))
	(save-excursion
	  (if (<= (point) (+ acm-original-word-end 1))
	      (if (= 0 del)
		  ;; vorheriges Kommando war ein Insert
		  (progn
		    (let ((insertstring (buffer-substring-no-properties p1 p2))(diffchar (- p1 acm-original-word-begin)))
		      (goto-char (point-min))
		      (if (not (or (string= insertstring " ")
				   (string= insertstring "\n")))
			  (progn
			    (message "automatic-change-mode: inserting for all variables of the same type")
			    (while (re-search-forward (concat "\\(^\\|[ \t]\\)" acm-original-word acm-original-word-endtype) nil t)
			      (re-search-backward "\\([ \t\r][^ \t\r]\\)\\|^" nil t)
			      (if (looking-at "[ \t\r]")
				  (forward-char 1))
			      (forward-char diffchar)
			      (insert insertstring))))))
		;; vorheriges Kommando war ein Delete
		(let ((diffchar (- (point) acm-original-word-begin)))
		  ;; am Pfeil rumbasteln: nur durch cut'n'paste
		  (if (not (= (point) (+ acm-original-word-begin 1)))
		      (progn
			(message "automatic-change-mode: deleting for all variables of the same type")
			(goto-char (point-min))
			(while (re-search-forward (concat "\\(^\\|[ \t]\\)" acm-original-word acm-original-word-endtype) nil t)
			  (re-search-backward "\\([ \t\r][^ \t\r]\\)\\|^" nil t)
			  (if (looking-at "[ \t\r]")
			      (forward-char 1))
			  (forward-char diffchar)
			  (delete-char del))))))))))
  (setq acm-original-word nil))


(defun current-longword-quote-withmarks ()
  (let ((begin) (end))
    (save-excursion
      ;; die nächste Zeile ist sehr wichtig für die dynamische Ausdrucksexpandierung (dabbrev.el)
      (save-match-data
	(save-excursion
	(if (re-search-backward "[ \t\r]\\|^" nil t)
	    (progn
	      (if (looking-at "[ \t\r]")
		  (forward-char 1))))
	(setq begin (point)))
	(setq acm-original-word-begin begin)
	(if (re-search-forward "\\([ \t\r]\\|$\\)\\|%" nil t)
	    (progn
	      (if (not (= (point) 1))
		  (backward-char 1))
	      (if (not (looking-at "[ \t\r]"))
		  (if (looking-at "%")
		      (setq acm-original-word-endtype "%")
		    (setq acm-original-word-endtype "\\($\\|[ \t]\\)")
		    (forward-char 1)))))
	(setq end (point))
	(setq acm-original-word-end end)))
    (buffer-substring-no-properties begin end)))




;; wichtiger Hack vom Rainer Schöpf (14.8.1996, da es echte Probleme mit dem UNDO gab)
(defadvice undo (around no-change-functions activate)
  (let ((before-change-functions nil)
	(after-change-functions nil))
    ad-do-it))

