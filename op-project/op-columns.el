

(load "picture")

(message "loading column-mode")


(defun this-is-column-mode (endmarker)
  "Sagt, ob sich der Cursor zwischen Block-Anfang (%%%C Zahl)
und Block-Ende (%%%C gleiche Zahl) befindet. Gibt true oder
false zurück. Argument ist die Position des gefundenen Endmarkers
und nur gültig, wenn Funktion wahr ist."
  (let ((mode-mark-number))
    (save-excursion
      (if (re-search-backward "%%%C" nil t)
	  (progn
	    (forward-word 2)
	    (setq mode-mark-number (current-word))))
      (if (re-search-forward "%%%C" nil t)
	  (progn
	    (set endmarker (point))
	    (forward-word 1))
	(set endmarker -1)
	(setq mode-mark-number nil))
    (string-equal (current-word) mode-mark-number))))



(defun quarter-right ()
"Verschiebt in einem %%%C Zahl -Block spaltenweise nach rechts durch Einsetzen von Blanks"
  (interactive)
;;;
;;; insert space if not column-mode
;;;
  (let ((endmark) (save-column (current-column)) (coldum) (save-position (+ (point) 1)) (leng (length "%%%C"))
	(before-change-functions nil)(after-change-functions nil))
    (if (this-is-column-mode 'endmark)
;;;
;;; else shift one column downward till
;;;
	(progn
	  (while (< (point) endmark)
	    (save-excursion
	      (end-of-line)
	      (setq coldum (current-column)))
	    (if (<= save-column coldum)
		(progn
		  (beginning-of-line)
		  (goto-char (+ (point) save-column))
		  (if (<= (+ (point) leng) endmark)
		      (progn
			(insert-char ?  1)
			(setq endmark (+ endmark 1))
			(backward-char 1)))))
	    (next-line 1))
	  (goto-char save-position))
      (forward-word 1))))


(defun quarter-left ()
"Verschiebt in einem %%%C Zahl -Block spaltenweise nach rechts durch Loeschen von Zeichen"
  (interactive)
;;;
;;; insert space if not column-mode
;;;
  (let ((endmark) (save-column (current-column)) (coldum) (save-position (- (point) 1)) (leng (length "%%%C"))
	(before-change-functions nil)(after-change-functions nil))
    (if (this-is-column-mode 'endmark)
;;;
;;; else shift one column downward till
;;;
	(progn
	  (while (< (point) endmark)
	    (save-excursion
	      (end-of-line)
	      (setq coldum (current-column)))
	    (if (<= save-column coldum)
		(progn
		  (beginning-of-line)
		  (goto-char (+ (point) save-column))
		  (if (<= (+ (point) leng) endmark)
		      (progn
			(backward-delete-char-untabify 1)
			(setq endmark (- endmark 1))
			(if (not (looking-at "$"))
			   (forward-char 1))))))
	    (next-line 1))
	  (goto-char save-position))
      (forward-word -1))))


(defun quarter-tab-right ()
  (interactive)
;;;
;;; insert space if not column-mode
;;;
  (let ((endmark) (save-column (current-column)) (coldum) (save-position (+ (point) tab-width)) (leng (length "%%%C"))
	(before-change-functions nil)(after-change-functions nil))
    (if (this-is-column-mode 'endmark)
;;;
;;; else shift one column downward till
;;;
	(progn
	  (while (< (point) endmark)
	    (save-excursion
	      (end-of-line)
	      (setq coldum (current-column)))
	    (if (<= save-column coldum)
		(progn
		  (beginning-of-line)
		  (goto-char (+ (point) save-column))
		  (if (<= (+ (point) leng) endmark)
		      (progn
			(insert-char ?  tab-width)
			(setq endmark (+ endmark tab-width))
			(backward-char 8)))))
	    (next-line 1))
	  (goto-char save-position))
      (insert-char ?	 1))))


(defun kill-rectangle (start end)
  "Delete rectangle, stay at point."
  (interactive "r")
  (setq killed-rectangle (delete-extract-rectangle start end))
  (exchange-point-and-mark))



(defun quarter-down ()
  (interactive)
  (let ((endmark) (save-position (+ (point) 1)))
    (if (this-is-column-mode 'endmark)
	(progn
	    (previous-line 1)
	    (picture-open-line 1)
	    (setq endmark (+ endmark 1))
	    (goto-char save-position)
	    (let ((a (point)) (save-column (current-column)))
	      (goto-char (- endmark 4))
	      (if (>= (current-column) save-column)
		  (beginning-of-line)
		(previous-line 1)
		(beginning-of-line))
	      (picture-clear-rectangle a (point))
	      (goto-char a)
	      (previous-line 1)
	      (picture-yank-rectangle)
	      (goto-char (+ save-position save-column))))
;;; C-down-key
      (save-excursion
	(scroll-previous-line)))))


(defun quarter-up ()
  (interactive)
  (let ((endmark) (save-position (point)) (save-column (current-column)) (save-position (point)))
    (if (this-is-column-mode 'endmark)
	(progn
	  (previous-line 1)
	  (let ((a (point)))
	    (goto-char (- endmark 4))
	    (if (>= (current-column) save-column)
		(progn
		  (picture-open-line 1)
		  (setq endmark (+ endmark 1))
		  (previous-line 1)
		  (beginning-of-line))
	      (previous-line 1)
	      (picture-open-line 1)
	      (setq endmark (+ endmark 1))
	      (previous-line 1)
	      (beginning-of-line))
	    (picture-clear-rectangle a (point))
	    (goto-char save-position)
	    (beginning-of-line)
	    (picture-yank-rectangle)
	    (goto-char save-position)
	    (beginning-of-line)
	    (previous-line 1)
	    (beginning-of-line)
	    (kill-line 1)
	    (goto-char (+ (point) save-column))))
;;; C-up key
      (save-excursion
	(scroll-next-line)))))


;;;
;;; elisp help key lookup
;;;   In short, a keymap entry may be a keymap, a command, a keyboard
;;; macro, a symbol which leads to one of them, or an indirection or `nil'.
;;; Here is an example of a sparse keymap with two characters bound to
;;; commands and one bound to another keymap.  This map is the normal value
;;; of `emacs-lisp-mode-map'.  Note that 9 is the code for TAB, 127 for
;;; DEL, 27 for ESC, 17 for `C-q' and 24 for `C-x'.
;;;
;;;	(keymap (9 . lisp-indent-line)
;;;		(127 . backward-delete-char-untabify)
;;;		(27 keymap (17 . indent-sexp) (24 . eval-defun)))
;;;


(if xemacsp
    (progn
      ;;XEmacs: andere Key-Definitionen als GNU-emacs
      (local-set-key [(control down)] 'quarter-down)
      (local-set-key [(control up)] 'quarter-up)
      (local-set-key [(control right)] 'quarter-right)
      (local-set-key [(control left)] 'quarter-left)
      (local-set-key [(control tab)] 'quarter-tab-right))
  (local-set-key [C-down] 'quarter-down)
  (local-set-key [C-up] 'quarter-up)
  (local-set-key [C-right] 'quarter-right)
  (local-set-key [C-left] 'quarter-left)
  (local-set-key [C-tab] 'quarter-tab-right))

(local-set-key [?\C->] '(lambda () (interactive) (scroll-right 2) (backward-char 2)))
(local-set-key [?\C-<]	'(lambda () (interactive) (scroll-left 2) (forward-char 2)))
(local-set-key [(meta down)]	 '(lambda () (interactive) (save-excursion (scroll-next-line))))
(local-set-key [(meta-up)]	'(lambda () (interactive) (save-excursion (scroll-previous-line))))




(defvar column-ruler
  (concat "1   5    10        20        30        40        50        60        70        80        90       100       110       120       130       140       150       160       170       180       190       200       210       220\n"
          "[   |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |\n")
  "*String displayed above current line by column-ruler.")



(defun current-line-x ()
  (let ((begin) (end) (line) (already-modified (buffer-modified-p)))
    (save-excursion
      (beginning-of-line)
      (setq begin (point))
      (end-of-line)
      (setq end (point))
      (end-of-line)
      (setq end (point)))
    (setq line (buffer-substring-no-properties begin end))
    (set-buffer-modified-p already-modified)
    (format line)))


(defvar op-current-headline-string nil)
(defun column-ruler (&optional leiste setheadlinestring)
  "Inserts either the current headlines (without argument) or a
column ruler momentarily above current line, till next keystroke.
The ruler is defined by the value of column-ruler.
The key typed is executed unless it is SPC.
Leiste show al row position counter, setheadlinestring sets the 
variable of op-current-headline-string to the actual op-mode value."
  (interactive "P")
  (if leiste
      (progn
	(let ((truncate-lines-sv truncate-lines))
	  (setq truncate-lines t)
	  (momentary-string-display column-ruler (save-excursion (beginning-of-line) (point))
				    nil "Type SPC or any command to erase ruler.")
	  (setq truncate-lines truncate-lines-sv)))
    (let ((headlines-string "------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------\n")
	  (truncate-lines-sv truncate-lines) (mark1) (mark2) (mark3))
      (save-excursion
	(if (not (looking-at "^[*]"))
	    (progn
	      (beginning-of-line)
	      (setq mark1 (point))
	      (condition-case nil
		  (outline-previous-visible-heading 1)
		(error "no previous headline"))
	      (setq mark2 (point))
	      (goto-char mark1)
	      (end-of-line)
	      (setq mark3 (point))
	      (goto-char mark1)
	      (if (not (re-search-forward "^[ \t]+==>" mark3 t))
		  (progn
		    (goto-char mark1)
		    (if (re-search-backward "^[ \t]+==>" mark2 t)
			(setq headlines-string (concat (current-line-x) "\n" headlines-string)))))
	      (condition-case nil
		  (outline-previous-visible-heading 1)
		(error "no previous headline"))
	      (setq headlines-string (concat (current-line-x) "\n" headlines-string))))
	(if (>= emacs-major-version 20) ;; because of fucking changes of bastards
	    (if (bobp)
		(progn
		  (outline-next-visible-heading 1)
		  (setq headlines-string (concat (current-line-x) headlines-string))
		  )))
	(while (not (looking-at "^[*][^*]"))
	  (outline-up-heading 1)
	  (setq headlines-string (concat (current-line-x) "\n" headlines-string))))
      (setq headlines-string (concat "\nvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv\n"
				     headlines-string))
      (setq truncate-lines t)
      (if setheadlinestring
	  (setq op-current-headline-string headlines-string)
	(momentary-string-display headlines-string (save-excursion (beginning-of-line) (point))
				  nil "Type SPC or any command to erase ruler."))
      (setq truncate-lines truncate-lines-sv))))


(if gnuemacsp
    (local-set-key "\C-c\C-g" 'column-ruler)
  (define-key (current-local-map) [(control c) g] 'column-ruler)  ; Control-g im XEmacs funktioniert erst nach 
                                                                  ; timeout von C-c
  (define-key (current-local-map) [(control c) (control g)] 'column-ruler))  ; trotzdem der Gewohnheit halber noch dazu




(defun current-number ()
  (interactive)
  (let ((begin) (end))
    (if (not (looking-at "[ \t]\\|$"))
	(save-excursion
	  (if (re-search-backward "[ \t\n\r]\\|^" nil t)
	      (progn
		(if (looking-at "[ \t\n\r]")
		    (forward-char 1))))
	  (setq begin (point))
	  (if (re-search-forward "[ \t\n\r]\\|$" nil t)
	      (progn
		(if (looking-at "[ \t\n\r]")
		    (backward-char 1))))
	  (setq end (point))
	  (condition-case nil
	      (string-to-number (buffer-substring-no-properties begin end))
	    (error nil)))
      nil)))

  
(defun sum-column (beg end)
"Sum the numbers in a column. The mark should be set at the row and number
where the addition should start and end at the last number of the row.
The result will be written two lines below last number."
  (interactive "r")
  (let ((before-change-functions nil)(after-change-functions nil))
    (goto-char beg)
    (let ((sum (float 0)) (col))
      (save-excursion
	(goto-char end)
	(re-search-forward "[ \t\n\r]\\|$" nil nil)
	(backward-char 2)
	(setq end (point))
	(setq col (current-column)))
      (let ((eol) (cc (current-column)) (p1) (p2))
	(while (<= (point) end)
	  (if (current-number)
	      (setq sum (+ sum (current-number))))
	  (next-line 1)
	  (save-excursion
	    (beginning-of-line)
	    (setq p1 (point))
	    (end-of-line)
	    (setq p2 (point)))
	  (untabify p1 p2)
	  (save-excursion
	    (end-of-line)
	    (setq eol (current-column)))
	  (if (and (< cc eol)
		   (not (= cc (current-column))))
	      (progn
		(beginning-of-line)
		(goto-char (+ (point) cc)))))
	(save-excursion
	  (beginning-of-line)
	  (setq p1 (point))
	  (end-of-line)
	  (setq p2 (point)))
	(untabify p1 p2)
	
	(while (< (- (current-column) 1) (- col (length (int-to-string sum))))
	  (picture-forward-column 1))
	(while (> (- (current-column) 1) (- col (length (int-to-string sum))))
	  (backward-char 1))
	(insert-char ?= (length (int-to-string sum)))
	(if (looking-at "....")
	    (delete-char (length (int-to-string sum)))
	  (while (not (looking-at "$"))
	    (delete-char 1)))
	(next-line 1)
	(save-excursion
	  (beginning-of-line)
	  (setq p1 (point))
	  (end-of-line)
	  (setq p2 (point)))
	(untabify p1 p2)
	
	(while (< (- (current-column) 1) (- col (length (int-to-string sum))))
	  (picture-forward-column 1))
	(while (> (- (current-column) 1) (- col (length (int-to-string sum))))
	  (backward-char 1))
	(insert (int-to-string sum))
	(if (looking-at "....")
	    (delete-char (length (int-to-string sum)))
	  (while (not (looking-at "$"))
	    (delete-char 1)))))))

;    (delete-char (length (int-to-string sum)))))


;   123.4	  
;   432.1
;    asdasdasd

      
;   123.4
;   432.1
	123
	123



;; ähnliche Funktion noch in op-consistent.el
(defun current-longword-quote ()
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
	    (if (not (= (point) 1))
		(backward-char 1))
	    (if (not (looking-at "[ \t\r]"))
		(forward-char 1))))
      (setq end (point)))
    (regexp-quote (buffer-substring-no-properties begin end))))



(defun p ()
  (interactive)
  (picture-mode)
  (if gnuemacsp
      (local-set-key "\C-c\C-g" 'column-ruler)
    (define-key (current-local-map) [(control c) g] 'column-ruler))
  (message " PICTURE-MODE:  Type C-c C-c in this buffer to return to op-mode"))




;%%%C 2
;1111111111111111
;2222222222222222
;3333333333333333
;4444444444444444
;5555555555555555
;66666666666666666666666666666666666666666666666666
;7777777777777777777777777777777777777777777777777
;8888888888888888888888888888888888888888888888888

;      %%%C 2


;%%%C 1
;qweqwqwe
;qweqwqwesdfsdfsdf
;qweqwqwe

;   backward-word backward-word
;
;a

;qweqwe
;qweqwe asdfasd fasdf

;asdf;%%%C 1 sdfas dfasdfasdf

;aösfldkajösfdl kasöfdl a
;asdflökasfödl aksjföd laksjd f
;asdf ölaksfjdö alsfjd



