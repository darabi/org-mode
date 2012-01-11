

;;; search for current word ============================================================


(defun search-forward-for-current-word (&optional mark noruler noerror)
  (interactive "P")
  (let (bow eow s)
    (forward-word 1)
    (setq bow (save-excursion (skip-syntax-backward "w")
			      (point)))
    (setq eow (save-excursion (skip-syntax-forward "w")
			      (point)))
    (setq s (buffer-substring-no-properties bow eow))
    (if mark
	(re-search-forward (concat ".\\<" (regexp-quote s) "\\>.") nil noerror)
      (re-search-forward (concat "\\<" (regexp-quote s) "\\>") nil noerror))
    (backward-word 1)
    (if (not noruler)
	(column-ruler))))

(defun search-backward-for-current-word (&optional mark noruler noerror)
  (interactive "P")
  (let (bow eow s)
    (setq bow (save-excursion (skip-syntax-backward "w")
			      (point)))
    (setq eow (save-excursion (skip-syntax-forward "w")
			      (point)))
    (setq s (buffer-substring-no-properties bow eow))
    (if mark
	(re-search-backward (concat ".\\<" (regexp-quote s) "\\>.") nil noerror)
      (re-search-backward (concat "\\<" (regexp-quote s) "\\>") nil noerror))
    (forward-char 1)
    (if (not noruler)
	(column-ruler))))

(define-key (current-global-map) "\M-s" 'search-forward-for-current-word)
(define-key (current-global-map) "\M-r" 'search-backward-for-current-word)



(defun search-this-string-forward (&optional mark)
  (interactive "P")
  (let ((dum))
    (if (this-is-column-mode 'dum)
	(let ((current-window (get-buffer-window (current-buffer))) (current-word (current-longword-quote)))
	  (if (eq (previous-window) current-window)
	      (progn
		(if (> (frame-height) 24)
		  (split-window-vertically 12)
		  (split-window-vertically 6))
		(other-window  1)))
	  (other-window -1)
	  (re-search-forward "[ \t]\\|$" nil t)
	  (if mark
	      (if (re-search-forward (concat "!\\<" current-word  "\\>!") nil t)
		  (recenter)
		(goto-char (point-min))
		(re-search-forward (concat "!\\<"  current-word "\\>!") nil t))
	    (if (re-search-forward (concat "\\<" current-word  "\\>") nil t)
		(recenter)
	      (goto-char (point-min))
	      (re-search-forward (concat "\\<"	current-word "\\>") nil t)))
	  (column-ruler)
	  (other-window 1))
      (search-forward-for-current-word mark t))))


(defun search-this-string-backward (&optional mark)
  (interactive "P")
  (let ((dum))
    (if (this-is-column-mode 'dum)
	(let ((current-window (get-buffer-window (current-buffer))) (current-word (current-longword-quote)))
	  (if (eq (previous-window) current-window)
	      (progn
		(if (> (frame-height) 24)
		    (split-window-vertically 12)
		  (split-window-vertically 6))
		(other-window  1)))
	  (other-window -1)
	  (re-search-backward "[ \t]\\|^" nil t)
	  (if mark
	      (if (re-search-forward (concat "!\\<" current-word  "\\>!") nil t)
		  (recenter)
		(goto-char (point-min))
		(re-search-forward (concat "!\\<"  current-word "\\>!") nil t))
	    (if (re-search-backward (concat "\\<" current-word "\\>") nil t)
		(recenter)
	      (goto-char (point-max))
	      (re-search-backward (concat "\\<" current-word "\\>") nil t)))
	  (column-ruler)
	  (other-window 1))
      (search-backward-for-current-word mark t))))


(local-set-key "\M-s" 'search-this-string-forward)
(local-set-key "\M-r" 'search-this-string-backward)



;;; search in headlines only ============================================================

(defun isearch-forward-in-headings (&optional not-regexp no-recursive-edit)
  (interactive "P\np")
  (let ((before-change-functions nil)(after-change-functions nil))
  (isearch-mode t (null not-regexp) 'make-headline-isearch-string-forward (not no-recursive-edit))))

(defun isearch-backward-in-headings (&optional not-regexp no-recursive-edit)
  (interactive "P\np")
  (let ((before-change-functions nil)(after-change-functions nil))
  (isearch-mode nil (null not-regexp) 'make-headline-isearch-string-backward (not no-recursive-edit))))

(defun make-headline-isearch-string-forward ()
  (if (< (length isearch-string) 4)
      (progn
	(setq isearch-string (concat
			      "[\n\r]\\*+"	 ; 2
			      "[^\n\r]+"	 ; 1
			      isearch-string))	 ; 2+1 < 4
	(beginning-of-line)
	(setq isearch-other-end (point))
	(isearch-update))))
(defun make-headline-isearch-string-backward ()
  (if (< (length isearch-string) 4)
      (progn
	(setq isearch-string (concat
			      "[\n\r]\\*+"	 ; 2
			      "[^\n\r]+"	 ; 1
			      isearch-string))	 ; 2+1 < 4
	(end-of-line)
	(setq isearch-other-end (point))
	(isearch-update))))

(define-key (current-local-map) "\C-\M-n" 'isearch-forward-in-headings)
(define-key (current-local-map) "\C-\M-p" 'isearch-backward-in-headings)


;;; ;;; search with visible regions ============================================================

;;; 1.) search current word and make it visible ============================================================

(defun search-word-forward-expand-to-visible ()
  "searches for the next occurence of the actual word and expands tree locally"
  (interactive)
  (let ((before-change-functions nil)(after-change-functions nil))
    (if (not (= am-i-at-the-same-point (point)))
	(progn
	  (while (progn
		   (search-forward-for-current-word nil t)
		   (hide-other-hide t)
		   (save-excursion
		     (re-search-backward "\n\\|\r" nil t)
		     (looking-at "\r"))))
					;	(save-excursion
					;	  (search-forward-for-current-word nil t)
					;	  (setq am-i-at-the-same-point (point)))
	  (if window-system
	      (hilit-repaint-command ?-))
	  (column-ruler)
	  )
      (message "last match")
      (ding)
      (setq am-i-at-the-same-point 999999))))


(defun search-word-backward-expand-to-visible ()
  "searches for the last occurence of the actual word and expands tree locally"
  (interactive)
  (let ((before-change-functions nil)(after-change-functions nil))
  (if (not (= am-i-at-the-same-point (point)))
      (progn
	(while (progn
		 (search-backward-for-current-word nil t)
		 (hide-other-hide t)
		 (save-excursion
		   (re-search-backward "\n\\|\r" nil t)
		   (looking-at "\r"))))
;	(save-excursion
;	  (search-backward-for-current-word nil t)
;	  (setq am-i-at-the-same-point (point)))
	(if window-system
	    (hilit-repaint-command ?-))
	(column-ruler))
    (message "last match")
    (ding)
    (setq am-i-at-the-same-point 999999))))

(define-key (current-local-map) "\M-n" 'search-word-forward-expand-to-visible)
(define-key (current-local-map) "\M-p" 'search-word-backward-expand-to-visible)

;;; 2.) search forward in visible regions only ============================================================

;; ein nachprogrammiertes re-search-forward, einfacher und dafür transparenter
;; steht hier nur für Testzwecke, isearch-visible ist in den Details vom Verhalten vollständiger
(defun res ()
  (let ((t1 "") (t2 0) (t3 0) (point (point)) (re 0))
    (while (not (= t2 27)) ; falls nicht ESC
      (setq t2 (if (>= emacs-major-version 20)
		   (read-char)
		 (read-char (concat "Re-Search-String: " t1))))
      ;(if (>= (length t3) 3) (debug))
      (if (= t2 127) ; ist ein DEL
	  ;; dann letzten Buchstaben wegstreichen
	  (progn
	    (setq t1 (substring t1 (- (length t1)) -1))
	    (re-search-backward t1 nil t))
	(if (= t2 19) ; weiteres search forward (cntl s)
	    (setq re 1)
	  (if (= t2 18) ; weiteres search backward (cntl r)
	      (setq re -1)
	    (setq re 0)
	    ;; sonst letzten Buchstaben dranhängen
	    (setq t1 (concat t1 (char-to-string t2))))))
      ;; Cursor nach vorne setzen, aber nicht über den Bufferanfang hinaus
      (if (<= re 0)
	  (if (not (bobp))
	      (backward-char (length t1)))
	(while (<= t3 (length t1))
	  (if (not (bobp))
	      (backward-char 1))
	  (setq t3 (1+ t3))))
      ;; und suchen
      (save-excursion
	(if (= re -1)
	    (if (re-search-backward t1 nil t)
		(setq point (point))
	      (ding)
	      (if (= re 0)
		  (setq t1 (substring t1 (- (length t1)) -1))))
	  ;;(if (= (length t1) 3)(debug))
	  (if (re-search-forward t1 nil t)
	      (setq point (point))
	    (ding)
	    (if (= re 0)
		(setq t1 (substring t1 (- (length t1)) -1))))))
      (goto-char point)
      )))




(if gnuemacsp
    (progn
      (define-key (current-local-map) [?\C-\S-S] 'isearch-visible-forward)
      (define-key (current-local-map) [?\C-\S-R] 'isearch-visible-backward))
  (define-key (current-local-map) [(control c) S] 'isearch-visible-forward)
  (define-key (current-local-map) [(control c) S] 'isearch-visible-backward))
  


