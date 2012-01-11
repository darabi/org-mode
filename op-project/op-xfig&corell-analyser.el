


(defun current-longword ()
;;; ACHTUNG !!! siehe auch Funktion "current-longword-quote" im File op-columns.el !!!!
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

(defun current-line ()
  (let ((begin) (end))
    (save-excursion
      (beginning-of-line)
      (setq begin (point))
      (end-of-line)
      (setq end (point)))
    (regexp-quote (buffer-substring-no-properties begin end))))

(defun current-line-noquote ()
  (let ((begin) (end))
    (save-excursion
      (beginning-of-line)
      (setq begin (point))
      (end-of-line)
      (setq end (point)))
    (buffer-substring-no-properties begin end)))



(defun remove-double-lines ()
  "removes all lines in a buffer that are two following times the same"
  (interactive)
  (beginning-of-buffer)
  (while
      (progn
	(setq dumvar (current-line))
	(next-line 1)
	(if (string= (current-line) dumvar)
	    (kill-line 1))
	(not (looking-at "^$")))))



(defun op-jump-to-line ()
  "Searches for the occurence of the line in the buffer
of the following window"
  (interactive)
  (let ((line ""))
    (beginning-of-line)
    (setq line (current-line-noquote))
    ;; Filtere die Steuerungs-Sterne am Anfang der Zeile heraus
    (if (posix-string-match "^\\*+[^*][ \t]*" line)
	(setq line (substring line (match-end 0) nil)))
    ;; für MPX: filtere auch Meilenstein und Unterprojekt mit heraus
    (if (posix-string-match "\\[Meilenstein\\][ \t]*" line)
	(setq line (substring line (match-end 0) nil)))
    (if (posix-string-match "||[ \t]*\\[Unterprojekt\\].*||[ \t]*" line)
	(setq line (substring line (match-end 0) nil)))
    (other-window 1)
    (end-of-line)
    (if (search-forward line nil t)
	(progn
	  (beginning-of-line)
	  (recenter))
      (progn
	(beginning-of-buffer)
	(if (search-forward line nil t)
	    (progn
	      (message "Wrapped search found match")
	      (beginning-of-line)
	      (recenter)))))
    (other-window -1)))

(if (fboundp 'define-project-mode-key)
    (define-project-mode-key  "\M-i" 'op-jump-to-line))


(defun op-jump-to-line-at-mouse-position (event)
  (interactive "e")
  (mouse-set-point event)
  (op-jump-to-line))


(defun xop-sort-reference-lines ()
  (interactive)
  (sort-lines nil (point-min) (point-max)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defun xfig-text-extractor ()
"Extracts from the xfig file at the current cursor position the text entries"
  (interactive)
  (let (find-file-not-found-hooks find-file-not-found-hooks)
    (add-hook 'find-file-not-found-hooks
	      (function (lambda ()
			  (error "Keinen solchen File gefunden: %s" buffer-file-name))))
    (let ((current-buff (current-buffer)))
      (find-file (current-longword))
      (occur "^4")
      (switch-to-buffer "*Occur*")
      (toggle-read-only t)
      (toggle-read-only)
      (kill-line)
      (kill-line)
      (while (not (looking-at "^$"))
	(beginning-of-line)
	(forward-word 15)
	(kill-line 0)
	(end-of-line)
	(delete-backward-char 1)
	(next-line 1))
      (delete-window)
      (switch-to-buffer current-buff)
      (split-window-vertically 6)
      (switch-to-buffer "*Occur*")
      ;;
      (goto-char (point-min))
      (while (re-search-forward "\\00$" nil t)
	(replace-match "" nil nil))
      (goto-char (point-min))
      ;;
      (goto-char (point-min))
      (while (re-search-forward "\\\\$" nil t)
	(replace-match "" nil nil))
      (goto-char (point-min))
      ;;
      (toggle-read-only t)
      (toggle-read-only)
      (switch-to-buffer "*xfig-text-in-figure*")
      (kill-buffer (current-buffer))
      (rename-buffer "*xfig-text-in-figure*")
      (local-set-key "\C-c\C-c" 'op-jump-to-line)
      (if (featurep 'xemacs)
	  (local-set-key [button2] 'op-jump-to-line-at-mouse-position)
	(local-set-key [mouse-2] 'op-jump-to-line-at-mouse-position))
      (local-set-key "q" 'delete-window)
      (local-set-key "\C-c\C-s" 'xop-sort-reference-lines)
      (beginning-of-buffer))))

(if (fboundp 'define-project-mode-key)
    (define-project-mode-key  "\M-x" 'xfig-text-extractor))



(defun xfig-importer (file)
  "Imports from the fig-file at FILE the text entries" 
  (interactive "fxfig figure file: ")
  (find-file file)
  (message "xfig nach Standard figure 3.0/3.1")
  (let ((current-buff (current-buffer)))
    (occur "^4")
    (switch-to-buffer "*Occur*")
    (toggle-read-only t)
    (toggle-read-only)
    (kill-line)
    (kill-line)
    (while (not (looking-at "^$"))
      (beginning-of-line)
      (forward-word 15)
      (kill-line 0)
      (end-of-line)
      (delete-backward-char 1)
      (next-line 1))
    (delete-window)
    (switch-to-buffer current-buff)
    (split-window-vertically 6)
    (switch-to-buffer "*Occur*")
    ;;
    (goto-char (point-min))
    (while (re-search-forward "\\00$" nil t)
      (replace-match "" nil nil))
    (goto-char (point-min))
    ;;
    (goto-char (point-min))
    (while (re-search-forward "\\\\$" nil t)
      (replace-match "" nil nil))
    (goto-char (point-min))
   
 ;;
    (toggle-read-only t)
    (toggle-read-only)
    (switch-to-buffer "*xfig-text-in-figure*")
    (kill-buffer (current-buffer))
    (rename-buffer "*xfig-text-in-figure*")
    (local-set-key "\C-c\C-c" 'op-jump-to-line)
    (if (featurep 'xemacs)
	(local-set-key [button2] 'op-jump-to-line-at-mouse-position)
      (local-set-key [mouse-2] 'op-jump-to-line-at-mouse-position))
    (local-set-key "q" 'delete-window)
    (local-set-key "\C-c\C-s" 'xop-sort-reference-lines)
    (beginning-of-buffer)
    (other-window 1)
    (kill-buffer (current-buffer))
    (switch-to-buffer current-buff)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defun corell-text-extractor ()
"Extracts from the corell-draw eps-file at the current cursor position the
text entries"
  (interactive)
  (let (find-file-not-found-hooks find-file-not-found-hooks)
    (add-hook 'find-file-not-found-hooks
	      (function (lambda ()
			  (error "Keinen solchen File gefunden: %s" buffer-file-name))))
    (let ((current-buff (current-buffer))(dumvar)(dumbuf))
      (find-file (current-longword))
      (setq dumbuf (current-buffer))
      (let ((beg (point))) (search-forward "%%BeginSetup" nil t) (delete-region beg (point)))
      (while (re-search-forward "^/" nil t)
	(next-line 1)
	(end-of-line)
	(setq dumvar (point))
	(beginning-of-line)
	(catch 'loop
	  (while (search-forward " (" dumvar t)
	    (kill-line 0)
	    (re-search-forward ")" dumvar t)
	    (backward-char 1)
	    (kill-line)

	    (next-line 1)
	    (end-of-line)
	    (setq dumvar (point))
	    (previous-line 1)
	    (beginning-of-line)
	    (kill-line)

	    (next-line 1)
	    (if (looking-at "^T")
		(progn
		  (switch-to-buffer "*Occur*")
		  (toggle-read-only t)
		  (toggle-read-only)
		  (hilit-yank 1)
		  (newline)
		  (switch-to-buffer dumbuf)
		  (throw 'loop t)))

	    (switch-to-buffer "*Occur*")
	    (toggle-read-only t)
	    (toggle-read-only)
	    (hilit-yank 1)
	    (switch-to-buffer dumbuf))))

      (switch-to-buffer dumbuf)
      (revert-buffer nil t)
      (kill-buffer dumbuf)


      (switch-to-buffer current-buff)
      (split-window-vertically 6)
      (switch-to-buffer "*Occur*")
      (toggle-read-only t)
      (toggle-read-only)


;ßae, oe, ue, ss ersetzen
      (beginning-of-buffer)
      (while (search-forward "\\304" nil t)
	(replace-match "Ä" nil t))
      (beginning-of-buffer)
      (while (search-forward "\\326" nil t)
	(replace-match "Ö" nil t))
      (beginning-of-buffer)
      (while (search-forward "\\334" nil t)
	(replace-match "Ü" nil t))
      (beginning-of-buffer)
      (while (search-forward "\\344" nil t)
	(replace-match "ä" nil t))
      (beginning-of-buffer)
      (while (search-forward "\\366" nil t)
	(replace-match "ö" nil t))
      (beginning-of-buffer)
      (while (search-forward "\\374" nil t)
	(replace-match "ü" nil t))
      (beginning-of-buffer)
      (while (search-forward "\\337" nil t)
	(replace-match "ß" nil t))
      (beginning-of-buffer)
      (while (search-forward "\\050" nil t)
	(replace-match "(" nil t))
      (beginning-of-buffer)
      (while (search-forward "\\051" nil t)
	(replace-match ")" nil t))


      (remove-double-lines)

      (beginning-of-buffer)
      (let ((save-case case-fold-search))
	(setq case-fold-search nil)
	(replace-regexp "\\([a-zäöü.,!;)]\\)\\([A-ZÄÖÜ]\\)" "\\1  \\2")
	(setq case-fold-search save-case))

      (rename-buffer "*corell-text-in-figure*")
      (local-set-key "\C-c\C-c" 'op-jump-to-line)
      (local-set-key [mouse-2] 'op-jump-to-line-at-mouse-position)
      (local-set-key "\C-c\C-s" 'xop-sort-reference-lines)
      (beginning-of-buffer))))

(if (fboundp 'define-project-mode-key)
    (define-project-mode-key  "\M-y" 'corell-text-extractor))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

