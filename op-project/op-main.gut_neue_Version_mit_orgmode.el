
;;;
;;; Project-Mode-File,
;;; geschrieben 12.11.1994 - 27.8.1997 von Stefan Walther (SMW), Mainz, Rudolf-Diesel-Str. 3
;;;
;;; Spezialmode zum Verwalten von Projektplanungsfiles
;;;


(defun make-project-elc-file ()
  "Macht aus diesem File den File op-project.el, von dem aus dann alles startet"
  (let ((before-change-functions nil)(after-change-functions nil))
    (goto-char (point-min))
    (write-file "op-project.el")	; sichern, damit bei einer Unterbrechung nicht in op-main.el 
					; herumgeschrieben wird
    (next-line 25) ; überspringt dieses Stückchen Code
    (while (search-forward ";;;;;;;includeforelc!" nil t)
      (save-match-data
	(if (looking-at "20")
	    (progn
	      (if (>= emacs-major-version 20)
		  (progn
		    (forward-line 1)
		    (beginning-of-line)
		    (insert-char ?\; 1)
		    (forward-line 1)
		    (beginning-of-line))
		(forward-line 2)
		(beginning-of-line)
		(insert-char ?\; 1)
		(previous-line 1)
		(beginning-of-line)
		))))
      (if (or (re-search-forward "^(load " nil t)
	      (looking-at "^(load "))
	  (progn
	    (replace-match ";;;;;;(load " nil t)
	    (re-search-forward "\"\\(.*\\)\"" nil t)
	    (next-line 1)
	    (beginning-of-line)
	    (insert-file-contents (concat (match-string 1) ".el"))
	    (insert-char ?\n 1))))
    (write-file "op-project.el")
    ;;
    ;; Auskommentiert, damit Kompatibilität von GNU-Emacs zu Xemacs gewährleistet ist
    ;; (sind nicht binär-kompatibel...)
    ;;(byte-compile-file (buffer-file-name))
    ;;
    ))

;; erzeuge op-project durch Ausführen von 
;; (make-project-elc-file)
;; mit C-x C-e  in diesem Buffer





;;==================== ORG-MODE (der Fremdgängermode...) ===================
(if (boundp 'load--with--org-mode)
    (if load--with--org-mode
	(progn
	  (progn
;;;;;;;includeforelc!
(load "op-outline-mode-emacs20+")
           )
	  (load "op-project__orgmode-install")
	  (org-mode)))
  )
;;==========================================================================



(if (< emacs-major-version 20)
    (standard-display-european 1))
(make-variable-buffer-local 'fill-column)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (not (featurep 'xemacs))
    (progn
      (setq xemacsp nil)
      (setq gnuemacsp t))
    (setq xemacsp t)
    (setq gnuemacsp nil))

(if xemacsp
    (progn
      (require 'overlay)
      (require 'x-popup-menu)))



(message (concat "Laden des Buffers " buffer-file-truename))
(switch-to-buffer (file-name-nondirectory buffer-file-name))
(message (concat "... und ist im Buffer " (prin1-to-string (current-buffer))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; für temporäre Buffer brauche ich die Filedefinition
(if (not buffer-file-name) (setq buffer-file-name "*tmp*"))
(if (and (not (boundp 'buffer-file-truename))
	 (not buffer-file-truename)) 
    (setq buffer-file-truename "*tmp*"))



(let ((buffer-was-read-only buffer-read-only)) ;; dauert an bis zum Ende dieses Files


(if gnuemacsp
    (if window-system
	(progn
;;;;;;;includeforelc!
(load "hilit19.variation")
      )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;---------------------------------------------------------------------------
;; system dependency

 ;; xemacs compatibility
 (when (string-match "xemacs\\|lucid" emacs-version)
   ;; use pre-packaged compatiblity layer
   (require 'overlay))

 ;; xemacs and emacs-19 compatibility
 (when (or (not (fboundp 'add-to-invisibility-spec))
           (not (fboundp 'remove-from-invisibility-spec)))
   ;; `buffer-invisibility-spec' mutators snarfed from Emacs 20.3 lisp/subr.el
   (defun add-to-invisibility-spec (arg)
     (cond
      ((or (null buffer-invisibility-spec) (eq buffer-invisibility-spec t))
       (setq buffer-invisibility-spec (list arg)))
      (t
       (setq buffer-invisibility-spec
             (cons arg buffer-invisibility-spec)))))
   (defun remove-from-invisibility-spec (arg)
     (if buffer-invisibility-spec
         (setq buffer-invisibility-spec
               (delete arg buffer-invisibility-spec)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; postscript stuff
      

(load "ps-print")



;;;;;;;includeforelc!
(load "ps-print-invisible")



(if (< emacs-major-version 20)
   (progn
;;;;;;;includeforelc!
(load "ps-print-landscape")   ;;; Hack, aber funktioniert
;;;;;;;includeforelc!
(load "op-ps-output-design-emacs19")
;; ^
;; | (muss nicht sein !!)
)
(make-variable-buffer-local 'ps-landscape-mode)
(setq ps-landscape-mode t) 
;; ^^^^^^^^^^^^^^^^^^^^^^^
;; | (muss nicht sein !!)
)


;;
;;
;; Die nächsten defadvice sind dafür, daß ps-print auch mit dem 
;; Outline-mode läuft
;;

(defun ps-op-print-region ()
  (interactive)
  (let ((ps-print-buffer nil)(before-change-functions nil)(after-change-functions nil)
	(current-buf (current-buffer)))
    (if (buffer-modified-p)
	(error "Please save buffer first: Undo problems might occur")
      (save-excursion
	(goto-char (point-min))
	(replace-regexp "\r.*" ""))
      (if xemacsp (setq mark-active (mark)))
      (ps-print-region (if mark-active (region-beginning)(point-min))
		       (if mark-active (region-end)(point-max)))
      (switch-to-buffer current-buf)
      (if (buffer-modified-p)
	  (undo 0)))))
(defun ps-op-print-region-with-faces ()
  (interactive)
  (let ((ps-print-buffer nil)(before-change-functions nil)(after-change-functions nil)
	(current-buf (current-buffer)))
    (if (buffer-modified-p)
	(error "Please save buffer first: Undo problems might occur")
      (save-excursion
	(goto-char (point-min))
	(replace-regexp "\r.*" ""))
      (if xemacsp (setq mark-active (mark)))
      (ps-print-region-with-faces (if mark-active (region-beginning)(point-min))
				  (if mark-active (region-end)(point-max)))
      (switch-to-buffer current-buf)
      (if (buffer-modified-p)
	  (undo 0)))))
(defun ps-op-print-buffer ()
  (interactive)
  (let ((ps-print-buffer nil)(before-change-functions nil)(after-change-functions nil)
	(current-buf (current-buffer)))
    (if (buffer-modified-p)
	(error "Please save buffer first: Undo problems might occur")
      (save-excursion
	(goto-char (point-min))
	(replace-regexp "\r.*" ""))
      (ps-print-buffer)
      (switch-to-buffer current-buf)
      (if (buffer-modified-p)
	  (undo 0)))))
(defun ps-op-print-buffer-with-faces ()
  (interactive)
  (let ((ps-print-buffer nil)(before-change-functions nil)(after-change-functions nil)
	(current-buf (current-buffer)))
    (if (buffer-modified-p)
	(error "Please save buffer first: Undo problems might occur")
      (save-excursion
	(goto-char (point-min))
	(replace-regexp "\r.*" ""))
      (ps-print-buffer-with-faces)
      (switch-to-buffer current-buf)
      (if (buffer-modified-p)
	  (undo 0)))))



;;; wird noch einmal wiederholt in op-hilit.xemacs.el
(if xemacsp
    (defun hilit-repaint-command (&optional var)
      t))

;;;;;;;includeforelc!
(load "op-calendar/op-calendar")
;;; gehört eigentlich zu op-appt-changes (siehe Kommentar unten)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; (execute-kbd-macro 'insertmacro)


;;; GNU emacs
;;;
;;; outline-back-to-heading	      outline-backward-same-level
;;; outline-end-of-heading	      outline-end-of-subtree
;;; outline-flag-region		      outline-flag-subtree
;;; outline-forward-same-level	      outline-get-last-sibling
;;; outline-get-next-sibling	      outline-level
;;; outline-minor-mode		      outline-mode
;;; outline-next-heading	      outline-next-preface
;;; outline-next-visible-heading      outline-on-heading-p
;;; outline-previous-visible-heading  outline-up-heading
;;;

;;;
;;; XEmacs mit noutline
;;;
;;;    outline-font-lock-face ()                             outline-level ()
;;;    outline-next-preface ()                               outline-next-heading ()
;;;    outline-previous-heading ()                           outline-visible ()
;;;    outline-back-to-heading (&optional invisible-ok)      outline-on-heading-p (&optional invisible-ok)
;;;    outline-insert-heading ()                             outline-invent-heading (head up)
;;;    outline-promote (&optional children)                  outline-demote (&optional children)
;;;    outline-head-from-level (level head &optional alist)  outline-map-region (fun beg end)
;;;    outline-move-subtree-up (&optional arg)               outline-move-subtree-down (&optional arg)
;;;    outline-end-of-heading ()                             outline-next-visible-heading (arg)
;;;    outline-previous-visible-heading (arg)                outline-mark-subtree ()
;;;    outline-discard-extents (&optional beg end)           alt: outline-discard-extents (from to)
;;;    outline-flag-region (from to flag)                    outline-isearch-open-invisible (overlay)
;;;    hide-entry ()                                         show-entry ()
;;;    hide-body ()                                          hide-region-body (start end)
;;;    show-all ()                                           hide-subtree ()
;;;    hide-leaves ()                                        show-subtree ()
;;;    outline-show-heading ()                               hide-sublevels (levels)
;;;    hide-other ()                                         outline-toggle-children ()
;;;    outline-flag-subtree (flag)                           outline-end-of-subtree ()
;;;    show-branches ()                                      show-children (&optional level)
;;;    outline-up-heading (arg &optional invisible-ok)       outline-forward-same-level (arg)
;;;    outline-get-next-sibling ()                           outline-backward-same-level (arg)
;;;    outline-get-last-sibling ()                           outline-headers-as-kill (beg end)

(message "0")



;;; vor emacs-19.29
;;;(set-input-mode (car (current-input-mode)) (nth 1 (current-input-mode)) t)
(iso-accents-mode 1)
(if (fboundp 'iso-accents-mode)
    (add-hook 'text-mode-hook (function (lambda () (iso-accents-mode 1)))))

;; Zeilenumbruch !!
(setq fill-column 99)


;; zur Kontrolle
(message "1")

;;;
;;; Die naechsten vier werden überschrieben, wenn op-column-mode geladen wird
;;;
(if xemacsp
    (progn
      (local-set-key [(meta left)] '(lambda () (interactive) (scroll-right 2) (backward-char 2)))
      (local-set-key [(meta right)]  '(lambda () (interactive) (scroll-left 2) (forward-char 2)))
      (local-set-key [(meta down)]	 '(lambda () (interactive) (save-excursion (scroll-next-line))))
      (local-set-key [(meta up)]	'(lambda () (interactive) (save-excursion (scroll-previous-line)))))
  (local-set-key [M-left] '(lambda () (interactive) (scroll-right 2) (backward-char 2)))
  (local-set-key [M-right]  '(lambda () (interactive) (scroll-left 2) (forward-char 2)))
  (local-set-key [M-down]	 '(lambda () (interactive) (save-excursion (scroll-next-line))))
  (local-set-key [M-up]	'(lambda () (interactive) (save-excursion (scroll-previous-line)))))




;;;
;;; Fuer das Display zu lange Zeilen werden mit C-x t umgebrochen oder abgebrochen
;;;
(defun truncate-lines ()
  "Toggles between truncated and not truncated lines"
  (interactive) 
  (if truncate-lines 
      (set-variable (quote truncate-lines) nil) 
    (set-variable (quote truncate-lines) t)) 
  (redraw-display))

(define-key ctl-x-map "t" 'truncate-lines)
(setq truncate-lines t)

(setq scroll-step 1)

(defun outline-blank-line-toggle ()
  "Toggles between blank-line and not blank-line mode"
  (interactive) 
  (if outline-blank-line 
      (progn
	(set-variable (quote outline-blank-line) nil)
	(message "Outline blank line set to `nilŽ"))
    (set-variable (quote outline-blank-line) t) 
    (message "Outline blank line set to `trueŽ"))
  (redraw-display))




;;;=============================================================================

;; braucht die Variable truncate-lines, sonst unabhängig
;;;;;;;includeforelc!
(load "op-isearch")


;;;;;;;includeforelc!
(load "isearch-outline")


;;;;;;;includeforelc!
(load "outlines")



(defun isearch-toggle-outlines ()
  "Toggle searching of hidden text within isearch."
  (interactive)
  (setq isearch-outlines (not isearch-outlines))
  (if isearch-outlines
      (message "\C-s \C-r  isearch sucht auch in von Outline-versteckten Feldern")
    (message "\C-s \C-r  isearch sucht nur in sichtbaren Feldern"))
  (isearch-update))




;; glyphs: einfachstes Einfügen von Bildern
;;;;;;;includeforelc!
(load "op-glyph-images")



;;;=============================================================================


;;;
;;; Backup files [name~]
;;; siehe op-crypt.el, wo die ~-Files umbenannt werden
(make-variable-buffer-local 'version-control)
(setq version-control nil)





;; zur Kontrolle
(message "1a")


;;; das nächste steht hier, weil es den indentet-text-mode braucht,
;;; und der ist sehr unkooperativ (muß also durch weiteres überschrieben werden).
;;;;;;;includeforelc!
(load "op-indent")


;; zur Kontrolle
(message "2")

;
; Hideif-Mode in abgeänderter Weise
;
;; laden des geänderten op-hideif (= hideif.el)- Modes
;;;;;;;includeforelc!
(load "op-hideif")


(hide-ifdef-mode t)
(setq hide-ifdef-lines t)
(setq hide-ifdef-initially t)


;; zur Kontrolle
(message "3")


;
; Outline-minor-mode einschalten, nötig für den Project-Mode
;
; nach der emacs-version 19.31
(defvar outline-minor-mode-prefix "\C-c\C-o")



(make-variable-buffer-local 'outline-regexp)
(setq outline-regexp "[*\^L]+")


(if gnuemacsp
    (progn
      (if (< emacs-major-version 20)
	  (progn
;;;;;;;includeforelc!
(load "op-outline-mode-emacs19")  ;; nix anderes als der outline-mode OHNE die 
	;; unnötige Menübalkenbelegung
	;; else
)
;;;;;;;includeforelc!
(load "op-outline-mode-emacs20+")  ;; nix anderes als der outline-mode alt (der neue hat mir zu viele Bugs),
        ;; OHNE die unnötige Menübalkenbelegung
	)))

(if xemacsp
    (progn
      (load "op-outline-mode-emacs20+")
      ))
(outline-minor-mode)

;;; SMW !!! wegen der Kompatibilität zum project-mode: Einfach Überladen der Funktion
(defun outline-discard-extents (from to)
  "Delete hideshow extents in region defined by FROM and TO."
  (when (< to from)
    (setq from (prog1 to (setq to from))))
  (map-extents #'(lambda (ex ignored) (delete-extent ex))
	       (current-buffer) from to nil 'end-closed 'outline))

(message "3a")










(message "3b")




(defun hide-all-ifdefs ()
  (interactive)
  (goto-char (point-min))
  (let ((already-modified (buffer-modified-p))(beg (point))(last-found (point))
	(anfang_offen)(still-to-search) (string-store)
	(before-change-functions nil)(after-change-functions nil))
    (setq still-to-search (point-max))
    (goto-char beg)
    (if (re-search-forward "[\r\n][ \t]*###[ \tn]+\\(\\w+\\)" nil t)
	(progn
	  (setq string-store (match-string 1))
	  (setq anfang-offen (match-beginning 0))
	  (while ( < (point) still-to-search)
	    (re-search-forward	  "[\r\n][ \t]*###bis" nil t)
	    (progn
	      (setq last-found (point))
	      (narrow-to-region anfang-offen last-found)
	      (goto-char (point-min))
	      ;; wenn der gefundene String definiert ist, dann falte ifdef NICHT zusammen
	      (if (not (hif-defined (intern string-store))) 
		  (while (search-forward "\n" nil t)
		    (replace-match "\r" nil t))
		(goto-char (point-min))
		(while (search-forward "###" nil t)
		  (beginning-of-line)
		  (backward-char 1)
		  (if (looking-at "\n")
		      (replace-match "\r" nil t))
		  (end-of-line)))
	      (goto-char (point-max))
	      (setq last-found (point))
	      (widen)
	      (goto-char last-found)
	      (if (re-search-forward "[\r\n][ \t]*###[ \tn]+\\(\\w+\\)" nil t)
		  (progn
		    (setq anfang-offen (match-beginning 0))
		    (setq string-store (match-string 1)))
		(goto-char still-to-search))))))
    (set-buffer-modified-p already-modified)))



;;;;;;;includeforelc!
(load "op-hide")



;; from outline.el with docstring fixed.
(defun hif-outline-flag-region (from to flag)
  "Hides or shows lines from FROM to TO, according to FLAG.  If FLAG
is \\n (newline character) then text is shown, while if FLAG is \\^M
\(control-M) the text is hidden."
  (let ((modp (buffer-modified-p)))
    (unwind-protect (progn
		      (subst-char-in-region from to
					    (if (= flag ?\n) ?\^M ?\n)
					    flag t) )
      (set-buffer-modified-p modp))))


;;;
;;; load the functions for crypting and decrypting the project mode
;;;
;;;;;;;includeforelc!
(load "op-crypt")



;; zur Kontrolle
(message "4")


;;; ----------------------------------------------------------------------------------------
;;; ----------------------------------------------------------------------------------------
;;; ----------------------------------------------------------------------------------------



;
; Definition des Prefix-Arguments für den Project-Mode
;
(defvar project-mode-prefix nil)
;(defvar project-mode-keymap (make-sparse-keymap))

(setq project-mode-prefix "\M-c\M-o")

(defvar outline-minor-mode-map nil)
(defun define-project-mode-key (key command)
  (define-key outline-minor-mode-map (concat project-mode-prefix key) command))



;;;
;;;
;;; SCHNITTSTELLENPROGRAMMIERUNGEN ZU ANDEREN FILES
;;;
;;;

;;;;;;;includeforelc!
(load "op-xfig&corell-analyser")

;;;;;;;includeforelc!
(load "op-mpx-analyser")

;;;;;;;includeforelc!
(load "op-dia-syncer")




;;;;;;;includeforelc!
(load "csv")

;;;;;;;includeforelc!
(load "op-csv")




;;;;;;;includeforelc!
(load "op-BF")
;; hier war der File für das Programm BrainForest Pro, das auf dem Palm läuft

;; zur Kontrolle
(message "5")

;;;
;;; Die nächsten Zeilen laden den Hyperbole-Mode
;;;
;;;;;;;includeforelc!
(load "op-hyper/sql-mode")


;;;;;;;includeforelc!
(load "op-hyper/op-hyper")


;;;;;;;includeforelc!
(load "op-hyper/op-consistent")
(automatic-change-mode t)            ; Alle Labels automatisch aktualisieren 
(automatic-change-mode)     	     ;(nil macht es aber nicht aus -- geht nur durch toggeln)


;; zur Kontrolle
(message "6")


;;;----------------------------------------------------------------------------------------------------



;;; für die #include-files
;;;;;;;includeforelc!
(load "op-includer")



(condition-case var

;;; ediff-Erweiterung, damit fine-diffs besser unterstützt werden

(load "ediff")
(load "op-ediff")

;;;;;;;includeforelc!
(load "op-ediff")

  (error "Problem loading op-ediff package, probably problem connecting to diff program"))

;;;;;;;includeforelc!
(load "op-ediff-mods")



;; zur Kontrolle
(message "7")


;;; für den gemeinsamen cvs-Pool
;;;;;;;includeforelc!
(load "op-cvs")


;; zur Kontrolle
(message "7")

;;;--------------------------------------------------------------------------------

;
; Logbuch-Mode (ist auch Text-Mode)
; (steht hier, um den Indentet-Text-Mode zu überschreiben, weil das
; Hilit das abfragt)
;;;;;;;includeforelc!
(load "op-logbuch")



(logbuch-mode)


; --------------------------------------------------------------------------------





;;SMW isearch

;;;;;;;includeforelc!
(load "op-search")  ;;; alle Search-Funktionen



;;;;;;;includeforelc!
(load "op-columns")

(if gnuemacsp
    (local-set-key "\C-c\C-g" 'column-ruler)
  (define-key outline-minor-mode-map [(control c) g] 'column-ruler))



;; zur Kontrolle
(message "8")


;---------------------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------------------
;
; Project-Mode-Funktionen --- entspricht Zusammenspiel von Outline-Mode mit Hideif-Mode
;
;


(local-set-key "q" '(lambda()(interactive)(if (= (point)(point-min))(bury-buffer-smartly)(insert-char ?q 1))))
(defun bury-buffer-smartly ()
  (interactive)
  (bury-buffer)
  (if (or (equal 
	   (substring 
	    (buffer-name (car (buffer-list))) 0 1)
	   " ")
	  (equal 
	   (substring 
	    (buffer-name (car (buffer-list))) 0 1)
	   "*"))
      (bury-buffer-smartly)))
    


(defun top-heading ()
  (interactive)
  (backward-char 1)
  (re-search-backward "^[*][^*]" nil t)
  (beginning-of-line))
(define-project-mode-key "^" 'top-heading)


(defun next-top-heading ()
  (interactive)
  (forward-char 1)
  (re-search-forward "^[*][^*]" nil t)
  (beginning-of-line))
(define-project-mode-key "v" 'next-top-heading)


(defun very-top-heading ()
  (interactive)
  (goto-char (point-min))
  (re-search-forward "^[*][^*]" nil t)
  (beginning-of-line))
(define-project-mode-key "\M-^" 'very-top-heading)



(define-project-mode-key  "\M-s" 'show-subtree-hide)
;;## dies ist die alte Version vor dem Umstieg auf XEmacs, könnte noch einmal zu aktivieren sein,
;;## jedoch funktioniere bei Tests leider die Funktion "show-subtree" nicht, warum auch immer
;;(defun show-subtree-hide (&optional called-noninteractive)
;;  (interactive "P")
;;  (let ((before-change-functions nil)(after-change-functions nil)
;;	(already-modified (buffer-modified-p)))
;;    (show-subtree)
;;    (toggle-read-only t)(toggle-read-only)
;;    (save-excursion
;;      (hide-all-ifdefs)
;;      (end-of-show called-noninteractive)
;;      (setq selective-display t))
;;    (set-buffer-modified-p already-modified)))
(defun show-subtree-hide (&optional called-noninteractive)
  (interactive)
  (let ((before-change-functions nil)(after-change-functions nil)
	(already-modified (buffer-modified-p)))
    (save-excursion	
      (beginning-of-line)
      (let ((already-modified (buffer-modified-p))(beg (point)))
	(outline-end-of-subtree)
	(if (not (re-search-forward "^*" nil t))
	    (point-max))
	(narrow-to-region beg (point))
	(goto-char beg)
	(show-all-hide)
	(toggle-read-only t)(toggle-read-only) ; gerade mal unklar: könnte auch eine Zeile 
	                                       ; obendran müssen, wahrscheinlich jedoch nicht
	(save-excursion
	  (hide-all-ifdefs)
	  (end-of-show called-noninteractive)
	  (setq selective-display t))
	(widen)
	(set-buffer-modified-p already-modified)))))



(define-project-mode-key  "\M-d" 'hide-subtree-hide)
(defun hide-subtree-hide ()
  (interactive)
  (let ((before-change-functions nil)(after-change-functions nil))
  (save-excursion
    (beginning-of-line)
    (let ((already-modified (buffer-modified-p))(beg (point)))
      (outline-end-of-subtree)
      (if (not (re-search-forward "^*" nil t))
	  (point-max))
      (narrow-to-region beg (point))
      (goto-char beg)
      (hide-other-hide t)
      (widen)
      (setq selective-display t)
      (set-buffer-modified-p already-modified)))))


(define-project-mode-key "\M-k" 'show-branches-hide)
(defun show-branches-hide ()
  (interactive)
  (let ((before-change-functions nil)(after-change-functions nil))
  (save-excursion
    (let ((already-modified (buffer-modified-p))(beg (point)))
      (outline-end-of-subtree)
      (narrow-to-region beg (point))
      (goto-char (point-min))
      (show-ifdefs)
      (hide-ifdefs)
      (hide-body)
      (widen)
      (end-of-show)
      (setq selective-display t)
      (set-buffer-modified-p already-modified)))))


(define-project-mode-key "\M-j" 'show-all-branches-hide)
(defun show-all-branches-hide ()
  (interactive)
  (let ((before-change-functions nil)(after-change-functions nil))
  (save-excursion
    (let ((already-modified (buffer-modified-p))(beg)(end))
      ;; stelle die letzte Hauptüberschrift fest
      (save-excursion
	(goto-char (point-max))
	(outline-previous-visible-heading 1)
	(if (= (outline-level) 1)
	    (setq end (point))
	  (outline-up-heading 100))
	(setq end (point)))
      (setq beg (point))
      (while 
	  (progn
	    (save-excursion
	      (outline-end-of-subtree)
	      (narrow-to-region beg (point))
	      (goto-char (point-min))
	      (show-ifdefs)
	      (hide-ifdefs)
	      (hide-body)
	      (toggle-read-only t)(toggle-read-only)
	      (widen))
	    (outline-get-next-sibling)
	    (setq beg (point))
	    (not (= (point) end))))
      (save-excursion
	(outline-end-of-subtree)
	(narrow-to-region beg (point))
	(goto-char (point-min))
	(show-ifdefs)
	(hide-ifdefs)
	(hide-body)
	(widen)
	(end-of-show)
	(setq selective-display t)
	(set-buffer-modified-p already-modified))))))





(define-project-mode-key "\M-l" 'hide-leaves-hide)
(defun hide-leaves-hide ()
  (interactive)
  (let ((before-change-functions nil)(after-change-functions nil))
  (save-excursion
    (beginning-of-line)
    (show-subtree-hide t)
    (hide-leaves))
    (end-of-show)
    (setq selective-display t)))


(define-project-mode-key "\M-e" 'show-entry-hide)
(defun show-entry-hide ()
  (interactive)
  (let ((before-change-functions nil)(after-change-functions nil)
	(already-modified (buffer-modified-p))(beg (point)))
    (beginning-of-line)
    (if (< emacs-major-version 20)
	(while (re-search-forward "\r\\*" beg t)
	  (replace-match "\n\*" nil nil)))
    (show-entry)
    (setq beg (save-excursion
		(beginning-of-line)
		(point)))
    (toggle-read-only t)(toggle-read-only)
    (save-excursion
      (if (not (outline-next-heading))
	  (outline-end-of-subtree)
;	(beginning-of-line)
;	(next-line 1))
	)  ;; testweise Änderung 8.9.1996
      (narrow-to-region beg (point))
      (goto-char (point-min))
      (show-subtree-hide t)
      (widen)
      (end-of-show)
      (setq selective-display t)
      (set-buffer-modified-p already-modified))))


(define-project-mode-key "\M-c" 'hide-entry-hide)
(defun hide-entry-hide ()
  (interactive)
  (let ((before-change-functions nil)(after-change-functions nil)
	(already-modified (buffer-modified-p)))
    (beginning-of-line)
    (hide-entry)
    (toggle-read-only t)(toggle-read-only)
    (setq selective-display t)
    (beginning-of-line)
    (set-buffer-modified-p already-modified)))

(define-project-mode-key "\M-a" 'show-all-hide)
(defun show-all-hide ()
  (interactive)
  (let ((before-change-functions nil)(after-change-functions nil)
	(already-modified (buffer-modified-p)))
    (save-excursion
      (show-all)
      (toggle-read-only t)(toggle-read-only)
      (setq selective-display t)
      (hide-ifdefs)
      (end-of-show))
    (set-buffer-modified-p already-modified)))

; hide-body ungeaendert
(define-project-mode-key "\M-t" 'hide-body)


;; zur Kontrolle
(message "9")



(define-project-mode-key "\C-i" 'show-children-hide)
(defun show-children-hide (&optional levels)
  (interactive "P")
  (let ((before-change-functions nil)(after-change-functions nil))
  (show-children levels)
  (save-excursion
    (let ((already-modified (buffer-modified-p))(beg (point))(last-found (point))
	  (anfang_offen)(still-to-search))
      (toggle-read-only t)(toggle-read-only)
      (outline-end-of-subtree)
      (next-line 1)
      (setq still-to-search (point))
      (goto-char beg)
      (if (re-search-forward "\r[ \t]*###[ \tn]+" nil t)
	  (progn
	    (setq anfang-offen (point))
	    (while ( < (point) still-to-search)
	      (re-search-forward    "\r[ \t]*###bis" nil t)
	      (progn
		(setq last-found (point))
		(narrow-to-region anfang-offen last-found)
		(goto-char (point-min))
		(while (search-forward "\n" nil t)
		  (replace-match "\r" nil t))
		(goto-char (point-max))
		(setq last-found (point))
		(widen)
		(goto-char last-found)
		(if (re-search-forward "\r[ \t]*###[ \tn]+" nil t)
		    (setq anfang-offen (point))
		  (goto-char still-to-search)))))
	(goto-char still-to-search))
      (end-of-show)
      (setq selective-display t)
      (set-buffer-modified-p already-modified)))))


(define-project-mode-key "\M-o" 'hide-other-hide)
(defun hide-other-hide (&optional called-noninteractive)
  (interactive "P")
  ;;
  ;; hide-other im emacs19 ist buggy, daher der erhebliche Umstand in den nächsten Zeilen
  ;; hide-other im emacs20 habe ich nicht mehr kontrolliert
  ;;
  (let ((before-change-functions nil)(after-change-functions nil)
	(already-modified (buffer-modified-p))(beg (point))(last-found (point))
	(anfang-offen)(still-to-search))
    (show-ifdefs)
    (hide-other)
    (toggle-read-only t)(toggle-read-only)
    (save-excursion
      (beginning-of-line)
      (setq still-to-search (point-max))
      (goto-char (point-min))
      (if (re-search-forward "\r[ \t]*###[ \tn]+" nil t)
	  (progn
	    (setq anfang-offen (point))
	    (while ( < (point) still-to-search)
	      (re-search-forward    "\r[ \t]*###bis" nil t)
	      (progn
		(setq last-found (point))
		(narrow-to-region anfang-offen last-found)
		(goto-char (point-min))
		(while (search-forward "\n" nil t)
		  (replace-match "\r" nil t))
		(goto-char (point-max))
		(setq last-found (point))
		(widen)
		(goto-char last-found)
		(if (re-search-forward "\r[ \t]*###[ \tn]+" nil t)
		    (setq anfang-offen (point))
		  (goto-char still-to-search)))))
	(goto-char still-to-search))

      (if gnuemacsp
	  (end-of-show called-noninteractive)
	(end-of-show t))

      (setq selective-display t)
      (set-buffer-modified-p already-modified))))


(define-project-mode-key "\M-n" 'hide-all)
(defun hide-all ()
;; speziell wichtig geworden nach dem verkorksten outline-Mode des emacs20
  (interactive)
  (save-excursion 
    (beginning-of-buffer)
    (while (re-search-forward "^\\*[^*]" nil t)
        (progn
	  (beginning-of-line)
          (hide-subtree)
	  (end-of-line)
	  ))))

(define-project-mode-key "\M-q" 'hide-sublevels-hide)
(defun hide-sublevels-hide (levels)
  (interactive "P")
  (if xemacsp (setq mark-active (mark)))
  (save-excursion
    ;; tmp1 and tmp2 for region or buffer
    (let ((tmp1 (if mark-active (if (<= (region-beginning)(region-end))(region-beginning)(region-end)) (point-min))) 
	  (tmp2 (if mark-active (if (<= (region-beginning)(region-end))(region-end)(region-beginning)) (point-max))))
      ;; wenn an einem Punkt und eine Marke gesetzt (d.h. nur C-Space z.B. auf einer Hauptüberschrift), 
      ;; dann erweitere diese Überschrift - dazu muss die Markierung die nächste Zeile erreichen
      (if (or (= tmp1 tmp2) 
	      ;; oder wenn man gerade auf einer Überschrift steht
	      (save-excursion (beginning-of-line)(looking-at "*")))
	  (progn
	    (setq tmp1 (save-excursion (beginning-of-line) (point)))
	    (setq tmp2 (save-excursion (beginning-of-line) (next-line 1) (point)))))
      ;;
      (narrow-to-region tmp1 tmp2)
      (let ((before-change-functions nil)(after-change-functions nil)
	    (already-modified (buffer-modified-p))(last-found (point))(anfang_offen (point))
	    (still-to-search (point)))
; hide-other ist buggy, daher der erhebliche Umstand in den nächsten Zeilen
	(show-ifdefs)
	(hide-sublevels levels)
	(save-excursion
	  (beginning-of-line)
	  (toggle-read-only t)(toggle-read-only)
	  (setq still-to-search (point-max))
	  (goto-char (point-min))
	  (while ( < (point) still-to-search)
	    (if (re-search-forward	  "\\(\r[ \t]*###[ \tn]+\\).*\n*.*\r[ \t]*###bis" nil t)
		(progn
		  (setq anfang_offen (point))
		  (narrow-to-region (match-beginning 1) anfang_offen)
		  (goto-char (point-min))
		  (while (search-forward "\n" nil t)
		    (replace-match "\r" nil t))
		  (goto-char (point-max))
		  (setq last-found (point))
		  (widen)
		  (goto-char last-found))
	      (goto-char (point-max))))
	  (setq selective-display t)
	  (set-buffer-modified-p already-modified)))
      (widen)))
  (if (not (looking-at "*"))(if mark-active (goto-char (region-beginning)))))






(define-project-mode-key "\M-y" 'show-current-place)
(defun show-current-place (&optional called-noninteractive)
  (interactive "P")
  (let ((before-change-functions nil)(after-change-functions nil)
	(already-modified (buffer-modified-p)))
  (show-subtree)
;SMWt  (toggle-read-only t)(toggle-read-only)
  (save-excursion
    (end-of-show called-noninteractive)
    (setq selective-display t))
  (set-buffer-modified-p already-modified)))





(define-project-mode-key "\M-f" 'outline-forward-jump-same-level)
(defun outline-forward-jump-same-level ()
  (interactive)
  (let ((before-change-functions nil)(after-change-functions nil))
  (let ((thislevel (outline-level))(end)(tmpplace1)(tmpplace2))
    (save-excursion
      (goto-char (point-max))
      (outline-back-to-heading)
      (setq end (point)))
    (setq tmpplace1 (point))
    (save-excursion
      (while
	  (progn
	    (outline-next-heading)
	    (and (not (= (outline-level) thislevel))
		 (not (= (point) end)))))
      (if (= (outline-level) thislevel)
	  (setq tmpplace2 (point))
	(setq tmpplace2 tmpplace1)))
    (if (not (= tmpplace1 tmpplace2))
	(goto-char tmpplace2)
      (message "no next visible heading of the same level")))))


(define-project-mode-key "\M-b" 'outline-backward-jump-same-level)
(defun outline-backward-jump-same-level ()
  (interactive)
  (let ((before-change-functions nil)(after-change-functions nil))
  (let ((thislevel (outline-level))(begin)(tmpplace1)(tmpplace2))
    (save-excursion
      (goto-char (point-min))
      (outline-next-heading)
      (setq begin (point)))
    (setq tmpplace1 (point))
    (save-excursion
      (while
	  (progn
	    (backward-char 1)
	    (outline-back-to-heading)
	    (and (not (= (outline-level) thislevel))
		 (not (= (point) begin)))))
      (if (= (outline-level) thislevel)
	  (setq tmpplace2 (point))
	(setq tmpplace2 tmpplace1)))
    (if (not (= tmpplace1 tmpplace2))
	(goto-char tmpplace2)
      (message "no previous visible heading of the same level")))))



(defun show-really-everything ()
  (interactive)
  (let ((p (point))(before-change-functions nil)(after-change-functions nil)(already-modified (buffer-modified-p)))
    (widen)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "\r" nil t)
	(replace-match "\n" nil t)))
    (setq selective-display t)
    (outline-flag-region (point-min)(point-max) nil)
    (set-buffer-modified-p already-modified)
    (show-invisible)
  (goto-char p))
  (if window-system
      (hilit-repaint-command t)))


(defun show-really-everything-in-region ()
  (interactive)
  (let ((p (point))(before-change-functions nil)(after-change-functions nil))
    (if xemacsp (setq mark-active (mark)))
    (narrow-to-region (if mark-active (region-beginning)(point-min))
		      (if mark-active (region-end)(point-max)))
    (let ((already-modified (buffer-modified-p)))
      (goto-char (point-min))
      (save-excursion
	(while (search-forward "\r" nil t)
	  (replace-match "\n" nil t)))
	(setq selective-display t)
	(set-buffer-modified-p already-modified))
  (goto-char p))
  (show-invisible)
  (outline-flag-region (if mark-active (region-beginning)(point-min))
		       (if mark-active (region-end)(point-max)) 
		       nil)
  (widen)
  (if window-system
      (hilit-repaint-command t)))


(defun sre ()
"like show-really-everything, but trims buffer and contracts it again"
  (interactive)
  (let ((p (point))(before-change-functions nil)(after-change-functions nil))
  (save-excursion
    (let ((already-modified (buffer-modified-p)))
      (widen)
      (goto-char (point-min))
      (while (search-forward "\r" nil t)
	(replace-match "\n" nil t))
      (setq selective-display t)
      (outline-flag-region (point-min)(point-max) nil)
      (set-buffer-modified-p already-modified)
      (goto-char p)))
  (show-invisible)
  (trim-buffer)
  (hide-other-hide t)
  (if window-system
      (hilit-repaint-command t))))





(defun end-of-show (&optional called-noninteractive)
  (let ((before-change-functions nil)(after-change-functions nil)
	(already-modified (buffer-modified-p)))
  (op-hide-at-end-of-show)
  (hide-end-of-include-statments)
  ;; Mache den File schreibbar. Wichtig, da im outline ein Bug ist, der nicht mit der Version-Control zusammenarbeitet
  (toggle-read-only t)(toggle-read-only)
  (if (and (not called-noninteractive)
	   window-system)
      (hilit-repaint-command t))
  (set-buffer-modified-p already-modified)))




(defun op-sort ()
"Sortiert die aktuelle Region nach den Überschriften der obersten verfügbaren Hierarchie.
 Die sortierte Region wird zur Kennzeichnung nicht farblich hervorgehoben"
  (interactive)
  (let ((before-change-functions nil)(after-change-functions nil))
  (save-excursion
    (narrow-to-region (region-beginning) (region-end))
    (goto-char (point-min))
    (re-search-forward "^\\*" nil t)
    (hide-other-hide)
    (sort-regexp-fields nil "^\\**[ \\t]*\\w*.*$" "\\<\\w*.*" (point-min) (point-max))
    (widen))))


;; zur Kontrolle
(message "10")


;
; aktivieren der Tastatur
;
;(setq map (copy-keymap (current-local-map)))
;(copy-keymap logbuch-mode-map)
;(use-local-map project-mode-keymap)
;(setq minor-mode-map-alist
;      (cons
;	(cons 'outline-mode outline-mode-map)
;	    minor-mode-map-alist))




;;;--------------------------------------------------------------------------------




(defun remove-simple-re-comments (dummy comment-start-string comment-end-string)
  (interactive "sHit return to REMOVE comments with regular expressions from the current region\nsComment start re-string: \nsComment end re-string: ")
  (let ((before-change-functions nil)(after-change-functions nil))
  (if xemacsp (setq mark-active (mark)))
  (narrow-to-region
   (if mark-active (region-beginning)(point-min))
   (if mark-active (region-end)(point-max)))
  (goto-char (point-min))
  (let ((already-modified (buffer-modified-p))(beg (point))(last-found (point))
	(anfang_offen)(still-to-search))
    (setq still-to-search (point-max))
    (goto-char beg)
    (if (re-search-forward comment-start-string nil t)
	(progn
	  (re-search-backward comment-start-string nil t)
	  (setq anfang-offen (match-beginning 0))
	  (while ( < (point) still-to-search)
	    (re-search-forward comment-end-string nil t)
	    (progn
	      (setq last-found (point))
	      (kill-region anfang-offen last-found)
	      (setq last-found (point))
	      (setq still-to-search (point-max))
	      (goto-char last-found)
	      (if (re-search-forward comment-start-string nil t)
		  (progn
		    (re-search-backward comment-start-string nil t)
		    (setq anfang-offen (match-beginning 0)))
		(goto-char still-to-search))))))
    (widen)
    (set-buffer-modified-p already-modified))))




(defun remove-simple-comments (dummy comment-start-string comment-end-string)
  (interactive "sHit return to REMOVE comments from the current region\nsComment start string: \nsComment end string: ")
  (let ((before-change-functions nil)(after-change-functions nil))
  (if xemacsp (setq mark-active (mark)))
  (narrow-to-region
   (if mark-active (region-beginning)(point-min))
   (if mark-active (region-end)(point-max)))
  (goto-char (point-min))
  (let ((already-modified (buffer-modified-p))(beg (point))(last-found (point))
	(anfang_offen)(still-to-search))
    (setq still-to-search (point-max))
    (goto-char beg)
    (if (search-forward comment-start-string nil t)
	(progn
	  (search-backward comment-start-string nil t)
	  (setq anfang-offen (match-beginning 0))
	  (while ( < (point) still-to-search)
	    (search-forward comment-end-string nil t)
	    (progn
	      (setq last-found (point))
	      (kill-region anfang-offen last-found)
	      (setq last-found (point))
	      (setq still-to-search (point-max))
	      (goto-char last-found)
	      (if (search-forward comment-start-string nil t)
		  (progn
		    (search-backward comment-start-string nil t)
		    (setq anfang-offen (match-beginning 0)))
		(goto-char still-to-search))))))
    (widen)
    (set-buffer-modified-p already-modified))))



(defun remove-/*-comments (dummy)
; needs perl to be installed
  (interactive "sHit return to REMOVE C-like comments in the current buffer")
  (let ((before-change-functions nil)(after-change-functions nil))
    ;(save-excursion
    ;  (goto-char (if mark-active (region-beginning)(point-min)))
    ;  (insert-char ?\n 1))
    (save-excursion
      (if xemacsp (setq mark-active (mark)))

      (condition-case nil
	  (progn
	    (shell-command-on-region
	     (if mark-active (region-beginning)(point-min))
	     (if mark-active (region-end)(point-max))
; genereller Test:
; "perl -e \"print \\\"Hallo Stefan\\\" \" "
; for perl: 
; "$/ = undef;$_ = <>;s#/\*[^*]*\*+([^/*][^*]*\*+)*/|("(\\.|[^"\\])*"|'(\\.|[^'\\])*'|\n+|.[^/"'\\]*)#$2#g;print"
; for bash: 
; "perl -ne \"\$/ = undef;\$_ = <>;s#/\\*[^*]*\\*+([^/*][^*]*\\*+)*/|(\"(\\\\.|[^\"\\\\])*\"|'(\\\\.|[^'\\\\])*'|\\n+|.[^/\"'\\\\]*)#\$2#g;print\" " )
; for emacs
	     "perl -e \"\\\$/ = undef;\\\$_ = <>;s#/\\\\*[^*]*\\\\*+([^/*][^*]*\\\\*+)*/|(\\\"(\\\\\\\\.|[^\\\"\\\\\\\\])*\\\"|'(\\\\\\\\.|[^'\\\\\\\\])*'|\\\\n+|.[^/\\\"'\\\\\\\\]*)#\\$2#g;print\" " t)
	    )
       (error nil)))))



;; zur Kontrolle
(message "11")



;;; ------------------------------------------------------------------------------------------------------------




(defvar am-i-at-the-same-point 999999)
(make-variable-buffer-local 'am-i-at-the-same-point)



;;;--------------------------------------------------------------------------------




(defun begin-of-word (n)
  "Goes to beginning of word that cursor is on or after"
  (interactive "p")
  (let ((before-change-functions nil)(after-change-functions nil))
  (if (< (point) (point-max))
       (forward-char 1))
  (forward-word (- n))))




(defun capitalize-current-word (n)
  "Capitalizes the first letter of the word hat point is on or after"
  (interactive "p")
  (save-excursion
    (begin-of-word 1)
    (if (< n 0)
	(progn
	  (forward-word n)
	  (capitalize-word 1))
      (capitalize-word n))))
(defun downcase-current-word (n)
  "Capitalizes the first letter of the word hat point is on or after"
  (interactive "p")
  (save-excursion
    (begin-of-word 1)
    (if (< n 0)
	(progn
	  (forward-word n)
	  (downcase-word 1))
      (downcase-word n))))
(defun upcase-current-word (n)
  "Capitalizes the first letter of the word hat point is on or after"
  (interactive "p")
  (save-excursion
    (begin-of-word 1)
    (if (< n 0)
	(progn
	  (forward-word n)
	  (upcase-word 1))
      (upcase-word n))))
(defun Downcase-current-word (n)
  "Like downcase-word, except downcases from the beginning of word.
   Stops at the end of a line if there are no additional blanks."
  (interactive "p")
  (if (< n 0)
      (progn
	(save-excursion
	  (begin-of-word 1)
	  (forward-word n)
	  (downcase-word 1)))
    (begin-of-word 1)
    (downcase-word n)
    (let ((position1 (point)) (position2))
      (save-excursion
	(end-of-line)
	(re-search-backward "[a-z0-9A-Z]" nil t)
	(forward-char 1)
	(setq position2 (point)))
      (if (not (= position1 position2))
	  (progn
	    (begin-of-word -1)
	    (backward-word 1))))))
(defun Upcase-current-word (n)
  "Like upcase-word, except upcases from the beginning of word.
   Stops at the end of a line."
  (interactive "p")
  (if (< n 0)
      (progn
	(save-excursion
	 (begin-of-word 1)
	 (forward-word n)
	 (upcase-word 1)))
    (begin-of-word 1)
    (upcase-word n)
    (let ((position1 (point)) (position2))
      (save-excursion
	(end-of-line)
	(re-search-backward "[a-z0-9A-Z]" nil t)
	(forward-char 1)
	(setq position2 (point)))
      (if (not (= position1 position2))
	  (progn
	    (begin-of-word -1)
	    (backward-word 1))))))
(define-key global-map "\M-l" 'downcase-current-word)
(define-key global-map "\M-u" 'upcase-current-word)
(define-key global-map "\M-L" 'Downcase-current-word)
(define-key global-map "\M-U" 'Upcase-current-word)
(define-key global-map "\M-C" 'capitalize-current-word)







;;; ********************************************************************************
;;;  Umlautiges	 ---  siehe auch noch op-main.el
;;; ********************************************************************************



(defun replace-umlaute (dummy)
; needs perl to be installed
  (interactive "sHit return to REPLACE Umlaute (ü->ue e.g.) in current buffer/region")
  (save-excursion
    (goto-char (point-min))
    (let ((before-change-functions nil)(after-change-functions nil))
      (insert-char ?\n	1))
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
       "perl -ne \"\\$/=undef;\\$_=<>;%convert=(\\\"=C4\\\",\\\"Ä\\\",\\\"=D6\\\",\\\"Ö\\\",\\\"=DC\\\",\\\"Ü\\\",\\\"=E4\\\",\\\"ä\\\",\\\"=F6\\\",\\\"ö\\\",\\\"=FC\\\",\\\"ü\\\",\\\"=DF\\\",\\\"ß\\\",\\\"=3D\\\",\\\"=\\\",\\\"=20\\\",\\\" \\\");s/(=C4|=D6|=DC|=E4|=F6|=FC|=DF|=20|=3D)/\\$convert{\\$1}/g;print\" " t)
      (goto-char (point-min))
      (insert-char ?\n 1)
      (shell-command-on-region
       (point-min)
       (point-max)
       "perl -ne \"\\$/=undef;\\$_=<>;%convert=(\\\"=3DC4\\\",\\\"Ä\\\",\\\"=3DD6\\\",\\\"Ö\\\",\\\"=3DDC\\\",\\\"Ü\\\",\\\"=3DE4\\\",\\\"ä\\\",\\\"=3DF6\\\",\\\"ö\\\",\\\"=3DFC\\\",\\\"ü\\\",\\\"=3DDF\\\",\\\"ß\\\",\\\"=3D20\\\",\\\" \\\");s/(
=3DC4|=3DD6|=3DDC|=3DE4|=3DF6|=3DFC|=3DDF|=3D20)/\\$convert{\\$1}/g;print\" " t)
      (goto-char (point-min))
      (replace-string "=\n" "\n")
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




;;;--------------------------------------------------------------------------------



(defun trim-buffer ()
  (interactive)
  (beginning-of-line)
  (save-excursion
    (let ((selective-display selective-display)(before-change-functions nil)(after-change-functions nil))
      (setq selective-display nil)
      (goto-char (point-min))
;;; Wenn kein Column-Mode-Zeichen: Tabify für alles
      (if (not (re-search-forward "%%%C" nil t))
	  (tabify (point-min) (point-max))
;;; wenn doch: Tabify nur dort, wo kein Column-mode ist
	(let ((endmark) (tmp))
	  (tabify (point-min) (point))
	  (while (this-is-column-mode 'endmark)
	    (goto-char endmark)
	    (forward-char 2)
	    (setq tmp (point))
	    (if (re-search-forward "%%%C" nil t)
		(tabify tmp (point))))
	  (tabify (point) (point-max))))
      (goto-char (point-min))
;;; Ersetze alle Leerzeichen am Ende einer Zeile durch nichts
      (replace-regexp "[ \t]+\r" "\r")
      (replace-regexp "[ \t]+$" ""))))


(defun reset-display ()
  (interactive)
  (widen)
  (setq selective-display t)
  (if window-system
      (hilit-repaint-command t)))


;; zur Kontrolle
(message "12")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun op-shift-right (arg)
  (interactive "P")
  (let ((before-change-functions nil)(after-change-functions nil))
  (if (not arg) (setq arg 1))
  (save-excursion
    (narrow-to-region (region-beginning) (region-end))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "\r" nil t)
	(replace-match "\n" nil t)))
    (goto-char (point-min))
    ;;
    ;; Erst Leaves indentieren
    ;;
    (goto-char (point-min))
    (while (re-search-forward "^  " nil t)
      (replace-match "" nil nil))
    (goto-char (point-min))
    (indent-code-rigidly (point-min) (point-max) (* arg tab-width) "^[*#]")
    (goto-char (point-min))
    ;;
    ;; dann die Überschriften
    ;;
    (let ((before-change-functions nil)(after-change-functions nil))
      (insert-char ?\n 1)
      (backward-char 1)
      (while (outline-next-heading)
	(insert-char ?* arg)
	(re-search-forward "[ \t]+" nil t)
	(insert-char ?\t arg))
      (goto-char (point-min))
      (delete-char 1 nil))
    (hide-ifdefs)
    (widen)
    (end-of-show))))
(define-project-mode-key ">" 'op-shift-right)


(defun op-shift-left (arg)
  (interactive "P")
  (let ((before-change-functions nil)(after-change-functions nil))
  (if (not arg) (setq arg 1))
  (let ((before-change-functions nil)(after-change-functions nil))
    (save-excursion
      (narrow-to-region (region-beginning) (region-end))
      (save-excursion
	(goto-char (point-min))
	(while (search-forward "\r" nil t)
	  (replace-match "\n" nil t)))
      (goto-char (point-min))
      ;;
      ;; Erst Leaves indentieren
      ;;
      (indent-code-rigidly (point-min) (point-max) (* arg tab-width -1) "^[^ \t]")
      (indent-code-rigidly (point-min) (point-max) 2 "\\*\\|^[ \t]\\|#")

      (goto-char (point-min))
      ;;
      ;; dann die Überschriften
      ;;
      (insert-char ?\n 1)
      (backward-char 1)
      (while (outline-next-heading)
	(beginning-of-line)
	(delete-char arg nil)
	  
	;; falls eine Einer-Überschrift
	(if (looking-at "\\*[^*]")
	    (if (re-search-forward "\t" (save-excursion (end-of-line) (point)) t)
		(replace-match " " nil nil))
	  (re-search-forward "[ \t]+" nil t)
	  (delete-char (* arg -1) nil)))
      (goto-char (point-min))
      (delete-char 1 nil))
    (hide-ifdefs)
    (widen)
    (end-of-show))))
(define-project-mode-key "<" 'op-shift-left)


; --------------------------------------------------------------------------------
; --------------------------------------------------------------------------------
;; zur Kontrolle
(message "13")



;;;----------------------------------------------------------------------------------------------------
;;;----------------------------------------------------------------------------------------------------
;;;----------------------------------------------------------------------------------------------------



(define-key (current-local-map) "\C-?" 'backward-delete-char-untabify) ; Backspace
(define-key outline-minor-mode-map "\C-?" 'backward-delete-char-untabify) ; Backspace



;; und hier noch etwas vom org-mode, wenn vorhanden
;;
(if (fboundp 'org-cycle)
    (define-key (current-local-map) [(control tab)] 'org-cycle))
(if (fboundp 'org-mode)
    (setq org-cycle-emulate-tab nil))




;; zur Kontrolle
(message "14")


;;;----------------------------------------------------------------------------------------------------
;;;----------------------------------------------------------------------------------------------------
;;;----------------------------------------------------------------------------------------------------



;
; Define Help-Key
;
(defun op-help ()
  (interactive) 
  ;; außer bei meiner eigenen Entwicklungsumgebung sollte der .doc-File in /site-lisp/op-project stehen
  (if (file-readable-p (expand-file-name (concat data-directory "../../site-lisp/op-project/op-project.doc")))
      (progn
	(find-file-other-window (expand-file-name (concat data-directory "../../site-lisp/op-project/op-project.doc")))
	(toggle-read-only))
    (if (file-readable-p "~/op-project/op-project.doc")
	(progn
	  (find-file-other-window "~/op-project/op-project.doc") 
	  (toggle-read-only))
      (message "op-project.doc not found !"))))
(define-project-mode-key "\C-hm" 'op-help)



;;;----------------------------------------------------------------------------------------------------


;; zur Kontrolle
(message "15")


;
; hilit-mode
;
(if window-system
    (if (not (x-display-color-p))
	(defvar hilit-background-mode 'mono
	  "'mono inhibits color, 'dark or 'light indicate the background brightness.")))



(if gnuemacsp
    (if window-system
	(if (x-display-color-p)
	    (require 'hilit19.variation))))


(if gnuemacsp
    (progn
      (setq hilit-inhibit-rebinding t) ; wenn der Server nur wenige Farben hat, geht sonst manchmal Yank nicht mehr
      ;;(setq hilit-face-check nil) ; Fonds werden ja meistens nicht extra geändert, sonst hilft auch noch C-u S-C-l
      (setq hilit-auto-rehighlight 10000) ; Anzahl der automatisch upgedateten Zeilen bei Benutzung des X-Servers
))

; variable major-mode gives current major mode



(message "15a")

;; sollte beim GNU emacs bereits hier kommen, beim XEmacs jedoch erst am Ende
(if window-system
  (if gnuemacsp
      (progn
;;;;;;;includeforelc!
(load "op-hilit.gnuemacs")
	)))


;; zur Kontrolle
(message "16")



;;; Zusammenfalten am Anfang -
;;; kommt beim XEmacs wegen des anderen Hiliting-Systems erst zum Schluss
;;;
(if gnuemacsp                
  (if (not (string-match "op-project" (file-name-nondirectory buffer-file-name)))        
      (progn        
         (goto-char (point-min))
         (if (re-search-forward "^\\*" nil t)
            (progn
               (beginning-of-line)
               (hide-all))))))




(message "16a")


;;;--------------------------------------------------------------------------------
;;;--------------------------------------------------------------------------------
;;;--------------------------------------------------------------------------------

;;
;;
;; kommt noch Mal bei op-menus.xemacs.el für die Menüs!!
;;
(local-set-key "\C-c\C-h" 'op-insert-signature)
(local-set-key "\C-c\C-j" 'insert-date-in-brackets)
(local-set-key "\C-c\C-k" 'insert-date)

(local-set-key "\C-c\C-t" '(lambda() (interactive)(insert "Tel.: ")))
(local-set-key "\C-c\C-f" '(lambda() (interactive)(insert "Fax:  ")))
(local-set-key "\C-c\C-a" '(lambda() (interactive)
			     (let ((calendar-date-display-form  '(day "." month "." year)))
			       (insert (concat "==> Anruf am " (calendar-date-string (calendar-current-date)) " bei ")))))
(local-set-key "\C-c\C-e" '(lambda() (interactive)
			     (insert (concat "==> erledigt am " (calendar-date-string (calendar-current-date))))))





;;; hier sollte eigentlich stehen:
;;;(load "op-calendar/op-calendar")
;;; is' aber nich, weil die Idio... den Message-Buffer löschen (warum auch immer)

;;;;;;;includeforelc!
(load "op-appt-changes")

(message "16b")

;;; (defvar signature "SMW"
;;; "Signature-String you insert with set-signature")
(defvar signature "<Variable \"signature\" ist nicht definiert>"

"Signature-String you insert with set-signature")


(message "16c")



(message "Loading users init file from op-project.el")
;;; hat der benutzer einen persönlichen Ini-File, dann führe diesen aus, im anderen Fall nehme einen aus dem
;;; allgemeinen Ladepfad
;;;
;;; steht .op-ini.el in ~ ?
(condition-case nil
    (if (file-readable-p "~/.op-ini.el")
        (progn
          (load "~/.op-ini.el")
          (message "op-ini.el aus ~/.op-ini.el genommen"))
      ;; oder op-ini.el in ~/op-project ?
      (if (file-readable-p "~/op-project/op-ini.el")
          (progn
            (load "~/op-project/op-ini.el")
            (message "op-ini.el aus ~/op-project/op-ini.el genommen"))
        ;; sonst nehme den aus dem Pfad
        (if (load "op-ini.el")
	    (message "op-ini.el aus Default-Pfad genommen")
          (message "no user init file found"))))
  (error "Fehler im persönlichen Initialisierungsfile [.]op-ini.el ! "))




;; zur Kontrolle
(message "17")
(message "emacs lädt den project-mode ... ")

;; reparieren des diary-files, wenn dieser nach einem Absturz korrumpiert vorhanden ist
(if (not (string= diary-file ""))
    (if (file-exists-p diary-file)
	(if (file-readable-p diary-file)
	    (save-excursion
	      (find-file diary-file)
	      (if (and (file-exists-p (make-auto-save-file-name))
		       (not (file-newer-than-file-p diary-file (make-auto-save-file-name))))
		  (progn
		    (recover-file diary-file)
		    (message "diary-file   %s   wurde automatisch wiederhergestellt !" diary-file)))
	      (bury-buffer)))))

(message "17a")

;;;;;;;includeforelc!
(load "op-schedule")

(message "17b")

;;;--------------------------------------------------------------------------------


(defun op-insert-signature ()
  (interactive)
  (let ((before-change-functions nil)(after-change-functions nil))
  (let ((calendar-date-display-form  '(day "." month "." year)))
    (insert (concat "[" signature "," (calendar-date-string (calendar-current-date)) "," (substring (current-time-string) 11 16) "]")))))

(defun insert-date-in-brackets ()
  (interactive)
  (let ((before-change-functions nil)(after-change-functions nil))
  (let ((calendar-date-display-form  '(day "." month "." year)))
    (insert (concat "[" signature "," (calendar-date-string (calendar-current-date)) "]")))))

(defun insert-date ()
  (interactive)
  (let ((before-change-functions nil)(after-change-functions nil))
  (let ((calendar-date-display-form  '(day "." month "." year)))
    (insert (calendar-date-string (calendar-current-date))))))



;;;--------------------------------------------------------------------------------

;; und zum Schluß noch:
;; Meldung, wenn ein auto-save-file existiert
;; (gehoert thematisch eigentlich in op-crypt.el) 
(if (boundp 'use-keymap-for-crypted-files)
    nil
  (defvar use-keymap-for-crypted-files nil)
  (make-variable-buffer-local 'use-keymap-for-crypted-files))

(if (or (and use-keymap-for-crypted-files
	     (file-exists-p (concat (substring (make-auto-save-file-name) 0 -1) ".crypt#")))
	(and (not use-keymap-for-crypted-files)
	     (file-exists-p (make-auto-save-file-name))))
    (if use-keymap-for-crypted-files
	(progn
	  (ding)
	  (message "%s has auto save data; consider M-x recover-op-file"
		  (file-name-nondirectory buffer-file-name))
	  (sleep-for 3))
      (ding)
      (message "%s has auto save data; consider M-x recover-op-file"
	      (file-name-nondirectory buffer-file-name))
      (sleep-for 3)))


;; zur Kontrolle
(message "18")

;;
;; 
;; sollte beim GNU emacs bereits hier kommen, beim XEmacs jedoch erst am Ende
(if gnuemacsp
    (progn
;;;;;;;includeforelc!
      (load "op-menus.gnuemacs")
      ))
(if xemacsp
    (when (featurep 'menubar)
      (progn
;;;;;;;includeforelc!
(load "op-menus.xemacs")
	)))



(message "19")

;; das am längsten andauernde let in diesem Programm
(if buffer-was-read-only
    (toggle-read-only t))

) ; ende (let

(message "20")
;;;
;;; Resetten des Displays
;;;


(setq selective-display t)


;;;
;;; kommt beim XEmacs erst hier, da das Hiliting anders ist als beim GNU-Emacs
;;;
;;; hiliting
(if xemacsp
  (if window-system
      (progn
;;;;;;;includeforelc!
(load "op-hilit.xemacs")
)))


;;
;; bis hierher läufts
;;
(message (concat "... und ist im Buffer " (prin1-to-string (current-buffer))))



;;;
;;; zusammenfalten
(if (not (string-match "op-project" (file-name-nondirectory buffer-file-name)))
    (progn
      ;;
      ;; Mini-BUG im Programm, wenn es im XEmacs läuft:
      ;; klappt warum auch immer nicht - lasse ich jetzt sein.
      ;;
      (goto-char (point-min))
      (if xemacsp
	  (hilit-repaint-command-interactive)
	(hilit-repaint-command))
      (hide-other-hide)
      (goto-char (point-min))
      (if (re-search-forward "^\\*" nil t)
	  (progn
	    (beginning-of-line)))))



(message "... und fertig mit dem Project-mode")
