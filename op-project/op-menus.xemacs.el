;;;;;
;;;;;
;;;;;
;;;;; Makro
;;;;; "C-s define- key C-a 2*<C-k> TAB 2*<C-d> [ C-s \" SPC . C-SPC 3*<backspace> <right> [ <backspace> ( C-e <backspace> ]"))
;;;;;
;;;;(if xemacsp
;;;;    (defvar op-mode-menu-bar-map (make-keymap))
;;;;  (defvar op-mode-menu-bar-map (make-sparse-keymap)))
;;;;;
;;;;; Eintragen in vorhandene Menüs
;;;;;
;;;;; gefährlich !!
;;;;;
;;;;; Eintragen in vorhandene Menüs
;;;;;
;;;;; bei gecrypteten Files: nehme auch die .crypt-Funktionen in die richtigen Menus auf
;;;;(add-submenu '("Edit") '("Save Region Cryptet As..." 
;;;;			  ["Save Test" (write-region-crypted)]))
;;;;
;;;;;; XEmacs menu.
;;;;(defvar tcl-xemacs-menu
;;;;  '(["Beginning of function" tcl-beginning-of-defun t]
;;;;    ["End of function" tcl-end-of-defun t]
;;;;    ["Mark function" tcl-mark-defun t]
;;;;    ["Indent region" indent-region (tcl-mark)]
;;;;    ["Comment region" comment-region (tcl-mark)]
;;;;    ["Uncomment region" tcl-uncomment-region (tcl-mark)]
;;;;    "----"
;;;;    ["Show Tcl process buffer" inferior-tcl t]
;;;;    ["Send function to Tcl process" tcl-eval-defun
;;;;     (and inferior-tcl-buffer (get-buffer inferior-tcl-buffer))]
;;;;    ["Send region to Tcl process" tcl-eval-region
;;;;     (and inferior-tcl-buffer (get-buffer inferior-tcl-buffer))]
;;;;    ["Send file to Tcl process" tcl-load-file
;;;;     (and inferior-tcl-buffer (get-buffer inferior-tcl-buffer))]
;;;;    ["Restart Tcl process with file" tcl-restart-with-file t]
;;;;    "----"
;;;;    ["Tcl help" tcl-help-on-word tcl-help-directory-list]
;;;;    ["Send bug report" tcl-submit-bug-report t])
;;;;  "XEmacs menu for Tcl mode.")
;;;;
;;;;;; Emacs does menus via keymaps.  Do it in a function in case we
;;;;;; later decide to add it to inferior Tcl mode as well.
;;;;(defun tcl-add-emacs-menu (map)
;;;;  (define-key map [menu-bar] (make-sparse-keymap "Tcl"))
;;;;  ;; This fails in Emacs 19.22 and earlier.
;;;;  (require 'lmenu)
;;;;  (let ((menu (make-lucid-menu-keymap "Tcl" tcl-xemacs-menu)))
;;;;    (define-key map [menu-bar tcl] (cons "Tcl" menu))
;;;;    ;; The following is intended to compute the key sequence
;;;;    ;; information for the menu.  It doesn't work.
;;;;    (x-popup-menu nil menu)))



;;;==========================================================================================
;;;
;;; die mode map soll buffer local sein, d.h. die Menüs sollen nor für den Project Mode gelten
;;;

(set-buffer-menubar (copy-sequence current-menubar))

;
;;;==========================================================================================
(setq menubar-show-keybindings t)



(add-menu-button '("File") ["Save Region Cryptet As..." (write-region-crypted)])

(add-menu-button '("File") ["Save As..." 
			    (lambda()(interactive)
			      (if use-keymap-for-crypted-files 
				  (ask-for-crypt-write)
				(write-file buffer-file-name)))])
(add-menu-button '("File") ["Save Current Buffer"
			  (lambda()(interactive)
			    (if use-keymap-for-crypted-files 
				(save-buffer-crypted)
			      (save-buffer-with-control)))])
(add-menu-button '("Help") ["[OP] Outline-Project-Mode: Hilfe" (op-help)])

;; bis hierher

(add-menu-button '("Edit") ["Backward Current Word" (search-this-string-backward) t])
(add-menu-button '("Edit") ["Forward Current Word"  (search-this-string-forward) t])
(add-menu-button '("Edit") ["Transpose Words" (transpose-words) t])
(add-menu-button '("Edit") ["Transpose Chars"  (transpose-chars) t])


;;; Muß nicht sein, hilft aber manchmal
;--------------------
(defvar outline
;      (cons "OP-Outline" (make-keymap "OUTLINE"))
;    (cons "OP-Outline" (make-sparse-keymap "OUTLINE"))))
	'(	   ["Outline Hide Other" hide-other t]
		   ["Outline Hide Sublevels" hide-sublevels t]
		   ["Outline Hide Subtree" hide-subtree t]
		   ["Outline Hide Entry" hide-entry t]
		   ["Outline Hide Body" hide-body t]
		   ["Outline Hide Leaves" hide-leaves t]
		   "----"
		   ["Outline Show Subtree" show-subtree t]
		   ["Outline Show Children" show-children t]
		   ["Outline Show Branches" show-branches t]
		   ["Outline Show Entry" show-entry t]
		   ["Outline Show All" show-all t]
		   "----"
		   ["Outline Toggle Children" outline-toggle-children t]
		   "----"
		   ["Outline Previous Same Level" outline-backward-same-level t]
		   ["Outline Next Same Level" outline-forward-same-level t]
		   ["Outline Previous Visible Heading" outline-previous-visible-heading t]
		   ["Outline Next Visible Heading" outline-next-visible-heading t]
		   ["Outline Up Heading" outline-up-heading t]
		   "----"
		   ["Mark Subtree" outline-mark-subtree t]
		   ["Move Subtree Up" outline-move-subtree-up t]
		   ["Move Subtree Down" outline-move-subtree-down t]
		   ["Outline Promote" outline-promote t]
		   ["Outline Demote" outline-demote t]
		   ["Outline Insert Heading" outline-insert-heading t]
		   "----"
		   ["Outline Blank Lines Mode Toggle" outline-blank-line-toggle t]
		   ))

		   


;--------------------

(defvar op-conv
;      (cons "OP-CONV" (make-keymap "OP-CONV"))
;    (cons "OP-CONV" (make-sparse-keymap "OP-CONV"))))
	'(	   ["Import all new Dia XML text tags as headline" dia-inserter t]
		   ["Import all new Dia XML text tags as headline from cursor position" dia-inserter-from-file t]
		   ["Prepare Dia XML synchronisation " dia-ID-synchronisator t]
		   ["Forward to next <dia id=...> tag" next-dia-tag t]
		   ["Backward to previous <dia id=...> tag" previous-dia-tag t]
		   "----"
		   ["MPX-File von Cursorposition Outline-Form mit Kommentaren	[neg. Argument]"
		    ((lambda ()(interactive)(mpx-text-extractor -1))) t]
		   ["MPX-File von Cursorposition Outline-Form ohne Kommentare	[\"-\"-Argument]" 
		    ((lambda ()(interactive)(mpx-text-extractor '-))) t]
		   ["MPX-File von Cursorposition mit Kommentaren	[pos. Argument]" 
		    ((lambda ()(interactive)(mpx-text-extractor 1))) t]
		   ["MPX-File von Cursorposition extrahieren" mpx-text-extractor t]
		   ["MPX-File importeren " mpx-import t]
		   "----"
		   ["csv-File in extra Fenster importieren" csv-importer t]
		   "----"
		   ["xfig-Text von Cursorposition extrahieren" xfig-text-extractor t]
		   ["xfig-Text in extra Fenster importieren " xfig-importer t]
		   "----"
		   ["Import this BrainForest file	(M-x from_BF)" from_BF t]
		   ["Export this OP file to BrainForest	(M-x to_BF)"   to_BF t]
		   ["Corell EPS-File von Cursorposition extrahieren" corell-text-extractor t]
		   ))


;--------------------

(defvar op-search
;      (cons "OP-SEARCH" (make-keymap "OP-SEARCH"))
;    (cons "OP-SEARCH" (make-sparse-keymap "OP-SEARCH"))))
	'(	   ["Isearch Forward Only In Headings" isearch-forward-in-headings t]
		   ["Isearch Backward Only In Headings" isearch-backward-in-headings t]
		   "----"
		   ["Forward To Current .Word." ((lambda()(interactive)(search-forward-for-current-word t))) t]
		   ["Backward To Current .Word." ((lambda()(interactive)(search-backward-for-current-word t))) t]
		   "----"
		   ["Forward Word Expand To Visible" search-word-forward-expand-to-visible t]
		   ["Backward Word Expand To Visible" search-word-backward-expand-to-visible t]
		   ["Forward Current Word" search-this-string-forward t]
		   ["Backward Current Word" search-this-string-backward t]
		   "----"
		   ["List All Found Headlines" outlines-show t]
		   "----"
		   ["Toggle 'isearch': Only Outline Visible Or Searchable   [C-o in isearch]" isearch-toggle-outlines t]
		   ))


;--------------------

(defvar op-include
;      (cons "OP-INCLUDE" (make-keymap "OP-INCLUDE"))
;    (cons "OP-INCLUDE" (make-sparse-keymap "OP-INCLUDE"))))
	'(	   ["Insert Files Instead Of #include" insert-file-at-include-statement t]
		   "----"
		   ["Save File And Remove From #include" extract-and-save-includes t]
		   ["Load Files at #include" load-files-at-include-statements t]
		   ["Save File From This #include And Remove It" extract-and-save-this-include t]
		   ["Load File Of This #include" load-file-at-include-statement t]
		   "----"
		   ["Write Region To File + Insert \"#include\" + mh-Mail" kill-region-insert-include-and-mail t]
		   ["Write Region To File And Insert \"#include\"" kill-region-insert-include t]
		   ["Write Region To File" write-region-crypted t]
		   ["Write Buffer To File" ask-for-crypt-write t]
		   "----"
		   "----"
		   ["Insert Picture As Glyph (temporarily only)" insert-image-as-glyph--temporarily t]
		   ))

;--------------------

(defvar op-cvs
;      (cons "OP-CVS" (make-keymap "OP-CVS"))
;    (cons "OP-CVS" (make-sparse-keymap "OP-CVS"))
	'(	   ["Emacs: cvs commit" cvs-commit t]
		   ["Emacs: cvs update" cvs-update t]
		   ["Emacs: cvs examine (test for update)" cvs-examine t]
		   ["Emacs: cvs checkout" cvs-checkout t]
		   ["Emacs: Load CVS Mode" ('(lambda ()(interactive)(load "pcl-cvs")(cvs-mode)))]
		   "----"
		   ["Save Stripped File To CVS And To You" write-this-include-for-cvs t]
		   ["Save All Stripped File To CVS And To You" write-includes-for-cvs t]
		   ["Load File From CVS And Merge With Yours" load-this-include-for-cvs t]
		   ["Load All Files From CVS And Merge With Yours" load-includes-for-cvs t]
		   "----"
		   ["Load All Files From CVS And Merge With Yours" ediff-.op-to-.cvs.op-file t]
		   ["Make .cvs.op Copy From Current File" make-.cvs.op-copy-from-current t]
		   ))
;--------------------

(defvar op-ifdef
	'(	   ["Hide Enclosing Ifdef Blocks This Line" hide-ifdef-block t]
		   ["Show Enclosing Ifdef Blocks This Line" show-ifdef-block t]
		   "----"
		   ["Down Nested Visible Ifdef" 
		    ((lambda()(interactive)(down-ifdef)(search-forward "###" nil t)))]
		   ["Up Nested Visible Ifdef" 
		    ((lambda()(interactive)(up-ifdef)(search-forward "###" nil t)))]
		   ["Beginning of Previous Visible Block This Level" 
		    ((lambda()(interactive)(backward-ifdef)(search-forward "###" nil t)))]
		   ["End Of Next Visible Block This Level" 
		    ((lambda()(interactive)(forward-ifdef)(search-forward "###" nil t)))]
		   ["Previous Ifdef Statement" 
		    ((lambda()(interactive)(previous-ifdef)(search-forward "###" nil t)))]
		   ["Next Ifdef Statement" 
		    ((lambda()(interactive)(next-ifdef)(search-forward "###" nil t)))]
;		   "----"
;		   ["List Global Ifdef List" ((lambda()(interactive)(prin1 hide-ifdef-define-alist)))]
;		   ["Set Ifdef Defined List" hide-ifdef-use-define-alist t]
;		   ["Set Associated Name List" hide-ifdef-set-define-alist t]
		   "----"
		   ["Show Ifdef Variable List"((lambda()(interactive)(prin1 hide-ifdef-env)))]
		   ["Set Ifdef Variable To Hide" hide-ifdef-undef t]
		   ["Set Ifdef Variable To Show" hide-ifdef-define t]
		   "----"
		   ["Hide Nondefined Blocks In Region" hide-ifdefs-in-region t]
		   ["Show Ifdef Blocks In Region" show-ifdefs-in-region t]
		   ["Hide Nondefined Ifdef Blocks (default)" hide-ifdefs t]
		   ["Show All Ifdef Blocks" show-ifdefs t]
		   "----"
		   ["Insert Ifdef Begin" insert-ifdef-begin t]
		   ["Insert Elseif" 
		    ((lambda()(interactive) (insert "  ###dagegen jedoch nicht\n")(backward-char 1)))]
		   ["Insert Ifdef End" 
		    ((lambda()(interactive) (insert "  ###bis hierher\n")(backward-char 1)))]
		   ))

(defun insert-ifdef-begin ()
  (interactive)
  ;(insert "\n")
  (insert 
   (concat "  ### " 
	   (read-string "what") 
	   "\n"
	   ))
  (backward-char 1))

;--------------------

(defvar op-misc
	'(	   ["OP Picture-Mode  	p" p t]
		   ["OP Vollkalender"      ((lambda()(interactive)(project-schedule nil nil t)))]
		   ["OP Projektzeitplan"   project-schedule t]
		   ["OP Kalenderplaner 	d" d t]
		   ["OP Kalender in einem extra Fenster" calendar-in-extra-window t]
		   ["OP Kalender       	c" c t]
		   "----"
		   ["Automatic Change Mode OFF (for links only)" 
		    ((lambda()(interactive)(automatic-change-mode t)(automatic-change-mode)))]
		   ["Automatic Change Mode ON  (for links only)" 
		    ((lambda()(interactive)(automatic-change-mode t)))]
		   "----"
		   ["Eval Hypermark" select-hyper-func t]
		   ["Hypermark Description" op-hyperlink-help t]
		   "----"
		   ["Reduce To One Whitespace" fixup-whitespace t]
		   ["Insert TAB" tab-to-tab-stop t]
		   "----"
		   ["Format Region Softly (with included files)" op-softindent-region t]
		   ["Format Region OP-Mode Like" op-indent-region t]
		   ["Force Indent Line" force-indent t]
		   ["Standard Indent" re-indent t]
		   ["Fill Paragraph in Region" op-fill-paragraph-region t]
		   ["Report Generator on Region" make-report t]
		   "----"
		   ["Trim Whitespaces" trim-buffer t]
		   ["Where Am I - Column Ruler" column-ruler t]
		   ["Recolor Display" hilit-repaint-command-interactive t]
		   ["Recolor Display in Region" hilit-repaint-command-region t]
		   ["Reset Display" reset-display t]
		   "----"
		   ["Logbuch-Alarm aus" logbuch-alarm-off t]
		   ["Logbuch-Abfrage an" logbuch-alarm-on t]
		   ["Logbuch-Mode" logbuch-mode t]
		   ["Logbuch-Eintrag generieren" logbuch t]
		   "----"
		   ["Region drucken / Print" ps-print-region-with-faces t]
		   ["Buffer drucken / Print" ps-print-buffer-with-faces t]
		   "----"
		   ["Unterschrift einfügen" op-insert-signature t]
		   ["Datum in eckigen Klammern einfügen" insert-date-in-brackets t]
		   ["Datum einfügen" insert-date t]
		   ["Telefon einfügen  	[C-c C-t]" (lambda() (interactive)(insert "Tel.: "))    t]
		   ["Fax einfügen      	[C-c C-f]" (lambda() (interactive)(insert "Fax:  "))    t]
		   ["Anruf einfügen    	[C-c C-a]" (lambda() (interactive)
						     (let ((calendar-date-display-form  '(day "." month "." year)))
						     (insert (concat "==> Anruf am " 
								     (calendar-date-string (calendar-current-date)) 
								  " bei "))))    t]
		   ["Erledigt einfügen 	[C-c C-e]" '(lambda() (interactive)
						      (insert (concat "==> erledigt am " 
								      (calendar-date-string 
								 (calendar-current-date)))))    t]
		   ))

;--------------------

(defvar op-headings
	'(         ["Forward One Level Less" outline-forward-jump-same-level t]
		   ["Backward One Level Less" outline-backward-jump-same-level t]
		   ["Previous Same Level" outline-backward-same-level t]
		   ["Next Same Level" outline-forward-same-level t]
		   ["Previous" outline-previous-visible-heading t]
		   ["Next" outline-next-visible-heading t]
		   ["Up" outline-up-heading t]
		   ["Top" top-heading t]
		   ["Next Top" next-top-heading t]
		   ["Very Top" very-top-heading t]
		   "----"
		   ["Presentation Walk Forward" presentation-walk-forward t]
		   ["Presentation Walk Backward" presentation-walk-backward t]
		   "----"
		   "----"
		   ["Headline Indent [C-u: +1, C-u C-u: -1]" indent-like-last-headline t]
		   "----"
		   ["Shift Region Down" op-shift-right t]
		   ["Shift Region Up" op-shift-left t]
		   "----"
		   ["Set Show All Toggles In Region" set-show-ausblend-all t]
		   ["Toggle *E[rledigtes] / *F[ällt]W[eg] in region" ausblend-1 t]
		   ["Toggle *S[tändiges] / *W[iederkehrendes] in region" ausblend-2 t]
		   ["Toggle *V[erschobenes] in region" ausblend-3 t]
		   ["Toggle *I[n]B[earbeitung] in region" ausblend-4 t]
		   ["Toggle *T[ermin] in region" ausblend-6 t]
		   ["Toggle *R[essourcen] in region" ausblend-5 t]
		   ["Markierung   *D   für  D[ringendes] \t(ohne Funktion)" nil t]
		   "----"
		   ["List All Found Headlines" outlines-show t]
		   ["Outline Blank Lines Mode Toggle" outline-blank-line-toggle t]
		   ))

;--------------------

(defvar op-fold
	'(	   ["Hide All" hide-all t]
		   ["Hide All Others" hide-other-hide t]
		   ["Hide Most Others" hide-other-more t]
		   ["Hide Subtree" hide-subtree-hide t]
		   ["Only Headings" hide-body t]
		   ["Hide Leaves" hide-leaves-hide t]
		   ["Hide Entry" hide-entry-hide t]
		   "----"
		   ["Sublevels Till Level [ARG(=C-u)] //on region/on headline" hide-sublevels-hide t]
		   "----"
		   ["Show Everything In Buffer" show-really-everything t]
		   ["Show Everything In Region" show-really-everything-in-region t]
		   ["Show All" show-all-hide t]
		   ["Show All Branches" show-all-branches-hide t]
		   ["Show Subtree" show-subtree-hide t]
		   ["Show Branches" show-branches-hide t]
		   ["Show Children" show-children-hide t]
		   ["Show Entry" show-entry-hide t]
		   ["Show Current Place" show-current-place t]
		   ))

;--------------------

(defvar op-invisible
	'(	   ["Hide Region Invisible and Unsearchable" region-invisible-unsearchable t]
		   ["Hide Region Invisible and Searchable"   region-invisible-searchable t]
		   ["Make Current Position Visible" current-position-visible t]
		   ["Make Current Position Visible within isearch 	C-j" current-position-visible t]
		   ["Show All Invisible Parts" show-invisible t]
		   ["Show Everything" show-really-everything t]
		   ["Show Everything in Region" show-really-everything-in-region t]
		   ["Toggle Tuncate Lines 	C-x t" truncate-lines t]
		   ))


;--------------------




;(define-key (current-local-map) [menu-bar] op-mode-menu-bar-map)


;; kann man, muss man aber nicht...
(load "big-menubar")
;;; lösche sinnlose Menüs vom big-menubar Mode
(delete-menu-item '("Top"))
(delete-menu-item '("Bot"))
(delete-menu-item '(" | "))
(delete-menu-item '("<<"))
(delete-menu-item '(">>"))


(add-menu-button nil ["Open%_All" show-really-everything])

;; Update von allen Menu-Bars
(set-menubar-dirty-flag)


(add-menu nil "Out%_line" outline) ;L
(add-menu nil "OP-%_Fold" op-fold) ;F
(add-menu nil "OP-%_Headings" op-headings) ;H
(add-menu nil "OP-%_Search" op-search) ;S
(add-menu nil "OP-%_Invisible" op-invisible) ;I
(add-menu nil "OP%_-Misc" op-misc) ;M
(add-menu nil "OP-I%_nclude" op-include) ;N
(add-menu nil "OP-If%_Def" op-ifdef) ;D
(add-menu nil "OP-%_CVS" op-cvs) ;C
(add-menu nil "OP-Con%_v" op-conv) ;V
                                   ; OASFL-HIMNDCV

;;; Menüs vom outline Mode
(delete-menu-item '("Headings"))
(delete-menu-item '("Show"))
(delete-menu-item '("Hide"))


;;*****************************************************************************


(add-menu-button '("Help") ["[OP] Hyperlink Hilfe" (op-hyperlink-help)])
(add-menu-button '("Help") ["[OP] Hyperlink Hilfe" (op-hyperlink-help)])














(defun op-hyperlink-help ()
  (interactive)
  (switch-to-buffer "*op-hyperlink help*")
  (setq truncate-lines t)
  (insert-string "

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Kettenausführungen, genaueres siehe unter den Beispielen von EVAL
;   ->FILE:op-hyper.el*defun;EVAL:date
;   ->FILE:op-hyper.el*defun->;EVAL:date
;
;;;;;;;;;;;;;;;; Beispielliste aus den Kommentaren (z.T. erweitert) ;;;
;;
;; Beispiele SQL:
;;  
;   ->SQL:sqlfile.sql*startpunkt
;   ->SQL:sqlfile2.sql
;
;; ->>../op-project.el*defun
;; Beispiele FILE:
;; 
;;   ->>Wort sucht alle vorkommenden ganzen Wörter, ->Wort dieses nur in Überschriften
;;                                                    {(AUßER DIESE EXISTIERT NICHT, dann wird der Pfeil wie ein 
;;                                                     ->> behandelt !! (Grund: bessere Optik im File))
;;                                                     Dieses Verhalten, beschrieben hier in der
;;                                                     geschweifte Klammer kann durch vertauschen der
;;                                                     Funktionen FILE:func und FILE:func2 geändert werden, siehe
;;                                                     Kommentar über FILE:func2.}
;;   ->*Wort dieses nur am Anfang einer Einser-Überschrift, entsprechend ->**Wort bei einer Zweier-Überschrift
;;   ->Name* sucht in File \"Name\"
;; 
;   ->>FILE:../op-project.el*defun     ; mehrmals auslösen gibt jeweils das nächste Muster
;   ->>FILE:../op-project.el*defun->
;   ->>FILE:defun
;   ->>FILE:defun->
;   ->>FILE:../op-project.el*          ; Sucht nur den anderen File aus
;   ->>defun                           ; Defaultbutton: sucht im gleichen File das Wort \"defun\"
;   ->>defun                           ; Defaultbutton: sucht im gleichen File das Wort \"defun\"
;   ->>defun->                         ; dito, setzt den Cursor aber in ein anderes Fenster
;   ->FILE:../op-project.el*defun      ; mehrmals auslösen gibt jeweils das nächste Muster
;   ->FILE:../op-project.el*defun->
;   ->FILE:defun
;   ->FILE:defun->
;   ->FILE:../op-project.el*           ; Sucht nur den anderen File aus
;   ->defun                            ; Defaultbutton: sucht im gleichen File das Wort \"defun\"
;   ->defun                            ; Defaultbutton: sucht im gleichen File das Wort \"defun\"
;   ->defun->                          ; dito, setzt den Cursor aber in ein anderes Fenster
;   ->>../op-project.el*               ; sucht den File ../op-project.el heraus
;   ->../op-project.el*                ; sucht den File ../op-project heraus (-> geht hier nur, weil kein Einspring-
;                                      ;           punkt vorhanden ist, eigentlich muesste hier ->> stehen
;
;   ->../op-project.el                 ; doppelt verkürzte Form: wie oben, jedoch kann auch noch der Stern
;                                      ; weggelassen werden, da hier der File über den / erkannt wird
;   ->../op-project.el->               ; doppelt verkürzte Form: wie oben, jedoch kann auch noch der Stern
;                                      ; weggelassen werden, da hier der File über den / erkannt wird
;   ->op-consistent.el                 ; doppelt verkürzte Form: wie oben, jedoch kann auch noch der Stern
;                                      ; weggelassen werden, da hier der File über den . und Buchstabe 
;                                      ; als Verweis auf eine Extention gesucht wird
;   ->op-consistent.el->               ; doppelt verkürzte Form: wie oben, jedoch kann auch noch der Stern
;                                      ; weggelassen werden, da hier der File über den . und Buchstabe 
;                                      ; als Verweis auf eine Extention gesucht wird
;
;   ->>../op-project.el*defun->        ; in anderem File
;   ->>FILE:[ab]test                   ; alle Buttons dürfen Regexe enthalten, jedoch keine Blanks
;        atest btest ctest             ;    (bleibt bei atest und btest stehen, kennt aber kein ctest)
;   ->>FILE:\"a test\"->                 ; Blanks usw. sind erlaubt, doch nur in Anführungsstrichen  (\"a test\")
;   ->>FILE:\"\* test\"                  ; Achtgeben: Sterne gehören zu den Regexen, müssen daher gequotet werden
;   ->>FILE:\"\* test\"->
;   * test    $ test                   ; nur zum Testen
;
;   ->>A:Müller                        ; private Kurzform für ->FILE:adressen.op*\"name\", Adressenfile als Variable hier im Code
;
;   ->**Ueberschriftentest             ; steht ein * am Anfang des Musters, werden die Sterne nicht als Regexe interpretiert,
;                                      ;    sondern als \"Uberschriftengerechtes Muster\" (hier: `**	Ueberschriftentest')
;                                      ;    Verhalten: ist das Muster am Zeilenanfang zu finden, wird es auch dort genommen,
;                                      ;    ansonsten geht es durch den normalen Text.
; 
;   ->Ueberschriftentest
;   ->FILE:Ueberschriftentest
;
;**	Ueberschriftentest
;
;;
;; Beispiele FUNC:
;;  
;   ->FUNC:funcfile*startpunkt
;   ->FUNC:funcfile*Fakultät
;   ->FUNC:startpunkt
;
;;
;; Beispiele EVAL:
;;  
;   ->EVAL:xeyes
;   ->EVAL:xeyes&
;   ->EVAL:\"date --version\"  
;   ->EVAL:\"gcc -v\"
;   ->EVAL:xfig*test.fig
;   ->EVAL:\"xfig -bg red\"*test.fig    ; Argumente in Anführungsstrichen, Ausführungspunkt NUR vor dem 
;                                     ; ersten Blank (beim \"->EVAL:xxxx\"
;   ->EVAL:\"xfig -bg red\"*test.fig&   ; wie eben
;   ->FILE:op-hyper.el*defun;EVAL:\"date --version\"  
;   ->FILE:op-hyper.el*defun->;EVAL:\"date --version\"  
;				      ; bei Kettenargumenten: EVAL immer an den Schluß,
;                                     ;  andere Funktionen immer mit *-Zeichen. Nur je ein Buttontyp ist erlaubt.
;   ->EVAL:test.fig                   ; Defaultextention ist einprogrammiert (siehe oben)
;   ->!date                           ; Defaultbuttons
;   ->!\"date --version\"
;   ->!cat*op-hyper.el
;  
;;
;; Beispiele REF:
;;
;   ->REF:../op-hyper/op-hyper.el*1button      ; Muster ist hier \"1button\", kann aber beliebig sein
;   ->REF:../op-hyper/op-hyper.el*1button->
;   ->REF:1button                              ; auch hier: mehrmaliges Auslösen ohne Bewegen des 
;                                              ; Cursors im anderen Fenster sucht nächste Referenz
;   ->REF:1button->
;
; die alle sollten hier enden:   ->1button:FILE:defun   ; Doppelpunkt ist günstig, aber nicht Pflicht:
;                                ->1button#FILE:defun   ; geht z. B. auch
;
;
;;
;; Beispiele (siehe auch die bei REF:func) REF-R:
;;
;   ->REF-R:../op-hyper/op-hyper.el*2button->      ; Muster ist hier \"2button\", kann aber beliebig sein
;
;   ->REF-R:2button->
;
;   ->2button:REF-R:3button->
;
; die alle sollten hier enden:   ->3button:FILE:das_hier_gibts_nicht   
;
;; weiteres Beispiel: Loop zurück zum ersten Button wird nicht unendlich durchlaufen 
;; (ideal zum Einlesen vieler Files !!!)
;
;   ->4button:REF-R:5button->           
;        ->5button:REF-R:6button->
;             ->6button:REF-R:4button->
;
;
;; nicht gefunden wird dagegen die folgende Loop (gibt dann aber einen Emacs-Fehler)
;; (zum Debuggen empfiehlt sich REF-R:func in REF:func umzudefinieren) 
;   ->4button:REF-R:5button->           
;        ->5button:REF-R:6button->
;             ->6button:REF-R:5button->
;
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"
		 
		 )
  (goto-char (point-min))

)


