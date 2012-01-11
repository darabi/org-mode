

(if xemacsp
    (defvar op-mode-menu-bar-map (make-keymap))
  (defvar op-mode-menu-bar-map (make-sparse-keymap)))



;; Eintragen in vorhandene Menüs

;; gefährlich !!

(define-key (current-global-map) "\M-s" 'search-this-string-forward)
(define-key (current-global-map) "\M-r" 'search-this-string-backward)


(if gnuemacsp
    (progn
      ;; bei gecrypteten Files: nehme auch die .crypt-Funktionen in die richtigen Menus auf
      (define-key menu-bar-files-menu [write-region-crypted]
	'("Save Region As..." . write-region-crypted))
      (define-key menu-bar-files-menu [write-file]
	'("Save Buffer As..." . (lambda()(interactive)
				  (if use-keymap-for-crypted-files (ask-for-crypt-write)(write-file)))))
      (define-key menu-bar-files-menu [save-buffer]
	'("Save Buffer" . (lambda()(interactive)
			    (if use-keymap-for-crypted-files (save-buffer-crypted)(save-buffer-with-control)))))
      (define-key menu-bar-help-menu [op-help]
	'("[OP] Outline-Project-Mode: Hilfe" . op-help))
      ))



;; bis hierher




(if gnuemacsp
    (progn
      ;; Eintragen in vorhandene Menüs
      (define-key menu-bar-search-menu [search-this-string-backward]
	'("Backward Current Word" . search-this-string-backward))
      (define-key menu-bar-search-menu [search-this-string-forward]
	'("Forward Current Word" . search-this-string-forward))
      
      (define-key menu-bar-edit-menu [separator-mark]
	'("--")) ; ------------------------------------------------------------
      (define-key menu-bar-edit-menu [exchange-point-and-mark]
	'("Exchange Point And Mark " . exchange-point-and-mark))
      (define-key menu-bar-edit-menu [set-mark-command]
	'("Set Mark [=Beginning Of Region, Cursor=End]" . set-mark-command))))

;;; Muß nicht sein, hilft aber manchmal
;--------------------
(define-key op-mode-menu-bar-map [OUTLINE]
  (if xemacsp
      (cons "OP-Outline" (make-keymap "OUTLINE"))
    (cons "OP-Outline" (make-sparse-keymap "OUTLINE"))))

  (define-key op-mode-menu-bar-map [OUTLINE hide-other]
    '("Outline Hide Other" . hide-other))
  (define-key op-mode-menu-bar-map [OUTLINE hide-sublevels]
    '("Outline Hide Sublevels" . hide-sublevels))
  (define-key op-mode-menu-bar-map [OUTLINE hide-subtree]
    '("Outline Hide Subtree" . hide-subtree))
  (define-key op-mode-menu-bar-map [OUTLINE hide-entry]
    '("Outline Hide Entry" . hide-entry))
  (define-key op-mode-menu-bar-map [OUTLINE hide-body]
    '("Outline Hide Body" . hide-body))
  (define-key op-mode-menu-bar-map [OUTLINE hide-leaves]
    '("Outline Hide Leaves" . hide-leaves))

  (define-key op-mode-menu-bar-map [OUTLINE separator-show]
    '("--")) ; ------------------------------------------------------------

  (define-key op-mode-menu-bar-map [OUTLINE show-subtree]
    '("Outline Show Subtree" . show-subtree))
  (define-key op-mode-menu-bar-map [OUTLINE show-children]
    '("Outline Show Children" . show-children))
  (define-key op-mode-menu-bar-map [OUTLINE show-branches]
    '("Outline Show Branches" . show-branches))
  (define-key op-mode-menu-bar-map [OUTLINE show-entry]
    '("Outline Show Entry" . show-entry))
  (define-key op-mode-menu-bar-map [OUTLINE show-all]
    '("Outline Show All" . show-all))

  (define-key op-mode-menu-bar-map [OUTLINE separator-headings]
    '("--")) ; ------------------------------------------------------------

  (define-key op-mode-menu-bar-map [OUTLINE outline-backward-same-level]
    '("Outline Previous Same Level" . outline-backward-same-level))
  (define-key op-mode-menu-bar-map [OUTLINE outline-forward-same-level]
    '("Outline Next Same Level" . outline-forward-same-level))
  (define-key op-mode-menu-bar-map [OUTLINE outline-previous-visible-heading]
    '("Outline Previous" . outline-previous-visible-heading))
  (define-key op-mode-menu-bar-map [OUTLINE outline-next-visible-heading]
    '("Outline Next" . outline-next-visible-heading))
  (define-key op-mode-menu-bar-map [OUTLINE outline-up-heading]
    '("Outline Up" . outline-up-heading))




;--------------------
(define-key op-mode-menu-bar-map [OP-CONV]
  (if xemacsp
      (cons "OP-CONV" (make-keymap "OP-CONV"))
    (cons "OP-CONV" (make-sparse-keymap "OP-CONV"))))

(define-key op-mode-menu-bar-map [OP-CONV op-text-extractor]
  '("xfig-Text von Cursorposition extrahieren" . xfig-text-extractor))

(define-key op-mode-menu-bar-map [OP-CONV corell-text-extractor]
  '("Corell EPS-File von Cursorposition extrahieren" . corell-text-extractor))

(define-key op-mode-menu-bar-map [OP-CONV separator-others]
  '("--")) ; ------------------------------------------------------------

(define-key op-mode-menu-bar-map [OP-CONV mpx-text-extractor-4]
  '("MPX-File von Cursorposition Outline-Form mit Kommentaren	[neg. Argument]" . (lambda ()(interactive)(mpx-text-extractor -1))))
(define-key op-mode-menu-bar-map [OP-CONV mpx-text-extractor-3]
  '("MPX-File von Cursorposition Outline-Form ohne Kommentare	[\"-\"-Argument]" . (lambda ()(interactive)(mpx-text-extractor '-))))
(define-key op-mode-menu-bar-map [OP-CONV mpx-text-extractor-2]
  '("MPX-File von Cursorposition mit Kommentaren	[pos. Argument]" . (lambda ()(interactive)(mpx-text-extractor 1))))
(define-key op-mode-menu-bar-map [OP-CONV mpx-text-extractor-1]
  '("MPX-File von Cursorposition extrahieren" . mpx-text-extractor))

(define-key op-mode-menu-bar-map [OP-CONV separator-mpx]
  '("--")) ; ------------------------------------------------------------

(define-key op-mode-menu-bar-map [OP-CONV op-BF-import]
  '("Import this BrainForest file	(M-x from_BF)" . from_BF))
(define-key op-mode-menu-bar-map [OP-CONV op-BF-export]
  '("Export this OP file to BrainForest	(M-x to_BF)" . to_BF))






;--------------------
(define-key op-mode-menu-bar-map [OP-SEARCH]
  (if xemacsp
      (cons "OP-SEARCH" (make-keymap "OP-SEARCH"))
    (cons "OP-SEARCH" (make-sparse-keymap "OP-SEARCH"))))


(define-key op-mode-menu-bar-map [OP-SEARCH isearch-backward-in-headings]
  '("Isearch Backward Only In Headings" . isearch-backward-in-headings))
(define-key op-mode-menu-bar-map [OP-SEARCH isearch-forward-in-headings]
  '("Isearch Forward Only In Headings" . isearch-forward-in-headings))
(define-key op-mode-menu-bar-map [OP-SEARCH search-backward-for-current-word-1]
  '("Backward To Current .Word." . (lambda()(interactive)(search-backward-for-current-word t))))
(define-key op-mode-menu-bar-map [OP-SEARCH search-forward-for-current-word-1]
  '("Forward To Current .Word." . (lambda()(interactive)(search-forward-for-current-word t))))

(define-key op-mode-menu-bar-map [OP-SEARCH isearch-visible-backward]
  '("I-search Backward Visible Only" . isearch-visible-backward))
(define-key op-mode-menu-bar-map [OP-SEARCH isearch-visible-forward]
  '("I-search Forward Visible Only" . isearch-visible-forward))

(define-key op-mode-menu-bar-map [OP-SEARCH search-word-backward-expand-to-visible]
  '("Backward Word Expand To Visible" . search-word-backward-expand-to-visible))
(define-key op-mode-menu-bar-map [OP-SEARCH search-word-forward-expand-to-visible]
  '("Forward Word Expand To Visible" . search-word-forward-expand-to-visible))
(define-key op-mode-menu-bar-map [OP-SEARCH search-this-string-backward]
  '("Backward Current Word" . search-this-string-backward))
(define-key op-mode-menu-bar-map [OP-SEARCH search-this-string-forward]
  '("Forward Current Word" . search-this-string-forward))





;--------------------
(define-key op-mode-menu-bar-map [OP-INCLUDE]
  (if xemacsp
      (cons "OP-INCLUDE" (make-keymap "OP-INCLUDE"))
    (cons "OP-INCLUDE" (make-sparse-keymap "OP-INCLUDE"))))

(define-key op-mode-menu-bar-map [OP-INCLUDE insert-file-at-include-statement]
  '("Insert Files Instead Of #include" . insert-file-at-include-statement))
(define-key op-mode-menu-bar-map [OP-INCLUDE separator-inserts]
  '("--")) ; ------------------------------------------------------------
(define-key op-mode-menu-bar-map [OP-INCLUDE extract-and-save-includes]
  '("Save File And Remove From #include" . extract-and-save-includes))
(define-key op-mode-menu-bar-map [OP-INCLUDE load-files-at-include-statements]
  '("Load Files at #include" . load-files-at-include-statements))
(define-key op-mode-menu-bar-map [OP-INCLUDE extract-and-save-this-include]
  '("Save File From This #include And Remove It" . extract-and-save-this-include))
(define-key op-mode-menu-bar-map [OP-INCLUDE load-file-at-include-statement]
  '("Load File Of This #include" . load-file-at-include-statement))
(define-key op-mode-menu-bar-map [OP-INCLUDE separator-includes]
  '("--")) ; ------------------------------------------------------------
(define-key op-mode-menu-bar-map [OP-INCLUDE kill-region-insert-include-and-mail]
  '("Write Region To File + Insert \"#include\" + mh-Mail" . kill-region-insert-include-and-mail))
(define-key op-mode-menu-bar-map [OP-INCLUDE kill-region-insert-include]
  '("Write Region To File And Insert \"#include\"" . kill-region-insert-include))
(define-key op-mode-menu-bar-map [OP-INCLUDE write-region-crypted]
  '("Write Region To File" . write-region-crypted))
(define-key op-mode-menu-bar-map [OP-INCLUDE ask-for-crypt-write]
  '("Write Buffer To File" . ask-for-crypt-write))




;--------------------

(define-key op-mode-menu-bar-map [OP-CVS]
  (if xemacsp
      (cons "OP-CVS" (make-keymap "OP-CVS"))
    (cons "OP-CVS" (make-sparse-keymap "OP-CVS"))))


(define-key op-mode-menu-bar-map [OP-CVS cvs-commit]
  '("Emacs: cvs commit" . cvs-commit))
(define-key op-mode-menu-bar-map [OP-CVS cvs-update]
  '("Emacs: cvs update" . cvs-update))
(define-key op-mode-menu-bar-map [OP-CVS cvs-examine]
  '("Emacs: cvs examine (test for update)" . cvs-examine))
(define-key op-mode-menu-bar-map [OP-CVS cvs-checkout]
  '("Emacs: cvs checkout" . cvs-checkout))
(define-key op-mode-menu-bar-map [OP-CVS cvs-mode]
  '("Emacs: Load CVS Mode" . '(lambda ()(interactive)(load "pcl-cvs")(cvs-mode))))
(define-key op-mode-menu-bar-map [OP-CVS separator-PCL-CVS]
  '("--")) ; ------------------------------------------------------------
(define-key op-mode-menu-bar-map [OP-CVS write-this-include-for-cvs]
  '("Save Stripped File To CVS And To You" . write-this-include-for-cvs))
(define-key op-mode-menu-bar-map [OP-CVS write-includes-for-cvs]
  '("Save All Stripped File To CVS And To You" . write-includes-for-cvs))
(define-key op-mode-menu-bar-map [OP-CVS load-this-include-for-cvs]
  '("Load File From CVS And Merge With Yours" . load-this-include-for-cvs))
(define-key op-mode-menu-bar-map [OP-CVS load-includes-for-cvs]
  '("Load All Files From CVS And Merge With Yours" . load-includes-for-cvs))
(define-key op-mode-menu-bar-map [OP-CVS separator-CVS]
  '("--")) ; ------------------------------------------------------------
(define-key op-mode-menu-bar-map [OP-CVS ediff-.op-to-.cvs.op-file]
  '("Load All Files From CVS And Merge With Yours" . ediff-.op-to-.cvs.op-file))
(define-key op-mode-menu-bar-map [OP-CVS make-.cvs.op-copy-from-current]
  '("Make .cvs.op Copy From Current File" . make-.cvs.op-copy-from-current))







;--------------------
(define-key op-mode-menu-bar-map [OP-IFDEF]
  (if xemacsp
      (cons "OP-IFDEF" (make-keymap "OP-IFDEF"))
    (cons "OP-IFDEF" (make-sparse-keymap "OP-IFDEF"))))

(define-key op-mode-menu-bar-map [OP-IFDEF hide-ifdef-block]
  '("Hide Enclosing Ifdef Blocks This Line" . hide-ifdef-block))
(define-key op-mode-menu-bar-map [OP-IFDEF show-ifdef-block]
  '("Show Enclosing Ifdef Blocks This Line" . show-ifdef-block))
(define-key op-mode-menu-bar-map [OP-IFDEF separator-thisblock]
  '("--")) ; ------------------------------------------------------------
(define-key op-mode-menu-bar-map [OP-IFDEF down-ifdef]
  '("Down Nested Visible Ifdef" . (lambda()(interactive)(down-ifdef)(search-forward "###" nil t))))
(define-key op-mode-menu-bar-map [OP-IFDEF up-ifdef]
  '("Up Nested Visible Ifdef" . (lambda()(interactive)(up-ifdef)(search-forward "###" nil t))))
(define-key op-mode-menu-bar-map [OP-IFDEF backward-ifdef]
  '("Beginning of Previous Visible Block This Level" .  (lambda()(interactive)(backward-ifdef)(search-forward "###" nil t))))
(define-key op-mode-menu-bar-map [OP-IFDEF forward-ifdef]
  '("End Of Next Visible Block This Level" . (lambda()(interactive)(forward-ifdef)(search-forward "###" nil t))))
(define-key op-mode-menu-bar-map [OP-IFDEF previous-ifdef]
  '("Previous Ifdef Statement" .  (lambda()(interactive)(previous-ifdef)(search-forward "###" nil t))))
(define-key op-mode-menu-bar-map [OP-IFDEF next-ifdef]
  '("Next Ifdef Statement" . (lambda()(interactive)(next-ifdef)(search-forward "###" nil t))))

(define-key op-mode-menu-bar-map [OP-IFDEF separator-moves]
  '("--")) ; ------------------------------------------------------------

;(define-key op-mode-menu-bar-map [OP-IFDEF hide-ifdef-define-alist]
;  '("List Global Ifdef List" . (lambda()(interactive)(prin1 hide-ifdef-define-alist))))
;(define-key op-mode-menu-bar-map [OP-IFDEF hide-ifdef-use-define-alist]
;  '("Set Ifdef Defined List" . hide-ifdef-use-define-alist))
;(define-key op-mode-menu-bar-map [OP-IFDEF hide-ifdef-set-define-alist]
;  '("Set Associated Name List" . hide-ifdef-set-define-alist))
(define-key op-mode-menu-bar-map [OP-IFDEF hide-ifdef-set-define-alist]
  '("Show Ifdef Variable List" .(lambda()(interactive)(prin1 hide-ifdef-env))))
(define-key op-mode-menu-bar-map [OP-IFDEF hide-ifdef-undef]
  '("Set Ifdef Variable To Hide" . hide-ifdef-undef))
(define-key op-mode-menu-bar-map [OP-IFDEF hide-ifdef-define]
  '("Set Ifdef Variable To Show" . hide-ifdef-define))

(define-key op-mode-menu-bar-map [OP-IFDEF separator-variables]
  '("--")) ; ------------------------------------------------------------


(define-key op-mode-menu-bar-map [OP-IFDEF hide-ifdefs-in-region]
  '("Hide Nondefined Blocks In Region" . hide-ifdefs-in-region))
(define-key op-mode-menu-bar-map [OP-IFDEF show-ifdefs-in-region]
  '("Show Ifdef Blocks In Region" . show-ifdefs-in-region))
(define-key op-mode-menu-bar-map [OP-IFDEF hide-ifdefs]
  '("Hide Nondefined Ifdef Blocks (default)" . hide-ifdefs))
(define-key op-mode-menu-bar-map [OP-IFDEF show-ifdefs]
  '("Show All Ifdef Blocks" . show-ifdefs))

(define-key op-mode-menu-bar-map [OP-IFDEF show-inserters]
  '("--")) ; ------------------------------------------------------------

(define-key op-mode-menu-bar-map [OP-IFDEF insert-ifdef-end]
  '("Insert Ifdef End" . (lambda()(interactive)
			   ;(insert "\n")
			   (insert "  ###bis hierher\n")(backward-char 1))))
(define-key op-mode-menu-bar-map [OP-IFDEF insert-ifdef-else]
  '("Insert Elseif" . (lambda()(interactive)
			;(insert "\n")
			(insert "  ###dagegen jedoch nicht\n")(backward-char 1))))
(define-key op-mode-menu-bar-map [OP-IFDEF insert-ifdef-begin]
  '("Insert Ifdef Begin" . insert-ifdef-begin))

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
(define-key op-mode-menu-bar-map [OP-MISC]
  (if xemacsp
      (cons "OP-MISC" (make-keymap "OP-MISC"))
    (cons "OP-MISC" (make-sparse-keymap "OP-MISC"))))


(define-key op-mode-menu-bar-map [OP-MISC p]
  '("OP Picture-Mode" . p))
(define-key op-mode-menu-bar-map [OP-MISC project-schedule-1]
  '("OP Vollkalender" . (lambda()(interactive)(project-schedule nil nil t))))
(define-key op-mode-menu-bar-map [OP-MISC project-schedule]
  '("OP Projektzeitplan" . project-schedule))
(define-key op-mode-menu-bar-map [OP-MISC d]
  '("OP Kalenderplaner" . d))
(define-key op-mode-menu-bar-map [OP-MISC calendar-in-extra-window]
  '("OP Kalender in einem extra Fenster" . calendar-in-extra-window))
(define-key op-mode-menu-bar-map [OP-MISC c]
  '("OP Kalender" . c))
(define-key op-mode-menu-bar-map [OP-MISC separator-calendar]
  '("--")) ; ------------------------------------------------------------


(define-key op-mode-menu-bar-map [OP-MISC atm-off]
  '("automatic change mode OFF (for links only)" . (lambda()(interactive)(automatic-change-mode t)(automatic-change-mode))))
(define-key op-mode-menu-bar-map [OP-MISC atm-on]
  '("Automatic Change Mode ON  (for links only)" . (lambda()(interactive)(automatic-change-mode t))))

(define-key op-mode-menu-bar-map [OP-MISC separator-atmmode]
  '("--")) ; ------------------------------------------------------------

(define-key op-mode-menu-bar-map [OP-MISC select-hyper-func]
  '("Eval Hypermark" . select-hyper-func))
(define-key op-mode-menu-bar-map [OP-MISC separator-hyperbole]
  '("--")) ; ------------------------------------------------------------

(define-key op-mode-menu-bar-map [OP-MISC fixup-whitespace]
  '("Reduce To One Whitespace" . fixup-whitespace))
(define-key op-mode-menu-bar-map [OP-MISC tab-to-tab-stop]
  '("Insert TAB" . tab-to-tab-stop))

(define-key op-mode-menu-bar-map [OP-MISC separator-editing]
  '("--")) ; ------------------------------------------------------------

(define-key op-mode-menu-bar-map [OP-MISC op-softindent-region]
  '("Format Region Softly (with included files)" . op-softindent-region))
(define-key op-mode-menu-bar-map [OP-MISC op-indent-region]
  '("Format Region OP-Mode Like" . op-indent-region))
(define-key op-mode-menu-bar-map [OP-MISC force-indent]
  '("Force Indent Line" . force-indent))
(define-key op-mode-menu-bar-map [OP-MISC indent-like-last-headline]
  '("Headline Indent" . indent-like-last-headline))
(define-key op-mode-menu-bar-map [OP-MISC re-indent]
  '("Standard Indent" . re-indent))

(define-key op-mode-menu-bar-map [OP-MISC separator-indentations]
  '("--")) ; ------------------------------------------------------------

(define-key op-mode-menu-bar-map [OP-MISC trim-buffer]
  '("Trim Whitespaces" . trim-buffer))
(define-key op-mode-menu-bar-map [OP-MISC column-ruler]
  '("Where Am I" . column-ruler))
(define-key op-mode-menu-bar-map [OP-MISC hilit-repaint-command]
  '("Recolor Display" . hilit-repaint-command))
(define-key op-mode-menu-bar-map [OP-MISC reset-display]
  '("Reset Display" . reset-display))

(define-key op-mode-menu-bar-map [OP-MISC separator-logbuch]
  '("--")) ; ------------------------------------------------------------

(define-key op-mode-menu-bar-map [OP-MISC op-logbuch-alarm-off]
  '("Logbuch-Alarm aus" . logbuch-alarm-off))
(define-key op-mode-menu-bar-map [OP-MISC op-logbuch-alarm-on]
  '("Logbuch-Abfrage an" . logbuch-alarm-on))
(define-key op-mode-menu-bar-map [OP-MISC op-logbuch-mode]
  '("Logbuch-Mode" . logbuch-mode))
(define-key op-mode-menu-bar-map [OP-MISC op-logbuch]
  '("Logbuch-Eintrag generieren" . logbuch))


(define-key op-mode-menu-bar-map [OP-MISC separator-print] '("--")) 
;------------------------------------------------------------

(define-key op-mode-menu-bar-map [OP-MISC ps-print-region-with-faces]
  '("Region drucken / Print" . ps-print-region-with-faces))
(define-key op-mode-menu-bar-map [OP-MISC ps-print-buffer-with-faces]
  '("Buffer drucken / Print" . ps-print-buffer-with-faces))




;--------------------
(define-key op-mode-menu-bar-map [OP-HEADINGS]
  (if xemacsp
      (cons "OP-HEADINGS" (make-keymap "OP-HEADINGS"))
  (cons "OP-HEADINGS" (make-sparse-keymap "OP-HEADINGS"))))

(define-key op-mode-menu-bar-map [OP-HEADINGS ausblend-0]
  '("Toggle Everything In Region [...]" . ausblend-0))
(define-key op-mode-menu-bar-map [OP-HEADINGS ausblend-1-1]
  '("Toggle F[ällt]W[eg] In Region " . ausblend-1))
(define-key op-mode-menu-bar-map [OP-HEADINGS ausblend-2-2]
  '("Toggle S[tändig] W[iederkehrendes] In Region " . ausblend-2))
(define-key op-mode-menu-bar-map [OP-HEADINGS ausblend-2]
  '("Toggle V[erschobenes] In Region " . ausblend-2))
(define-key op-mode-menu-bar-map [OP-HEADINGS ausblend-4]
  '("Toggle R[essourcen]" . ausblend-4))
(define-key op-mode-menu-bar-map [OP-HEADINGS ausblend-1]
  '("Toggle E[rledigtes] In Region" . ausblend-1))
(define-key op-mode-menu-bar-map [OP-HEADINGS ausblend-3]
  '("Toggle D[ringendes] In Region" . ausblend-3))
(define-key op-mode-menu-bar-map [OP-HEADINGS ausblend-5]
  '("Toggle T[erminabsprachen] In Region " . ausblend-5))

(define-key op-mode-menu-bar-map [OP-HEADINGS separator-ausblenden]
  '("--")) ; ------------------------------------------------------------

(define-key op-mode-menu-bar-map [OP-HEADINGS op-shift-right]
  '("Shift Region Down" . op-shift-right))
(define-key op-mode-menu-bar-map [OP-HEADINGS op-shift-left]
  '("Shift Region Up" . op-shift-left))

(define-key op-mode-menu-bar-map [OP-HEADINGS separator-shifts]
  '("--")) ; ------------------------------------------------------------

(define-key op-mode-menu-bar-map [OP-HEADINGS outline-forward-jump-same-level]
  '("Forward Same Level" . outline-forward-jump-same-level))
(define-key op-mode-menu-bar-map [OP-HEADINGS outline-backward-jump-same-level]
  '("Backward Same Level" . outline-backward-jump-same-level))
(define-key op-mode-menu-bar-map [OP-HEADINGS outline-project-backward-same-level]
  '("Previous Same Level" . outline-backward-same-level))
(define-key op-mode-menu-bar-map [OP-HEADINGS outline-project-forward-same-level]
  '("Next Same Level" . outline-forward-same-level))
(define-key op-mode-menu-bar-map [OP-HEADINGS outline-project-previous-visible-heading]
  '("Previous" . outline-previous-visible-heading))
(define-key op-mode-menu-bar-map [OP-HEADINGS outline-next-visible-heading]
  '("Next" . outline-next-visible-heading))
(define-key op-mode-menu-bar-map [OP-HEADINGS outline-up-heading]
  '("Up" . outline-up-heading))
(define-key op-mode-menu-bar-map [OP-HEADINGS top-heading]
  '("Top" . top-heading))
(define-key op-mode-menu-bar-map [OP-HEADINGS next-top-heading]
  '("Next Top" . next-top-heading))
(define-key op-mode-menu-bar-map [OP-HEADINGS very-top-heading]
  '("Very Top" . very-top-heading))






;--------------------

(define-key op-mode-menu-bar-map [OP-SHOW]
  (if xemacsp
        (cons "OP-SHOW" (make-keymap "OP-SHOW"))
    (cons "OP-SHOW" (make-sparse-keymap "OP-SHOW"))))

(define-key op-mode-menu-bar-map [OP-SHOW hide-all]
  '("Hide All" . hide-all))
(define-key op-mode-menu-bar-map [OP-SHOW hide-other-hide]
  '("Hide All Others" . hide-other-hide))
(define-key op-mode-menu-bar-map [OP-SHOW hide-subtree-hide]
  '("Hide Subtree" . hide-subtree-hide))
(define-key op-mode-menu-bar-map [OP-SHOW hide-body]
  '("Only Headings" . hide-body))
(define-key op-mode-menu-bar-map [OP-SHOW hide-leaves-hide]
  '("Hide All Deeper Entries" . hide-leaves-hide))
(define-key op-mode-menu-bar-map [OP-SHOW hide-entry-hide]
  '("Hide Entry" . hide-entry-hide))

(define-key op-mode-menu-bar-map [OP-SHOW separator-hide]
  '("--")) ; ------------------------------------------------------------

(define-key op-mode-menu-bar-map [OP-SHOW hide-sublevels-hide]
  '("Sublevels Till Level [ARG]" . hide-sublevels-hide))

(define-key op-mode-menu-bar-map [OP-SHOW separator-sublevels]
  '("--")) ; ------------------------------------------------------------

(define-key op-mode-menu-bar-map [OP-SHOW show-really-everything]
  '("Show Everything In Buffer" . show-really-everything))
(define-key op-mode-menu-bar-map [OP-SHOW show-really-everything-in-region]
  '("Show Everything In Region" . show-really-everything-in-region))
(define-key op-mode-menu-bar-map [OP-SHOW show-all-hide]
  '("Show All" . show-all-hide))
(define-key op-mode-menu-bar-map [OP-SHOW show-all-branches-hide]
  '("Show All Branches" . show-all-branches-hide))
(define-key op-mode-menu-bar-map [OP-SHOW show-subtree-hide]
  '("Show Subtree" . show-subtree-hide))
(define-key op-mode-menu-bar-map [OP-SHOW show-branches-hide]
  '("Show Branches" . show-branches-hide))
(define-key op-mode-menu-bar-map [OP-SHOW show-children-hide]
  '("Show Children" . show-children-hide))
(define-key op-mode-menu-bar-map [OP-SHOW show-entry-hide]
  '("Show Entry" . show-entry-hide))
(define-key op-mode-menu-bar-map [OP-SHOW show-current-place]
  '("Show Current Place" . show-current-place))









;--------------------
(define-key (current-local-map) [menu-bar] op-mode-menu-bar-map)

