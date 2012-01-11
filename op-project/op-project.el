;(recenter (/ (* outline-readjust-position-percentage (frame-height)) 100))
;;;
;;; Project-Mode-File,
;;;
;;; geschrieben 12.11.1994 - 27.8.1997 von Stefan Walther (SMW), Mainz, Rudolf-Diesel-Str. 3
;;; walther@xemacs.net
;;;
;;; etliche Programmerweiterungen immer wieder über die Zeit, letzte Änderung: Oktober 2006
;;;
;;; Spezialmode zum Verwalten von Projektplanungsfiles
;;;
;;;



;; Die wichtigste Variable zum Debuggen ist (setq selective-display t) oder (setq selective-display nil)



(defun make-project-elc-file ()
  "Macht aus diesem File den File op-project.el, von dem aus dann alles startet"
  (let ((before-change-functions nil)(after-change-functions nil))
    (goto-char (point-min))
    (write-file "op-project.el")	; sichern, damit bei einer Unterbrechung nicht in op-main.el 
					; herumgeschrieben wird
    (next-line 35) ; überspringt dieses Stückchen Code
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
    (byte-compile-file (buffer-file-name))
    ;;
    ))

;; erzeuge op-project durch Ausführen von 
;; (make-project-elc-file)      ;<<<<< C-x C-e auf dem Blank hinter dieser Klammer ist ein Build 
;;;                                        wenn vorher der project-mode hochgerufen wurde!
;; mit C-x C-e  in diesem Buffer




;;===================================================================================================
;;===================================================================================================
;;===================================================================================================

;
; Definition des Prefix-Arguments für den Project-Mode
;
(defvar project-mode-prefix nil)
;(defvar project-mode-keymap (make-sparse-keymap))

(setq project-mode-prefix "\M-c\M-o")

(defvar outline-minor-mode-map nil)
(defun define-project-mode-key (key command)
  (define-key outline-minor-mode-map (concat project-mode-prefix key) command))



;;===================================================================================================
;;===================================================================================================


(message "Org-Mode wird geladen")


;;;==================== ORG-MODE (der Fremdgängermode...) ===================
(if (boundp 'load--with--org-mode)
    (if load--with--org-mode
	(progn
	  (progn
	    
;;;;;;;includeforelc!
;;;;;;(load "op-outline-mode-emacs20+")

;;; outline.el --- outline mode commands for Emacs

;; Copyright (C) 1986, 1993, 1994, 1995, 1997 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: outlines

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package is a major mode for editing outline-format documents.
;; An outline can be `abstracted' to show headers at any given level,
;; with all stuff below hidden.  See the Emacs manual for details.

;;; Code:



(defcustom selective-display-ellipses t
  "If you set the variable selective-display-ellipses to nil, the three dots at the
end of a line that precedes invisible lines do not appear. There is no visible
indication of the invisible lines. This variable becomes local automatically
when set.")

(defgroup outlines nil
  "Support for hierarchical outlining."
  :prefix "outline-"
  :group 'editing)

(defcustom outline-regexp nil
  "*Regular expression to match the beginning of a heading.
Any line whose beginning matches this regexp is considered to start a heading.
Note that Outline mode only checks this regexp at the start of a line,
so the regexp need not (and usually does not) start with `^'.
The recommended way to set this is with a Local Variables: list
in the file it applies to.  See also `outline-heading-end-regexp'."
  :type '(choice regexp (const nil))
  :group 'outlines)

;; Can't initialize this in the defvar above -- some major modes have
;; already assigned a local value to it.
(or (default-value 'outline-regexp)
    (setq-default outline-regexp "[*\^L]+"))

(defcustom outline-heading-end-regexp "\n"
  "*Regular expression to match the end of a heading line.
You can assume that point is at the beginning of a heading when this
regexp is searched for.  The heading ends at the end of the match.
The recommended way to set this is with a `Local Variables:' list
in the file it applies to."
  :type 'regexp
  :group 'outlines)


;(add-hook 'outline-view-change-hook 'recenter)


(defvar outline-mode-prefix-map nil)

(if outline-mode-prefix-map
    nil
  (if xemacsp
      (setq outline-mode-prefix-map (make-keymap))
    (setq outline-mode-prefix-map (make-sparse-keymap)))
  (define-key outline-mode-prefix-map "@" 'outline-mark-subtree)
  (define-key outline-mode-prefix-map "\C-n" 'outline-next-visible-heading)
  (define-key outline-mode-prefix-map "\C-p" 'outline-previous-visible-heading)
  (define-key outline-mode-prefix-map "\C-i" 'show-children)
  (define-key outline-mode-prefix-map "\C-s" 'show-subtree)
  (define-key outline-mode-prefix-map "\C-d" 'hide-subtree)
  (define-key outline-mode-prefix-map "\C-u" 'outline-up-heading)
  (define-key outline-mode-prefix-map "\C-f" 'outline-forward-same-level)
  (define-key outline-mode-prefix-map "\C-b" 'outline-backward-same-level)
  (define-key outline-mode-prefix-map "\C-t" 'hide-body)
  (define-key outline-mode-prefix-map "\C-a" 'show-all)
  (define-key outline-mode-prefix-map "\C-c" 'hide-entry)
  (define-key outline-mode-prefix-map "\C-e" 'show-entry)
  (define-key outline-mode-prefix-map "\C-l" 'hide-leaves)
  (define-key outline-mode-prefix-map "\C-k" 'show-branches)
  (define-key outline-mode-prefix-map "\C-q" 'hide-sublevels)
  (define-key outline-mode-prefix-map "\C-o" 'hide-other)
  (define-key outline-mode-prefix-map "\C- " 'outline-toggle-children)
  (define-key outline-mode-prefix-map "\C-^" 'outline-move-subtree-up)
  (define-key outline-mode-prefix-map "\C-v" 'outline-move-subtree-down)
  (define-key outline-mode-prefix-map [(control ?<)] 'outline-promote)
  (define-key outline-mode-prefix-map [(control ?>)] 'outline-demote)
  (define-key outline-mode-prefix-map "\C-m" 'outline-insert-heading)
)



(defvar outline-mode-menu-heading
  '("Headings"
    ["Up" outline-up-heading t]
    ["Next" outline-next-visible-heading t]
    ["Previous" outline-previous-visible-heading t]
    ["Next Same Level" outline-forward-same-level t]
    ["Previous Same Level" outline-backward-same-level t]
    ["New heading" outline-insert-heading t]
    ["Copy to kill ring" outline-headers-as-kill :active (region-active-p)]
    ["Toggle Children" outline-toggle-children t]
    ["Move subtree up" outline-move-subtree-up t]
    ["Move subtree down" outline-move-subtree-down t]
    ["Promote subtree" outline-promote t]
    ["Demote subtree" outline-demote t]))

(defvar outline-mode-menu-show
  '("Show"
    ["Show All" show-all t]
    ["Show Entry" show-entry t]
    ["Show Branches" show-branches t]
    ["Show Children" show-children t]
    ["Show Subtree" show-subtree t]))

(defvar outline-mode-menu-hide
  '("Hide"
    ["Hide Leaves" hide-leaves t]
    ["Hide Body" hide-body t]
    ["Hide Entry" hide-entry t]
    ["Hide Subtree" hide-subtree t]
    ["Hide Other" hide-other t]
    ["Hide Sublevels" hide-sublevels t]))



(defvar outline-mode-menu-bar-map nil)
(if outline-mode-menu-bar-map
    nil
  (if xemacsp
      (setq outline-mode-menu-bar-map (make-keymap))
  (setq outline-mode-menu-bar-map (make-sparse-keymap))))

(defconst outline-mode-map nil "")

(if outline-mode-map
    nil
  (if xemacsp
      (setq outline-mode-map (copy-keymap text-mode-map)) ;;Xemacs: korrekt??
    (setq outline-mode-map (nconc (make-sparse-keymap) text-mode-map)))
  (define-key outline-mode-map "\C-c" outline-mode-prefix-map)
  (define-key outline-mode-map [menu-bar] outline-mode-menu-bar-map))

(defcustom outline-minor-mode nil
  "Non-nil if using Outline mode as a minor mode of some other mode."
  :type 'boolean
  :group 'outlines)
(make-variable-buffer-local 'outline-minor-mode)
(or (assq 'outline-minor-mode minor-mode-alist)
    (setq minor-mode-alist (append minor-mode-alist
				   (list '(outline-minor-mode " Outl")))))

(defvar outline-font-lock-keywords
  '(;;
    ;; Highlight headings according to the level.
    (eval . (list (concat "^" outline-regexp ".+")
		  0 '(or (cdr (assq (outline-font-lock-level)
				    '((1 . font-lock-function-name-face)
				      (2 . font-lock-variable-name-face)
				      (3 . font-lock-keyword-face)
				      (4 . font-lock-builtin-face)
				      (5 . font-lock-comment-face)
				      (6 . font-lock-constant-face)
				      (7 . font-lock-type-face)
				      (8 . font-lock-string-face))))
			 font-lock-warning-face)
		  nil t)))
  "Additional expressions to highlight in Outline mode.")

(defun outline-font-lock-level ()
  (let ((count 1))
    (save-excursion
      (outline-back-to-heading)
      (condition-case nil
	  (while (not (bobp))
	    (outline-up-heading 1)
	    (setq count (1+ count)))
	(error (message "count stop in outline-font-lock-level"))))
    count))

(defvar outline-view-change-hook nil
  "Normal hook to be run after outline visibility changes.")

(defvar outline-mode-hook t
  "*This hook is run when outline mode starts.")

(defvar outline-blank-line t
  "*Non-nil means to leave unhidden blank line before heading.")

;;;###autoload
(defun outline-mode ()
  "Set major mode for editing outlines with selective display.
Headings are lines which start with asterisks: one for major headings,
two for subheadings, etc.  Lines not starting with asterisks are body lines.

Body text or subheadings under a heading can be made temporarily
invisible, or visible again.  Invisible lines are attached to the end
of the heading, so they move with it, if the line is killed and yanked
back.  A heading with text hidden under it is marked with an ellipsis (...).

Commands:\\<outline-mode-map>
\\[outline-next-visible-heading]   outline-next-visible-heading      move by visible headings
\\[outline-previous-visible-heading]   outline-previous-visible-heading
\\[outline-forward-same-level]   outline-forward-same-level        similar but skip subheadings
\\[outline-backward-same-level]   outline-backward-same-level
\\[outline-up-heading]   outline-up-heading		    move from subheading to heading

\\[hide-body]	make all text invisible (not headings).
\\[show-all]	make everything in buffer visible.
\\[hide-sublevels]  make only the first N levels of headers visible.

The remaining commands are used when point is on a heading line.
They apply to some of the body or subheadings of that heading.
\\[hide-subtree]   hide-subtree	make body and subheadings invisible.
\\[show-subtree]   show-subtree	make body and subheadings visible.
\\[show-children]   show-children	make direct subheadings visible.
		 No effect on body, or subheadings 2 or more levels down.
		 With arg N, affects subheadings N levels down.
\\[hide-entry]	   make immediately following body invisible.
\\[show-entry]	   make it visible.
\\[hide-leaves]	   make body under heading and under its subheadings invisible.
		     The subheadings remain visible.
\\[show-branches]  make all subheadings at all levels visible.

The variable `outline-regexp' can be changed to control what is a heading.
A line is a heading if `outline-regexp' matches something at the
beginning of the line.  The longer the match, the deeper the level.

Turning on outline mode calls the value of `text-mode-hook' and then of
`outline-mode-hook', if they are non-nil."
  (interactive)
  ;; SMW!!  (kill-all-local-variables)
  (use-local-map outline-mode-map)
  (setq mode-name "Outline")
  (setq major-mode 'outline-mode)
  (define-abbrev-table 'text-mode-abbrev-table ())
  (setq local-abbrev-table text-mode-abbrev-table)
  (set-syntax-table text-mode-syntax-table)
  (make-local-variable 'line-move-ignore-invisible)
  (setq line-move-ignore-invisible t)
  ;; Cause use of ellipses for invisible text.
  (add-to-invisibility-spec '(outline . t))
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat paragraph-start "\\|\\("
				outline-regexp "\\)"))
  ;; Inhibit auto-filling of header lines.
  (make-local-variable 'auto-fill-inhibit-regexp)
  (setq auto-fill-inhibit-regexp outline-regexp)
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate (concat paragraph-separate "\\|\\("
				   outline-regexp "\\)"))
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(outline-font-lock-keywords t))
  (make-local-variable 'change-major-mode-hook)
  (add-hook 'change-major-mode-hook 'show-all)
  (run-hooks 'text-mode-hook 'outline-mode-hook))

(defcustom outline-minor-mode-prefix "\C-c@"
  "*Prefix key to use for Outline commands in Outline minor mode.
The value of this variable is checked as part of loading Outline mode.
After that, changing the prefix key requires manipulating keymaps."
  :type 'string
  :group 'outlines)

(defvar outline-minor-mode-map nil)
(if outline-minor-mode-map
    nil
  (setq outline-minor-mode-map (make-sparse-keymap))
  (define-key outline-minor-mode-map [menu-bar]
    outline-mode-menu-bar-map)
  (define-key outline-minor-mode-map outline-minor-mode-prefix
    outline-mode-prefix-map))

(or (assq 'outline-minor-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'outline-minor-mode outline-minor-mode-map)
		minor-mode-map-alist)))

;;;###autoload
(defun outline-minor-mode (&optional arg)
  "Toggle Outline minor mode.
With arg, turn Outline minor mode on if arg is positive, off otherwise.
See the command `outline-mode' for more information on this mode."
  (interactive "P")
  (setq outline-minor-mode
	(if (null arg) (not outline-minor-mode)
	  (> (prefix-numeric-value arg) 0)))
  (if outline-minor-mode
      (progn
	(make-local-hook 'change-major-mode-hook)
	;; Turn off this mode if we change major modes.
	(add-hook 'change-major-mode-hook
		  '(lambda () (outline-minor-mode -1))
		  nil t)
	(make-local-variable 'line-move-ignore-invisible)
	(setq line-move-ignore-invisible t)
	;; Cause use of ellipses for invisible text.
	(add-to-invisibility-spec '(outline . t))
	(run-hooks 'outline-minor-mode-hook))
    (setq line-move-ignore-invisible nil)
    ;; Cause use of ellipses for invisible text.
    (remove-from-invisibility-spec '(outline . t)))
  ;; When turning off outline mode, get rid of any outline hiding.
  (or outline-minor-mode
      (show-all))
  (force-mode-line-update))

(defcustom outline-level 'outline-level
  "*Function of no args to compute a header's nesting level in an outline.
It can assume point is at the beginning of a header line."
  :type 'function
  :group 'outlines)

(defvar outline-heading-alist ()
  "Alist associating a heading for every possible level.
Each entry is of the form (HEADING . LEVEL).
This alist is used two ways: to find the heading corresponding to
a given level and to find the level of a given heading.
If a mode or document needs several sets of outline headings (for example
numbered and unnumbered sections), list them set by set and sorted by level
within each set.  For example in texinfo mode:

     (setq outline-heading-alist
      '((\"@chapter\" . 2) (\"@section\" . 3) (\"@subsection\" . 4)
           (\"@subsubsection\" . 5)
        (\"@unnumbered\" . 2) (\"@unnumberedsec\" . 3)
           (\"@unnumberedsubsec\" . 4)  (\"@unnumberedsubsubsec\" . 5)
        (\"@appendix\" . 2) (\"@appendixsec\" . 3)...
           (\"@appendixsubsec\" . 4) (\"@appendixsubsubsec\" . 5) ..))

Instead of sorting the entries in each set, you can also separate the
sets with nil.")
(make-variable-buffer-local 'outline-heading-alist)



;; This used to count columns rather than characters, but that made ^L
;; appear to be at level 2 instead of 1.  Columns would be better for
;; tab handling, but the default regexp doesn't use tabs, and anyone
;; who changes the regexp can also redefine the outline-level variable
;; as appropriate.

;;; Auskommentiert, da Buggy (SMW, 26.7.07)
;,(defun outline-level ()
;,  "Return the depth to which a statement is nested in the outline.
;,Point must be at the beginning of a header line.
;,This is actually either the level specified in `outline-heading-alist'
;,or else the number of characters matched by `outline-regexp'."
;,  (or (cdr (assoc (match-string 0) outline-heading-alist))
;,      (- (match-end 0) (match-beginning 0))))
;;; Ersatzfunktion SMW
(defun outline-level ()
  "Return the depth to which a statement is nested in the outline.
Point must be at the beginning of a header line.  This is actually
the number of characters that `outline-regexp' matches."
  (save-excursion
    (looking-at outline-regexp)
    (- (match-end 0) (match-beginning 0))))

(defun outline-next-preface ()
  "Skip forward to just before the next heading line.
If there's no following heading line, stop before the newline
at the end of the buffer."
  (if (re-search-forward (concat "\n\\(?:" outline-regexp "\\)")
			 nil 'move)
      (goto-char (match-beginning 0)))
  (if (and (bolp) (or outline-blank-line (eobp)) (not (bobp)))
      (forward-char -1)))

(defun outline-next-heading ()
  "Move to the next (possibly invisible) heading line."
  (interactive)
  (if (re-search-forward (concat "\n\\(" outline-regexp "\\)")
			 nil 'move)
      (goto-char (1+ (match-beginning 0)))))

(defsubst outline-visible ()
  "Non-nil if the character after point is visible."
  (not (get-char-property (point) 'invisible)))

(defun outline-previous-heading ()
  "Move to the previous (possibly invisible) heading line."
  (interactive)
  (re-search-backward (concat "^\\(?:" outline-regexp "\\)")
		      nil 'move))

(defsubst outline-invisible-p (&optional pos)
  "Non-nil if the character after point is invisible."
  (get-char-property (or pos (point)) 'invisible))

(defun outline-visible ()
  (not (outline-invisible-p)))
(make-obsolete 'outline-visible 'outline-invisible-p)

(defun outline-back-to-heading (&optional invisible-ok)
  "Move to previous heading line, or beg of this line if it's a heading.
Only visible heading lines are considered, unless INVISIBLE-OK is non-nil."
  (beginning-of-line)
  (or (outline-on-heading-p invisible-ok)
      (let (found)
	(save-excursion
	  (while (not found)
	    (or (re-search-backward (concat "^\\(?:" outline-regexp "\\)")
				    nil t)
		(error "before first heading"))
	    (setq found (and (or invisible-ok (not (outline-invisible-p)))
			     (point)))))
	(goto-char found)
	found)))

(defun outline-on-heading-p (&optional invisible-ok)
  "Return t if point is on a (visible) heading line.
If INVISIBLE-OK is non-nil, an invisible heading line is ok too."
  (save-excursion
    (beginning-of-line)
    (and (bolp) (or invisible-ok (not (outline-invisible-p)))
	 (looking-at outline-regexp))))

(defun outline-insert-heading ()
  "Insert a new heading at same depth at point."
  (interactive)
  (let ((head (save-excursion
		(condition-case nil
		    (outline-back-to-heading)
		  (error (outline-next-heading)))
		(if (eobp)
		    (or (caar outline-heading-alist) "")
		  (match-string 0)))))
    (unless (or (string-match "[ \t]\\'" head)
		(not (string-match (concat "\\`\\(?:" outline-regexp "\\)")
				   (concat head " "))))
      (setq head (concat head " ")))
    (unless (bolp) (end-of-line) (newline))
    (insert head)
    (unless (eolp)
      (save-excursion (newline-and-indent)))
    (run-hooks 'outline-insert-heading-hook)))

(defun outline-invent-heading (head up)
  (save-match-data
    ;; Let's try to invent one by repeating or deleting the last char.
    (let ((new-head (if up (substring head 0 -1)
                      (concat head (substring head -1)))))
      (if (string-match (concat "\\`\\(?:" outline-regexp "\\)")
                        new-head)
          ;; Why bother checking that it is indeed higher/lower level ?
          new-head
        ;; Didn't work, so ask what to do.
        (read-string (format "%s heading for `%s': "
                             (if up "Parent" "Demoted") head)
                     head nil nil)))))

(defun outline-promote (&optional children)
  "Promote headings higher up the tree.
If prefix argument CHILDREN is given, promote also all the children.
If the region is active in `transient-mark-mode', promote all headings
in the region."
  (interactive
   (list (if (and zmacs-regions (region-active-p)) 'region
	   (outline-back-to-heading)
	   (if current-prefix-arg nil 'subtree))))
  (cond
   ((eq children 'region)
    (outline-map-region 'outline-promote (region-beginning) (region-end)))
   (children
    (outline-map-region 'outline-promote
			(point)
			(save-excursion (outline-get-next-sibling) (point))))
   (t
    (outline-back-to-heading t)
    (let* ((head (match-string-no-properties 0))
	   (level (save-match-data (funcall outline-level)))
	   (up-head (or (outline-head-from-level (1- level) head)
			;; Use the parent heading, if it is really
			;; one level less.
			(save-excursion
			  (save-match-data
			    (outline-up-heading 1 t)
			    (and (= (1- level) (funcall outline-level))
				 (match-string-no-properties 0))))
                        ;; Bummer!! There is no lower level heading.
                        (outline-invent-heading head 'up))))

      (unless (rassoc level outline-heading-alist)
	(push (cons head level) outline-heading-alist))

      (replace-match up-head nil t)))))

(defun outline-demote (&optional children)
  "Demote headings lower down the tree.
If prefix argument CHILDREN is given, demote also all the children.
If the region is active in `transient-mark-mode', demote all headings
in the region."
  (interactive
   (list (if (and zmacs-regions (region-active-p)) 'region
	   (outline-back-to-heading)
	   (if current-prefix-arg nil 'subtree))))
  (cond
   ((eq children 'region)
    (outline-map-region 'outline-demote (region-beginning) (region-end)))
   (children
    (outline-map-region 'outline-demote
			(point)
			(save-excursion (outline-get-next-sibling) (point))))
   (t
    (let* ((head (match-string-no-properties 0))
	   (level (save-match-data (funcall outline-level)))
	   (down-head
	    (or (outline-head-from-level (1+ level) head)
		(save-excursion
		  (save-match-data
		    (while (and (progn (outline-next-heading) (not (eobp)))
				(<= (funcall outline-level) level)))
		    (when (eobp)
		      ;; Try again from the beginning of the buffer.
		      (goto-char (point-min))
		      (while (and (progn (outline-next-heading) (not (eobp)))
				  (<= (funcall outline-level) level))))
		    (unless (eobp)
		      (looking-at outline-regexp)
		      (match-string-no-properties 0))))
                ;; Bummer!! There is no higher-level heading in the buffer.
                (outline-invent-heading head nil))))

      (unless (rassoc level outline-heading-alist)
	(push (cons head level) outline-heading-alist))
      (replace-match down-head nil t)))))

(defun outline-head-from-level (level head &optional alist)
  "Get new heading with level LEVEL from ALIST.
If there are no such entries, return nil.
ALIST defaults to `outline-heading-alist'.
Similar to (car (rassoc LEVEL ALIST)).
If there are several different entries with same new level, choose
the one with the smallest distance to the assocation of HEAD in the alist.
This makes it possible for promotion to work in modes with several
independent sets of headings (numbered, unnumbered, appendix...)"
  (unless alist (setq alist outline-heading-alist))
  (let ((l (rassoc level alist))
	ll h hl l2 l2l)
    (cond
     ((null l) nil)
     ;; If there's no HEAD after L, any other entry for LEVEL after L
     ;; can't be much better than L.
     ((null (setq h (assoc head (setq ll (memq l alist))))) (car l))
     ;; If there's no other entry for LEVEL, just keep L.
     ((null (setq l2 (rassoc level (cdr ll)))) (car l))
     ;; Now we have L, L2, and H: see if L2 seems better than L.
     ;; If H is after L2, L2 is better.
     ((memq h (setq l2l (memq l2 (cdr ll))))
      (outline-head-from-level level head l2l))
     ;; Now we have H between L and L2.
     ;; If there's a separator between L and H, prefer L2.
     ((memq h (memq nil ll))
      (outline-head-from-level level head l2l))
     ;; If there's a separator between L2 and H, prefer L.
     ((memq l2 (memq nil (setq hl (memq h ll)))) (car l))
     ;; No separator between L and L2, check the distance.
     ((< (* 2 (length hl)) (+ (length ll) (length l2l)))
      (outline-head-from-level level head l2l))
     ;; If all else fails, just keep L.
     (t (car l)))))

(defun outline-map-region (fun beg end)
  "Call FUN for every heading between BEG and END.
When FUN is called, point is at the beginning of the heading and
the match data is set appropriately."
  (save-excursion
    (setq end (copy-marker end))
    (goto-char beg)
    (when (re-search-forward (concat "^\\(?:" outline-regexp "\\)") end t)
      (goto-char (match-beginning 0))
      (funcall fun)
      (while (and (progn
		    (outline-next-heading)
		    (< (point) end))
		  (not (eobp)))
	(funcall fun)))))

;; Vertical tree motion

(defun outline-move-subtree-up (&optional arg)
  "Move the currrent subtree up past ARG headlines of the same level."
  (interactive "p")
  (outline-move-subtree-down (- arg)))

(defun outline-move-subtree-down (&optional arg)
  "Move the currrent subtree down past ARG headlines of the same level."
  (interactive "p")
  (let ((movfunc (if (> arg 0) 'outline-get-next-sibling
		   'outline-get-last-sibling))
	(ins-point (make-marker))
	(cnt (abs arg))
	(tmp-string "")
	beg end folded)
    ;; Select the tree
    (outline-back-to-heading)
    (setq beg (point))
    (save-match-data
      (save-excursion (outline-end-of-heading)
		      (setq folded (outline-invisible-p)))
      (outline-end-of-subtree))
    (if (= (char-after) ?\n) (forward-char 1))
    (setq end (point))
    ;; Find insertion point, with error handling
    (goto-char beg)
    (while (> cnt 0)
      (or (funcall movfunc)
	  (progn (goto-char beg)
		 (error "Cannot move past superior level")))
      (setq cnt (1- cnt)))
    (if (> arg 0)
	;; Moving forward - still need to move over subtree
	(progn (outline-end-of-subtree)
	       (if (= (char-after) ?\n) (forward-char 1))))
    (move-marker ins-point (point))
    (setq tmp-string (buffer-substring beg end))
    (delete-region beg end)
    (insert tmp-string)
    (goto-char ins-point)
    (if folded (hide-subtree))
    (move-marker ins-point nil)))

(defun outline-end-of-heading ()
  (if (re-search-forward outline-heading-end-regexp nil 'move)
      (forward-char -1)))

(defun outline-next-visible-heading (arg)
  "Move to the next visible heading line.
With argument, repeats or can move backward if negative.
A heading line is one that starts with a `*' (or that
`outline-regexp' matches)."
  (interactive "p")
  (if (< arg 0)
      (beginning-of-line)
    (end-of-line))
  (while (and (not (bobp)) (< arg 0))
    (while (and (not (bobp))
		(re-search-backward (concat "^\\(?:" outline-regexp "\\)")
				    nil 'move)
		(outline-invisible-p)))
    (setq arg (1+ arg)))
  (while (and (not (eobp)) (> arg 0))
    (while (and (not (eobp))
		(re-search-forward (concat "^\\(?:" outline-regexp "\\)")
				   nil 'move)
		(outline-invisible-p (match-beginning 0))))
    (setq arg (1- arg)))
  (beginning-of-line))

(defun outline-previous-visible-heading (arg)
  "Move to the previous heading line.
With argument, repeats or can move forward if negative.
A heading line is one that starts with a `*' (or that
`outline-regexp' matches)."
  (interactive "p")
  (outline-next-visible-heading (- arg)))

(defun outline-mark-subtree ()
  "Mark the current subtree in an outlined document.
This puts point at the start of the current subtree, and mark at the end."
  (interactive)
  (let ((beg))
    (if (outline-on-heading-p)
	;; we are already looking at a heading
	(beginning-of-line)
      ;; else go back to previous heading
      (outline-previous-visible-heading 1))
    (setq beg (point))
    (outline-end-of-subtree)
    (push-mark (point) nil t)
    (goto-char beg)))


(defun outline-discard-extents (&optional beg end)
  "Clear BEG and END of overlays whose property NAME has value VAL.
Overlays might be moved and/or split.
BEG and END default respectively to the beginning and end of buffer."
  (unless beg (setq beg (point-min)))
  (unless end (setq end (point-max)))
  (if (< end beg)
      (setq beg (prog1 end (setq end beg))))
  (save-excursion
    (map-extents 
     #'(lambda (ex ignored) 
	 (if (< (extent-start-position ex) beg)
	    (if (> (extent-end-position ex) end)
		(progn
		  (set-extent-endpoints (copy-extent ex)
					(extent-start-position ex) beg)
		  (set-extent-endpoints ex end (extent-end-position ex)))
	      (set-extent-endpoints ex (extent-start-position ex) beg)))
	  (if (> (extent-end-position ex) end)
	      (set-extent-endpoints ex end (extent-end-position ex))
	       (delete-extent ex)))
     (current-buffer) beg end nil 'end-closed 'outline)))

;;; SMW !!! wegen der Kompatibilität zum project-mode: Einfach Überladen der Funktion, 
;;;     (wird im project-mode nochmals überladen!)
(defun outline-discard-extents (from to)
  "Delete hideshow extents in region defined by FROM and TO."
  (when (< to from)
    (setq from (prog1 to (setq to from))))
  (map-extents #'(lambda (ex ignored) (delete-extent ex))
	       (current-buffer) from to nil 'end-closed 'outline))

(defun outline-flag-region (from to flag)
  "Hide or show lines from FROM to TO, according to FLAG.
If FLAG is nil then text is shown, while if FLAG is t the text is hidden."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char from)
      (end-of-line)
      (outline-discard-overlays (point) to 'outline)
      (if flag
          (let ((o (make-overlay (point) to)))
            (overlay-put o 'invisible 'outline)
	    (overlay-put o 'isearch-open-invisible
			 'outline-isearch-open-invisible)))))
  (run-hooks 'outline-view-change-hook))
  


;; Function to be set as an outline-isearch-open-invisible' property
;; to the overlay that makes the outline invisible (see
;; `outline-flag-region').
(defun outline-isearch-open-invisible (overlay)
  (save-excursion
    (goto-char (overlay-start overlay))
    (show-entry)))


;; Exclude from the region BEG ... END all overlays
;; which have PROP as the value of the `invisible' property.
;; Exclude them by shrinking them to exclude BEG ... END,
;; or even by splitting them if necessary.
;; Overlays without such an `invisible' property are not touched.
(defun outline-discard-overlays (beg end prop)
  (if (< end beg)
      (setq beg (prog1 end (setq end beg))))
  (save-excursion
    (let ((overlays (overlays-in beg end))
	  o
	  o1)
      (while overlays
	(setq o (car overlays))
	(if (eq (overlay-get o 'invisible) prop)
	    ;; Either push this overlay outside beg...end
	    ;; or split it to exclude beg...end
	    ;; or delete it entirely (if it is contained in beg...end).
	    (if (< (overlay-start o) beg)
		(if (> (overlay-end o) end)
		    (progn
		      (setq o1 (outline-copy-overlay o))
		      (move-overlay o1 (overlay-start o1) beg)
		      (move-overlay o end (overlay-end o)))
		  (move-overlay o (overlay-start o) beg))
	      (if (> (overlay-end o) end)
		  (move-overlay o end (overlay-end o))
		(delete-overlay o))))
	(setq overlays (cdr overlays))))))

;; Make a copy of overlay O, with the same beginning, end and properties.
(defun outline-copy-overlay (o)
  (let ((o1 (make-overlay (overlay-start o) (overlay-end o)
			  (overlay-buffer o)))
	(props (overlay-properties o)))
    (while props
      (overlay-put o1 (car props) (nth 1 props))
      (setq props (cdr (cdr props))))
    o1))

(defun hide-entry ()
  "Hide the body directly following this heading."
  (interactive)
  (outline-back-to-heading)
  (outline-end-of-heading)
  (save-excursion
   (outline-flag-region (point) (progn (outline-next-preface) (point)) t)))

(defun show-entry ()
  "Show the body directly following this heading.
Show the heading too, if it is currently invisible."
  (interactive)
  (save-excursion
    (outline-back-to-heading t)
    (outline-flag-region (1- (point))
			 (progn (outline-next-preface) (point)) nil)))

(defun hide-body ()
  "Hide all of buffer except headings."
  (interactive)
  (hide-region-body (point-min) (point-max)))

(defun hide-region-body (start end)
  "Hide all body lines in the region, but not headings."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (if (outline-on-heading-p)
	  (outline-end-of-heading))
      (while (not (eobp))
	(outline-flag-region (point)
			     (progn (outline-next-preface) (point)) t)
	(if (not (eobp))
	    (progn
	      (forward-char
	       (if (looking-at "\n\n")
		   2 1))
	      (outline-end-of-heading)))))))

(defun show-all ()
  "Show all of the text in the buffer."
  (interactive)
  (outline-flag-region (point-min) (point-max) nil))

(defun hide-subtree ()
  "Hide everything after this heading at deeper levels."
  (interactive)
  (outline-flag-subtree t))

(defun hide-leaves ()
  "Hide all body after this heading at deeper levels."
  (interactive)
  (outline-back-to-heading)
  (outline-end-of-heading)
  (hide-region-body (point) (progn (outline-end-of-subtree) (point))))

(defun show-subtree ()
  "Show everything after this heading at deeper levels."
  (interactive)
  (if (not (looking-at "\\*")) ; only if outline regex is standard
      (outline-up-heading 0))
  (outline-flag-subtree nil))

(defun outline-show-heading ()
  "Show the current heading and move to its end."
  (outline-flag-region (- (point)
 			  (if (bobp) 0
 			    (if (and outline-blank-line
                                     (eq (char-before (1- (point))) ?\n))
 				2 1)))
		       (progn (outline-end-of-heading) (point))
		       nil))

(defun hide-sublevels (levels)
  "Hide everything but the top LEVELS levels of headers, in whole buffer."
  (interactive "p")
  (if (< levels 1)
      (error "Must keep at least one level of headers"))
  (setq levels (1- levels))
  (save-excursion
    (goto-char (point-min))
    ;; Keep advancing to the next top-level heading.
    (while (or (and (bobp) (outline-on-heading-p))
	       (outline-next-heading))
      (let ((end (save-excursion (outline-end-of-subtree) (point))))
	;; Hide everything under that.
	(outline-flag-region (point) end t)
	;; Show the first LEVELS levels under that.
	(if (> levels 0)
	    (show-children levels))
	;; Move to the next, since we already found it.
	(goto-char end)))))


;;;SMW ff
;;; Angleich der Verhaltensweise an den emacs19 - war für meinen Geschmack auch besser
;;; - diese hier ist deshalb auch aus dem emacs19 
; (defun hide-other ()
;   "Hide everything except for the current body and the parent headings."
;   (interactive)
;   (hide-sublevels 1)
;   (let ((last (point))
; 	(pos (point)))
;     (while (save-excursion
; 	     (and (re-search-backward "[\n\r]" nil t)
; 		  (eq (following-char) ?\r)))
;       (save-excursion
; 	(beginning-of-line)
; 	(if (eq last (point))
; 	    (progn
; 	      (outline-next-heading)
; 	      (outline-flag-region last (point) ?\n))
; 	  (show-children)
; 	  (setq last (point)))))))
;;;SMW
;;; und hier das Orginal vom emacs20
; (defun hide-other ()
;   "Hide everything except current body and parent and top-level headings."
;   (interactive)
;   (hide-sublevels 1)
;   (save-excursion
;     (outline-back-to-heading t)
;     (show-entry)
;     (while (condition-case nil (progn (outline-up-heading 1) t) (error nil))
;       (outline-flag-region (1- (point))
; 			   (save-excursion (forward-line 1) (point))
; 			   nil))))
(defun hide-other ()
  "Hide everything except current body and parent and top-level headings."
  (interactive)
  (hide-sublevels 1)
  (let (outline-view-change-hook)
    (save-excursion
      (outline-back-to-heading t)
      (show-entry)
      (while (condition-case nil (progn (outline-up-heading 1 t) (not (bobp)))
	       (error nil))
	(outline-flag-region (max 1 (1- (point)))
			     (save-excursion (forward-line 1) (point))
			     nil))))
  (run-hooks 'outline-view-change-hook))



(defun outline-toggle-children ()
  "Show or hide the current subtree depending on its current state."
  (interactive)
  (save-excursion
    (outline-back-to-heading)
    (if (not (outline-invisible-p (point-at-eol)))
	(hide-subtree)
      (show-children)
      (show-entry))))

(defun outline-flag-subtree (flag)
  (save-excursion
    (outline-back-to-heading)
    (outline-end-of-heading)
    (outline-flag-region (point)
			  (progn (outline-end-of-subtree) (point))
			  flag)))

(defun outline-end-of-subtree ()
  (outline-back-to-heading)
  (let ((first t)
	(level (funcall outline-level)))
    (while (and (not (eobp))
		(or first (> (funcall outline-level) level)))
      (setq first nil)
      (outline-next-heading))
    (if (bolp)
	(progn
	  ;; Go to end of line before heading
	  (forward-char -1)
          (if (and outline-blank-line (bolp))
 	      ;; leave blank line before heading
 	      (forward-char -1))))))

(defun show-branches ()
  "Show all subheadings of this heading, but not their bodies."
  (interactive)
  (show-children 1000))

(defun show-children (&optional level)
  "Show all direct subheadings of this heading.
Prefix arg LEVEL is how many levels below the current level should be shown.
Default is enough to cause the following heading to appear."
  (interactive "P")
  (setq level
	(if level (prefix-numeric-value level)
	  (save-excursion
	    (outline-back-to-heading)
	    (let ((start-level (funcall outline-level)))
	      (outline-next-heading)
	      (if (eobp)
		  1
		(max 1 (- (funcall outline-level) start-level)))))))
  (save-excursion
    (save-restriction
      (outline-back-to-heading)
      (setq level (+ level (funcall outline-level)))
      (narrow-to-region (point)
			(progn (outline-end-of-subtree)
			       (if (eobp) (point-max) (1+ (point)))))
      (goto-char (point-min))
      (while (and (not (eobp))
		  (progn
		    (outline-next-heading)
		    (not (eobp))))
	(if (<= (funcall outline-level) level)
	    (save-excursion
	      (outline-flag-region (save-excursion
				     (forward-char -1)
				     (if (bolp)
					 (forward-char -1))
				     (point))
				   (progn (outline-end-of-heading) (point))
				   nil)))))))


(defun outline-up-heading (arg &optional invisible-ok)
  "Move to the visible heading line of which the present line is a subheading.
With argument, move up ARG levels.
If INVISIBLE-OK is non-nil, also consider invisible lines."
  (interactive "p")
  (and (eq this-command 'outline-up-heading)
       (or (eq last-command 'outline-up-heading) (push-mark)))
  (outline-back-to-heading invisible-ok)
  (let ((start-level (funcall outline-level)))
    (if (eq start-level 1)
	(error "Already at top level of the outline"))
    (while (and (> start-level 1) (> arg 0) (not (bobp)))
      (let ((level start-level))
	(while (not (or (< level start-level) (bobp)))
	  (if invisible-ok
	      (outline-previous-heading)
	    (outline-previous-visible-heading 1))
	  (setq level (funcall outline-level)))
	(setq start-level level))
      (setq arg (- arg 1))))
  (looking-at outline-regexp))

(defun outline-forward-same-level (arg)
  "Move forward to the ARG'th subheading at same level as this one.
Stop at the first and last subheadings of a superior heading."
  (interactive "p")
  (outline-back-to-heading)
  (while (> arg 0)
    (let ((point-to-move-to (save-excursion
			      (outline-get-next-sibling))))
      (if point-to-move-to
	  (progn
	    (goto-char point-to-move-to)
	    (setq arg (1- arg)))
	(progn
	  (setq arg 0)
	  (error "No following same-level heading"))))))

(defun outline-get-next-sibling ()
  "Move to next heading of the same level, and return point or nil if none."
  (let ((level (funcall outline-level)))
    (outline-next-visible-heading 1)
    (while (and (not (eobp)) (> (funcall outline-level) level))
      (outline-next-visible-heading 1))
    (if (or (eobp) (< (funcall outline-level) level))
	nil
      (point))))

(defun outline-backward-same-level (arg)
  "Move backward to the ARG'th subheading at same level as this one.
Stop at the first and last subheadings of a superior heading."
  (interactive "p")
  (outline-back-to-heading)
  (while (> arg 0)
    (let ((point-to-move-to (save-excursion
			      (outline-get-last-sibling))))
      (if point-to-move-to
	  (progn
	    (goto-char point-to-move-to)
	    (setq arg (1- arg)))
	(progn
	  (setq arg 0)
	  (error "No previous same-level heading"))))))

(defun outline-get-last-sibling ()
  "Move to previous heading of the same level, and return point or nil if none."
  (let ((level (funcall outline-level)))
    (outline-previous-visible-heading 1)
    (while (and (> (funcall outline-level) level)
		(not (bobp)))
      (outline-previous-visible-heading 1))
    (if (< (funcall outline-level) level)
	nil
        (point))))

(defun outline-headers-as-kill (beg end)
  "Save the visible outline headers in region at the start of the kill ring.

Text shown between the headers isn't copied.  Two newlines are
inserted between saved headers.  Yanking the result may be a
convenient way to make a table of contents of the buffer."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((buffer (current-buffer))
	    start end)
	(with-temp-buffer
	  (with-current-buffer buffer
	    ;; Boundary condition: starting on heading:
	    (when (outline-on-heading-p)
	      (outline-back-to-heading)
	      (setq start (point)
		    end (progn (outline-end-of-heading)
			       (point)))
	      (insert-buffer-substring buffer start end)
	      (insert "\n\n")))
	  (let ((temp-buffer (current-buffer)))
	    (with-current-buffer buffer
	      (while (outline-next-heading)
		(unless (outline-invisible-p)
		  (setq start (point)
			end (progn (outline-end-of-heading) (point)))
		  (with-current-buffer temp-buffer
		    (insert-buffer-substring buffer start end)
		    (insert "\n\n"))))))
	  (kill-new (buffer-string)))))))

;;; outline.el ends here

           )
	  (require 'org-install)
	  (org-mode)
	  )
      (require 'op-org-table)
      )
  )
;;;==========================================================================


(message "Org-Mode ist geladen")




(defvar outline-readjust-position-percentage 30
  "Wieviel Prozent vom Bildschirm sollen nach Aktionen über dem Bildschirm liegen?")



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
      (require 'x-popup-menu)


      ;;
      ;; SMW: try to make XEmacs overlay compatible, dangerous!!
      ;;

      (unless (fboundp 'propertize)
	(defun propertize (string &rest props)
	  ;; stolen from erc's erc-propertize
	  (let ((str (copy-sequence string)))
	    (while props
	      (put-text-property 0 (length string)
				 (car props)
				 (cadr props)
				 str)
	      (setq props (cddr props)))
	    str)))

      (unless (fboundp 'float-time)
	(require 'time-date)
	(defun float-time (&optional specified-time)
	  (time-to-seconds (or specified-time
			       (current-time)))))

      (unless (fboundp 'remove-overlays)
	(defun remove-overlays (&optional beg end name val)
	  "Clear BEG and END of overlays whose property NAME has value VAL.
Overlays might be moved and or split. "
	  ;; Stolen from planner as planner-remove-overlays
	  (if (< end beg)
	      (setq beg (prog1 end (setq end beg))))
	  (save-excursion
	    (dolist (o (overlays-in beg end))
	      (when (eq (overlay-get o name) val)
		;; Either push this overlay outside beg...end
		;; or split it to exclude beg...end
		;; or delete it entirely (if it is contained in beg...end).
		(if (< (overlay-start o) beg)
		    (if (> (overlay-end o) end)
			(progn
			  (move-overlay (planner-copy-overlay o)
					(overlay-start o) beg)
			  (move-overlay o end (overlay-end o)))
		      (move-overlay o (overlay-start o) beg))
		  (if (> (overlay-end o) end)
		      (move-overlay o end (overlay-end o))
		    (delete-overlay o))))))))
      )) ; GEFÄHRLICH!





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; für temporäre Buffer brauche ich die Filedefinition
(if (not buffer-file-name) (setq buffer-file-name "*tmp*"))
(if (and (not (boundp 'buffer-file-truename))
	 (not buffer-file-truename)) 
    (setq buffer-file-truename "*tmp*"))


(message (concat "Laden des Buffers " buffer-file-truename))
(switch-to-buffer (file-name-nondirectory buffer-file-name))
(message (concat "... und ist im Buffer " (prin1-to-string (current-buffer))))



(let ((buffer-was-read-only buffer-read-only)) ;; dauert an bis zum Ende dieses Files


(if gnuemacsp
    (if window-system
	(progn
;;;;;;;includeforelc!
;;;;;;(load "hilit19.variation")

;;; hilit19.el --- customizable highlighting for Emacs19

;; Copyright (c) 1993, 1994 Free Software Foundation, Inc.

;; Author:   Jonathan Stigelman <stig@hackvan.com>
;; Maintainer: FSF
;;	(actually no longer maintained)
;; Keywords: faces

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Hilit19.el is a customizable highlighting package for Emacs19.  It supports
;; not only source code highlighting, but also Info, RMAIL, VM, gnus...
;; Hilit19 knows (or thinks it knows) how to highlight emacs buffers in
;; about 25 different modes.
;; 
;; WHERE TO GET THE LATEST VERSIONS OF HILIT19.EL (beta and release):
;;
;;      http://hackvan.com/pub/stig/src/elisp/
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TO SUBMIT BUG REPORTS (or feedback of any sort)...
;;
;;    M-x hilit-submit-feedback RET
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; hilit19.el,v 2.19 1993/09/08 18:44:10 stig Release
;;
;; LCD Archive Entry:
;; hilit19|Jonathan Stigelman|stig@hackvan.com|
;; Comprehensive (and comparatively fast) regex-based highlighting for Emacs 19|
;; 1993/09/08 18:44:10|Release 2.19|~/packages/hilit19.el.Z|
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; GENERAL OVERVIEW
;;
;;      This package installs numerous hooks to colorfully highlight your
;;      source code buffers as well as mail and news buffers.  Most 
;;      programming languages have predefined highlighting patterns.
;;	Just load hilit19 and files will be automatically highlighted as
;;      they're loaded.
;;
;;      Rehighlight a buffer by typing C-S-l (control-shift-lowercase-L).
;;
;;      If, when you edit the buffer, the coloring gets messed up, just
;;      redraw and the coloring will be adjusted.  If automatic highlighting
;;      in the current buffer has been turned off, then typing C-u C-S-l will
;;	force a rehighlight of the entire buffer.
;;
;;      Hilit19 can build faces by examining the names that you give to them
;;	For example, green/black-bold-italic-underline would be created as
;;	a face with a green foreground, and a black background, using a 
;;	bold-italic font...with underlining for good measure.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SETUP -- In your .emacs:
;;
;; 
;; (cond (window-system
;;        (setq hilit-mode-enable-list  '(not text-mode)
;;              hilit-background-mode   'light
;;              hilit-inhibit-hooks     nil
;;              hilit-inhibit-rebinding nil)
;; 
;;        (require 'hilit19)
;;        ))
;; 
;; If you like font-lock-mode and want to use both packages, then you can
;; disable hilit for the modes in which you want to use font-lock by listing
;; said modes in hilit-mode-enable-list.
;; 
;;      (hilit-translate type     'RoyalBlue   ; enable highlighting in C/C++
;;			 string	  nil)         ; disable string highlighting
;;
;; To get 100% of the utility of hilit19, you may also have to apply the
;; patches below for info.el and vm5.33L_19/vm-summary.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SETUP -- Are you using the right font for Emacs?
;;
;; Emacs cannot properly find bold and italic fonts unless you specify a
;; verbose X11 font name.  If you specify a font for emacs in your
;; .Xdefaults, it *MUST* be specified using the long form of the font name.
;; Here's a good font menu:
;;
;; (setq
;;  x-fixed-font-alist
;;  '("Font Menu"
;;    ("Misc"
;;     ("6x12" "-misc-fixed-medium-r-semicondensed--12-110-75-75-c-60-*-1")
;;     ("6x13" "-misc-fixed-medium-r-semicondensed--13-120-75-75-c-60-*-1")
;;     ("lucida 13"
;;      "-b&h-lucidatypewriter-medium-r-normal-sans-0-0-0-0-m-0-*-1")
;;     ("7x13" "-misc-fixed-medium-r-normal--13-120-75-75-c-70-*-1")
;;     ("7x14" "-misc-fixed-medium-r-normal--14-130-75-75-c-70-*-1")
;;     ("9x15" "-misc-fixed-medium-r-normal--15-140-*-*-c-*-*-1")
;;     ("")
;;     ("clean 8x8" "-schumacher-clean-medium-r-normal--*-80-*-*-c-*-*-1")
;;     ("clean 8x14" "-schumacher-clean-medium-r-normal--*-140-*-*-c-*-*-1")
;;     ("clean 8x10" "-schumacher-clean-medium-r-normal--*-100-*-*-c-*-*-1")
;;     ("clean 8x16" "-schumacher-clean-medium-r-normal--*-160-*-*-c-*-*-1")
;;     ("")
;;     ("sony 8x16" "-sony-fixed-medium-r-normal--16-120-100-100-c-80-*-1")
;;     ("")
;;     ("-- Courier --")
;;     ("Courier 10" "-adobe-courier-medium-r-normal--*-100-*-*-m-*-*-1")
;;     ("Courier 12" "-adobe-courier-medium-r-normal--*-120-*-*-m-*-*-1")
;;     ("Courier 14" "-adobe-courier-medium-r-normal--*-140-*-*-m-*-*-1")
;;     ("Courier 18" "-adobe-courier-medium-r-normal--*-180-*-*-m-*-*-1")
;;     ("Courier 18-b" "-adobe-courier-bold-r-normal--*-180-*-*-m-*-*-1")
;;     )))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; KNOWN BUGS/TO DO LIST/HELP WANTED/APPLY WITHIN
;;
;; * unbalanced, unescaped double quote characters can confuse hilit19.
;;   This will be fixed someday, so don't bug me about it.
;;
;; * ALTHOUGH HILIT19 IS FASTER THAN FONT-LOCK-MODE...
;;   For various reasons, the speed of the package could still stand to be
;;   improved.  If you care to do a little profiling and make things tighter...
;;
;; * hilit-toggle-highlight is flaky when auto-rehighlight is neither t nor nil.
;;   Does anyone actually USE this?  I think I might just remove it.
;;
;; PROJECTS THAT YOU CAN TAKE OVER BECAUSE I DON'T MUCH CARE ABOUT THEM...
;;
;; * Moved hilit-wysiwyg-replace here from my version of man.el, this is not
;;   a bug.  The bug is that I don't have a reverse operation yet...just a
;;   stub Wysiwyg-anything really belongs in a package of its own.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Thanks to the following people for their input:
;;    ebert@enpc.enpc.fr (Rolf EBERT), ada, LaTeX & bibtex highlights
;;    Vivek Khera <khera@cs.duke.edu>, gnus hooks + random advice & patches
;;    brian@athe.WUstl.EDU (Brian Dunford-Shore), prolog highlights
;;    John Ladwig <jladwig@soils.umn.edu>, 1st pass nroff highlights
;;    campo@sunthpi3.difi.unipi.it (Massimo Campostrini), fortran highlights
;;    jayb@laplace.MATH.ColoState.EDU (Jay Bourland), 1st pass dired
;;    Yoshio Turner <yoshio@CS.UCLA.EDU>, modula 2 highlights
;;    Fritz Knabe <knabe@ecrc.de>, advice & patches
;;    Alon Albert <alon@milcse.rtsg.mot.com>, advice & patches
;;    dana@thumper.bellcore.com (Dana A. Chee), working on the multi-frame bug
;;    derway@ndc.com (Don Erway), for breaking it...
;;    moss_r@summer.chem.su.oz.au (Richard Moss), first pass at add-pattern
;;    Olivier Lecarme <ol@aiguemarine.unice.fr>, Pascal & Icon patterns
;;
;; With suggestions and minor regex patches from numerous others...
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; hilit19.el,v
;; Revision 2.19  1993/09/08  18:44:10  stig
;; installed patch for elusive bug in hilit-rehighlight-region that caused
;; hilit-unhighlight-region to hang in an infinite loop.
;;
;; Revision 2.18  1993/08/27  03:51:00  stig
;; minor mods to lisp-mode and c/c++ mode patterns
;;
;; Revision 2.17  1993/08/25  02:19:17  stig
;; work-around for bug in next-overlay-change that caused dired and jargon-mode
;; to hang in an endless loop.  Perhaps other modes were doing this too.
;;
;; Revision 2.16  1993/08/22  19:46:00  stig
;; bug fix for next-overlay-change and accompanying change to
;; hilit-unhighlight-region
;;
;; Revision 2.15  1993/08/20  12:16:22  stig
;; minor change to fortran patterns
;;
;; Revision 2.14  1993/08/17  14:12:10  stig
;; added default face mapping for 'formula' which is needed for new latex
;; patterns.
;;
;; twiddled the calendar-mode patterns a bit.
;;
;; Revision 2.13  1993/08/16  04:33:54  stig
;; hilit-set-mode-patterns was screwing up two part patterns.  it doesn't now.
;;
;; Revision 2.12  1993/08/16  00:16:41  stig
;; changed references to default-bold-italic to just bold-italic because the
;;   font for that face is maintained by emacs.
;;
;; the pattern matcher now starts its searches from the end of the most
;;   recently highlighted region (which is not necessarily the end of the most
;;   recently matched regex).
;;
;; multiple errors in pattern matcher now just give an error instead of lots of
;;   annoying messages and dings.
;;
;; no longer use vm-summary-mode-hooks.
;;
;; some code moved from hilit-highlight-region to hilit-set-mode-patterns.
;;   This will affect you if you pass your patterns directly to
;;   hilit-highlight-region....use a pseudo-mode instead.
;;
;; pattern changes to C/C++, latex, texinfo, fortran, nroff, etc.
;;
;; Revision 2.11  1993/08/13  12:12:37  stig
;; removed some crufty commented-out code
;;
;; diverged lisp-mode and emacs-lisp-mode...also added lisp keywords.
;;
;; Revision 2.10  1993/08/13  09:47:06  stig
;; added calendar-mode, icon-mode and pascal-mode patterns
;;
;; commented out hilit-toggle-highlight because I want to phase it out entirely
;;
;; Revision 2.9  1993/08/13  08:44:22  stig
;; added optional case-fold argument to hilit-set-mode-patterns, this case-fold
;; parameter is now stored in hilit-patterns-alist.
;;
;; Revision 2.8  1993/08/12  22:05:03  stig
;; fixed some typos in documentation
;;
;; twiddled some of the color defaults for dark backgrounds
;;
;; always get 'mono color defaults if (not (x-display-color-p))
;;
;; added hilit-rehighlight-buffer-quietly to dired-after-readin-hook
;;
;; fixed bug in hilit-string-find that mishandled strings of the form: "\\"
;;
;; NEW FUNCTION: hilit-add-mode-pattern...  kinda like add-hook for patterns
;;
;; fixed minor pattern bugs for latex-mode and emacs-lisp-mode
;;
;; Revision 2.7  1993/07/30  02:43:01  stig
;; added const to the list of modifiers for C/C++ types
;;
;; Revision 2.6  1993/07/30  00:30:54  stig
;; now permit selection of arbitrary subexpressions for highlighting...
;; fixed keyword patterns for C/C++ using this technique.
;;
;; Revision 2.5  1993/07/28  05:02:56  stig
;; improvements to makefile regular expressions
;; removed about 130 lines just by compacting the big defconst for
;;   hilit-face-translation-table into a mapcar and defining a separate table
;;   of default faces.
;;
;; Revision 2.4  1993/07/27  14:09:05  stig
;; documented another "known problem" to "head off gripe mail at the pass."
;;
;; Revision 2.3  1993/07/27  02:15:49  stig
;; (hilit-lookup-face-create) incorporated patch which improves its behavior
;; with more than one frame...  Still can't have bold on the same face in two
;; different fonts sizes at the same time...
;;
;; Revision 2.2  1993/07/27  02:02:59  stig
;; vastly improved the makefile patterns
;; added hook for mh-show-mode
;;
;; Revision 2.1  1993/07/24  17:46:21  stig
;; Phasing out Info-select-hook...  Version 19.18 will use Info-selection-hook.
;;
;; Revision 2.0  1993/07/24  13:50:10  stig
;; better documentation and added the function hilit-submit-feedback.
;; C-S-l (control shift l) repaints the buffer.  Other bindings are optional.
;; multi-line highlights no longer cause problems when
;;   hilit-auto-rehighlight is 'visible
;; added hilit-predefined-face-list...
;; changed name of hilit-mode-alist to hilit-patterns-alist
;; added hilit-message-quietly to mail-setup-hook
;; added hilit-parser-alist which can be used to apply different patterns to
;;   different parts of a buffer.  This could be integrated in a far more
;;   elegant manner, but it presently serves the purpose of not applying
;;   message header patterns to message bodies in mail-mode and its kin.
;; hilit-set-mode-patterns now takes a list of modes and an optional parse-fn
;;

;;;;;; AND THIS CAN BE APPLIED TO VM 5.33L_19
;; 
;; *** ../site/vm5.33L_19/vm-summary.el    Fri Jun  4 22:17:11 1993
;; --- ./vm-summary.el     Tue Jun 22 16:39:30 1993
;; ***************
;; *** 152,158 ****
;;                   (insert "->")
;;                   (delete-char 2)
;;                   (forward-char -2)
;; !                 (and w vm-auto-center-summary (vm-auto-center-summary))))
;;             (and old-window (select-window old-window)))))))
;;   
;;   (defun vm-mark-for-display-update (message)
;; --- 152,159 ----
;;                   (insert "->")
;;                   (delete-char 2)
;;                   (forward-char -2)
;; !                 (and w vm-auto-center-summary (vm-auto-center-summary))
;; !                 (run-hooks 'vm-summary-pointer-hook)))
;;             (and old-window (select-window old-window)))))))
;;   
;;   (defun vm-mark-for-display-update (message)
;; 
;;;;;;

;;; Code:

;; User Options:

(defvar hilit-quietly nil
  "* If non-nil, this inhibits progress indicators during highlighting")

(defvar hilit-auto-highlight t
  "* T if we should highlight all buffers as we find 'em, nil to disable
  automatic highlighting by the find-file hook.")

(defvar hilit-auto-highlight-maxout 60000 ; hilit19 keeps getting bigger...
  "* auto-highlight is disabled in buffers larger than this")

(defvar hilit-auto-rehighlight t
  "* If this is non-nil, then hilit-redraw and hilit-recenter will also
  rehighlight part or all of the current buffer.  T will rehighlight the
  whole buffer, a NUMBER will rehighlight that many lines before and after
  the cursor, and the symbol 'visible' will rehighlight only the visible
  portion of the current buffer.  This variable is buffer-local.")

(make-variable-buffer-local 'hilit-auto-rehighlight)

(defvar hilit-auto-rehighlight-fallback '(20000 . 100)
  "* Cons of the form (THRESHOLD . FALLBACK), where FALLBACK is assigned to
  hilit-auto-rehighlight if the size of a newly opened buffer is larger than
  THRESHOLD.")

(defvar hilit-face-check t
  "* T slows down highlighting but permits the user to change fonts without
  losing bold and italic faces...  T causes hilit-lookup-face-create to dig
  through the frame parameters for the current window every time it's called.
  If you never change fonts in emacs, set this to NIL.")

;; Variables which must be set before loading hilit19.

(defvar hilit-inhibit-rebinding nil
  "If non-nil, this inhibits replacement of recenter, yank, and yank-pop.")

(defvar hilit-inhibit-hooks nil
  "If non-nil, this inhibits installation of hooks for Info, gnus, & vm.")

(defvar hilit-background-mode 'light
  "'mono inhibits color, 'dark or 'light indicate the background brightness.")

(defvar hilit-mode-enable-list nil
  "If a list of modes to exclusively enable or specifically disable.
The sense of the list is negated if it begins with the symbol 'not'.
Set this variable before you load hilit19.

Ex:  (perl-mode jargon-mode c-mode)	; just perl, C, and jargon modes
     (not text-mode)			; all modes except text mode")

;; Variables that are not generally modified directly

(defvar hilit-parser-alist nil
  "alist of major-mode values and parsers called by hilit-rehighlight-buffer.

Parsers for a given mode are IGNORED for partial rehighlights...maybe you'd
like to make this more universal?")

(defvar hilit-patterns-alist nil
  "alist of major-mode values and default highlighting patterns

A highlighting pattern is a list of the form (start end face), where
start is a regex, end is either a regex or a match number for start, and face
is the name of an entry in hilit-face-translation-table, the name of a face,
or nil (which disables the pattern).

Each entry in the alist is of the form:
	(mode . (case-fold pattern [pattern ...]))

See the hilit-lookup-face-create documentation for valid face names.")

(defvar hilit-predefined-face-list (face-list)
  "List of faces with which hilit-lookup-face-create will NOT tamper.

If hilit19 is dumped into emacs at your site, you may have to set this in
your init file.")

(eval-when-compile (setq byte-optimize t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use this to report bugs:

(eval-when-compile (require 'reporter))	; no compilation gripes

(defun hilit-submit-feedback ()
  "Submit feedback on hilit19 to hilit@hackvan.com"
  (interactive)
  (require 'reporter)
  (and (y-or-n-p "Do you really want to submit a report on hilit19? ")
       (reporter-submit-bug-report
	"Jonathan Stigelman <hilit@hackvan.com>"
	"hilit19.el (Release 2.19)"
	(and (y-or-n-p "Do you need to include a dump hilit variables? ")
	     (append
	      '(
		hilit-quietly    		hilit-inhibit-hooks
		hilit-background-mode		hilit-mode-enable-list
		hilit-auto-highlight		hilit-auto-highlight-maxout
		hilit-auto-rehighlight		hilit-auto-rehighlight-fallback
		hilit-face-check
		)
	      (and (y-or-n-p "Have you modified the standard patterns? ")
		   (yes-or-no-p "Are your patterns *REALLY* relevant? ")
		   '(hilit-parser-alist
		     hilit-patterns-alist
		     hilit-predefined-face-list
		     ))))
	 (function
	  (lambda ()
	    (and (y-or-n-p "Is this a problem with font display? ")
		 (insert "\nFrame Configuration:\n====================\n"
			 (prin1-to-string (frame-configuration-to-register ?F))
			 "\n"
			 ))))
	 nil
	 (concat
	  "This is (check all that apply, and delete what's irrelevant):\n"
	  "  [ ] a _MASSIVE_THANK_YOU_ for writing hilit19.el\n"
	  "  [ ] An invitation to attend the next Hackers Conference\n"
	  "  [ ] You're a RIGHTEOUS HACKER, what are your rates?\n"
	  "  [ ] I've used the force and read the source, but I'M CONFUSED\n"
	  "  [ ] a PATCH. (output of 'diff -uw old.el new.el' or 'diff -cw')\n"
	  "  [ ] a SERIOUS AND REPRODUCIBLE BUG that is not an EMACS bug\n"
	  "     - I *swear* that it's not already mentioned in the KNOWN BUGS\n"
	  "     - I HAVE CHECKED ftp.hackvan.com:/pub/stig/src/elisp/hilit19.el.gz\n"
	  "       for a newer release that fixes the problem.\n"
	  "    >> I HAVE ALSO CHECKED ftp.hackvan.com:/pub/stig/src/elisp/hl319.el.gz\n"
	  "       This is the alpha version...what will become hilit19 (Beta 3.0).\n"
	  "\n"
	  "Hey Stig, I *know* you're busy but...\n"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; These faces are either a valid face name, or nil
;; if you want to change them, you must do so AFTER hilit19 is loaded


(defconst hilit-default-face-table
  '(
    ;; used for C/C++ and Emacs Lisp and perl
    (comment	firebrick-italic    moccasin           italic)
    (include	purple		    Plum1	       bold-italic)
    (define	ForestGreen-bold    green	       bold)
    (defun	blue-bold	    cyan-bold	       bold-italic)
    (decl	RoyalBlue	    cyan	       bold)
    (type	nil		    yellow	       nil)
    (keyword	RoyalBlue	    cyan	       bold-italic)
    (label	red-underline	    orange-underlined  underline)
    (string	grey40		    orange	       underline)
    
    ;; some further faces for Ada
    (struct	  black-bold        white-bold	       bold)
    (glob-struct  magenta	    Plum1	       default-bold-underline)
    (named-param  DarkGoldenrod	    Goldenrod	       underline)
	
    ;; and another one for LaTeX
    (crossref	  DarkGoldenrod	    Goldenrod	       underline)
    (formula	  Goldenrod	    DarkGoldenrod      underline)
 
    ;; compilation buffers
    (active-error default/pink-bold  default/DeepPink-bold  default-underline)
    (error	  red-bold           yellow	       bold)
    (warning	  blue-italic	     green	       italic)

    ;; Makefiles (some faces borrowed from C/C++ too)
    (rule	  blue-bold-underline cyan-underline   default-bold-underline)
    
    ;; VM, GNUS and Text mode
    (msg-subject    blue-bold       yellow             bold)
    (msg-from	    purple-bold	    green	       bold)
    (msg-header	    firebrick-bold  cyan	       italic)
    (msg-separator  black/tan-bold  black/lightblue    nil)
    (msg-quote	    ForestGreen	    pink	       italic)

    (summary-seen     grey40	    white	       nil)
    (summary-killed   grey50	    white	       nil)
    (summary-Xed      OliveDrab2    green	       nil)
    (summary-deleted  firebrick	    white	       italic)
    (summary-unread   RoyalBlue	    yellow	       bold)
    (summary-new      blue-bold	    yellow-bold	       bold-italic)
    (summary-current  default/skyblue-bold green/dimgrey-bold reverse-default)

    (gnus-group-unsubscribed grey50		white	    nil)
    (gnus-group-empty	     nil		nil	    nil)
    (gnus-group-full	     ForestGreen	green	    italic)
    (gnus-group-overflowing  firebrick		red	    bold-italic)

    ;; dired mode
    (dired-directory blue-bold         cyan            bold)
    (dired-link	     firebrick-italic  green	       italic)
    (dired-ignored   ForestGreen       moccasin	       nil)
    (dired-deleted   red-bold-italic   orange	       bold-italic)
    (dired-marked    purple	       Plum1	       nil)
	
    ;; Info-mode, and jargon-mode.el and prep.ai.mit.edu:/pub/gnu/jargon*
    (jargon-entry    blue-bold	       cyan            bold)
    (jargon-xref     purple-bold       Plum1	       italic)
    (jargon-keyword  firebrick-underline yellow	       underline)
    )
  "alist of default faces (face . (light-default dark-default mono-default))

There is no way for the user to modify this table such that it will have any
effect upon the translations used by hilit19.  Instead, use the function
hilit-translate AFTER hilit19 has been loaded.

See also the documentation for hilit-lookup-face-create.")

(defconst hilit-face-translation-table
  (let ((index (or (and (x-display-color-p)
			(cdr (assq hilit-background-mode
				   '((light . 1) (dark . 2)))))
		   3)))
    (mapcar (function (lambda (x) (cons (car x) (nth index x))))
	    hilit-default-face-table))
  "alist that maps symbolic face-names to real face names")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To translate one face to another...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro hilit-translate (&rest args)
  "(hilit-translate FROM TO FROM TO ...): translate each face FROM to the
value of its TO face.  This is like setq for faces.

The function hilit-lookup-face-create will repeatedly translate until no more
translations for the face exist in the translation table.

See the documentation for hilit-lookup-face-create for names of valid faces."
  (or (zerop (% (length args) 2))
      (error "wrong number of args"))
  (let (cmdl from to)
    (while args
      (setq from (car args) to (nth 1 args) args (nthcdr 2 args)
	    cmdl (cons (list 'hilit-associate ''hilit-face-translation-table
			     (list 'quote from) to)
		       cmdl)))
    (cons 'progn cmdl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; This function actually translates and then creates the faces...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defun hilit-lookup-face-create (face &optional force)
  "Get a FACE, or create it if it doesn't exist.  In order for it to
properly create the face, the following naming convention must be used:
    [reverse-](fgcolor[/bgcolor])[-bold][-italic][-underline]
Example: (hilit-lookup-face-create 'comment-face) might create and return 'red

Each color is either the name of an X color (see .../X11/lib/X11/rgb.txt),
a hexadecimal specification of the form \"hex-[0-9A-Fa-f]+\", or \"default\".

An optional argument, FORCE, will cause the face to be recopied from the
default...which is probably of use only if you've changed fonts.

See the documentation for hilit-translate and hilit-face-translation-table."

;; translate the face ...
  (let ((trec t) visited)
    (while trec
      (cond ((memq face visited) (error "face translation loop: %S" visited))
	    (t (setq visited (cons face visited)
		     trec (assq face hilit-face-translation-table))
	       (and trec (setq face (cdr trec)))))))

  ;; make the face if we need to...
  (let* ((fn (symbol-name face))
	 (frame (selected-frame))
	 (basefont (cdr (assq 'font (frame-parameters frame))))
	 error fgcolor bgcolor)
    (cond
     ((or (null face)			
	  (memq face hilit-predefined-face-list))
      ;; do nothing if the face is nil or if it's predefined.
      )
     ((or force
	  (not (memq face (face-list)))
	  (and hilit-face-check
	       (not (string= (get face 'basefont) basefont))))
      (copy-face 'default 'scratch-face)
      (if (string-match "^reverse-?" fn)
	  (progn (invert-face 'scratch-face)
		 (setq fn (substring fn (match-end 0)))))

      ;; parse foreground color
      (if (string-match "^\\(hex-\\)?\\([A-Za-z0-9]+\\)" fn)
	  (setq fgcolor (concat
			 (if (match-beginning 1) "#")
			 (substring fn (match-beginning 2) (match-end 2)))
		fn (substring fn (match-end 0)))
	(error "bad face name %S" face))

      ;; parse background color
      (if (string-match "^/\\(hex-\\)?\\([A-Za-z0-9]+\\)" fn)
	  (setq bgcolor (concat
			 (and (match-beginning 1) "#")
			 (substring fn (match-beginning 2) (match-end 2)))
		fn (substring fn (match-end 0))))
      
      (and (string= "default" fgcolor) (setq fgcolor nil))
      (and (string= "default" bgcolor) (setq bgcolor nil))
      
      ;; catch errors if we can't allocate the color(s)
      (condition-case nil
	  (progn (set-face-foreground 'scratch-face fgcolor)
		 (set-face-background 'scratch-face bgcolor)
		 (copy-face 'scratch-face face)
		 (put face 'basefont basefont))
	(error (message "couldn't allocate color for '%s'"
			(symbol-name face))
	       (setq face 'default)
	       (setq error t)))
      (or error
	  ;; don't bother w/ bold or italic if we didn't get the color
	  ;; we wanted, but ignore errors making the face bold or italic
	  ;; if the font isn't available, there's nothing to do about it...
	  (progn
	    (set-face-font face nil frame)
	    (set-face-underline-p face (string-match "underline" fn))
	    (if (string-match ".*bold" fn)
		;; make face bold in all frames
		(make-face-bold face nil 'noerr))
	    (if (string-match ".*italic" fn)
		;; make face italic in all frames
		(make-face-italic face nil 'noerr))
	    ))
      )))
  face)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Region Highlight/Unhighlight code (Both overlay and text-property versions)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsubst hilit-region-set-face (start end face-name &optional prio prop)
  "Highlight region from START to END using FACE and, optionally, PRIO.
The optional 5th arg, PROP is a property to set instead of 'hilit."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face face-name)
    (overlay-put overlay (or prop 'hilit) t)
    (and prio (overlay-put overlay 'priority prio))))

(defun hilit-unhighlight-region (start end &optional quietly)
  "Unhighlights the region from START to END, optionally in a QUIET way"
  (interactive "r")
  (or quietly hilit-quietly (message "Unhighlighting"))
  (let ((lstart 0))
    (while (and start (> start lstart) (< start end))
      (mapcar (function (lambda (ovr)
			  (and (overlay-get ovr 'hilit) (delete-overlay ovr))))
	      (overlays-at start))
      (setq lstart start start (next-overlay-change start))))
  (or quietly hilit-quietly (message "Done unhighlighting")))

;;;; These functions use text properties instead of overlays.  Text properties
;;;; are copied through kill and yank...which might be convenient, but is not
;;;; terribly efficient as of 19.12, ERGO it's been disabled
;;
;;(defsubst hilit-region-set-face (start end face-name &optional prio prop)
;;  "Highlight region from START to END using FACE and, optionally, PRIO.
;;The optional 5th arg, PROP is a property to set instead of 'hilit."
;;    (put-text-property start end 'face face-name)
;;    )
;;
;;(defun hilit-unhighlight-region (start end &optional quietly)
;;  "Unhighlights the region from START to END, optionally in a QUIET way"
;;  (interactive "r")
;;  (let ((buffer-read-only nil)
;;	(bm (buffer-modified-p)))
;;    (remove-text-properties start end '(face))
;;    (set-buffer-modified-p bm)))
;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pattern Application code and user functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hilit-highlight-region (start end &optional patterns quietly)
  "Highlights the area of the buffer between START and END (the region when
interactive).  Without the optional PATTERNS argument, the pattern for
major-mode is used.  If PATTERNS is a symbol, then the patterns associated
with that symbol are used.  QUIETLY suppresses progress messages if
non-nil."
  (interactive "r")
  (cond ((null patterns)
	 (setq patterns (cdr (assq major-mode hilit-patterns-alist))))
	((symbolp patterns)
	 (setq patterns (cdr (assq patterns hilit-patterns-alist)))))  
  ;; txt prop: (setq patterns (reverse patterns))
  (let ((case-fold-search (car patterns))
	(prio (1- (length patterns)))
	;; txt prop: (buffer-read-only nil)
	;; txt prop: (bm (buffer-modified-p))
	p pstart pend face mstart (puke-count 0))
    ;; txt prop: (unwind-protect
    (setq patterns (cdr patterns))	; remove case-fold from head of pattern
    (save-excursion
      (save-restriction
	(narrow-to-region start end)
	(while patterns
	  (setq p (car patterns))
	  (setq pstart (car p)
		pend (nth 1 p)
		face (hilit-lookup-face-create (nth 2 p)))
	  (if (not face)		; skipped if nil
	      nil
	    (or quietly hilit-quietly
		(message "highlighting %d: %s%s" prio pstart 
			 (if (stringp pend) (concat " ... " pend) "")))
	    (goto-char (point-min))
	    (condition-case msg
		(cond 
		      ((symbolp pstart)
		       ;; inner loop -- special function to find pattern
		       (let (region)
			 (while (setq region (funcall pstart pend))
			   (hilit-region-set-face (car region) (cdr region)
						  face prio))))
		      ((stringp pend)
		       ;; inner loop -- regex-start ... regex-end
		       (while (re-search-forward pstart nil t nil)
			 (goto-char (setq mstart (match-beginning 0)))
			 (if (re-search-forward pend nil t nil)
			     (hilit-region-set-face mstart (match-end 0)
						    face prio)
			   (forward-char 1))))
		      ((numberp pend)
		       ;; inner loop -- just one regex to match whole pattern
		       (while (re-search-forward pstart nil t nil)
			 (goto-char (match-end pend))
			 (hilit-region-set-face  (match-beginning pend)
						 (match-end pend) face prio)))
		      (t (error "malformed pattern")))
	      (error (if (> (setq puke-count (1+ puke-count)) 1)
			 (error msg)
		       (message "Error: '%s'" msg)
		       (ding) (sit-for 4)))))
	  (setq prio (1- prio)
		patterns (cdr patterns)))
	))
    (or quietly hilit-quietly (message "")) ; "Done highlighting"
    ;; txt prop: (set-buffer-modified-p bm)) ; unwind protection
    ))

(defun hilit-rehighlight-region (start end &optional quietly)
  "Re-highlights the region, optionally in a QUIET way"
  (interactive "r")
  (save-restriction
    (widen)
    (setq start (apply 'min start (mapcar 'overlay-start (overlays-at start)))
	  end (apply 'max end (mapcar 'overlay-end (overlays-at end))))
    (hilit-unhighlight-region start end quietly)
    (hilit-highlight-region   start end nil quietly)))

(defun hilit-rehighlight-buffer (&optional quietly)
  "Re-highlights the buffer, optionally in a QUIET way"
  (interactive "")
  (let ((parse-fn (cdr (assq major-mode hilit-parser-alist))))
    (if parse-fn
	(funcall parse-fn quietly)
      (hilit-rehighlight-region (point-min) (point-max) quietly)))
  nil)

(defun hilit-rehighlight-buffer-quietly ()
  (hilit-rehighlight-buffer t))

(defun hilit-rehighlight-message (quietly)
  "Highlight a buffer containing a news article or mail message."
  (save-excursion
    (goto-char (point-min))
    ;; find separation between headers and body (either a blank line or
    ;; the message separator line in mail-mode)
    (re-search-forward "^\\(\\|--text follows this line--\\)$" nil 'noerr)
    (hilit-unhighlight-region (point-min) (point-max) quietly)
    (hilit-highlight-region (point-min) (point) 'msg-header quietly)
    (hilit-highlight-region (point) (point-max) 'msg-body quietly)))

(defalias 'hilit-highlight-buffer 'hilit-rehighlight-buffer)

;; Well, I want to remove this function...there's one sure way to find out if
;; anyone uses it or not...and that's to comment it out.
;; 
;; (defun hilit-toggle-highlight (arg)
;;   "Locally toggle highlighting.  With arg, forces highlighting off."
;;   (interactive "P")
;;   ;; FIXME -- this loses numeric information in hilit-auto-rehighlight
;;   (setq hilit-auto-rehighlight
;;         (and (not arg) (not hilit-auto-rehighlight)))
;;   (if hilit-auto-rehighlight
;;       (hilit-rehighlight-buffer)
;;     (hilit-unhighlight-region (point-min) (point-max)))
;;   (message "Rehighlighting is set to %s" hilit-auto-rehighlight))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HOOKS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hilit-find-file-hook ()
  "Find-file hook for hilit package.  See the variable hilit-auto-highlight."
  (cond ((and hilit-auto-highlight
	      (assq major-mode hilit-patterns-alist))
	 (if (> buffer-saved-size (car hilit-auto-rehighlight-fallback))
	     (setq hilit-auto-rehighlight
		   (cdr hilit-auto-rehighlight-fallback)))
	 (if (> buffer-saved-size hilit-auto-highlight-maxout)
	     nil
	   (let ((bm (buffer-modified-p)))
	     (hilit-rehighlight-buffer)
	     (set-buffer-modified-p bm))))))

(defun hilit-repaint-command (arg)
  "Rehighlights according to the value of hilit-auto-rehighlight, or the
prefix argument if that is specified.
\t\\[hilit-repaint-command]\t\trepaint according to hilit-auto-rehighlight
\t^U \\[hilit-repaint-command]\trepaint entire buffer
\t^U - \\[hilit-repaint-command]\trepaint visible portion of buffer
\t^U n \\[hilit-repaint-command]\trepaint n lines to either side of point"
  (interactive "P") 
  (let (st en quietly)
    (or arg (setq arg hilit-auto-rehighlight))
    (cond ((or (eq  arg 'visible) (eq arg '-))
	   (setq st (window-start) en (window-end) quietly t))
	  ((numberp arg)
	   (setq st (save-excursion (forward-line (- arg)) (point))
		 en (save-excursion (forward-line arg) (point))))
	  (arg
	   (hilit-rehighlight-buffer)))
    (if st
	  (hilit-rehighlight-region st en quietly))))

(defun hilit-recenter (arg)
  "Recenter, then rehighlight according to hilit-auto-rehighlight.  If called
with an unspecified prefix argument (^U but no number), then a rehighlight of
the entire buffer is forced."
  (interactive "P")
  (recenter arg)
  ;; force display update
  (sit-for 0)
  (hilit-repaint-command (consp arg))
  (if (fboundp 'font-lock-fontify-buffer) ;SMW
      (save-excursion
	;(font-lock-fontify-buffer)
	))
  )

(defun hilit-yank (&optional arg)
  "Yank with rehighlighting"
  (interactive "*P")
  (let ((transient-mark-mode nil))
    (if arg
	(yank arg)
      (yank))
    (if (not (string= (substring (prin1-to-string (get-buffer-window (current-buffer))) 14 22)
		      (substring (prin1-to-string (minibuffer-window)) 14 22)))
	(progn
	  (if xemacsp (setq mark-active (mark)))
	  (if hilit-auto-rehighlight
	      (hilit-rehighlight-region (if mark-active (region-beginning)(point-min))
					(if mark-active (region-end)(point-max)) t))
	  (if (fboundp 'font-lock-fontify-region)
	      (save-excursion
		(font-lock-fontify-region (if mark-active (region-beginning)(point-min))
					  (if mark-active (region-end)(point-max)))))))
    (setq this-command 'yank)))

(defun hilit-yank-pop (arg)
  "Yank-pop with rehighlighting"
  (interactive "*p")
  (let ((transient-mark-mode nil))
    (if arg
	(yank-pop arg)
      (yank-pop))
    (if (not (string= (substring (prin1-to-string (get-buffer-window (current-buffer))) 14 22)
		      (substring (prin1-to-string (minibuffer-window)) 14 22)))
	(progn
	  (if xemacsp (setq mark-active (mark)))
	  (if hilit-auto-rehighlight
	      (hilit-rehighlight-region (if mark-active (region-beginning)(point-min))
					(if mark-active (region-end)(point-max)) t))
	  (if (fboundp 'font-lock-fontify-region)
	      (save-excursion
		(font-lock-fontify-region (if mark-active (region-beginning)(point-min))
					  (if mark-active (region-end)(point-max)))))))
    (setq this-command 'yank)))



;;; this line highlighting stuff is untested.  play with it only if you feel
;;; adventurous...don't ask me to fix it...though you're welcome to.  -- Stig
;; 
;; (defun hilit-rehighlight-line-quietly (&rest args)
;;   "Quietly rehighlight just this line.
;; Useful as an after change hook in VM/gnus summary buffers and dired buffers.
;; If only there were an after-change-function, that is..."
;;   (save-excursion
;;     (push-mark nil t)
;;     (hilit-rehighlight-yank-region)
;;     (and orig-achange-function (apply orig-achange-function args))))
;; 
;; (defun hilit-install-line-hooks ()
;;   (make-variable-buffer-local 'after-change-function)
;;   (make-local-variable 'orig-achange-function)
;;   (setq orig-achange-function after-change-function)
;;   (setq after-change-function 'hilit-rehighlight-line-quietly))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wysiwyg Stuff...  take it away and build a whole package around it!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; ; For the Jargon-impaired, WYSIWYG === What You See Is What You Get
;; ; Sure, it sucks to type.  Oh, well.
;; (defun hilit-wysiwyg-replace ()
;;   "Replace overstruck text with normal text that's been overlaid with the 
;; appropriate text attribute.  Suitable for a find-file hook."
;;   (save-excursion
;;     (goto-char (point-min))
;;     (let ((wysb (hilit-lookup-face-create 'wysiwyg-bold))
;; 	  (wysu (hilit-lookup-face-create 'wysiwyg-underline))
;; 	  (bmod (buffer-modified-p)))
;;       (while (re-search-forward "\\(.\b.\\)+" nil t)
;; 	(let ((st (match-beginning 0)) (en (match-end 0)))
;; 	  (goto-char st)
;; 	  (if (looking-at "_")
;; 	      (hilit-region-set-face st en wysu 100 'wysiwyg)
;; 	    (hilit-region-set-face st en wysb 100 'wysiwyg))
;; 	  (while (and (< (point) en) (looking-at ".\b"))
;; 	    (replace-match "") (forward-char))
;; 	  ))
;;       (set-buffer-modified-p bmod))))
;; 
;; ; is this more appropriate as a write-file-hook or a write-contents-hook?
;; (defun hilit-wysiwyg-write-repair ()
;;   "Replace wysiwyg overlays with overstrike text."
;;   (message "*sigh* hilit-wysiwyg-write-repair not implemented yet")
;;
;; For efficiency, this hook should copy the current buffer to a scratch
;; buffer and do its overstriking there.  Overlays are not copied, so it'll
;; be necessary to hop back and forth.  This is OK since you're not fiddling
;; with--making or deleting--any overlays.  THEN write the new buffer,
;; delete it, and RETURN T. << important
;;
;; Just so you know...there is already an emacs function called
;; underline-region that does underlining.  I think that the thing to do is
;; extend that to do overstriking as well.
;;
;;  (while (< start end)
;;    (mapcar (function (lambda (ovr)
;;			  (and (overlay-get ovr 'hilit) (delete-overlay ovr))))
;;	    (overlays-at start))
;;    (setq start (next-overlay-change start)))
;;  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization.  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(and (not hilit-inhibit-rebinding)
     window-system
     (progn 
       (substitute-key-definition 'yank     'hilit-yank
				  (current-global-map))
       (substitute-key-definition 'yank-pop 'hilit-yank-pop
				  (current-global-map))
       (substitute-key-definition 'recenter 'hilit-recenter
				  (current-global-map))))

(if xemacsp
    (global-set-key [(control L)] 'hilit-repaint-command) ;;Xemacs korrekt??
  (global-set-key [?\C-\S-l] 'hilit-repaint-command))

(and window-system
     (add-hook 'find-file-hooks 'hilit-find-file-hook t))

(eval-when-compile (require 'gnus))	; no compilation gripes

(and (not hilit-inhibit-hooks)
     window-system
     (condition-case c
	 (progn

	   ;; BUFFER highlights...
	   (mapcar (function
		    (lambda (hook)
		      (add-hook hook 'hilit-rehighlight-buffer-quietly)))
		   '(
		     Info-selection-hook

;; runs too early		     vm-summary-mode-hooks
		     vm-summary-pointer-hook
		     vm-preview-message-hook
		     vm-show-message-hook

		     rmail-show-message-hook
		     mail-setup-hook 
		     mh-show-mode-hook

		     dired-after-readin-hook
		     ))
	   )
       (error (message "Error loading highlight hooks: %s" c)
	      (ding) (sit-for 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Default patterns for various modes.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; do I need this?  I changed the defconst to a defvar because defconst is
;;; inappropriate, but I don't know why I wanted hilit-patterns-alist to be
;;; reset on every reload...

(setq hilit-patterns-alist nil) 

(defun hilit-associate (alist key val)
  "creates, or destructively replaces, the pair (key . val) in alist"
  (let ((oldentry (assq key (eval alist))))
    (if oldentry
	(setcdr oldentry val)
      (set alist (cons (cons key val) (eval alist))))))
  
(defun hilit-set-mode-patterns (modelist patterns
					 &optional parse-fn case-fold)
  "Sets the default highlighting patterns for MODE to PATTERNS.
See the variable hilit-mode-enable-list.

Takes optional arguments PARSE-FN and CASE-FOLD."
  ;; change pattern
  (mapcar (function (lambda (p)
		      (and (stringp (car p))
			   (null (nth 1 p))
			   (setcar (cdr p) 0))))
	  patterns)
  (setq patterns (cons case-fold patterns))
  
  (or (consp modelist) (setq modelist (list modelist)))
  (let (ok (flip (eq (car hilit-mode-enable-list) 'not)))
    (mapcar (function
	     (lambda (m)
	       (setq ok (or (null hilit-mode-enable-list)
			    (memq m hilit-mode-enable-list)))
	       (and flip (setq ok (not ok)))
	       (and ok
		    (progn
		      (and parse-fn
			   (hilit-associate 'hilit-parser-alist m parse-fn))
		      (hilit-associate 'hilit-patterns-alist m patterns)))))
	    modelist)))

(defun hilit-add-pattern (pstart pend face &optional mode first)
  "Highlight pstart with face for the current major-mode.
Optionally, place the new pattern first in the pattern list"
  (interactive "sPattern start regex: \nsPattern end regex (default none): \nxFace: ")

  (and (equal pstart "") (error "Must specify starting regex"))
  (cond ((equal pend "") (setq pend 0))
	((string-match "^[0-9]+$" pend) (setq pend (string-to-int pend))))
  (or mode (setq mode major-mode))
  (let ((old-patterns (cdr (assq mode hilit-patterns-alist)))
	(new-pat (list pstart pend face)))
    (cond ((not old-patterns)
	   (hilit-set-mode-patterns mode (list new-pat)))
	  (first
	   (setcdr old-patterns (cons new-pat (cdr old-patterns))))
	  (t 
	   (nconc old-patterns (list new-pat)))))
  (and (interactive-p) (hilit-rehighlight-buffer)))

(defun hilit-string-find (qchar)
  "looks for a string and returns (start . end) or NIL.  The argument QCHAR
is the character that would precede a character constant double quote.
Finds strings delimited by double quotes.  The first double quote may not be
preceded by QCHAR and the closing double quote may not be preceded by an odd
number of backslashes."
  (let (st en)
    (while (and (search-forward "\"" nil t)
		(eq qchar (char-after (1- (setq st (match-beginning 0)))))))
    (while (and (search-forward "\"" nil t)
		(save-excursion
		  (setq en (point))
		  (forward-char -1)
		  (skip-chars-backward "\\\\")
		  (forward-char 1)
		  (not (zerop (% (- en (point)) 2))))))
    (and en (cons st en))))    

;; return types on same line...
;; ("^[a-zA-z].*\\(\\w\\|[$_]\\)+\\s *\\(\\(\\w\\|[$_]\\)+\\s *((\\|(\\)[^)]*)+" nil defun)

;; On another note, a working pattern for grabbing function definitions for C is
;; 
;; ("^[a-zA-Z_]+.*[;{]$" nil ForestGreen)  ; global defns ( start at col 1 )
;; ("^[a-zA-Z_]+.*(" ")" defun)
;; ; defuns assumed to start at col 1, not with # or {
;; 
;; this will make external declarations/definitions green, and function
;; definitions the defun face.  Hmmm - seems to work for me anyway.

(let ((comments     '(("/\\*" "\\*/" comment)))
      (c++-comments '(("//.*$" nil comment)
		      ("^/.*$" nil comment)))
      (strings      '((hilit-string-find ?' string)))
      (preprocessor '(("^#[ \t]*\\(undef\\|define\\).*$" "[^\\]$" define)
		      ("^#.*$" nil include))))

  (hilit-set-mode-patterns
   '(c-mode c++-c-mode elec-c-mode)
   (append
    comments strings preprocessor
    '(
      ;; function decls are expected to have types on the previous line
      ("^\\(\\w\\|[$_]\\)+\\s *\\(\\(\\w\\|[$_]\\)+\\s *((\\|(\\)[^)]*)+" nil defun)
      ("^\\(typedef\\|struct\\|union\\|enum\\).*$" nil decl)
      ;; datatype -- black magic regular expression
      ("[ \n\t({]\\(\\(const\\|register\\|volatile\\|unsigned\\|extern\\|static\\)\\s +\\)*\\(\\(\\w\\|[$_]\\)+_t\\|float\\|double\\|void\\|char\\|short\\|int\\|long\\|FILE\\|\\(\\(struct\\|union\\|enum\\)\\([ \t]+\\(\\w\\|[$_]\\)*\\)\\)\\)\\(\\s +\\*+)?\\|[ \n\t;()]\\)" nil type)
      ;; key words
      ("[^_]\\<\\(return\\|goto\\|if\\|else\\|case\\|default\\|switch\\|break\\|continue\\|while\\|do\\|for\\)\\>[^_]" 1 keyword)
      )))

  (hilit-set-mode-patterns
   'c++-mode
   (append
    comments c++-comments strings preprocessor
    '(
      ;; function decls are expected to have types on the previous line
      ("^\\(\\(\\w\\|[$_]\\)+::\\)?\\(\\w\\|[$_]\\)+\\s *\\(\\(\\w\\|[$_]\\)+\\s *((\\|(\\)[^)]*)+" nil defun)
      ("^\\(\\(\\w\\|[$_]\\)+[ \t]*::[ \t]*\\)?\\(\\(\\w\\|[$_]\\)+\\|operator.*\\)\\s *\\(\\(\\w\\|[$_]\\)+\\s *((\\|(\\)[^)]*)+" nil defun)
      ("^\\(template\\|typedef\\|struct\\|union\\|class\\|enum\\|public\\|private\\|protected\\).*$" nil decl)
      ;; datatype -- black magic regular expression
      ("[ \n\t({]\\(\\(const\\|register\\|volatile\\|unsigned\\|extern\\|static\\)\\s +\\)*\\(\\(\\w\\|[$_]\\)+_t\\|float\\|double\\|void\\|char\\|short\\|int\\|long\\|FILE\\|\\(\\(struct\\|union\\|enum\\|class\\)\\([ \t]+\\(\\w\\|[$_]\\)*\\)\\)\\)\\(\\s +\\*+)?\\|[ \n\t;()]\\)" nil type)
      ;; key words
      ("[^_]\\<\\(return\\|goto\\|if\\|else\\|case\\|default\\|switch\\|break\\|continue\\|while\\|do\\|for\\|public\\|protected\\|private\\|delete\\|new\\)\\>[^_]"
       1 keyword))))

  (hilit-set-mode-patterns
   '(objc-mode objective-C-mode)
   (append
    comments c++-comments strings preprocessor
    '(
      ;; function decls are expected to have types on the previous line
      ("^\\(\\(\\w\\|[$_]\\)+::\\)?\\(\\w\\|[$_]\\)+\\s *\\(\\(\\w\\|[$_]\\)+\\s *((\\|(\\)[^)]*)+" nil defun)
      ("^\\(\\(\\w\\|[$_]\\)+[ \t]*::[ \t]*\\)?\\(\\(\\w\\|[$_]\\)+\\|operator.*\\)\\s *\\(\\(\\w\\|[$_]\\)+\\s *((\\|(\\)[^)]*)+" nil defun)

      ("^\\(template\\|typedef\\|struct\\|union\\|class\\|enum\\|public\\|private\\|protected\\).*$" nil decl)
      ;; datatype -- black magic regular expression
      ("[ \n\t({]\\(\\(const\\|register\\|volatile\\|unsigned\\|extern\\|static\\)\\s +\\)*\\(\\(\\w\\|[$_]\\)+_t\\|float\\|double\\|void\\|char\\|short\\|int\\|long\\|FILE\\|\\(\\(struct\\|union\\|enum\\|class\\)\\([ \t]+\\(\\w\\|[$_]\\)*\\)\\)\\)\\(\\s +\\*+)?\\|[ \n\t;()]\\)" nil type)
      ;; key words
      ("[^_]\\<\\(return\\|goto\\|if\\|else\\|case\\|default\\|switch\\|break\\|continue\\|while\\|do\\|for\\|public\\|protected\\|private\\|interface\\|implementation\\|end\\|super\\|self\\)\\>[^_]"
       1 keyword))))
  )

(hilit-set-mode-patterns
 'perl-mode
 '(("\\s #.*$" nil comment)
   ("^#.*$" nil comment)
   ("\"[^\\\"]*\\(\\\\\\(.\\|\n\\)[^\\\"]*\\)*\"" nil string)
   ("^\\(__....?__\\|\\s *\\sw+:\\)" nil label)
   ("^require.*$" nil include)
   ("^package.*$" nil decl)
   ("^\\s *sub\\s +\\(\\w\\|[_']\\)+" nil defun)
   ("\\b\\(do\\|if\\|unless\\|while\\|until\\|else\\|elsif\\|for\\|foreach\\|continue\\|next\\|redo\\|last\\|goto\\|return\\|die\\|exit\\)\\b" nil keyword)))

(hilit-set-mode-patterns
 'ada-mode
 '(;; comments
   ("--.*$" nil comment)
   ;; main structure
   ("[ \t\n]procedure[ \t]" "\\([ \t]\\(is\\|renames\\)\\|);\\)" glob-struct)
   ("[ \t\n]task[ \t]" "[ \t]is" glob-struct)
   ("[ \t\n]function[ \t]" "return[ \t]+[A-Za-z_0-9]+[ \t]*\\(is\\|;\\|renames\\)" glob-struct)
   ("[ \t\n]package[ \t]" "[ \t]\\(is\\|renames\\)" glob-struct)
   ;; if there is nothing before "private", it is part of the structure
   ("^[ \t]*private[ \t\n]" nil glob-struct)
   ;; if there is no indentation before the "end", then it is most
   ;; probably the end of the package
   ("^end.*$" ";" glob-struct)
   ;; program structure -- "null", "delay" and "terminate" omitted
   ("[ \n\t]\\(in\\|out\\|select\\|if\\|else\\|case\\|when\\|and\\|or\\|not\\|accept\\|loop\\|do\\|then\\|elsif\\|else\\|for\\|while\\|exit\\)[ \n\t;]" nil struct)
   ;; block structure
   ("[ \n\t]\\(begin\\|end\\|declare\\|exception\\|generic\\|raise\\|return\\|package\\|body\\)[ \n\t;]" nil struct)
   ;; type declaration
   ("^[ \t]*\\(type\\|subtype\\).*$" ";" decl)
   ("[ \t]+is record.*$" "end record;" decl)
   ;; "pragma", "with", and "use" are close to C cpp directives
   ("^[ \t]*\\(with\\|pragma\\|use\\)" ";" include)
   ;; nice for named parameters, but not so beautiful in case statements
   ("[A-Za-z_0-9.]+[ \t]*=>"   nil named-param)
   ;; string constants probably not everybody likes this one
   ("\"" ".*\"" string)))

(hilit-set-mode-patterns
 'fortran-mode
 '(("^[*Cc].*$" nil comment)
   ("'[^'\n]*'" nil string)
   ("\\(^[ \t]*[0-9]+\\|[ \t]continue[ \t\n]\\|format\\)" nil define)
   ("[ \t]\\(do\\|do[ \t]*[0-9]+\\|go[ \t]*to[ \t]*[0-9]+\\|end[ \t]*do\\|if\\|else[ \t]*if\\|then\\|else\\|end[ \t]*if\\)[ \t\n(]" nil define)
   ("[ \t]\\(call\\|program\\|subroutine\\|function\\|stop\\|return\\|end\\|include\\)[ \t\n]" nil include)
   ("[ \t]\\(parameter[\t\n ]*([^)]*)\\|data\\|save\\|common[ \t\n]*/[^/]*/\\)" 
    nil decl)
   ("^     ." nil type)
   ("implicit[ \t]*none" nil decl)
   ("\\([ \t]\\|implicit[ \t]*\\)\\(dimension\\|integer\\|real\\|double[ \t]*precision\\|character\\|logical\\|complex\\|double[ \t]*complex\\)\\([*][0-9]*\\|[ \t\n]\\)" nil keyword)
   )
 nil 'case-insensitive)

(hilit-set-mode-patterns
 '(m2-mode modula-2-mode)
 '(("(\\*" "\\*)" comment)
   (hilit-string-find ?\\ string)
   ("^[ \t]*PROCEDURE[ \t]+\\w+[^ \t(;]*" nil defun)
   ("\\<\\(RECORD\\|ARRAY\\|OF\\|POINTER\\|TO\\|BEGIN\\|END\\|FOR\\|IF\\|THEN\\|ELSE\\|ELSIF\\|CASE\\|WHILE\\|DO\\|MODULE\\|FROM\\|RETURN\\|IMPORT\\|EXPORT\\|VAR\\|LOOP\\|UNTIL\\|\\DEFINITION\\|IMPLEMENTATION\\|AND\\|OR\\|NOT\\|CONST\\|TYPE\\|QUALIFIED\\)\\>" nil keyword)
   )
 nil 'case-insensitive)

(hilit-set-mode-patterns 'prolog-mode
 '(("/\\*" "\\*/" comment)
   ("%.*$" nil comment)
   (":-" nil defun)
   ("!" nil label)
   ("\"[^\\\"]*\\(\\\\\\(.\\|\n\\)[^\\\"]*\\)*\"" nil string)
   ("\\b\\(is\\|mod\\)\\b" nil keyword)
   ("\\(->\\|-->\\|;\\|==\\|\\\\==\\|=<\\|>=\\|<\\|>\\|=\\|\\\\=\\|=:=\\|=\\\.\\\.\\|\\\\\\\+\\)" nil decl)
   ("\\(\\\[\\||\\|\\\]\\)" nil include)))

(hilit-set-mode-patterns
 '(
   LaTeX-mode japanese-LaTeX-mode SliTeX-mode
   japanese-SliTeX-mode FoilTeX-mode latex-mode
   )
 '(
   ;; comments
   ("[^\\]%.*$" nil comment)

   ;; the following two match \foo[xx]{xx} or \foo*{xx} or \foo{xx}
   ("\\\\\\(sub\\)*\\(paragraph\\|section\\)\\(\*\\|\\[.*\\]\\)?{" "}"
    keyword)
   ("\\\\\\(chapter\\|part\\)\\(\*\\|\\[.*\\]\\)?{" "}" keyword)
   ("\\\\footnote\\(mark\\|text\\)?{" "}" keyword)
   ("\\\\[a-z]+box" nil keyword)
   ("\\\\\\(v\\|h\\)space\\(\*\\)?{" "}" keyword)

   ;; (re-)define new commands/environments/counters
   ("\\\\\\(re\\)?new\\(environment\\|command\\){" "}" defun)
   ("\\\\new\\(length\\|theorem\\|counter\\){" "}" defun)

   ;; various declarations/definitions
   ("\\\\\\(setlength\\|settowidth\\|addtolength\\|setcounter\\|addtocounter\\)" nil define)
   ("\\\\\\(title\\|author\\|date\\|thanks\\){" "}" define)

   ("\\\\documentstyle\\(\\[.*\\]\\)?{" "}" decl)
   ("\\\\\\(begin\\|end\\|nofiles\\|includeonly\\){" "}" decl)
   ("\\\\\\(raggedright\\|makeindex\\|makeglossary\\|maketitle\\)\\b" nil
    decl)
   ("\\\\\\(pagestyle\\|thispagestyle\\|pagenumbering\\){" "}" decl)
   ("\\\\\\(normalsize\\|small\\|footnotesize\\|scriptsize\\|tiny\\|large\\|Large\\|LARGE\\|huge\\|Huge\\)\\b" nil decl)
   ("\\\\\\(appendix\\|tableofcontents\\|listoffigures\\|listoftables\\)\\b"
    nil decl)
   ("\\\\\\(bf\\|em\\|it\\|rm\\|sf\\|sl\\|ss\\|tt\\)\\b" nil decl)

   ;; label-like things
   ("\\\\item\\(\\[[^]]*\\]\\)?" nil label)
   ("\\\\caption\\(\\[[^]]*\\]\\)?{" "}" label)

   ;; formulas
   ("[^\\]\\\\("  "\\\\)" formula)                   ; \( \)
   ("[^\\]\\\\\\[" "\\\\\\]" formula)                ; \[ \]
   ("[^\\$]\\(\\$\\(\\$[^$]*\\$\\|[^$]*\\)\\$\\)" 1 formula) ; '$...$' or '$$...$$'
   
   ;; things that bring in external files
   ("\\\\\\(include\\|input\\|bibliography\\){" "}" include)

   ;; "wysiwyg" emphasis -- these don't work with nested expressions
   ;; ("{\\\\\\(em\\|it\\|sl\\)" "}" italic)
   ;; ("{\\\\bf" "}" bold)

   ("``" "''" string)

   ;; things that do some sort of cross-reference
   ("\\\\\\(\\(no\\)?cite\\|\\(page\\)?ref\\|label\\|index\\|glossary\\){" "}" crossref)
   ))

(hilit-set-mode-patterns
 'bibtex-mode
 '(;;(";.*$"			nil	comment)
   ("%.*$"			nil	comment)
   ("@[a-zA-Z]+"		nil	keyword)
   ("{[ \t]*[-a-z:_A-Z0-9]+,"	nil	label) ; is wrong sometimes
   ("^[ \t]*[a-zA-Z]+[ \t]*="	nil	define)))

(hilit-set-mode-patterns
 'compilation-mode
 '(
   ("^[-_.\"A-Za-z0-9]+\\(:\\|, line \\)[0-9]+: warning:.*$" nil warning)
   ("^[-_.\"A-Za-z0-9]+\\(:\\|, line \\)[0-9]+:.*$" nil error)
   ))

(hilit-set-mode-patterns
 'makefile-mode
 '(("^#.*$" nil comment)
   ("[^$]#.*$" nil comment)
   ;; rules
   ("^[^ \t\n]*%[^ \t\n]*[ \t]*::?[ \t]*[^ \t\n]*[ \t]*\\(#.*\\)?$" nil rule)
   ("^[.][A-Za-z][A-Za-z]?\..*$" nil rule)
   ;; variable definition
   ("^[_A-Za-z0-9]+[ \t]*\+?=" nil define)
   ("\\( \\|:=\\)[_A-Za-z0-9]+[ \t]*\\+=" nil define)
   ;; variable references
   ("\\$\\([^ \t\n{(]\\|[{(]@?[_A-Za-z0-9:.,%/=]+[)}]\\)" nil keyword)
   ("^[A-Za-z0-9.,/_-]+[ \t]*:.*$" nil defun)
   ("^include " nil include)))

(let* ((header-patterns '(("^Subject:.*$" nil msg-subject)
			  ("^From:.*$" nil msg-from)
			  ("^--text follows this line--$" nil msg-separator)
			  ("^[A-Za-z][A-Za-z0-9-]+:" nil msg-header)))
       (body-patterns '(("^\\(In article\\|[ \t]*\\w*[]<>}|]\\).*$"
			 nil msg-quote)))
       (message-patterns (append header-patterns body-patterns)))
  (hilit-set-mode-patterns 'msg-header header-patterns)
  (hilit-set-mode-patterns 'msg-body body-patterns)
  (hilit-set-mode-patterns '(vm-mode text-mode mail-mode rmail-mode
			     gnus-article-mode news-reply-mode mh-show-mode)
			   message-patterns
			   'hilit-rehighlight-message))

(hilit-set-mode-patterns
 'gnus-group-mode
 '(("^ U.*$" nil gnus-group-unsubscribed)
   ("^\\*? +[01]?[0-9]:.*$" nil gnus-group-empty)
   ("^ +[2-9][0-9]:.*$" nil gnus-group-full)
   ("^ +[0-9][0-9][0-9]+:.*$" nil gnus-group-overflowing)))

(hilit-set-mode-patterns
 'vm-summary-mode
 '(("^   .*$" nil summary-seen)
   ("^->.*$" nil  summary-current)
   ("^  D.*$" nil summary-deleted)
   ("^  U.*$" nil summary-unread)
   ("^  N.*$" nil summary-new)))


;;; this will match only comments w/ an even (zero is even) number of quotes...
;;; which is still inadequate because it matches comments in multi-line strings
;;; how anal do you want to get about never highlighting comments in strings?
;;; I could twiddle with this forever and still it wouldn't be perfect.
;;;   (";\\([^\"\n]*\"[^\"\n]*\"\\)*[^\"\n]*$" nil comment)

(hilit-set-mode-patterns
 '(emacs-lisp-mode lisp-interaction-mode)
 '(
   (";.*" nil comment)

;;; This almost works...but I think I'll stick with the parser function 
;;;("[^?]\\(\"\\(\"\\||\\([^\"]+\\|[\\]\\([\\][\\]\\)*\"\\)*\"\\)\\)" 1 string)
   (hilit-string-find ?\\ string)

   ("^\\s *(def\\(un\\|macro\\|advice\\|alias\\|subst\\)[ \t\n]"
    "\\()\\|nil\\)" defun)
   ("^\\s *(defvar\\s +\\S +" nil decl)
   ("^\\s *(defconst\\s +\\S +" nil define)
   ("^\\s *(\\(provide\\|require\\|\\(auto\\)?load\\).*$" nil include)
   ("\\s *\\&\\(rest\\|optional\\)\\s *" nil keyword)
   ("(\\(let\\*?\\|cond\\|if\\|or\\|and\\|map\\(car\\|concat\\)\\|prog[n1*]?\\|while\\|lambda\\|function\\|set\\([qf]\\|car\\|cdr\\)?\\|nconc\\|eval-when-compile\\|condition-case\\|unwind-protect\\|catch\\|throw\\|error\\)[ \t\n]" 1 keyword)
   ))

(hilit-set-mode-patterns
 '(lisp-mode ilisp-mode)
 '(
   (";.*" nil comment)
   ("#|" "|#" comment)
;;; This almost works...but I think I'll stick with the parser function 
;;;("[^?]\\(\"\\(\"\\||\\([^\"]+\\|[\\]\\([\\][\\]\\)*\"\\)*\"\\)\\)" 1 string)
   (hilit-string-find ?\\ string)

   ;; this is waaaaaaaay too slow
   ;;   ("^\\s *(def\\(un\\|macro\\|advice\\|alias\\|method\\|subst\\)\\s \\S +[ \t\n]+\\(nil\\|(\\(([^()]*)\\|[^()]+\\)*)\\)" nil defun)
   ("^\\s *(def\\(un\\|macro\\|advice\\|subst\\|method\\)\\s " "\\()\\|nil\\)" defun)

   ("^\\s *(\\(def\\(var\\|type\\|parameter\\)\\|declare\\)\\s +\\S +" nil decl)
   ("^\\s *(def\\(const\\(ant\\)?\\|class\\|struct\\)\\s \\S +[ \t\n]+" nil define)
   ("^\\s *(\\(provide\\|require\\|\\(auto\\)?load\\).*$" nil include)
   ("[ \t]\\&\\(key\\|rest\\|optional\\|aux\\)\\s *" nil keyword)
   ("(\\(let\\*?\\|locally\\|cond\\|if\\*?\\|or\\|and\\|map\\(car\\|c[ao]n\\)?\\|prog[nv1*]?\\|while\\|when\\|unless\\|do\\(\\*\\|list\\|times\\)\\|list\\|lambda\\|function\\|values\\|set\\([qf]\\|car\\|cdr\\)?\\|rplac[ad]\\|nconc\\|block\\|go\\|return\\(-from\\)?\\|[ec]?\\(type\\)?case\\|multiple-value-\\(bind\\|setq\\|list\\|call\\|prog1\\)\\|unwind-protect\\|handler-case\\|catch\\|throw\\|eval-when\\(-compile\\)?\\)[ \t\n]" 1 keyword)
   ))


(hilit-set-mode-patterns
 'plain-tex-mode
 '(("^%%.*$" nil comment)
   ("{\\\\em\\([^}]+\\)}" nil comment)
   ("\\(\\\\\\w+\\)" nil keyword)
   ("{\\\\bf\\([^}]+\\)}" nil keyword)
   ("^[ \t\n]*\\\\def[\\\\@]\\(\\w+\\)" nil defun)
   ("\\\\\\(begin\\|end\\){\\([A-Za-z0-9\\*]+\\)}" nil defun)
   ;; ("[^\\\\]\\$\\([^$]*\\)\\$" nil string)
   ("\\$\\([^$]*\\)\\$" nil string)
   ))

;; Reasonable extensions would include smarter parameter handling for such
;; things as the .IX and .I macros, which alternate the handling of following
;; arguments.

(hilit-set-mode-patterns
 'nroff-mode
 '(("^\\.[\\\][\\\"].*$" nil comment)
   ("^\\.so .*$" nil include)
   ("^\\.[ST]H.*$" nil defun)
;;   ("^[^\\.].*\"[^\\\"]*\\(\\\\\\(.\\)[^\\\"]*\\)*\"" nil string)
   ("\"" "[^\\]\"" string)
   ("^\\.[A-Z12\\\\].*$" nil define)
   ("\\([\\\][^ ]*\\)" nil keyword)
   ("^\\.[A-Z].*$" nil keyword))
 nil 'case-insensitive)

(hilit-set-mode-patterns
 'texinfo-mode
 '(("^\\(@c\\|@comment\\)\\>.*$" nil comment)
   ("@\\(emph\\|strong\\|b\\|i\\){[^}]+}" nil comment)
;; seems broken
;; ("\\$[^$]*\\$" nil string)
   ("@\\(file\\|kbd\\|key\\){[^}]+}" nil string)
   ("^\\*.*$" nil defun)
   ("@\\(if\\w+\\|format\\|item\\)\\b.*$" nil defun)
   ("@end +[A-Za-z0-9]+[ \t]*$" nil defun)
   ("@\\(samp\\|code\\|var\\){[^}]+}" nil defun)
   ("@\\w+\\({[^}]+}\\)?" nil keyword)
   ))

(hilit-set-mode-patterns
 'dired-mode
 (append
  '(("^D.*$"  nil dired-deleted)
   ("^\\*.*$" nil dired-marked)
   ("^  d.*$" nil dired-directory)
   ("^  l.*$" nil dired-link)
   ("^  -.*#.*#$" nil dired-ignored))
  (list (cons
	 (concat "^  .*\\("
		 (mapconcat 'regexp-quote completion-ignored-extensions "\\|")
		 "\\)$")
	 '(nil dired-ignored)))))

(hilit-set-mode-patterns
 'jargon-mode
 '(("^:[^:]*:" nil jargon-entry)
   ("{[^}]*}+" nil jargon-xref)))

(hilit-set-mode-patterns
 'Info-mode
 '(("^\\* [^:]+:+" nil jargon-entry)
   ("\\*[Nn]ote\\b[^:]+:+" nil jargon-xref)
   ("  \\(Next\\|Prev\\|Up\\):" nil jargon-xref)
   ("- \\(Variable\\|Function\\|Macro\\|Command\\|Special Form\\|User Option\\):.*$"
    nil jargon-keyword)))	; lisp manual

(hilit-set-mode-patterns
 'calendar-mode
 '(("[A-Z][a-z]+ [0-9]+" nil define)	; month and year
   ("S  M Tu  W Th  F  S" nil label)))	; week days

(hilit-set-mode-patterns
 'asm-mode
 '(("/\\*" "\\*/" comment)
   ("^#[ \t]*\\(undef\\|define\\).*$" "[^\\]$" define)
   ("^#.*$" nil include)
   ;; labels
   ("^.+:" nil defun)
   ;; assembler directives
   ("^[ \t]*\\..*$" nil decl)
   ;; register names
   ("\\$[a-z0-9]+" nil string)
   ;; mnemonics
   ("^[ \t]*[a-z]+" nil struct)))

(hilit-set-mode-patterns
 'pascal-mode
 '(("(\\*" "\\*)" comment)
   ("{" "}" comment)
   ;; Doesn't work when there are strings in comments....
   ;; ("'[^']*'" nil string)
   ("^#.*$" nil include)
   ("^[ \t]*\\(procedure\\|function\\)[ \t]+\\w+[^ \t(;]*" nil defun)
   ("\\<\\(program\\|begin\\|end\\)\\>" nil defun)
   ("\\<\\(external\\|forward\\)\\>" nil include)
   ("\\<\\(label\\|const\\|type\\|var\\)\\>" nil define)
   ("\\<\\(record\\|array\\|file\\)\\>" nil type)
   ("\\<\\(of\\|to\\|for\\|if\\|then\\|else\\|case\\|while\\|do\\|until\\|and\\|or\\|not\\|with\\|repeat\\)\\>" nil keyword)
   )
 nil 'case-insensitive)

(hilit-set-mode-patterns
 'icon-mode
 '(("#.*$" nil comment)
   ("\"[^\\\"]*\\(\\\\.[^\\\"]*\\)*\"" nil string)
   ;; charsets: these do not work because of a conflict with strings
   ;; ("'[^\\']*\\(\\\\.[^\\']*\\)*'" nil string)
   ("^[ \t]*procedure[ \t]+\\w+[ \t]*(" ")" defun)
   ("^[ \t]*record.*(" ")" include)
   ("^[ \t]*\\(global\\|link\\)[ \t\n]+[A-Za-z_0-9]+\\([ \t\n]*,[ \t\n]*[A-Za-z_0-9]+\\)*" nil include)
   ("^[ \t]*\\(local\\|static\\)[ \t\n]+[A-Za-z_0-9]+\\([ \t\n]*,[ \t\n]*[A-Za-z_0-9]+\\)*" nil decl)
   ("\\<\\(initial\\|end\\)\\>" nil glob-struct)
   ("\\<\\(while\\|until\\|return\\|every\\|if\\|then\\|else\\|to\\|case\\|of\\|suspend\\|create\\|do\\|repeat\\|break\\)\\>" nil keyword)
   ))

;; as you can see, I had two similar problems for Pascal and Icon. In
;; Pascal, strings are delimited with ' and an embedded quote is doubled,
;; thus string syntax would be extremely simple. However, if a string
;; occurs within a comment, the following text is considered a string.
;; 
;; In Icon, strings are similar to C ones, but there are also charsets,
;; delimited with simple quotes. I could not manage to use both regexps at
;; the same time.

;; The problem I have with my patterns for Icon is that this language has a
;; string similar constant to the C one (but a string can be cut on several
;; lines, if terminated by a dash and continued with initial blanks, like
;; this:
;;         "This is a somewhat long -
;;          string, written on three -
;;          successive lines"
;; in order to insert a double quote in a string, you have to escape it
;; with a \), bu also a character set constant (named a charset), which
;; uses single quotes instead of double ones. It would seem intuitive to
;; highlight both constants in the same way.


(provide 'hilit19.variation)

;;; hilit19 ends here.

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
;; postscript and pdf stuff
      

(load "ps-print")



;;;;;;;includeforelc!
;;;;;;(load "ps-print-invisible")

;;; ps-print-invisible.el - addon to ps-print package that deals
;;  with invisible text printing in xemacs

;; Author: Greg Chernov
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;; 
;; Put ps-print-invisible.el on your load path.
;; (require 'ps-print-invisible)
;; ps-print-buffer-with-faces will not print invisible parts of the buffer.
;; Work with invisible extents/text properties only 
;; (xemacs hideshow and noutline packages). 

(defun ps-generate-postscript-with-faces (from to)
  ;; Some initialization...
  (setq ps-current-effect 0)

  ;; Build the reference lists of faces if necessary.
  (when (or ps-always-build-face-reference
	    ps-build-face-reference)
    (message "Collecting face information...")
    (ps-build-reference-face-lists))

  ;; Black/white printer.
  (setq ps-black-white-faces-alist nil)
  (and (eq ps-print-color-p 'black-white)
       (ps-extend-face-list ps-black-white-faces nil
			    'ps-black-white-faces-alist))

  ;; Generate some PostScript.
  (save-restriction
    (narrow-to-region from to)
    (ps-print-ensure-fontified from to)
    (let ((face 'default)
	  (position to))
      (cond
       ((memq ps-print-emacs-type '(xemacs lucid))
       ;; Build the list of extents...
       ;;(debug)
	(let ((a (cons 'dummy nil))
	      record type extent extent-list
	      (list-invisible (ps-print-find-invisible-xmas from to)))
	  (ps-x-map-extents 'ps-mapper nil from to a)
	  (setq a (sort (cdr a) 'car-less-than-car)
		extent-list nil)
	  
	  ;; Loop through the extents...
	  (while a
	    (setq record (car a)
		  position (car record)
		  
		  record (cdr record)
		  type (car record)
		  
		  record (cdr record)
		  extent (car record))
	    
	    ;; Plot up to this record.
	    ;; XEmacs 19.12: for some reason, we're getting into a
	    ;; situation in which some of the records have
	    ;; positions less than 'from'.  Since we've narrowed
	    ;; the buffer, this'll generate errors.  This is a hack,
	    ;; but don't call ps-plot-with-face unless from > point-min.
	    (and (>= from (point-min))
		 (ps-plot-with-face from (min position (point-max)) face))
	    
	    (cond
	     ((eq type 'push)
	      (and (or (ps-x-extent-face extent)
		       (extent-property extent 'invisible))
		   (setq extent-list (sort (cons extent extent-list)
					   'ps-extent-sorter))))
	     
	     ((eq type 'pull)
	      (setq extent-list (sort (delq extent extent-list)
				      'ps-extent-sorter))))
	    
	    
	    (setq face (if extent-list
			   (let ((prop (extent-property (car extent-list) 'invisible)))
			     (if (or (and (eq buffer-invisibility-spec t)
					  (not (null prop)))
				     (and (consp buffer-invisibility-spec)
					  (or (memq prop buffer-invisibility-spec)
					      (assq prop buffer-invisibility-spec))))
				 'emacs--invisible--face
			       (ps-x-extent-face (car extent-list))))
			 'default)
		  from position
		  a (cdr a)))))

       ((eq ps-print-emacs-type 'emacs)
	(let ((property-change from)
	      (overlay-change from)
	      (save-buffer-invisibility-spec buffer-invisibility-spec)
	      (buffer-invisibility-spec nil)
	      before-string after-string)
	  (while (< from to)
	    (and (< property-change to)	; Don't search for property change
					; unless previous search succeeded.
		 (setq property-change (next-property-change from nil to)))
	    (and (< overlay-change to)	; Don't search for overlay change
					; unless previous search succeeded.
		 (setq overlay-change (min (ps-e-next-overlay-change from)
					   to)))
	    (setq position (min property-change overlay-change)
		  before-string nil
		  after-string nil)
	    ;; The code below is not quite correct,
	    ;; because a non-nil overlay invisible property
	    ;; which is inactive according to the current value
	    ;; of buffer-invisibility-spec nonetheless overrides
	    ;; a face text property.
	    (setq face
		  (cond ((let ((prop (get-text-property from 'invisible)))
			   ;; Decide whether this invisible property
			   ;; really makes the text invisible.
			   (if (eq save-buffer-invisibility-spec t)
			       (not (null prop))
			     (or (memq prop save-buffer-invisibility-spec)
				 (assq prop save-buffer-invisibility-spec))))
			 'emacs--invisible--face)
			((get-text-property from 'face))
			(t 'default)))
	    (let ((overlays (ps-e-overlays-at from))
		  (face-priority -1))	; text-property
	      (while (and overlays
			  (not (eq face 'emacs--invisible--face)))
		(let* ((overlay (car overlays))
		       (overlay-invisible
			(ps-e-overlay-get overlay 'invisible))
		       (overlay-priority
			(or (ps-e-overlay-get overlay 'priority) 0)))
		  (and (> overlay-priority face-priority)
		       (setq before-string
			     (or (ps-e-overlay-get overlay 'before-string)
				 before-string)
			     after-string
			     (or (and (<= (ps-e-overlay-end overlay) position)
				      (ps-e-overlay-get overlay 'after-string))
				 after-string)
			     face-priority overlay-priority
			     face
			     (cond
			      ((if (eq save-buffer-invisibility-spec t)
				   (not (null overlay-invisible))
				 (or (memq overlay-invisible
					   save-buffer-invisibility-spec)
				     (assq overlay-invisible
					   save-buffer-invisibility-spec)))
			       'emacs--invisible--face)
			      ((ps-e-overlay-get overlay 'face))
			      (t face)
			      ))))
		(setq overlays (cdr overlays))))
	    ;; Plot up to this record.
	    (and before-string
		 (ps-plot-string before-string))
	    (ps-plot-with-face from position face)
	    (and after-string
		 (ps-plot-string after-string))
	    (setq from position)))))
      (ps-plot-with-face from to face))))


(defun ps-print-find-invisible-xmas (from to)
  (let ((list nil))
    (map-extents '(lambda (ex ignored)
		    (let ((prop (extent-property ex 'invisible)))
		      (if (or (and (eq buffer-invisibility-spec t)
				   (not (null prop)))
			      (or (memq prop buffer-invisibility-spec)
				  (assq prop buffer-invisibility-spec)))
			  (setq list (cons (list 
					      (extent-start-position ex)
					      (extent-end-position ex))
					     list))))
		    nil)
		 (current-buffer)
		 from to nil 'start-and-end-in-region 'invisible)
    (reverse list)))


(defun ps-mapper (extent list)
  ;;(debug)
  (let ((beg (ps-x-extent-start-position extent))
	(end (ps-x-extent-end-position extent))
	(inv-lst list-invisible)
	(found nil))
    (while (and inv-lst
		(not found))
      (let ((inv-beg (caar inv-lst))
	    (inv-end (cadar inv-lst)))
	(if (and (>= beg inv-beg)
		 (<= end inv-end)
		 (not (extent-property extent 'invisible)))
	    (setq found t))
	(setq inv-lst (cdr inv-lst))))
    (if (not found) 
	(nconc list
	       (list (list beg 'push extent)
		     (list end 'pull extent)))))
      nil)


(provide 'ps-print-invisible)


;;; ps-print-invisible.el ends here


(if (< emacs-major-version 20)
   (progn
;;;;;;;includeforelc!
;;;;;;(load "ps-print-landscape")   ;;; Hack, aber funktioniert

;;
;;
;;
;;   ps-print-landscape facilities,
;;   add-on to ps-print.el, with hacks by Alexander Ebbes and Stefan Walther, 14.1.1998
;;   
;;   Alexander Ebbes <ebbes@uni-mainz.de>
;;   Stefan Walther  <walther@uni-mainz.de>
;;
;;   Written not nice but fast.
;;
;;   LOAD
;;
;;   (require 'ps-print)
;;   (require 'ps-print-landscape)
;;   ;;if you want to stay in portrait mode, insert then
;;   (ps-portrait-mode t)
;;
;;   USE
;;
;;   ;;Switch to landscape mode:
;;   (ps-landscape-mode t)  ;; no arg: toggeling
;;   ;;Switch to portrait mode:
;;   (ps-portrait-mode t)   ;; no arg: toggeling
;;
;;   Afterwards all ps-print-commands should use ps in landscape or portrait format
;;


(setq ps-landscape-mode nil)

(defun ps-landscape-mode (&optional on)  ;;; SMW
  "Toggle ps landscape mode: all ps-print commands are used for landscape mode.
With ARG not nil sets mode definivly to landscape"
  (interactive "P")
  (if (not on)
      (if ps-landscape-mode
	  (progn
	    (load "ps-print")
	    (setq ps-landscape-mode nil)
	    (message "ps-print is in portrait mode"))
	(load "ps-print-landscape")
	(setq ps-landscape-mode t)
	(message "ps-print is in landscape mode"))
    (load "ps-print-landscape")
    (setq ps-landscape-mode t)
    (message "ps-print is in landscape mode")))


(defun ps-portrait-mode (&optional on)   ;;;SMW
  "Toggle ps landscape mode: all ps-print commands are used for landscape mode.
With ARG not nil sets mode definivly to landscape"
  (interactive "P")
  (if (not on)
      (if ps-landscape-mode
	  (progn
	    (load "ps-print")
	    (setq ps-landscape-mode nil)
	    (message "ps-print is in portrait mode"))
	(load "ps-print-landscape")
	(setq ps-landscape-mode t)
	(message "ps-print is in landscape mode"))
    (load "ps-print")
    (setq ps-landscape-mode nil)
    (message "ps-print is in portrait mode")))




  
(define-key ctl-x-map "t" '(lambda ()
                             "toggle truncate-lines"
                             (interactive)
                             (if  truncate-lines
                                 (set-variable 'truncate-lines nil)
                               (set-variable 'truncate-lines t))
                             (redraw-display)))




(defun ps-get-page-dimensions ()
  (setq ps-page-dimensions (assq ps-paper-type ps-pages-alist))
  (let ((ps-page-height (nth ps-page-width-i ps-page-dimensions))    ;;; AE + SMW
	(ps-page-width (nth ps-page-height-i ps-page-dimensions)))   ;;; AE + SMW
    (setq ps-print-height (- ps-page-height ps-top-margin ps-bottom-margin))
    (setq ps-print-width (- ps-page-width ps-left-margin ps-right-margin))))

(defun ps-begin-file ()
  (setq ps-showpage-count 0)

  (ps-output ps-adobe-tag)
  (ps-output "%%Title: " (buffer-name) "\n") ;Take job name from name of
					;first buffer printed
  (ps-output "%%Creator: " (user-full-name) "\n")
  (ps-output "%%CreationDate: " 
	     (time-stamp-hh:mm:ss) " " (time-stamp-mon-dd-yyyy) "\n")
  (ps-output "%% DocumentFonts: Helvetica Helvetica-Bold "
	     ps-font " " ps-font-bold " " ps-font-italic " "
	     ps-font-bold-italic "\n")
  (ps-output "%%Pages: (atend)\n")
  (ps-output "%%Orientation: Landscape\n")   ;;; AE + SMW
  (ps-output "%%EndComments\n\n")

  (ps-output "630 0 translate\n")    ;;; AE + SMW
  (ps-output "90 rotate\n")          ;;; AE + SMW

  (ps-output-boolean "Duplex" ps-spool-duplex)
  (ps-output-boolean "PrintHeader" ps-print-header)
  (ps-output-boolean "PrintHeaderFrame" ps-print-header-frame)
  (ps-output-boolean "ShowNofN" ps-show-n-of-n)

  (ps-output (format "/LeftMargin %d def\n" ps-right-margin))   ;;; AE + SMW
  (ps-output (format "/RightMargin %d def\n" ps-left-margin))   ;;; AE + SMW
  (ps-output (format "/BottomMargin %d def\n" ps-top-margin))   ;;; AE + SMW
  (ps-output (format "/TopMargin %d def\n" ps-bottom-margin))   ;;; AE + SMW

  (ps-get-page-dimensions)
  (ps-output (format "/PrintWidth %d def\n" ps-print-width))
  (ps-output (format "/PrintHeight %d def\n" ps-print-height))
  
  (ps-output (format "/LineHeight %s def\n" ps-line-height))
  
  (ps-output ps-print-prologue)

  (ps-output (format "/f0 %d /%s Font\n" ps-font-size ps-font))
  (ps-output (format "/f1 %d /%s Font\n" ps-font-size ps-font-bold))
  (ps-output (format "/f2 %d /%s Font\n" ps-font-size ps-font-italic))
  (ps-output (format "/f3 %d /%s Font\n" ps-font-size
		     ps-font-bold-italic))

  (ps-output "%%EndPrologue\n"))

(provide 'ps-print-landscape)


;;; ps-print-landscape.el ends here
;;;;;;;includeforelc!
;;;;;;(load "op-ps-output-design-emacs19")




(ps-landscape-mode t)

(defun time-stamp-hh:mm ()
  "Return the current time as a string in \"HH:MM:SS\" form."
  (concat (substring (current-time-string) 11 16) " Uhr"))

(setq ps-right-header
  (list "/pagenumberstring load" 'time-stamp-dd-mon-yy 'time-stamp-hh:mm:ss))

;;(setq ps-header-lines 2)

;;(set ps-spool-duplex nil)

;; Achten auf
;;(setq ps-lpr-command
;;(setq ps-lpr-switches

(setq ps-paper-type 'ps-a4)


(setq ps-print-prologue "% ISOLatin1Encoding stolen from ps_init.ps in GhostScript 2.6.1.4:
% If the ISOLatin1Encoding vector isn't known, define it.
/ISOLatin1Encoding where { pop } {
% Define the ISO Latin-1 encoding vector.
% The first half is the same as the standard encoding,
% except for minus instead of hyphen at code 055.
/ISOLatin1Encoding
StandardEncoding 0 45 getinterval aload pop
    /minus
StandardEncoding 46 82 getinterval aload pop
%*** NOTE: the following are missing in the Adobe documentation,
%*** but appear in the displayed table:
%*** macron at 0225, dieresis at 0230, cedilla at 0233, space at 0240.
% \20x
    /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef
    /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef
    /dotlessi /grave /acute /circumflex /tilde /macron /breve /dotaccent
    /dieresis /.notdef /ring /cedilla /.notdef /hungarumlaut /ogonek /caron
% \24x
    /space /exclamdown /cent /sterling
	/currency /yen /brokenbar /section
    /dieresis /copyright /ordfeminine /guillemotleft
	/logicalnot /hyphen /registered /macron
    /degree /plusminus /twosuperior /threesuperior
	/acute /mu /paragraph /periodcentered
    /cedilla /onesuperior /ordmasculine /guillemotright
	/onequarter /onehalf /threequarters /questiondown
% \30x
    /Agrave /Aacute /Acircumflex /Atilde
	/Adieresis /Aring /AE /Ccedilla
    /Egrave /Eacute /Ecircumflex /Edieresis
	/Igrave /Iacute /Icircumflex /Idieresis
    /Eth /Ntilde /Ograve /Oacute
	/Ocircumflex /Otilde /Odieresis /multiply
    /Oslash /Ugrave /Uacute /Ucircumflex
	/Udieresis /Yacute /Thorn /germandbls
% \34x
    /agrave /aacute /acircumflex /atilde
	/adieresis /aring /ae /ccedilla
    /egrave /eacute /ecircumflex /edieresis
	/igrave /iacute /icircumflex /idieresis
    /eth /ntilde /ograve /oacute
	/ocircumflex /otilde /odieresis /divide
    /oslash /ugrave /uacute /ucircumflex
	/udieresis /yacute /thorn /ydieresis
256 packedarray def
} ifelse

/reencodeFontISO { %def
  dup
  length 5 add dict			% Make a new font (a new dict
					% the same size as the old
					% one) with room for our new
					% symbols.

  begin					% Make the new font the
					% current dictionary.


    { 1 index /FID ne
      { def } { pop pop } ifelse
    } forall				% Copy each of the symbols
					% from the old dictionary to
					% the new except for the font
					% ID.

    /Encoding ISOLatin1Encoding def	% Override the encoding with
					% the ISOLatin1 encoding.

    % Use the font's bounding box to determine the ascent, descent,
    % and overall height; don't forget that these values have to be
    % transformed using the font's matrix.
    FontBBox
    FontMatrix transform /Ascent exch def pop
    FontMatrix transform /Descent exch def pop
    /FontHeight Ascent Descent sub def

    % Define these in case they're not in the FontInfo (also, here
    % they're easier to get to.
    /UnderlinePosition 1 def
    /UnderlineThickness 1 def

    % Get the underline position and thickness if they're defined.
    currentdict /FontInfo known {
      FontInfo

      dup /UnderlinePosition known {
	dup /UnderlinePosition get
	0 exch FontMatrix transform exch pop
	/UnderlinePosition exch def
      } if

      dup /UnderlineThickness known {
	/UnderlineThickness get
	0 exch FontMatrix transform exch pop
	/UnderlineThickness exch def
      } if

    } if

    currentdict				% Leave the new font on the
					% stack

    end					% Stop using the font as the
					% current dictionary.

    definefont				% Put the font into the font
					% dictionary

    pop					% Discard the returned font.
} bind def

/Font {
  findfont exch scalefont reencodeFontISO
} def

/F {					% Font select
  findfont
  dup /Ascent get /Ascent exch def
  dup /Descent get /Descent exch def
  dup /FontHeight get /FontHeight exch def
  dup /UnderlinePosition get /UnderlinePosition exch def
  dup /UnderlineThickness get /UnderlineThickness exch def
  setfont
} def

/FG /setrgbcolor load def

/bg false def
/BG {
  dup /bg exch def
  { mark 4 1 roll ] /bgcolor exch def } if
} def

/dobackground {				% width --
  currentpoint
  gsave
    newpath
    moveto
    0 Ascent rmoveto
    dup 0 rlineto
    0 Descent Ascent sub rlineto
    neg 0 rlineto
    closepath
    bgcolor aload pop setrgbcolor
    fill
  grestore
} def

/dobackgroundstring {			% string --
  stringwidth pop
  dobackground
} def

/dounderline {				% fromx fromy --
  currentpoint
  gsave
    UnderlineThickness setlinewidth
    4 2 roll
    UnderlinePosition add moveto
    UnderlinePosition add lineto
    stroke
  grestore
} def

/eolbg {
  currentpoint pop
  PrintWidth LeftMargin add exch sub dobackground
} def

/eolul {
  currentpoint exch pop
  PrintWidth LeftMargin add exch dounderline
} def

/SL {					% Soft Linefeed
  bg { eolbg } if
  ul { eolul } if
  currentpoint LineHeight sub LeftMargin exch moveto pop
} def

/HL /SL load def			% Hard Linefeed

/sp1 { currentpoint 3 -1 roll } def

% Some debug
/dcp { currentpoint exch 40 string cvs print (, ) print = } def
/dp { print 2 copy
   exch 40 string cvs print (, ) print = } def

/S {
  bg { dup dobackgroundstring } if
  ul { sp1 } if
  show
  ul { dounderline } if
} def

/W {
  ul { sp1 } if
  ( ) stringwidth			% Get the width of a space
  pop					% Discard the Y component
  mul					% Multiply the width of a
					% space by the number of
					% spaces to plot
  bg { dup dobackground } if
  0 rmoveto
  ul { dounderline } if
} def

/BeginDSCPage {
  /vmstate save def
} def

/BeginPage {
  PrintHeader {
    PrintHeaderFrame { HeaderFrame } if
    HeaderText
  } if
  LeftMargin
  BottomMargin PrintHeight add
  moveto				% move to where printing will
					% start.
} def

/EndPage {
  bg { eolbg } if
  ul { eolul } if
  showpage				% Spit out a page
} def

/EndDSCPage {
  vmstate restore
} def

/ul false def

/UL { /ul exch def } def

/h0 14 /Helvetica-Bold Font
/h1 12 /Helvetica Font

/h1 F

/HeaderLineHeight FontHeight def
/HeaderDescent Descent def
/HeaderPad 2 def

/SetHeaderLines {
  /HeaderOffset TopMargin 2 div def
  /HeaderLines exch def
  /HeaderHeight HeaderLines HeaderLineHeight mul HeaderPad 2 mul add def
  /PrintHeight PrintHeight HeaderHeight sub def
} def

/HeaderFrameStart {
  LeftMargin BottomMargin PrintHeight add HeaderOffset add
} def

/HeaderFramePath {
  PrintWidth 0 rlineto
  0 HeaderHeight rlineto
  PrintWidth neg 0 rlineto
  0 HeaderHeight neg rlineto
} def

/HeaderFrame {
  gsave
    0.4 setlinewidth
    HeaderFrameStart moveto
    1 -1 rmoveto
    HeaderFramePath
    0 setgray fill
    HeaderFrameStart moveto
    HeaderFramePath
    gsave 1 setgray fill grestore      % AE + SMW
    gsave 0 setgray stroke grestore
  grestore
} def

/HeaderStart {
  HeaderFrameStart
  exch HeaderPad add exch
  HeaderLineHeight HeaderLines 1 sub mul add HeaderDescent sub HeaderPad add
} def

/strcat {
  dup length 3 -1 roll dup length dup 4 -1 roll add string dup
  0 5 -1 roll putinterval
  dup 4 2 roll exch putinterval
} def

/pagenumberstring {
  PageNumber 32 string cvs
  ShowNofN {
    (/) strcat
    PageCount 32 string cvs strcat
  } if
} def

/HeaderText {
  HeaderStart moveto

  HeaderLinesRight HeaderLinesLeft
  Duplex PageNumber 1 and 0 eq and { exch } if

  {
    aload pop
    exch F
    gsave
      dup xcheck { exec } if
      show
    grestore
    0 HeaderLineHeight neg rmoveto
  } forall

  HeaderStart moveto

   {
    aload pop
    exch F
    gsave
      dup xcheck { exec } if
      dup stringwidth pop
      PrintWidth exch sub HeaderPad 2 mul sub 0 rmoveto
      show
    grestore
    0 HeaderLineHeight neg rmoveto
  } forall
} def

/ReportFontInfo {
  2 copy
  /t0 3 1 roll Font
  /t0 F
  /lh FontHeight def
  /sw ( ) stringwidth pop def
  /aw (01234567890abcdefghijklmnopqrstuvwxyz) dup length exch
  stringwidth pop exch div def
  /t1 12 /Helvetica-Oblique Font
  /t1 F
  72 72 moveto
  gsave
    (For ) show
    128 string cvs show
    ( ) show
    32 string cvs show
    ( point, the line height is ) show
    lh 32 string cvs show
    (, the space width is ) show
    sw 32 string cvs show
    (,) show
  grestore
  0 FontHeight neg rmoveto
  (and a crude estimate of average character width is ) show
  aw 32 string cvs show
  (.) show
  showpage
} def

% 10 /Courier ReportFontInfo
")
;; ^
;; | (muss nicht sein !!)
)
(make-variable-buffer-local 'ps-landscape-mode)
(setq ps-landscape-mode t) 
;; ^^^^^^^^^^^^^^^^^^^^^^^
;; | (muss nicht sein !!)
)


;;;;;;;includeforelc!
;;;;;;(load "op-pdfprint")







(defun pdf-spool-buffer ()
  (interactive)
    (let ((filedir (file-name-directory (buffer-file-name (current-buffer))))
	  (filename (buffer-file-name (current-buffer))))
      (save-excursion
	(ps-spool-buffer)
	(switch-to-buffer "*PostScript*")
	(goto-char (point-min))
	(write-file (concat filename ".ps"))
	(kill-buffer (current-buffer)) 
	(if (equal system-type 'windows-nt)
	    (default-dir-real-shell-command (concat "%GVDIR%/ps2pdf.bat " (concat filename ".ps") (concat filename ".pdf")))
	  (default-dir-real-shell-command (concat "$GVDIR/ps2pdf.linux " (concat filename ".ps"))))
	(delete-file (concat filename ".ps")))
      (message (concat "File " (concat filename ".pdf") " written to file directory " filedir)))
    )



(defun pdf-spool-buffer-with-faces ()
  (interactive)
    
    (let ((filedir (file-name-directory (buffer-file-name (current-buffer))))
	  (filename (buffer-file-name (current-buffer))))
      (save-excursion
	(ps-spool-buffer-with-faces)
	(switch-to-buffer "*PostScript*")
	(goto-char (point-min))
	(write-file (concat filename ".ps"))
	(kill-buffer (current-buffer)) 
	(if (equal system-type 'windows-nt)
	    (default-dir-real-shell-command (concat "%GVDIR%/ps2pdf.bat " (concat filename ".ps") (concat filename ".pdf")))
	  (default-dir-real-shell-command (concat "$GVDIR/ps2pdf.linux " (concat filename ".ps"))))
	(delete-file (concat filename ".ps")))
      (message (concat "File " (concat filename ".pdf") " written to file directory " filedir)))
    )



(defun pdf-spool-region ()
  (interactive)
;  (if (buffer-modified-p)
;      (error "Please save buffer first: Undo problems might occur")
    
    (let ((filedir (file-name-directory (buffer-file-name (current-buffer))))
	  (filename (buffer-file-name (current-buffer))))
      (save-excursion
	(ps-spool-region (region-beginning) (region-end))
	(switch-to-buffer "*PostScript*")
	(goto-char (point-min))
	(write-file (concat filename ".ps"))
	(kill-buffer (current-buffer)) 
	(if (equal system-type 'windows-nt)
	    (default-dir-real-shell-command (concat "%GVDIR%/ps2pdf.bat " (concat filename ".ps") (concat filename ".pdf")))
	  (default-dir-real-shell-command (concat "$GVDIR/ps2pdf.linux " (concat filename ".ps"))))
	(delete-file (concat filename ".ps")))
      (message (concat "Region " (concat filename ".pdf") " written to file directory " filedir)))
    )



(defun pdf-spool-region-with-faces ()
  (interactive)
;  (if (buffer-modified-p)
;      (error "Please save buffer first: Undo problems might occur")
    
    (let ((filedir (file-name-directory (buffer-file-name (current-buffer))))
	  (filename (buffer-file-name (current-buffer))))
      (save-excursion
	(ps-spool-region-with-faces (region-beginning) (region-end))
	(switch-to-buffer "*PostScript*")
	(goto-char (point-min))
	(write-file (concat filename ".ps"))
	(kill-buffer (current-buffer)) 
	(if (equal system-type 'windows-nt)
	    (default-dir-real-shell-command (concat "%GVDIR%/ps2pdf.bat " (concat filename ".ps") (concat filename ".pdf")))
	  (default-dir-real-shell-command (concat "$GVDIR/ps2pdf.linux " (concat filename ".ps"))))
	(delete-file (concat filename ".ps")))
      (message (concat "File " (concat filename ".pdf") " written to file directory " filedir)))
    )










;;; wird noch einmal wiederholt in op-hilit.xemacs.el
;(if xemacsp
;    (defun hilit-repaint-command (&optional var)
;      (isearch-dehighlight t)
;      t))

;;;;;;;includeforelc!
;;;;;;(load "op-calendar/op-calendar")

; (setq debug-on-error t)
;;;
;;;
;;;   Dieser File ist ein Teil der OP-Suite zum Organisieren großer und sehr großer Projekte
;;;   mit Hilfe des emacs und des outline-Modus (daher der Name: Outline-Project). 
;;;   Dieser File kann aber auch alleine Verwendet werden als Kalender unter verschiedenen 
;;;   Betriebssystemen, getestet unter Unix, NT, VM/CMS, MVS und VMS. Einzige Bedingung ist
;;;   ein gemeinsam verfügbares Directory.
;;;
;;;   Zeile zum Starten des Kalenders:
;;;   emacs -l ~/op-project/op-calendar/op-calendar.el -f calendar-in-extra-window -iconic &
;;;
;;;   Feiertage etc. werden nicht extra berechnet, sondern mit Hilfe eines gcal-Interfaces 
;;;   eingelesen. Dieses gcal-Interface ist im Moment nur Offline verfügbar, d.h. es muss
;;;   ein File op-calendar-holidays.el[c] erzeugt werden. Dies geschieht mit Hilfe der Funktion 
;;;   insert-gcal-holidays-in-calendar hier unten im File. Der dort einzulesende File musste 
;;;   vorher durch gcal erzeugt worden sein:
;;;   LANG=de gcal -u -n -q de -H no --date-format="%w,  %1%D%2 %M %y" --cc-holidays=de+fr+us+nl+gb 2012+1960 >tmp/gcal-output
;;;   Nach diesem File wird dann von der Funktion insert-gcal-holidays-in-calendar gefragt
;;;
;;;   Eigenschaften des OP-Kalenders:
;;;   - Vollwertiger Kalender
;;;   - persönlicher Terminplaner kombiniert mit Gruppenterminplaner
;;;   - Feiertage verschiedener Nationen werden angezeigt, aber nicht als Feiertag eingetragen
;;;   - Nationale Feiertage werden in den vier gcal-Abstufungen wiedergegeben (in den meisten 
;;;     Bundesländern, in einigen Bundesländern,...)
;;;   - Tage wie der 1. April oder Muttertag werden unter Merktagen wiedergegeben, jedoch außer auf
;;;     Wunsch nicht speziell gekennzeichnet
;;;   - halbwegs sinnvolle Optik unter allen oben aufgeführten Betriebssystemen
;;;   - voll der deutschen Sprache mächtig
;;;   - Schnellfunktion (c) im gleichen Buffer oder in einem extra Fenster (calendar-in-extra-window)
;;;
;;;   Der File op-calendar.el ist der File op-calendar-changes.el mit includetem hilit19.el
;;;
;;;
;;;   Geschrieben um Weihnachten 1997 von Dr. Stefan Walther, Rudolf-Diesel-Str. 3, 55131 Mainz
;;;
;;;
;;;
;;; command emacs -l ~/op-project/op-calendar/op-calendar -f calendar-in-extra-window -iconic &

(require 'ps-print)

;(setq diary-file "~/op-project/op-diary")
;(setq group-diary-file "~/op-project/op-group-diary")
;(setq number-of-diary-entries 8 )   ; Für wieviele Tage soll das Tagebuch ausgerechnet werden ?


(if (boundp 'op-project-calendar-loaded)  ; wenn der Kalender schon einmal geladen wurde, dann nichts
    nil


(if (not (boundp 'diary-fill-column))
    (setq diary-fill-column 82)) ;; Fill-column in den Diary-Files



(defun set-load-path ()
  "Erweitere den Pfad der Kommando-Optionen um den Platz, an dem op-calendar steht.
Dient dazu, dass der File op-calendar-holidaylist einfach im selben Directory stehen kann,
aber nicht beim Benutzer im Load-path eingetragen sein muss."
  (let ((command-line-args command-line-args))
    (while (and command-line-args
		(not (posix-string-match "[/\\]op-calendar.el" (car command-line-args))))
      (setq command-line-args (cdr command-line-args)))
    (if command-line-args
	(setq load-path
	      (if (string= (substring (expand-file-name (file-name-directory (car command-line-args))) -1) "/")
		  (cons (substring (expand-file-name (file-name-directory (car command-line-args))) 0 -1)
			load-path)
		(cons (expand-file-name (file-name-directory (car command-line-args)))
		      load-path))))))

;; aktiviere den momentanen Ladepfad
(set-load-path)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun insert-weekday-diary-entry (&optional args)
  "Setzt diary-weekday in den Kalender ein"
  (interactive)
  (let ((dayofweek)(weeknumber))
    (setq dayofweek (completing-read "Welcher Wochentag: " '(("Montag" 1)
							     ("Dienstag" 2)
							     ("Mittwoch" 3)
							     ("Donnerstag" 4)
							     ("Freitag" 5)
							     ("Samstag" 6)
							     ("Sonntag" 7))
				     nil t))
    (setq weeknumber (completing-read "in jeder wievielten Woche im Monat: " '(("1" 1)
									       ("2" 2)
									       ("3" 3)
									       ("4" 4)
									       ("5" 5)) 
				      nil t))
    (make-diary-entry
     (format "%s(diary-weekday   %s \"%s\")"
             sexp-diary-entry-symbol
	     weeknumber
	     dayofweek) args)))



(defun diary-weekday (weeknumber dayofweek)
"Eintrag, wenn DAYOFWEEK in WEEKNUMBER übereinstimmt,
z.B. jeder 2. Donnerstag im Monat."
  (let ((dayname (calendar-day-of-week date))
	(day (extract-calendar-day date))
	(weeklist nil))
    (cond ((= weeknumber 1)
	   (setq weeklist '(1 2 3 4 5 6 7)))
	  ((= weeknumber 2)
	   (setq weeklist '(8 9 10 11 12 13 14)))
	  ((= weeknumber 3)
	   (setq weeklist '(15 16 17 18 19 20 21)))
	  ((= weeknumber 4)
	   (setq weeklist '(22 23 24 25 26 27 28)))
	  ((= weeknumber 5)
	   (setq weeklist '(29 30 31))))
    (setq dayofweek (downcase dayofweek))
    (cond ((string= dayofweek "montag")
	   (setq dayofweek 1))
	  ((string= dayofweek "dienstag")
	   (setq dayofweek 2))
	  ((string= dayofweek "mittwoch")
	   (setq dayofweek 3))
	  ((string= dayofweek "donnerstag")
	   (setq dayofweek 4))
	  ((string= dayofweek "freitag")
	   (setq dayofweek 5))
	  ((string= dayofweek "samstag")
	   (setq dayofweek 6))
	  ((string= dayofweek "sonntag")
	   (setq dayofweek 7)))
    (if (and (memq day weeklist) 
	     (= dayname dayofweek))
	entry)))

(message "Kalender geladen bis Marke 0/1")

(setq european-calendar-display-form
  '((if dayname (concat dayname ", ")) day "." monthname " " year))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(if (not (boundp 'signature))
    (setq signature ""))


(setq general-holidays nil)
(setq local-holidays nil)
(setq other-holidays nil)
(setq christian-holidays nil) 
(setq hebrew-holidays nil)
;(setq islamic-holidays nil)
(setq oriental-holidays nil)
;(setq solar-holidays nil)

(setq islamic-holidays
  '((holiday-islamic
     1 1
     (format "[Islamisches Neujahrsfest %d]]"
             (let ((m displayed-month)
                   (y displayed-year))
               (increment-calendar-month m y 1)
               (extract-calendar-year
                (calendar-islamic-from-absolute
                 (calendar-absolute-from-gregorian
                  (list
                   m (calendar-last-day-of-month m y) y)))))))
    (holiday-islamic 9 1 "[[Beginn der muslimischen Fastenzeit Ramadan [Merktag]]]")
    (holiday-islamic 9 30 "[[Ende der muslimischen Fastenzeit Ramadan [Merktag]]]")))

(defvar solar-n-hemi-seasons
  '("Frühjahrs- Tag- und Nachtgleiche" "Sommersonnenwende" "Herbst-Tag- und Nachtgleiche" "Wintersonnenwende")
  "List of season changes for the northern hemisphere.")

(defvar solar-s-hemi-seasons
  '("Herbst-Tag- und Nachtgleiche" "Wintersonnenwende" "Frühjahrs- Tag- und Nachtgleiche" "Sommersonnenwende")
  "List of season changes for the southern hemisphere.")

(require 'solar)
(defun solar-sunrise-sunset-string (date)
  "String of *local* times of sunrise, sunset, and daylight on Gregorian DATE."
  (let ((l (solar-sunrise-sunset date)))
    (format
     "%s, %s at %s (%s Stunden Tageslicht)"
     (if (car l)
         (concat "Sonnenaufgang " (apply 'solar-time-string (car l)))
       "No sunrise")
     (if (car (cdr l))
         (concat "Sonnenuntergang " (apply 'solar-time-string (car (cdr l))))
       "Kein Sonnenuntergang")
     (eval calendar-location-name)
     (car (cdr (cdr l))))))

(require 'lunar)
(defun lunar-phase-name (phase)
  "Name of lunar PHASE.
0 = new moon, 1 = first quarter, 2 = full moon, 3 = last quarter."
  (cond ((= 0 phase) "Neumond")
        ((= 1 phase) "Zunehmender Halbmond")
        ((= 2 phase) "Vollmond")
        ((= 3 phase) "Abnehmender Halbmond")))



(message "Kalender geladen bis Marke 1a")



;; Lade das Diary, wenn es sich um den ersten Neuaufruf des OP-Modes handelt
;; und checke die Diary-files auf Syntaxfehler
(if (not (boundp 'op-diary-already-loaded))
    (progn
      (let ((current-buf (current-buffer)))
	;; if there is an error in the diary file
	(condition-case nil
	    (diary)
	  (error "Fehler im persönlichen Diary-File %s oder im Gruppendiaryfile %s" diary-file 
		 (if (boundp 'group-diary-file) group-diary-file "[wird nicht benutzt]")))
	(setq op-diary-already-loaded t)
	(switch-to-buffer current-buf))))



(setq mark-calendar-holidays-initially t)

(message "Kalender geladen bis Marke 1b")



;;; ========================================================================
;;; ========================================================================

(setq emacs-mainframe (selected-frame))

(defconst calendar-buffer "*Kalender*"
  "Name of the buffer used for the calendar.")

(defconst holiday-buffer "*Feier- und Merktage*"
  "Name of the buffer used for the displaying the holidays.")

(defconst fancy-diary-buffer "*Termine*"
  "Name of the buffer used for the optional fancy display of the diary.")

(defconst lunar-phases-buffer "*Mondphasen*"
  "Name of the buffer used for the lunar phases.")


(defvar calendar-setup nil
  "The frame set up of the calendar.
The choices are `one-frame' (calendar and diary together in one separate,
dedicated frame) or `two-frames' (calendar and diary in separate, dedicated
frames); with nil the current frame is used. Other value may lead to
strange results.")
(make-variable-buffer-local 'calendar-setup)


(message "Kalender geladen bis Marke 2")


(if window-system
    (progn
      (if xemacsp
	  (require 'overlay))
      (require 'hilit19.variation)
      (hilit-set-mode-patterns
       'calendar-mode
       '(("[A-Z][a-z]+ [0-9]+" nil define)	; Monat und Jahr
	 ("Mo Di Mi Do Fr Sa So" nil struct)	; Wochentage, geht davon aus, dass der Sonntag der letzte Wochentag ist
	 ))))

(if gnuemacsp
    (progn
      (require 'calendar)
      (require 'diary-lib)
      (require 'holidays)
      (require 'appt)
      ))
(if xemacsp
    (progn
      (require 'calendar)
      (require 'diary-lib)
      (load "holidays")
      (load "appt")
      ))


(message "Kalender geladen bis Marke 3")


(setq mark-sundays t)


(if (not (fboundp 'mark-all-calendar-holidays-original))
    (fset 'mark-all-calendar-holidays-original (symbol-function 'mark-calendar-holidays)))

(defun mark-calendar-holidays ()
  "Wie calendar-holidays, aber ohne die \"merktage\"-, sondern 
nur mit der \"gcal\"-feiertage\"-Liste."
  (interactive)
  (let ((calendar-holidays nil))
    (setq calendar-holidays gcal-feiertage)
    (mark-all-calendar-holidays-original)
    ))


(defun mark-all-calendar-holidays ()
  "Wie calendar-holidays, aber mit der \"merktage\"-
und der \"gcal\"-feiertage\"-Liste."
  (interactive)
  (let ((calendar-holidays nil))
    (setq calendar-holidays
	  (append general-holidays local-holidays other-holidays
		  christian-holidays hebrew-holidays islamic-holidays
		  oriental-holidays solar-holidays
		  gcal-feiertage merktage
		  ))
    (mark-all-calendar-holidays-original)
    ))


(define-key calendar-mode-map "?" 'calendar-kurzinfo)
(defun calendar-kurzinfo ()
  "Go to the info node for the calendar."
  (interactive)
  (if calendar-setup
      (progn
	(make-frame-visible emacs-mainframe)
	(select-frame emacs-mainframe)))
  (switch-to-buffer "*Kalender-Kurzanleitung*")
  (if (< emacs-major-version 20)
      (standard-display-european t))
  (insert 
   "Kurzanleitung für den Kalender-Mode:

Es gibt generell drei Stufen von Kalendern, die miteinander
zusammenspielen:
- Persönlicher Terminkalender (nicht einsehbar von anderen)
- Persönlicher einsehbarer Kalender (damit einem jemand anderes einen
  Termin eintragen kann)
- Allgemeiner Gruppenkalender (für Termine, die für alle interessant
  sind)
Mithilfe von #include-Statements (auch rekursiv) im Kalenderfile sind 
alle Kombinationen zwischen verschiedenen Zugriffstufen erlaubt.

Alle hier genannten Funktionen sind auch über die Menüs ansprechbar.

Im Kalender-Fenster:

   Fenster aus dem Weg schaffen

       Strg-x Strg-y ==> Fenster iconifizieren
       Strg-x k ==> Fensterbuffer rauswerfen (z.B. bei der
                    Feiertagsliste)
       q ==> löscht beim ersten Mal das Terminkalenderfenster, dann
             iconifiziert es den Kalender
       Strg-x Strg-c vernichtet das Gesamtprogramm

   Bewegen:

       Cursortasten ==> im Kalenderfenster anderen Tag auswählen
       f ==> einen Monat vorwärts
       b ==> einen Monat rückwärts
       .  ==> zurück zum heutigen Datum
       Strg-v ==> 3 Monate nach vorne
       Alt-v ==> 3 Monate nach hinten
       Strg-l ==> Kalender zentrieren und neu einfärben
       Strg-o ==> Wechseln zum Terminkalenderfenster
       Alt-d ==> Terminkalenderfenster nach unten scrollen
       Alt-u ==> Terminkalenderfenster nach oben scrollen

   Informationen:

       n ==> Info über den heutigen Tag und die Kalenderwoche
       Menü -> News: Aktuelle Informationen

   Terminkalender ansehen:

       d ==> Zeigt Terminkalender des bezeichneten Tages
       Strg-u [Anzahl] d zeigt soviele Tage ab dem gekennzeichneten
             Tag
       q ==> Fenster verlassen
       D ==> Terminkalnder von jemand anderem ansehen

   Termine eintragen:

       Termine für einen Tag eintragen
	   i d ==> Termin in den persönlichen Terminkalender eintragen
	   i g d ==> Termin in den Gruppenterminkalender eintragen
	   i o d ==> Termin in einen anderen Terminkalenderfile
                     eintragen
       Termine für einen Zeitraum eintragen
           - auf den Anfangstag gehen
           - Strg-Freizeichentaste drücken
           - zum letzten Tag gehen
           dann (b steht für Block):
           i b ==> Zeitraum im persönlichen Terminkalender
           i g b ==> Zeitraum im Gruppenterminkalender
           i o b ==> Zeitraum in einem fremden Terminkalender
       Regelmäßige Termine eintragen:
           i w ==> wöchentlich
           i m ==> monatlich
           i y ==> jährlich
           i c ==> zyklisch wiederkehrende Tage
           i g w, i o w usw entsprechend für Gruppenterminkalender und
               Fremdkalender

Im Tagebuch-File:

   Alt-x cde ==> sucht im persönlichen Kalender sowie im
           Gruppenkalender nach dem aktuelle Eintrag (nicht sehr gut
           programmiert...)

")
  (setq buffer-read-only t)
  (goto-char (point-min))
  ;;
  (setq my-mode-map (cons 'keymap text-mode-map))
  (local-set-key "q" '(lambda ()(interactive)(kill-buffer (current-buffer))(iconify-frame)))
  (message "q verläßt den Buffer"))


(message "Kalender geladen bis Marke 3a")


;; SMW spezial
(defvar diary-noentry-string "bisher kein Termin"
  "If \"\", no diary will be popped up if no diary entry exists.
If not empty string, this string will be shown in empty diary entries")


(defun calendar-week-number (date)
  "Gives you the number of the week of DATE in the current year"
  (let* ((mark-diary-entries-in-calendar mark-diary-entries-in-calendar)
	 (mark-diary-entries-in-calendar nil))
    (calendar-goto-date (reverse (cons  (extract-calendar-year date) '(1 1))))
    (calendar-end-of-week 1)
     ;; in calendar-end-of-week ist ein Bug: daher
      (if (> (calendar-day-number (calendar-cursor-to-date t)) 7)
  	(calendar-backward-week 1))
      ;; Kalenderwoche 1 ist immer die erste vollständige Woche im neuen Jahr!!
    (let ((offset (calendar-day-number (calendar-cursor-to-date t))))
      (if (>= offset 7) (setq offset (- offset 7)))
      (calendar-goto-date date)
      (ceiling (/ (- (calendar-day-number date) offset) 7.0)))))


(defun calendar-day-message ()
  (interactive)
  (let* ((day-number (calendar-day-number (calendar-cursor-to-date t)))
	 (datestring (calendar-date-string (calendar-cursor-to-date t)))
	 (calendarweek (calendar-week-number (calendar-cursor-to-date t)))
	 (mark-diary-entries-in-calendar mark-diary-entries-in-calendar)
	 (mark-diary-entries-in-calendar nil))
    (if window-system
	(progn
	  (hilit-repaint-command t)
	  (if mark-sundays (mark-calendar-days-named 7)))) ;; alle Sonntage rot einfärben
    (if (< day-number 20)
	(message ">> %s, der %dte Tag des Jahres in Kalenderwoche %d <<"
		 datestring day-number calendarweek)
      (message ">> %s, der %dste Tag des Jahres in Kalenderwoche %d <<"
	       datestring day-number calendarweek)
    )))

(message "Kalender geladen bis Marke 3b")


(defun calendar-day-message-dummy ()
  (calendar-day-message))



(defun holiday-extract-from-list (kallist)
  "Returns kallist if visible, returns nil 
  if it is not visible in the current calendar window."
  (let ((m displayed-month)
        (y displayed-year)
	month)
    (setq month (car (car kallist)))
    (setq year (nth 2 (car kallist)))
    ;; 
    (if (> 2 (abs (- (+ (* 12 y) m) (+ (* 12 year) month))))
	(progn
	  (increment-calendar-month m y (- 11 month))
	  (if (> m 9) (list kallist) nil)))))

(message "Kalender geladen bis Marke 3c")

;;; Marke: XEmacs
(if (not (get-buffer calendar-buffer))
    (progn
      (let ((current-buf (current-buffer)))

	(load "op-calendar/op-calendar-holidaylist")

	(setq calendar-setup nil)

	(setq calendar-day-name-array
	      ["Sonntag" "Montag" "Dienstag" "Mittwoch" "Donnerstag" "Freitag" "Samstag"])
	
	(if (or (< emacs-major-version 20) xemacsp)
	    (setq calendar-month-name-array
		  ["Januar" "Februar" "März" "April" "Mai" "Juni" "Juli" "August" "September" "Oktober" "November" "Dezember"])
	  (setq calendar-month-name-array
		["Januar" "Februar" "März" "April" "Mai" "Juni" "Juli" "August" "September" "Oktober" "November" "Dezember"]))
	(setq calendar-week-start-day 1)
	(setq number-of-diary-entries 8)	   ; für wieviele Tage soll das Tagebuch ausgerechnet werden ?
 

	;; if XEmacs
	(if xemacsp
	    (progn
	      (setq time-stamp-month-numbers
		'(("Jan" . 1) ("Feb" . 2) ("Mär" . 3) ("Apr" . 4) ("Mai" . 5) ("Jun" . 6)
		  ("Jul" . 7) ("Aug" . 8) ("Sep" . 9) ("Okt" . 10) ("Nov" . 11) ("Dez" . 12)))
	      
	      (setq time-stamp-month-full-names
		["(zero)" "Januar" "Februar" "März" "April" "Mai" "Juni"
		 "Juli" "August" "September" "Oktober" "November" "Dezember"])
	      
	      (setq time-stamp-weekday-numbers
		'(("Son" . 0) ("Mon" . 1) ("Die" . 2) ("Mit" . 3)
		  ("Don" . 4) ("Fre" . 5) ("Sam" . 6)))

	      (setq time-stamp-weekday-full-names
		["Sonntag" "Montag" "Dienstag" "Mittwoch" "Donnerstag" "Freitag" "Samstag"])
	      
	      (setq time-stamp-format "%02d.%02m.%02y // %02H:%02M:%02S %u")

	      (defvar timezone-months-assoc
		'(("JAN" .  1)("FEB" .  2)("MÄR" .  3)
		  ("APR" .  4)("MAI" .  5)("JUN" .  6)
		  ("JUL" .  7)("AUG" .  8)("SEP" .  9)
		  ("OKT" . 10)("NOV" . 11)("DEZ" . 12))
		"Alist of first three letters of a month and its numerical representation.")

	      ))







	(calendar)
	(setq calendar-time-display-form '(24-hours ":" minutes (if time-zone " (") time-zone (if time-zone ")")))
	(setq calendar-date-display-form	'(day "." month "." year))
	(european-calendar)
	(if mark-calendar-holidays-initially
	    (mark-calendar-holidays))

	(calendar-day-message)

;	(modify-syntax-entry ?ä "w")
;	(modify-syntax-entry ?Ä "w")
;	(modify-syntax-entry ?ö "w")
;	(modify-syntax-entry ?Ö "w")
;	(modify-syntax-entry ?ü "w")
;	(modify-syntax-entry ?Ü "w")
;	(modify-syntax-entry ?ß "w")

	(defun actual-week-numberf (date)
	  "Gives you the number of the week of DATE in the current year"
	  (calendar-goto-date (reverse (cons  (extract-calendar-year date) '(1 1))))
	  (calendar-end-of-week 1)
	  ;; in calendar-end-of-week ist ein Bug: daher
	  (if (> (calendar-day-number (calendar-cursor-to-date t)) 7)
	      (calendar-backward-week 1))
	  (let ((offset (- 7 (calendar-day-number (calendar-cursor-to-date t)))))
	    (calendar-goto-date date)
	    (ceiling (/ (+ (calendar-day-number date) offset) 7.0))))
	
	(setq actual-week-number (actual-week-numberf (calendar-current-date)))

	(if window-system
	    (progn
	      (hilit-repaint-command t)
	      (if mark-sundays (mark-calendar-days-named 7)))) ;; alle Sonntage rot einfärben
	(delete-window)
	(setq diary-list-include-blanks t)

	(add-hook 'diary-display-hook 'fancy-diary-display)
	(add-hook 'diary-display-hook 'appt-make-list)

	;; the next two statements are enabeling the #include "filename" statement in the diary file
	(add-hook 'list-diary-entries-hook 'include-other-diary-files)
	(add-hook 'mark-diary-entries-hook 'mark-included-diary-files)
	(add-hook 'list-diary-entries-hook 'sort-diary-entries t)
	
	(setq today-visible-calendar-hook 'calendar-mark-today)
	
	(setq appt-message-warning-time 25)  ; Wieviele Minuten vorher will ich vor einem Termin gewarnt werden ?
	(setq appt-display-duration 8)	   ; Wieviele Sekunden soll die Display-Zeile erinnern ?

	(switch-to-buffer current-buf))))


(message "Kalender geladen bis Marke 4")


(defvar original-window-config nil
  "Current windown configuration before starting the calendar with \"c\"")
(defun c (&optional ask-for-date read-window-config)
  "calls calendar and re-hilits it"
  (interactive)
  (if (not read-window-config)
      (setq original-window-config (current-window-configuration)))
  (if (frame-live-p calendar-frame) (delete-frame calendar-frame))
  (if (< emacs-major-version 20)
      (standard-display-european t))
  (setq display-time-24hr-format t)
  (setq display-time-day-and-date t)
  (setq calendar-setup nil)
  (split-window-vertically)
  (calendar-basic-noframe-setup ask-for-date)
  (switch-to-buffer calendar-buffer)
  (if (> (window-height) 9) (shrink-window (- (window-height) 9)))
  (setq pop-up-windows t)
  (setq pop-up-frame-function 'switch-to-buffer)
  (setq special-display-function 'switch-to-buffer)
  (let ((pop-up-windows nil)
	(view-diary-entries-initially nil))
    ;;
    ;; hier werden die Feiertage geladen !!!
    (require 'op-calendar-holidaylist)
    ;;
    ;(calendar-recenter)
    (if mark-calendar-holidays-initially
	(mark-calendar-holidays))
    (unless mark-diary-entries-in-calendar
      (calendar-day-message))
    (define-key calendar-mode-map "d" 'view-diary-entries-2)
    (define-key calendar-mode-map "D" 'view-other-diary-entries-2)
    ;; Umdefinition der Funktion make-diary-entry
))


  

(defun calendar-basic-noframe-setup (&optional arg)
  (interactive "P")
  (set-buffer (get-buffer-create calendar-buffer))
  (calendar-mode)
  (let* ((date (if arg
                   (calendar-read-date t)
                 (calendar-current-date)))
         (month (extract-calendar-month date))
         (year (extract-calendar-year date)))
    (increment-calendar-month month year (- calendar-offset))
    (generate-calendar-window month year)
    (if (and view-diary-entries-initially (calendar-date-is-visible-p date))
        (view-diary-entries
         (if (vectorp number-of-diary-entries)
             (aref number-of-diary-entries (calendar-day-of-week date))
           number-of-diary-entries))))
  (let* ((diary-buffer (get-file-buffer diary-file)))
    (if view-calendar-holidays-initially
	(list-calendar-holidays)))
  (run-hooks 'initial-calendar-window-hook))


(defun calendar-forward-month-center ()
  (interactive)
  (if mark-diary-entries-in-calendar
      (progn
	(setq mark-diary-entries-in-calendar nil)
	(calendar-forward-month 1)
	(setq mark-diary-entries-in-calendar t))
    (calendar-forward-month 1))
  (calendar-recenter))

(defun calendar-backward-month-center ()
  (interactive)
  (if mark-diary-entries-in-calendar
      (progn
	(setq mark-diary-entries-in-calendar nil)
	(calendar-backward-month 1)
	(setq mark-diary-entries-in-calendar t))
    (calendar-backward-month 1))
  (calendar-recenter))

(defun calendar-goto-today-rehilit ()
  (interactive)
  (calendar-goto-today)
  (if window-system
      (progn
	(hilit-repaint-command t)
	(if mark-sundays (mark-calendar-days-named 7)))) ;; alle Sonntage rot einfärben
  (calendar-recenter))

(defun calendar-recenter ()
  (interactive)
  ;; besser, aber langsamer
  ;;(calendar-backward-week 21)
  ;;(calendar-forward-week 21)
  ;; WARNUNG
  ;; im Anblick verwirrend, aber mir ist nix anderes eingefallen
  (let ((mark-diary-entries-in-calendar mark-diary-entries-in-calendar))
    (setq mark-diary-entries-in-calendar nil)
    (calendar-backward-month 12)
    (calendar-forward-month 12)
    (if window-system
	(progn
	  (hilit-repaint-command t)
	  (if mark-sundays (mark-calendar-days-named 7)))))) ;; alle Sonntage rot einfärben

(defun calendar-quit () 
  "Quits first the diary window, afterwards iconifies the calendar"
  (interactive)
  ;; falls Diary existiert, lösche mit q den Diary-Buffer,
  ;; ansonsten iconifizieren den Calender
  (if calendar-setup
      (if (get-buffer fancy-diary-buffer)
	  (progn
	    (switch-to-buffer-other-frame fancy-diary-buffer)
	    (kill-buffer (current-buffer)))
	(exit-calendar)
	(if calendar-setup (other-frame -1) (other-window -1)))
    (exit-calendar))
  (set-window-configuration original-window-config))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define-key calendar-mode-map [menu-bar edit] 'undefined)
(define-key calendar-mode-map [menu-bar search] 'undefined)
(define-key calendar-mode-map [menu-bar scroll] 'undefined)

(define-key calendar-mode-map [down-mouse-2] 'calendar-mouse-2-date-menu)
(define-key calendar-mode-map [mouse-2] 'ignore)

(defvar calendar-mouse-3-map (make-sparse-keymap "Kalender"))
(define-key calendar-mode-map [down-mouse-3] calendar-mouse-3-map)

(if xemacsp
    (define-key calendar-mode-map [(control button3)] calendar-mouse-3-map)
  (define-key calendar-mode-map [C-down-mouse-3] calendar-mouse-3-map))

(define-key calendar-mode-map [menu-bar moon]
  (cons "Phasen" (make-sparse-keymap "Phasen")))

(define-key calendar-mode-map [menu-bar moon moon]
  '("Mondphasen" . calendar-phases-of-moon))

(define-key calendar-mode-map [menu-bar diary]
  (cons "Basis" (make-sparse-keymap "Grundfunktionen")))

(define-key calendar-mode-map [menu-bar diary weekday]
  '("Wochentagstermin jede n-te Monatswoche eintragen" . insert-weekday-diary-entry))
(define-key calendar-mode-map [menu-bar diary cyc]
  '("Zyklisch wiederkehrendes Datum in Privatkalender eintragen" . insert-cyclic-diary-entry))
(define-key calendar-mode-map [menu-bar diary blk]
  '("Zeitraum in Privatkalender eintragen (ab Marke)" . insert-block-diary-entry))
(define-key calendar-mode-map [menu-bar diary ann]
  '("Jahrestag in Privatkalender eintragen" . insert-anniversary-diary-entry))
(define-key calendar-mode-map [menu-bar diary yr]
  '("Jährlichen wiederkehrenden Termin in Privatkalender eintragen" . insert-yearly-diary-entry))
(define-key calendar-mode-map [menu-bar diary mon]
  '("Monatlich wiederkehrenden Termin in Privatkalender eintragen" . insert-monthly-diary-entry))
(define-key calendar-mode-map [menu-bar diary wk]
  '("Wöchentlich wiederkehrenden Termin in Privatkalender eintragen" . insert-weekly-diary-entry))
(define-key calendar-mode-map [menu-bar diary ent]
  '("Termin in Privatterminkalender eintragen" . insert-diary-entry))
(define-key calendar-mode-map [menu-bar diary markdate]
  '("Setze Marke bei diesem Datum [auch (C-SPC)]" . calendar-set-mark))
(define-key calendar-mode-map [menu-bar diary all]
  '("Termineintragungen des private Terminkalenders" . show-all-diary-entries))
(define-key calendar-mode-map [menu-bar diary mark]
 '("Markiere alle Tage" . mark-diary-entries))
(define-key calendar-mode-map [menu-bar diary view]
  '("Termine ab heutigen Tag" . view-diary-entries))
(define-key calendar-mode-map [menu-bar diary view]
  '("Anderer Tagebuchfile" . view-other-diary-entries))

(if (< emacs-major-version 20)
    (progn
      (define-key calendar-mode-map [menu-bar holidays]
	(cons "Feiertage" (make-sparse-keymap "Feiertage")))
      (define-key calendar-mode-map [menu-bar holidays unmark]
	'("Feiertage nicht anzeigen" . calendar-unmark-rehilit))
      (define-key calendar-mode-map [menu-bar holidays all-known]
	'("Alle benamten Tage anzeigen" . mark-all-calendar-holidays))
      (define-key calendar-mode-map [menu-bar holidays mark]
	'("Feiertage anzeigen" . mark-calendar-holidays))
      (if xemacsp
	  nil
	(define-key calendar-mode-map [menu-bar holidays 1-day]
	  '("Heutige Feiertage" . calendar-cursor-holidays))))
  (define-key calendar-mode-map [menu-bar Holidays]
    (cons "Feiertage" (make-sparse-keymap "Feiertage")))
  (define-key calendar-mode-map [menu-bar Holidays unmark]
    '("Feiertage nicht anzeigen" . calendar-unmark-rehilit))
  (define-key calendar-mode-map [menu-bar Holidays all-known]
    '("Alle benamten Tage anzeigen" . mark-all-calendar-holidays))
  (define-key calendar-mode-map [menu-bar Holidays mark]
    '("Feiertage anzeigen" . mark-calendar-holidays))
  (if xemacsp
      nil
    (define-key calendar-mode-map [menu-bar Holidays 1-day]
      '("Heutige Feiertage" . calendar-cursor-holidays))))

(define-key calendar-mode-map [menu-bar goto]
  (cons "Bewegen" (make-sparse-keymap "Bewegen")))


(define-key calendar-mode-map [menu-bar goto alien]
  (cons "Terminkalender mit anderen Rechnungsweisen" (make-sparse-keymap "Terminkalender mit anderen Rechnungsweisen")))

(define-key calendar-mode-map [menu-bar goto alien heb]
  '("Hebräisches Datum eintragen" . calendar-mouse-insert-hebrew-diary-entry))
(define-key calendar-mode-map [menu-bar goto alien isl]
  '("Islamisches Datum eintragen" . calendar-mouse-insert-islamic-diary-entry))
(define-key calendar-mode-map [menu-bar goto alien french]
  '("Französisches Datum" . calendar-goto-french-date))
(define-key calendar-mode-map [menu-bar goto alien mayan]
  (cons "Maya-Datum" (make-sparse-keymap "Maya-Datum")))
(define-key calendar-mode-map [menu-bar goto alien ethiopic]
  '("Äthiopisches Datum" . calendar-goto-ethiopic-date))
(define-key calendar-mode-map [menu-bar goto alien coptic]
  '("Koptisches Datum" . calendar-goto-coptic-date))
(define-key calendar-mode-map [menu-bar goto alien chinese]
  '("Chinesisches Datum" . calendar-goto-chinese-date))
(define-key calendar-mode-map [menu-bar goto alien julian]
  '("Julianisches Kalenderdatum" . calendar-goto-julian-date))
(define-key calendar-mode-map [menu-bar goto alien islamic]
  '("Islamisches Datum" . calendar-goto-islamic-date))
(define-key calendar-mode-map [menu-bar goto alien persian]
  '("Persisches Datum" . calendar-goto-persian-date))
(define-key calendar-mode-map [menu-bar goto alien hebrew]
  '("Hebräisches Datum" . calendar-goto-hebrew-date))
(define-key calendar-mode-map [menu-bar goto alien astro]
  '("Astronomisches Datum" . calendar-goto-astro-day-number))
(define-key calendar-mode-map [menu-bar goto alien iso]
  '("ISO-Datum" . calendar-goto-iso-date))



(define-key calendar-mode-map [menu-bar goto bk-12]
  '("Ein Jahr zurück" . "4\ev"))
(define-key calendar-mode-map [menu-bar goto bk-3]
  '("3 Monate zurück" . scroll-calendar-right-three-months))
(define-key calendar-mode-map [menu-bar goto bk-1]
  '("1 Monat zurück" . scroll-calendar-right))
(define-key calendar-mode-map [menu-bar goto fwd-12]
  '("Ein Jahr weiter" . "4\C-v"))
(define-key calendar-mode-map [menu-bar goto fwd-3]
  '("3 Monate weiter" . scroll-calendar-left-three-months))
(define-key calendar-mode-map [menu-bar goto fwd-1]
  '("1 Monat weiter" . scroll-calendar-left))


(define-key calendar-mode-map [menu-bar goto gregorian]
  '("Anderes Datum" . calendar-goto-date))
(define-key calendar-mode-map [menu-bar goto gregorian-month]
  '("Anderer Monat" . calendar-other-month))
(define-key calendar-mode-map [menu-bar goto end-of-year]
  '("Zum Jahresende" . calendar-end-of-year))
(define-key calendar-mode-map [menu-bar goto beginning-of-year]
  '("Zum Jahresanfang" . calendar-beginning-of-year))
(define-key calendar-mode-map [menu-bar goto end-of-month]
  '("Zum Monatsende" . calendar-end-of-month))
(define-key calendar-mode-map [menu-bar goto beginning-of-month]
  '("Zum Monatsbeginn" . calendar-beginning-of-month))
(define-key calendar-mode-map [menu-bar goto end-of-week]
  '("Zum Ende der Woche" . calendar-end-of-week))
(define-key calendar-mode-map [menu-bar goto beginning-of-week]
  '("Zum Anfang der Woche" . calendar-beginning-of-week))
(define-key calendar-mode-map [menu-bar goto today]
  '("Zum heutigen Datum" . calendar-goto-today))


(define-key calendar-mode-map [menu-bar goto alien mayan prev-rnd]
  '("Previous Round" . calendar-previous-calendar-round-date))
(define-key calendar-mode-map [menu-bar goto alien mayan nxt-rnd]
  '("Next Round" . calendar-next-calendar-round-date))
(define-key calendar-mode-map [menu-bar goto alien mayan prev-haab]
  '("Previous Haab" . calendar-previous-haab-date))
(define-key calendar-mode-map [menu-bar goto alien mayan next-haab]
  '("Next Haab" . calendar-next-haab-date))
(define-key calendar-mode-map [menu-bar goto alien mayan prev-tzol]
  '("Previous Tzolkin" . calendar-previous-tzolkin-date))
(define-key calendar-mode-map [menu-bar goto alien mayan next-tzol]
  '("Next Tzolkin" . calendar-next-tzolkin-date))



(defun calendar-mouse-2-date-menu (event)
  "Pop up menu for Mouse-2 for selected date in the calendar window."
  (interactive "e")
  (let* ((date (calendar-event-to-date t))
         (selection
          (x-popup-menu
           event
           (list (calendar-date-string date t nil)
                 (list
                  ""
                  '("Termineintragungen" . calendar-mouse-view-diary-entries)
                  '("Neuen Termin im privaten Terminkalender eintragen" . calendar-mouse-insert-diary-entry)
                  '("Neuen Termin im persönlichen Terminkalender eintragen" . calendar-mouse-insert-personal-diary-entry)
                  '("Neuen Termin im Gruppenterminkalender eintragen" . calendar-mouse-insert-group-diary-entry)
                  '("Tag markieren" . calendar-mouse-set-mark)
                  '("Ferientage" . calendar-mouse-holidays)
                  '("Sonnenaufgang/Sonnenuntergang" . calendar-mouse-sunrise/sunset)
                  '("Anderer Kalender" . calendar-mouse-print-dates)
                  '("Anderer Terminfile"
                    . calendar-mouse-view-other-diary-entries)
                  )))))
    (and selection (call-interactively selection))))

(define-key calendar-mouse-3-map [exit-calendar]
  '("Kalender verlassen" . exit-calendar))
(define-key calendar-mouse-3-map [show-diary]
  '("Terminkalender" . show-all-diary-entries))
(define-key calendar-mouse-3-map [lunar-phases]
  '("Mondphasen" . calendar-phases-of-moon))
(define-key calendar-mouse-3-map [unmark]
  '("Markierung löschen" . calendar-unmark))
(define-key calendar-mouse-3-map [mark-holidays]
  '("Feiertage kennzeichnen" . mark-calendar-holidays))
(define-key calendar-mouse-3-map [list-holidays]
  '("Feiertage auflisten" . list-calendar-holidays))
(define-key calendar-mouse-3-map [mark-diary-entries]
  '("Termintage kennzeichnen" . mark-diary-entries))
(define-key calendar-mouse-3-map [scroll-backward]
  '("Drei Monate rückwärts" . scroll-calendar-right-three-months))
(define-key calendar-mouse-3-map [scroll-forward]
  '("Drei Monate vorwärts" . scroll-calendar-left-three-months))


(defun calendar-mouse-insert-personal-diary-entry ()
  "Insert diary entry for mouse-selected date."
  (interactive)
  (let ((tmp diary-file))
    (setq diary-file personal-diary-file)
    (save-excursion
      (calendar-mouse-goto-date (calendar-event-to-date))
      (insert-diary-entry nil))
    (setq diary-file tmp)))

(defun calendar-mouse-insert-group-diary-entry ()
  "Insert diary entry for mouse-selected date."
  (interactive)
  (let ((tmp diary-file))
    (setq diary-file group-diary-file)
    (save-excursion
      (calendar-mouse-goto-date (calendar-event-to-date))
      (insert-diary-entry nil))
    (setq diary-file tmp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





(define-key calendar-mode-map " " 'calendar-scroll-diary-down) ; Blank-Taste
(define-key calendar-mode-map [menu-bar goto calendar-scroll-down-diary-1]
  '("Scolle Terminkalenderfenster noch unten" . calendar-scroll-diary-down))
(define-key calendar-mode-map "\C-?" 'calendar-scroll-diary-up) ; delete-Taste
(define-key calendar-mode-map [menu-bar goto calendar-scroll-up-diary-1]
  '("Scolle Terminkalenderfenster noch oben" . calendar-scroll-diary-up))



(define-key calendar-mode-map "\M-d" 'calendar-scroll-diary-down)
(define-key calendar-mode-map [menu-bar goto scroll-down-diary]
  '("Blättere Terminkalenderfenster noch unten" . calendar-scroll-diary-down))
(defun calendar-scroll-diary-down () 
  "Scrolls the diary window"
  (interactive)
  (let ((calbuf (current-buffer)) dum)
    (if (get-buffer fancy-diary-buffer)
	(progn
	  (if calendar-setup
	      (switch-to-buffer-other-frame fancy-diary-buffer)
	    (switch-to-buffer-other-frame fancy-diary-buffer))
	  (condition-case dum
	      (scroll-up 5)
	    (error "End of buffer"))
	  (if calendar-setup
	      (switch-to-buffer-other-frame calbuf)
	    (switch-to-buffer-other-frame calbuf))))))

(define-key calendar-mode-map "\M-u" 'calendar-scroll-diary-up)
(define-key calendar-mode-map [menu-bar goto scroll-up-diary]
  '("Blättere Terminkalenderfenster nach oben" . calendar-scroll-diary-up))
(defun calendar-scroll-diary-up () 
  "Scrolls the diary window"
  (interactive)
  (let ((calbuf (current-buffer)) dum)
    (if (get-buffer fancy-diary-buffer)
	(progn
	  (if calendar-setup
	      (switch-to-buffer-other-frame fancy-diary-buffer)
	    (switch-to-buffer-other-frame fancy-diary-buffer))
	  (condition-case dum
	      (scroll-down 5)
	    (error "End of buffer"))
	  (if calendar-setup
	      (switch-to-buffer-other-frame calbuf)
	    (switch-to-buffer-other-frame calbuf))))))


(define-key calendar-mode-map "\C-o" 'change-focus)
(define-key calendar-mode-map [menu-bar goto change-focus]
  '("Ins andere Fenster" . change-focus))
(defun change-focus ()
  (interactive)
  (if calendar-setup
      (raise-frame (next-frame nil 'visible))
    (other-window 1)))









(message "Kalender geladen bis Marke 5")


(define-key calendar-mode-map "." 'calendar-goto-today-rehilit)
(define-key calendar-mode-map [menu-bar goto today]
  '("Zum heutigen Datum" . calendar-goto-today-rehilit))
(define-key calendar-mode-map "b" 'calendar-backward-month-center)
(define-key calendar-mode-map [menu-bar goto bk-1]
  '("1 Monat zurück" . calendar-backward-month-center))
(define-key calendar-mode-map "f" 'calendar-forward-month-center)
(define-key calendar-mode-map [menu-bar goto fwd-1]
  '("1 Monat vorwärts" . calendar-forward-month-center))
(define-key calendar-mode-map "\C-l" 'calendar-recenter)
(define-key calendar-mode-map [menu-bar goto recenter]
  '("Kalender neu zeichnen" . calendar-recenter))
(define-key calendar-mode-map "n" 'calendar-day-message)
(define-key calendar-mode-map [menu-bar goto daymess]
  '("Info über den Tag" . calendar-day-message))
(define-key calendar-mode-map "d" 'view-diary-entries-2)
(define-key calendar-mode-map "D" 'view-other-diary-entries-2)
(define-key calendar-mode-map "q" 'calendar-quit)
(if (not xemacsp)
    (define-key menu-bar-file-menu [quit-cal]
      '("Kalender iconifizieren" . calendar-quit)))
(define-key calendar-mode-map "c" 'mark-all-calendar-holidays)
(define-key calendar-mode-map [menu-bar goto change-diary-entry]
  '("Terminkalendereintrag suchen [M-x cde]" . change-diary-entry))



;; Mode-Line
(setq calendar-mode-line-format
  (list
   (substitute-command-keys "\\<calendar-mode-map>\\[scroll-calendar-left]")
   "Kalender"
   (substitute-command-keys "\\<calendar-mode-map>|\\[calendar-kurzinfo]=Info|\\[calendar-other-month]=Andere|\\[calendar-goto-today-rehilit]=Heute|")
   '(calendar-date-string (calendar-current-date) nil)
   (substitute-command-keys "\\<calendar-mode-map>\\[scroll-calendar-right]")))



;;; auto-fill-function neu definieren
(defun newline-and-tab ()
  (interactive)
  (if (not (<= (current-column) (current-fill-column)))
      (insert "\n\t\t")))


;; ================================================
;;
;; Modifizieren des Eintragens der Tagebucheinträge
;;
;;
(defvar calendar-date-display-form-tmp nil)

(defadvice insert-diary-entry (before set-date-substring activate)
  (setq calendar-date-display-form-tmp calendar-date-display-form)
  (setq calendar-date-display-form	'(day " " monthname " " year))
  )
(defadvice insert-diary-entry (after activate-auto-fill-function activate)
  (make-local-variable 'auto-fill-function)
  (setq auto-fill-function 'newline-and-tab)
  (set-fill-column diary-fill-column) ;; an den PS-Mode angeglichen
  (setq calendar-date-display-form calendar-date-display-form-tmp)
  )

(defadvice insert-block-diary-entry (after activate-auto-fill-function activate)
  (make-local-variable 'auto-fill-function)
  (setq auto-fill-function 'newline-and-tab)
  (set-fill-column diary-fill-column) ;; an den PS-Mode angeglichen
  )
(defadvice insert-cyclic-diary-entry (after activate-auto-fill-function activate)
  (make-local-variable 'auto-fill-function)
  (setq auto-fill-function 'newline-and-tab)
  (set-fill-column diary-fill-column) ;; an den PS-Mode angeglichen
  )
(defadvice insert-anniversary-diary-entry (after activate-auto-fill-function activate)
  (make-local-variable 'auto-fill-function)
  (setq auto-fill-function 'newline-and-tab)
  (set-fill-column diary-fill-column) ;; an den PS-Mode angeglichen
  )
(defadvice insert-yearly-diary-entry (after activate-auto-fill-function activate)
  (make-local-variable 'auto-fill-function)
  (setq auto-fill-function 'newline-and-tab)
  (set-fill-column diary-fill-column) ;; an den PS-Mode angeglichen
  )
(defadvice insert-monthly-diary-entry (after activate-auto-fill-function activate)
  (make-local-variable 'auto-fill-function)
  (setq auto-fill-function 'newline-and-tab)
  (set-fill-column diary-fill-column) ;; an den PS-Mode angeglichen
  )
(defadvice insert-weekly-diary-entry (after activate-auto-fill-function activate)
  (make-local-variable 'auto-fill-function)
  (setq auto-fill-function 'newline-and-tab)
  (set-fill-column diary-fill-column) ;; an den PS-Mode angeglichen
  )
(defadvice insert-weekday-diary-entry (after activate-auto-fill-function activate)
  (make-local-variable 'auto-fill-function)
  (setq auto-fill-function 'newline-and-tab)
  (set-fill-column diary-fill-column) ;; an den PS-Mode angeglichen
  )
;;
;;
;; ================================================
;;





;;;
;;; Für den Persönlichen, von aussen eintragbaren kalender: 
;;; Prefig i p setzt in den perönlichen, von aussen eintragbaren ein, 
;;; was sonst in den eigenen eigesetzt wird
;;;
;;; i p c		insert-cyclic-diary-entry
;;; i p b		insert-block-diary-entry
;;; i p a		insert-anniversary-diary-entry
;;; i p y		insert-yearly-diary-entry
;;; i p m		insert-monthly-diary-entry
;;; i p w		insert-weekly-diary-entry
;;; i p d		insert-diary-entry
;;;

(define-key calendar-mode-map "ipd" 'insert-personal-diary)
(defun insert-personal-diary (&optional args) 
  (interactive "P")
  (let ((tmp diary-file))
    (setq diary-file personal-diary-file)
    (insert-diary-entry args)
    (if (not (or (string-match user-login-name (file-name-nondirectory buffer-file-name))
		 (string-match user-real-login-name (file-name-nondirectory buffer-file-name))))
	(save-excursion
	  (insert "  <" signature ">")))
;    (save-excursion
;      (insert (concat "\n\t\t<File: " (file-name-nondirectory buffer-file-name) ">")))
    (setq diary-file tmp)))
(define-key calendar-mode-map "ipb" 'insert-personal-diary-block)
(defun insert-personal-diary-block (&optional args) 
  (interactive "P")
  (let ((tmp diary-file))
    (setq diary-file personal-diary-file)
    (insert-block-diary-entry args)
    (if (not (or (string-match user-login-name (file-name-nondirectory buffer-file-name))
		 (string-match user-real-login-name (file-name-nondirectory buffer-file-name))))
	(save-excursion
	  (insert "  <" signature ">")))
;    (save-excursion
;      (insert (concat "\n\t\t<File: " (file-name-nondirectory buffer-file-name) ">")))
    (setq diary-file tmp)))
(define-key calendar-mode-map "ipc" 'insert-personal-cyclic-diary)
(defun insert-personal-cyclic-diary (&optional args) 
  (interactive "P")
  (let ((tmp diary-file))
    (setq diary-file personal-diary-file)
    (insert-cyclic-diary-entry args)
    (if (not (or (string-match user-login-name (file-name-nondirectory buffer-file-name))
		 (string-match user-real-login-name (file-name-nondirectory buffer-file-name))))
	(save-excursion
	  (insert "  <" signature ">")))
;    (save-excursion
;      (insert (concat "\n\t\t<File: " (file-name-nondirectory buffer-file-name) ">")))
    (setq diary-file tmp)))
(define-key calendar-mode-map "ipa" 'insert-personal-anniversary-entry)
(defun insert-personal-anniversary-entry (&optional args)
  (interactive "P")
  (let ((tmp diary-file))
    (setq diary-file personal-diary-file)
    (insert-anniversary-diary-entry args)
    (if (not (or (string-match user-login-name (file-name-nondirectory buffer-file-name))
		 (string-match user-real-login-name (file-name-nondirectory buffer-file-name))))
	(save-excursion
	  (insert "  <" signature ">")))
;    (save-excursion
;      (insert (concat "\n\t\t<File: " (file-name-nondirectory buffer-file-name) ">")))
    (setq diary-file tmp)))
(define-key calendar-mode-map "ipy" 'insert-personal-yearly-diary-entry)
(defun insert-personal-yearly-diary-entry (&optional args)
  (interactive "P")
  (let ((tmp diary-file))
    (setq diary-file personal-diary-file)
    (insert-yearly-diary-entry args)
    (if (not (or (string-match user-login-name (file-name-nondirectory buffer-file-name))
		 (string-match user-real-login-name (file-name-nondirectory buffer-file-name))))
	(save-excursion
	  (insert "  <" signature ">")))
;    (save-excursion
;      (insert (concat "\n\t\t<File: " (file-name-nondirectory buffer-file-name) ">")))
    (setq diary-file tmp)))
(define-key calendar-mode-map "ipm" 'insert-personal-monthly-diary-entry)
(defun insert-personal-monthly-diary-entry (&optional args)
  (interactive "P")
  (let ((tmp diary-file))
    (setq diary-file personal-diary-file)
    (insert-monthly-diary-entry args)
    (if (not (or (string-match user-login-name (file-name-nondirectory buffer-file-name))
		 (string-match user-real-login-name (file-name-nondirectory buffer-file-name))))
	(save-excursion
	  (insert "  <" signature ">")))
;    (save-excursion
;      (insert (concat "\n\t\t<File: " (file-name-nondirectory buffer-file-name) ">")))
    (setq diary-file tmp)))
(define-key calendar-mode-map "ipw" 'insert-personal-weekly-diary-entry)
(defun insert-personal-weekly-diary-entry (&optional args)
  (interactive "P")
  (let ((tmp diary-file))
    (setq diary-file personal-diary-file)
    (insert-weekly-diary-entry args)
    (if (not (or (string-match user-login-name (file-name-nondirectory buffer-file-name))
		 (string-match user-real-login-name (file-name-nondirectory buffer-file-name))))
	(save-excursion
	  (insert "  <" signature ">")))
;    (save-excursion
;      (insert (concat "\n\t\t<File: " (file-name-nondirectory buffer-file-name) ">")))
    (setq diary-file tmp)))
(define-key calendar-mode-map "ipr" 'insert-personal-weekday-diary-entry)
(defun insert-personal-weekday-diary-entry (&optional args)
  (interactive "P")
  (let ((tmp diary-file))
    (setq diary-file personal-diary-file)
    (insert-weekday-diary-entry args)
    (if (not (or (string-match user-login-name (file-name-nondirectory buffer-file-name))
		 (string-match user-real-login-name (file-name-nondirectory buffer-file-name))))
	(save-excursion
	  (insert "  <" signature ">")))
;    (save-excursion
;      (insert (concat "\n\t\t<File: " (file-name-nondirectory buffer-file-name) ">")))
    (setq diary-file tmp)))
;;; den Gruppenkalenderfile ansehen (wie s normal)
(define-key calendar-mode-map "p" 'show-personal-entries)
(defun show-personal-entries ()
  (interactive)
  (let ((tmp diary-file))
    (setq diary-file personal-diary-file)
    (show-all-diary-entries)
    (setq diary-file tmp)))

(defadvice show-all-diary-entries (after show-really-everything-c activate)
  (show-really-everything-c))


;;;
;;; Für den Gruppenkalender: Prefig i g setzt in den Gruppenkalender ein, was sonst in den eigenen eigesetzt wird
;;;
;;; i g c		insert-cyclic-diary-entry
;;; i g b		insert-block-diary-entry
;;; i g a		insert-anniversary-diary-entry
;;; i g y		insert-yearly-diary-entry
;;; i g m		insert-monthly-diary-entry
;;; i g w		insert-weekly-diary-entry
;;; i g d		insert-diary-entry
;;;


(define-key calendar-mode-map "igd" 'insert-group-diary)
(defun insert-group-diary (&optional args) 
  (interactive "P")
  (let ((tmp diary-file))
    (setq diary-file group-diary-file)
    (insert-diary-entry args)
    (if (not (or (string-match user-login-name (file-name-nondirectory buffer-file-name))
		 (string-match user-real-login-name (file-name-nondirectory buffer-file-name))))
	(save-excursion
	  (insert "  <" signature ">")))
;    (save-excursion
;      (insert "\n\t\t<Gruppenterminkalenderfile>"))
    (setq diary-file tmp)))
(define-key calendar-mode-map "igb" 'insert-group-diary-block)
(defun insert-group-diary-block (&optional args) 
  (interactive "P")
  (let ((tmp diary-file))
    (setq diary-file group-diary-file)
    (insert-block-diary-entry args)
    (if (not (or (string-match user-login-name (file-name-nondirectory buffer-file-name))
		 (string-match user-real-login-name (file-name-nondirectory buffer-file-name))))
	(save-excursion
	  (insert "  <" signature ">")))
;    (save-excursion
;      (insert "\n\t\t<Gruppenterminkalenderfile>"))
    (setq diary-file tmp)))
(define-key calendar-mode-map "igc" 'insert-group-cyclic-diary)
(defun insert-group-cyclic-diary (&optional args) 
  (interactive "P")
  (let ((tmp diary-file))
    (setq diary-file group-diary-file)
    (insert-cyclic-diary-entry args)
    (if (not (or (string-match user-login-name (file-name-nondirectory buffer-file-name))
		 (string-match user-real-login-name (file-name-nondirectory buffer-file-name))))
	(save-excursion
	  (insert "  <" signature ">")))
;    (save-excursion
;      (insert "\n\t\t<Gruppenterminkalenderfile>"))
    (setq diary-file tmp)))
(define-key calendar-mode-map "iga" 'insert-group-anniversary-entry)
(defun insert-group-anniversary-entry (&optional args)
  (interactive "P")
  (let ((tmp diary-file))
    (setq diary-file group-diary-file)
    (insert-anniversary-diary-entry args)
    (if (not (or (string-match user-login-name (file-name-nondirectory buffer-file-name))
		 (string-match user-real-login-name (file-name-nondirectory buffer-file-name))))
	(save-excursion
	  (insert "  <" signature ">")))
;    (save-excursion
;      (insert "\n\t\t<Gruppenterminkalenderfile>"))
    (setq diary-file tmp)))
(define-key calendar-mode-map "igy" 'insert-group-yearly-diary-entry)
(defun insert-group-yearly-diary-entry (&optional args)
  (interactive "P")
  (let ((tmp diary-file))
    (setq diary-file group-diary-file)
    (insert-yearly-diary-entry args)
    (if (not (or (string-match user-login-name (file-name-nondirectory buffer-file-name))
		 (string-match user-real-login-name (file-name-nondirectory buffer-file-name))))
	(save-excursion
	  (insert "  <" signature ">")))
;    (save-excursion
;      (insert "\n\t\t<Gruppenterminkalenderfile>"))
    (setq diary-file tmp)))
(define-key calendar-mode-map "igm" 'insert-group-monthly-diary-entry)
(defun insert-group-monthly-diary-entry (&optional args)
  (interactive "P")
  (let ((tmp diary-file))
    (setq diary-file group-diary-file)
    (insert-monthly-diary-entry args)
    (if (not (or (string-match user-login-name (file-name-nondirectory buffer-file-name))
		 (string-match user-real-login-name (file-name-nondirectory buffer-file-name))))
	(save-excursion
	  (insert "  <" signature ">")))
;    (save-excursion
;      (insert "\n\t\t<Gruppenterminkalenderfile>"))
    (setq diary-file tmp)))
(define-key calendar-mode-map "igw" 'insert-group-weekly-diary-entry)
(defun insert-group-weekly-diary-entry (&optional args)
  (interactive "P")
  (let ((tmp diary-file))
    (setq diary-file group-diary-file)
    (insert-weekly-diary-entry args)
    (if (not (or (string-match user-login-name (file-name-nondirectory buffer-file-name))
		 (string-match user-real-login-name (file-name-nondirectory buffer-file-name))))
	(save-excursion
	  (insert "  <" signature ">")))
;    (save-excursion
;      (insert "\n\t<Gruppenterminkalenderfile>"))
    (setq diary-file tmp)))
(define-key calendar-mode-map "igr" 'insert-group-weekday-diary-entry)
(defun insert-group-weekday-diary-entry (&optional args)
  (interactive "P")
  (let ((tmp diary-file))
    (setq diary-file group-diary-file)
    (insert-weekday-diary-entry args)
    (if (not (or (string-match user-login-name (file-name-nondirectory buffer-file-name))
		 (string-match user-real-login-name (file-name-nondirectory buffer-file-name))))
	(save-excursion
	  (insert "  <" signature ">")))
;    (save-excursion
;      (insert "\n\t\t<Gruppenterminkalenderfile>"))
    (setq diary-file tmp)))
;;; den Gruppenkalenderfile ansehen (wie s normal)
(define-key calendar-mode-map "g" 'show-group-entries)
(defun show-group-entries ()
  (interactive)
  (let ((tmp diary-file))
    (setq diary-file group-diary-file)
    (show-all-diary-entries)
    (setq diary-file tmp)))
;;; nur den Gruppenkalender, ohne den eigenen
(define-key calendar-mode-map "e" 'view-group-calendar-only)
(defun view-group-calendar-only (&optional args) 
  (interactive "P")
  (let ((tmp diary-file))
    (setq diary-file group-diary-file)
    (view-diary-entries (if (not args) 1 args))
    (calendar-day-message)
    (setq diary-file tmp)))
;;; nur den eigenen, nicht den Gruppenkalender
;;; (programmiertes Function-Overloading in LISP)
(define-key calendar-mode-map "r" 'show-own-calendar-without-includes)
(defun show-own-calendar-without-includes (&optional args)
  (interactive "P")
  (fset 'dummy-func (symbol-function 'include-other-diary-files))
  (unwind-protect
      (progn
	(fset 'include-other-diary-files 'dummy)
	(view-diary-entries (if (not args) 1 args))
	(calendar-day-message))
    (fset 'include-other-diary-files (symbol-function 'dummy-func))))
(defun dummy (&optional args)
  ;;  '(lambda()(message "not including \"#include\" files")))
  (message "not including \"#include\" files"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Für andere Terminkalender


(define-key calendar-mode-map "iod" 'insert-other-diary)
(defun insert-other-diary (other-diary-file &optional args) 
  (interactive "fTerminkalenderfile: \nPnonmarking")
  (let ((tmp diary-file))
    (setq diary-file other-diary-file)
    (insert-diary-entry args)
    (save-excursion
      (insert "  <" signature ">"))
    (save-excursion
      (insert (concat "\n\t\t<File: " (file-name-nondirectory buffer-file-name) ">")))
    (setq diary-file tmp)))
(define-key calendar-mode-map "iob" 'insert-other-diary-block)
(defun insert-other-diary-block (other-diary-file &optional args) 
  (interactive "fTerminkalenderfile: \nPnonmarking")
  (let ((tmp diary-file))
    (setq diary-file other-diary-file)
    (insert-block-diary-entry args)
    (save-excursion
      (insert "  <" signature ">"))
    (save-excursion
      (insert (concat "\n\t\t<File: " (file-name-nondirectory buffer-file-name) ">")))
    (setq diary-file tmp)))
(define-key calendar-mode-map "ioc" 'insert-other-cyclic-diary)
(defun insert-other-cyclic-diary (other-diary-file &optional args) 
  (interactive "fTerminkalenderfile: \nPnonmarking")
  (let ((tmp diary-file))
    (setq diary-file other-diary-file)
    (insert-cyclic-diary-entry args)
    (save-excursion
      (insert "  <" signature ">"))
    (save-excursion
      (insert (concat "\n\t\t<File: " (file-name-nondirectory buffer-file-name) ">")))
    (setq diary-file tmp)))
(define-key calendar-mode-map "ioa" 'insert-other-anniversary-entry)
(defun insert-other-anniversary-entry (other-diary-file &optional args)
  (interactive "fTerminkalenderfile: \nPnonmarking")
  (let ((tmp diary-file))
    (setq diary-file other-diary-file)
    (insert-anniversary-diary-entry args)
    (save-excursion
      (insert "  <" signature ">"))
    (save-excursion
      (insert (concat "\n\t\t<File: " (file-name-nondirectory buffer-file-name) ">")))
    (setq diary-file tmp)))
(define-key calendar-mode-map "ioy" 'insert-other-yearly-diary-entry)
(defun insert-other-yearly-diary-entry (other-diary-file &optional args)
  (interactive "fTerminkalenderfile: \nPnonmarking")
  (let ((tmp diary-file))
    (setq diary-file other-diary-file)
    (insert-yearly-diary-entry args)
    (save-excursion
      (insert "  <" signature ">"))
    (save-excursion
      (insert (concat "\n\t\t<File: " (file-name-nondirectory buffer-file-name) ">")))
    (setq diary-file tmp)))
(define-key calendar-mode-map "iom" 'insert-other-monthly-diary-entry)
(defun insert-other-monthly-diary-entry (other-diary-file &optional args)
  (interactive "fTerminkalenderfile: \nPnonmarking")
  (let ((tmp diary-file))
    (setq diary-file other-diary-file)
    (insert-monthly-diary-entry args)
    (save-excursion
      (insert "  <" signature ">"))
    (save-excursion
      (insert (concat "\n\t\t<File: " (file-name-nondirectory buffer-file-name) ">")))
    (setq diary-file tmp)))
(define-key calendar-mode-map "iow" 'insert-other-weekly-diary-entry)
(defun insert-other-weekly-diary-entry (other-diary-file &optional args)
  (interactive "fTerminkalenderfile: \nPnonmarking")
  (let ((tmp diary-file))
    (setq diary-file other-diary-file)
    (insert-weekly-diary-entry args)
    (save-excursion
      (insert "  <" signature ">"))
    (save-excursion
      (insert (concat "\n\t\t<File: " (file-name-nondirectory buffer-file-name) ">")))
    (setq diary-file tmp)))
(define-key calendar-mode-map "ior" 'insert-other-weekday-diary-entry)
(defun insert-other-weekday-diary-entry (other-diary-file &optional args)
  (interactive "fTerminkalenderfile: \nPnonmarking")
  (let ((tmp diary-file))
    (setq diary-file other-diary-file)
    (insert-weekday-diary-entry args)
    (save-excursion
      (insert "  <" signature ">"))
    (save-excursion
      (insert (concat "\n\t\t<File: " (file-name-nondirectory buffer-file-name) ">")))
    (setq diary-file tmp)))






;; neue Funktion im privaten Kalender

(define-key calendar-mode-map "ir" 'insert-weekday-diary-entry)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;










(define-key calendar-mode-map [menu-bar moon sun]
  '("Sonnenaufgang/Sonnenuntergang   (S)" . (lambda()(interactive)(calendar-sunrise-sunset))))
(define-key calendar-mode-map [menu-bar tools latex]
  '("Präpariere LaTeX-Buffer" . calendar-mouse-cal-tex-menu))
(define-key calendar-mode-map [menu-bar diary view]
  '("Zeige Terminkalender von anderem File" . view-other-diary-entries-2))
(define-key calendar-mode-map [menu-bar diary show]
  '("Zeige Terminkalender" . view-diary-entries-2))
(define-key calendar-mode-map [menu-bar diary mark]
 '("Markieren der Tage mit Terminen" . mark-diary-entries))
(define-key calendar-mode-map [menu-bar diary unmark]
 '("Unmarkieren der Tage mit Terminen" . calendar-unmark-rehilit))
(define-key calendar-mode-map "u"   'calendar-unmark-rehilit)
(define-key calendar-mode-map [menu-bar Holidays unmark]
  '("Markierung löschen" . calendar-unmark-rehilit))
(define-key calendar-mode-map [menu-bar diary daymessage]
  '("Tagesorientierung" . calendar-day-message))
(define-key calendar-mode-map "P" 'ps-print-diary-entries)
(define-key calendar-mode-map [menu-bar diary ps-print-diary-entries]
  '("PS-Druck des anderen Fensters" . ps-print-diary-entries))
(defun ps-print-diary-entries ()
  (interactive)
  (if (featurep 'ps-print)
      (if (y-or-n-p "Buffer in anderem Fenster drucken? ")
	  (if calendar-setup
	      (progn
		(raise-frame (next-frame nil 'visible))
		(let ((current-buf (current-buffer)))
		  (switch-to-buffer-other-frame fancy-diary-buffer)
		  (set-buffer-modified-p nil)
		  (ps-print-buffer-with-faces)
		  (raise-frame (previous-frame nil 'visible))
		  (switch-to-buffer-other-frame current-buf)))
	    (other-window 1)
	    (set-buffer-modified-p nil)
	    (ps-print-buffer-with-faces)
	    (other-window -1)))
    (error "ps-print not yet loaded")))


(message "Kalender geladen bis Marke 6")


(defun calendar-unmark-rehilit ()
  (interactive)
  (calendar-unmark)
  (if window-system
      (progn
	(hilit-repaint-command t)
	(if mark-sundays (mark-calendar-days-named 7))))) ;; alle Sonntage rot einfärben


(define-key calendar-mode-map [menu-bar group]
  (cons "Termine" (make-sparse-keymap "Terminkalender")))
;;
(define-key calendar-mode-map [menu-bar group group]
  (cons "Gruppenterminkalender" (make-sparse-keymap "Gruppenterminkalender")))

(define-key calendar-mode-map [menu-bar group group insert-anniv]
  '("Jahrestag in Gruppentermin eintragen" . insert-group-anniversary-entry))
(define-key calendar-mode-map [menu-bar group group insert-yearly]
  '("Jährlichen Gruppentermin eintragen" . insert-group-yearly-diary-entry))
(define-key calendar-mode-map [menu-bar group group insert-monthly]
  '("Monatlichen Gruppentermin eintragen" . insert-group-monthly-diary-entry))
(define-key calendar-mode-map [menu-bar group group insert-weekly]
  '("Wöchentlichen Gruppentermin eintragen" . insert-group-weekly-diary-entry))
(define-key calendar-mode-map [menu-bar group group insert-weekday]
  '("Wochentagstermin jede n-te Monatswoche eintragen" . insert-group-weekday-diary-entry))
(define-key calendar-mode-map [menu-bar group group insert-cyclic]
  '("Zyklischen Gruppentermin eintragen" . insert-group-cyclic-diary))
(define-key calendar-mode-map [menu-bar group group insert-block]
  '("Zeitraum (ab Marke) als Gruppenterminin eintragen" . insert-group-diary-block))
(define-key calendar-mode-map [menu-bar group group insert-group]
  '("Gruppentermin eintragen" . insert-group-diary))

(define-key calendar-mode-map [menu-bar group group headline]
  '("*** Gruppenterminkalenderfunktionen: ***" . calendar-day-message-dummy))

(define-key calendar-mode-map [menu-bar group other]
  (cons "Fremde Terminkalender" (make-sparse-keymap "Fremde Terminkalender")))

(define-key calendar-mode-map [menu-bar group other insert-other-anniv]
  '("Jahrestag in anderen File eintragen" . insert-other-anniversary-entry))
(define-key calendar-mode-map [menu-bar group other insert-other-yearly]
  '("Jährlichen Termin in anderen File eintragen" . insert-other-yearly-diary-entry))
(define-key calendar-mode-map [menu-bar group other insert-other-monthly]
  '("Monatlichen Termin in anderen File eintragen" . insert-other-monthly-diary-entry))
(define-key calendar-mode-map [menu-bar group other insert-other-weekly]
  '("Wöchentlichen Termin in anderen File eintragen" . insert-other-weekly-diary-entry))
(define-key calendar-mode-map [menu-bar group other insert-weekday]
  '("Wochentagstermin jede n-te Monatswoche eintragen" . insert-other-weekday-diary-entry))
(define-key calendar-mode-map [menu-bar group other insert-other-cyclic]
  '("Zyklischen Termine in anderen File eintragen" . insert-other-cyclic-diary))
(define-key calendar-mode-map [menu-bar group other insert-other-block]
  '("Zeitraum (ab Marke) in anderen File eintragen" . insert-other-diary-block))
(define-key calendar-mode-map [menu-bar group other insert-other-diary]
  '("Termin in anderen File eintragen" . insert-other-diary))

(define-key calendar-mode-map [menu-bar group other headline]
  '("*** Terminkalenderfunktionen Fremdkalender: ***" . calendar-day-message-dummy))

(define-key calendar-mode-map [menu-bar group personal]
  (cons "Persönlicher Terminkalender" (make-sparse-keymap "Persönliche Terminkalender")))

(define-key calendar-mode-map [menu-bar group personal insert-personal-anniv]
  '("Jahrestag in persönlichen Kalender eintragen" . insert-personal-anniversary-entry))
(define-key calendar-mode-map [menu-bar group personal insert-personal-yearly]
  '("Jährlichen Termin in persönlichen Kalender eintragen" . insert-personal-yearly-diary-entry))
(define-key calendar-mode-map [menu-bar group personal insert-personal-monthly]
  '("Monatlichen Termin in persönlichen Kalender eintragen" . insert-personal-monthly-diary-entry))
(define-key calendar-mode-map [menu-bar group personal insert-personal-weekly]
  '("Wöchentlichen Termin in persönlichen Kalender eintragen" . insert-personal-weekly-diary-entry))
(define-key calendar-mode-map [menu-bar group personal insert-weekday]
  '("Wochentagstermin jede n-te Monatswoche eintragen" . insert-personal-weekday-diary-entry))
(define-key calendar-mode-map [menu-bar group personal insert-personal-cyclic]
  '("Zyklischen Termine in persönlichen Kalender eintragen" . insert-personal-cyclic-diary))
(define-key calendar-mode-map [menu-bar group personal insert-personal-block]
  '("Zeitraum (ab Marke) in persönlichen Kalender eintragen" . insert-personal-diary-block))
(define-key calendar-mode-map [menu-bar group personal insert-personal-diary]
  '("Termin in persönlichen Kalender eintragen" . insert-personal-diary))

(define-key calendar-mode-map [menu-bar group personal headline]
  '("*** Terminkalenderfunktionen persönlicher Kalender: ***" . calendar-day-message-dummy))


(define-key calendar-mode-map [menu-bar group privat]
  (cons "Privater Terminkalender" (make-sparse-keymap "Privater Terminkalender")))

(define-key calendar-mode-map [menu-bar group privat ann]
  '("Jahrestag in Privatkalender eintragen" . insert-anniversary-diary-entry))
(define-key calendar-mode-map [menu-bar group privat yr]
  '("Jährlichen wiederkehrenden Termin in Privatkalender eintragen" . insert-yearly-diary-entry))
(define-key calendar-mode-map [menu-bar group privat mon]
  '("Monatlich wiederkehrenden Termin in Privatkalender eintragen" . insert-monthly-diary-entry))
(define-key calendar-mode-map [menu-bar group privat wk]
  '("Wöchentlich wiederkehrenden Termin in Privatkalender eintragen" . insert-weekly-diary-entry))
(define-key calendar-mode-map [menu-bar group privat insert-weekday]
  '("Wochentagstermin jede n-te Monatswoche eintragen" . insert-weekday-diary-entry))
(define-key calendar-mode-map [menu-bar group privat cyc]
  '("Zyklisch wiederkehrendes Datum in Privatkalender eintragen" . insert-cyclic-diary-entry))
(define-key calendar-mode-map [menu-bar group privat blk]
  '("Zeitraum in Privatkalender eintragen (ab Marke)" . insert-block-diary-entry))
(define-key calendar-mode-map [menu-bar group privat ent]
  '("Termin in Privatkalender eintragen". insert-diary-entry))

(define-key calendar-mode-map [menu-bar group privat headline]
  '("*** Terminkalenderfunktionen Privatkalender: ***" . calendar-day-message-dummy))


(define-key calendar-mode-map [menu-bar group markdate]
  '("Setze Marke bei diesem Datum [auch (C-SPC)]" . calendar-set-mark))

(define-key calendar-mode-map [menu-bar group show-all-3]
  '("Termineinträge im Gruppenkalender" . show-group-entries))
(define-key calendar-mode-map [menu-bar group show-all-2]
  '("Termineinträge in personenzugeordneten Kalender" . show-personal-entries))
(define-key calendar-mode-map [menu-bar group show-all-1]
  '("Termineinträge des privaten Kalenders" . show-all-diary-entries))

(define-key calendar-mode-map [menu-bar group show-personal]
  '("Kalender ohne #include-Files" . show-own-calendar-without-includes))
(define-key calendar-mode-map [menu-bar group show-group]
  '("Terminkalender des Gruppenterminkalenders" . view-group-calendar-only))
(define-key calendar-mode-map [menu-bar group view-others]
  '("Zeige anderen Terminkalender" . view-other-diary-entries-2))
(define-key calendar-mode-map [menu-bar group view-diary]
  '("Zeige privaten Terminkalender" . view-diary-entries-2))




(message "Kalender geladen bis Marke 7")



(defun ask-for-calendar-date ()
  (let ((tag "1") (monat) (jahr))
    (setq tag (read-string "Tag: "))
    (setq monat (read-string "Monat: "))
    (setq jahr (read-string "Jahr: "))
    (list (string-to-number monat) (string-to-number tag) (string-to-number jahr))))
(defun d (&optional arg)
  "Short for calling the diary in this buffer"
  (interactive "P")
  (let ((calendar-setup calendar-setup))
    (setq calendar-setup nil)
  (let* ((end-of-namestring (string-match "\\.crypt\\|$" diary-file))
	 (save-string-tmp (match-end 0))
	 (diary-buffer-name (substring diary-file save-string-tmp end-of-namestring))
	 (switch-to-buffer diary-buffer-name)
	 (kill-buffer diary-file))
;;;
;;; Von diary-lib.el:
;;;(defun diary (&optional arg)
;;;  "Generate the diary window for ARG days starting with the current date.
;;; If no argument is provided, the number of days of diary entries is governed
;;; by the variable `number-of-diary-entries'.	This function is suitable for
;;;execution in a `.emacs' file."
;;;  (interactive "P")
    (if (or (not arg) (>= arg 0))
	(diary arg)
      (let ((d-file (substitute-in-file-name diary-file))
	    (date (ask-for-calendar-date)) (arg (- arg)))
	(if (and d-file (file-exists-p d-file))
	    (if (file-readable-p d-file)
		(list-diary-entries
		 date
		 (cond
		  (arg (prefix-numeric-value arg))
		  ((vectorp number-of-diary-entries)
		   (aref number-of-diary-entries (calendar-day-of-week date)))
		  (t number-of-diary-entries)))
	      (error "Your diary file is not readable!"))
	  (error "You don't have a diary file!")))
;      (if calendar-setup
;	  (kill-buffer calendar-buffer))
      ))))


(defun change-diary-entry ()
  "In den \"Fancy Diary Entries\" auf eine Position gehen, dann stellt es den Cursor an den
entsprechenden Eintrag in dem File DIARY-FILE"
  (interactive)
  (let ((begin) (end) (tmp-substring) (current-buf (current-buffer)))
    (save-excursion
      (beginning-of-line)
      (re-search-forward "[^ \t\r]\\|$" nil t)
      (backward-char 1)
      (setq begin (point))
      (end-of-line)
      (re-search-backward "[^ \t\r]\\|^" nil t)
      (forward-char 1)
      (setq end (point))
      (forward-char 1)
      (setq tmp-substring (buffer-substring-no-properties begin end)))
    ;;
    ;; gefährlich und noch nicht ausgereift
    (let ((current-buf (current-buffer)))
      (if (not (get-buffer "*scratch*"))
	  (progn
	    (make-frame-visible emacs-mainframe)
	    (select-frame emacs-mainframe)
	    (switch-to-buffer "*scratch*"))
	(if calendar-setup
	    (switch-to-buffer-other-frame "*scratch*")
	  (switch-to-buffer "*scratch*")))
      (if (boundp 'diary-file)
	  (progn
	    (find-file diary-file)
	    (goto-char (point-max))
	    (if (not (search-backward tmp-substring nil t))
		(progn
		  (message "Keinen IDENTISCHEN Eintrag im durchsuchten File gefunden: %s" diary-file)
		  (switch-to-buffer current-buf))
	      (show-really-everything-c)
	      (error "Eintrag gefunden"))))
	(if (boundp 'personal-diary-file)
	    (progn
	      (find-file personal-diary-file)
	      (goto-char (point-max))
	      (if (not (search-backward tmp-substring nil t))
		  (progn
		    (message "Keinen IDENTISCHEN Eintrag in den durchsuchten Files gefunden: %s %s" diary-file personal-diary-file)
		    (switch-to-buffer current-buf))
		(show-really-everything-c)
		(error "Eintrag gefunden"))))
	(if (boundp 'group-diary-file)
	    (progn
	      (find-file group-diary-file)
	      (goto-char (point-max))
	      (if (not (search-backward tmp-substring nil t))
		  (progn
		    (message "Keinen IDENTISCHEN Eintrag in den durchsuchten Files gefunden: %s %s %s" 
			     diary-file personal-diary-file group-diary-file)
		    (switch-to-buffer current-buf))
		(show-really-everything-c)
		(error "Eintrag gefunden")))))))

(defun show-really-everything-c ()
  (interactive)
  (let ((before-change-functions nil)(after-change-functions nil))
  (save-excursion
    (let ((already-modified (buffer-modified-p)))
      (widen)
      (goto-char (point-min))
      (while (search-forward "\r" nil t)
	(replace-match "\n" nil t))
      (setq selective-display nil)
      (set-buffer-modified-p already-modified)))))



(defun cde ()
  "Ruft change-diary-entry"
  (interactive)
  (change-diary-entry))





;;;
;;; Funktion herauskopiert aus
;;; diary-lib.el    66330  Emacs-Lisp	/usr/share/emacs/20.3/lisp/calendar/diary-lib.el
;;;
(if (>= emacs-major-version 20)
(defun diary-mail-entries (&optional ndays)
  "Send a mail message showing diary entries for next NDAYS days.
If no prefix argument is given, NDAYS is set to `diary-mail-days'.

You can call `diary-mail-entries' every night using an at/cron job.
For example, this script will run the program at 2am daily.  Since
`emacs -batch' does not load your `.emacs' file, you must ensure that
all relevant variables are set, as done here.

#!/bin/sh
# diary-rem.sh -- repeatedly run the Emacs diary-reminder
emacs -batch \\
-eval \"(setq diary-mail-days 3 \\
             european-calendar-style t \\
             diary-mail-addr \\\"user@host.name\\\" )\" \\
-l diary-lib -f diary-mail-entries 
at -f diary-rem.sh 0200 tomorrow

You may have to tweak the syntax of the `at' command to suit your
system.  Alternatively, you can specify a cron entry:
0 1 * * * diary-rem.sh
to run it every morning at 1am."
  (interactive "p")
  (let ((text nil)
	;; Use the fancy-diary-display as it doesn't hide rest of
	;; diary file with ^M characters.  It also looks nicer.
	(diary-display-hook 'fancy-diary-display))	
    (if (not current-prefix-arg)
	(setq ndays diary-mail-days))
    (calendar)
;; SMW ff
    (view-diary-entries-2 ndays)
;;  (set-buffer "*Fancy Diary Entries*")
    (set-buffer "*Termine*")
;; SMW
    (setq text (buffer-substring (point-min) (point-max)))

    ;; Now send text as a mail message.
    (mail)
    (mail-to)
    (insert diary-mail-addr)
    (mail-subject)
    (insert "Diary entries generated ")
    (insert (format-time-string "%a %d %b %Y" (current-time)))
    (mail-text)
    (insert text)
    (mail-send-and-exit nil)
    (exit-calendar)))
)






(if (< emacs-major-version 20)
;;;
;;; Hier gibt es so etwas wie einen Bug (keinen richtigen):
;;; Die Orginalfunktion hat ein "kill-buffer" drin, was dazu führt, daß, wenn der includete Buffer nicht
;;; abgespeichert ist, man gefragt wird, ob man diesen nicht rauswerfen will.
;;;
;;; Funktion herauskopiert aus
;;; diary-lib.el    61701  Emacs-Lisp	/usr/local/gnu/share/emacs/19.31/lisp/diary-lib.el
;;;
    (defun include-other-diary-files ()
      "Include the diary entries from other diary files with those of diary-file.
This function is suitable for use in `list-diary-entries-hook';
it enables you to use shared diary files together with your own.
The files included are specified in the diaryfile by lines of this form:
        #include \"filename\"
This is recursive; that is, #include directives in diary files thus included
are obeyed.  You can change the `#include' to some other string by
changing the variable `diary-include-string'."
      (goto-char (point-min))
      (if (buffer-file-name (get-buffer diary-file))
	  (if (buffer-modified-p (find-buffer-visiting diary-file))
	      (save-buffer (find-buffer-visiting diary-file))))
      (while (re-search-forward
	      (concat
	       "\\(\\`\\|\^M\\|\n\\)"
	       (regexp-quote diary-include-string)
	       " \"\\([^\"]*\\)\"")
	      nil t)
	(let ((diary-file (substitute-in-file-name
			   (buffer-substring-no-properties
			    (match-beginning 2) (match-end 2))))
	      ;; SMW ff
	      (diary-list-include-blanks t)  
	      ;; SMW
	      (list-diary-entries-hook 'include-other-diary-files)
	      (diary-display-hook 'ignore)
	      (diary-hook nil))
	  (if (file-exists-p diary-file)
	      (if (file-readable-p diary-file)
		  (unwind-protect
		      (progn
			(setq diary-entries-list
			      (append diary-entries-list
				      (list-diary-entries original-date number))))
		    ;; SMW ff
		    (if (buffer-file-name (get-buffer diary-file))
			(if (buffer-modified-p (find-buffer-visiting diary-file))
			    (save-buffer (find-buffer-visiting diary-file))))
		    (kill-buffer (find-buffer-visiting diary-file)))
		;; SMW
		(beep)
		(message "Can't read included diary file %s" diary-file)
		(sleep-for 2))
	    (beep)
	    (message "Can't find included diary file %s" diary-file)
	    (sleep-for 2))))
      (goto-char (point-min)))
;;;
;;; Funktion herauskopiert aus
;;; diary-lib.el    66330  Emacs-Lisp	/usr/share/emacs/20.3/lisp/calendar/diary-lib.el
;;;
  (defun include-other-diary-files ()
    "Include the diary entries from other diary files with those of diary-file.
This function is suitable for use in `list-diary-entries-hook';
it enables you to use shared diary files together with your own.
The files included are specified in the diaryfile by lines of this form:
        #include \"filename\"
This is recursive; that is, #include directives in diary files thus included
are obeyed.  You can change the `#include' to some other string by
changing the variable `diary-include-string'."
    (goto-char (point-min))
					; SMW ff
    (if (buffer-file-name (get-buffer diary-file))
	(if (buffer-modified-p (find-buffer-visiting diary-file))
	    (save-buffer (find-buffer-visiting diary-file))))
					; SMW
    (while (re-search-forward
	    (concat
	     "\\(\\`\\|\^M\\|\n\\)"
	     (regexp-quote diary-include-string)
	     " \"\\([^\"]*\\)\"")
	    nil t)
      (let ((diary-file (substitute-in-file-name
			 (buffer-substring-no-properties
			  (match-beginning 2) (match-end 2))))
	    (diary-list-include-blanks nil)
	    ;; SMW ff
	    (diary-list-include-blanks t)  
	    ;; SMW
	    (list-diary-entries-hook 'include-other-diary-files)
	    (diary-display-hook 'ignore)
	    (diary-hook nil))
	(if (file-exists-p diary-file)
	    (if (file-readable-p diary-file)
		(unwind-protect
		    (setq diary-entries-list
			  (append diary-entries-list
				  (list-diary-entries original-date number)))
		  ;; SMW ff
		  (if (buffer-file-name (get-buffer diary-file))
		      (if (buffer-modified-p (find-buffer-visiting diary-file))
			  (save-buffer (find-buffer-visiting diary-file))))
		  ;; SMW
		  (kill-buffer (find-buffer-visiting diary-file)))
	      (beep)
	      (message "Can't read included diary file %s" diary-file)
	      (sleep-for 2))
	  (beep)
	  (message "Can't find included diary file %s" diary-file)
	  (sleep-for 2))))
    (goto-char (point-min)))
  )


(defun remove-double-list-entries (a)
  ;;  ;; entferne doppelte "diary-noentry-string"-Elemente -- nicht vollständig getestet
  ;; Testversion:
  ;;  (setq a (list (list 1 2) (list 3 4) (list 1 2) (list 3 4) (list 3 4)(list 3 2)))
  ;;  (setq num 0)
  ;;  (while (< num 4)
  ;;    (setq num (1+ num))
  ;;    (if (member (nth num a) (nthcdr num a))
  ;;	(progn
  ;;	  (setq b (list (nth num a)))
  ;;	  (setq a (append b (delete (nth num a) a)))))
  ;;    (nthcdr 1 a))
  (let ((num 0))
    (while (< num (1- (length a)))
      (if (member (nth num a) (nthcdr num a))
	  (progn
	    (setq b (list (nth num a)))
	    (setq a (append (delete (nth num a) a) b))))
      (setq num (1+ num))))
  a)






(message "Kalender geladen bis Marke 8")


(if (< emacs-major-version 20)
;;;
;;; Funktion herauskopiert aus
;;; diary-lib.el    61701  Emacs-Lisp	/usr/local/gnu/share/emacs/19.31/lisp/diary-lib.el
;;;
    (defun list-diary-entries (date number)
      "Create and display a buffer containing the relevant lines in diary-file.
The arguments are DATE and NUMBER; the entries selected are those
for NUMBER days starting with date DATE.  The other entries are hidden
using selective display.

Returns a list of all relevant diary entries found, if any, in order by date.
The list entries have the form ((month day year) string).  If the variable
`diary-list-include-blanks' is t, this list includes a dummy diary entry
\(consisting of the empty string) for a date with no diary entries.

After the list is prepared, the hooks `nongregorian-diary-listing-hook',
`list-diary-entries-hook', `diary-display-hook', and `diary-hook' are run.
These hooks have the following distinct roles:

    `nongregorian-diary-listing-hook' can cull dates from the diary
        and each included file.  Usually used for Hebrew or Islamic
        diary entries in files.  Applied to *each* file.

    `list-diary-entries-hook' adds or manipulates diary entries from
        external sources.  Used, for example, to include diary entries
        from other files or to sort the diary entries.  Invoked *once* only,
        before the display hook is run.

    `diary-display-hook' does the actual display of information.  If this is
        nil, simple-diary-display will be used.  Use add-hook to set this to
        fancy-diary-display, if desired.  If you want no diary display, use
        add-hook to set this to ignore.

    `diary-hook' is run last.  This can be used for an appointment
        notification function."

      (if (< 0 number)
	  (let* ((original-date date);; save for possible use in the hooks
		 (old-diary-syntax-table)
		 (diary-entries-list)
		 (date-string (calendar-date-string date))
		 (d-file (substitute-in-file-name diary-file)))
       
	    (message "Preparing diary...")
	    (save-excursion
	      (let ((diary-buffer (find-buffer-visiting d-file)))
		(set-buffer (if diary-buffer
				diary-buffer
			      (find-file-noselect d-file t))))
	      (setq selective-display t)
	      (setq selective-display-ellipses nil)
	      (setq old-diary-syntax-table (syntax-table))
	      (set-syntax-table diary-syntax-table)
	      (unwind-protect
		  (let ((buffer-read-only nil)
			(diary-modified (buffer-modified-p))
			(mark (regexp-quote diary-nonmarking-symbol)))
		    (goto-char (1- (point-max)))
		    (if (not (looking-at "\^M\\|\n"))
			(progn
			  (forward-char 1)
			  (insert-string "\^M")))
		    (goto-char (point-min))
		    (if (not (looking-at "\^M\\|\n"))
			(insert-string "\^M"))
		    (subst-char-in-region (point-min) (point-max) ?\n ?\^M t)
		    (calendar-for-loop i from 1 to number do
				       (let ((d diary-date-forms)
					     (month (extract-calendar-month date))
					     (day (extract-calendar-day date))
					     (year (extract-calendar-year date))
					     (entry-found (list-sexp-diary-entries date)))
;;;; debug: bis hierher Datumsvorbereitung
;;;; jetzt: Suche
					 (while d
					   (let*
					       ((date-form (if (equal (car (car d)) 'backup)
							       (cdr (car d))
							     (car d)))
						(backup (equal (car (car d)) 'backup))
						(dayname
						 (concat
						  (calendar-day-name date) "\\|"
						  (substring (calendar-day-name date) 0 3) ".?"))
						(monthname
						 (concat
						  "\\*\\|"
						  (calendar-month-name month) "\\|"
						  (substring (calendar-month-name month) 0 3) ".?"))
						(month (concat "\\*\\|0*" (int-to-string month)))
						(day (concat "\\*\\|0*" (int-to-string day)))
						(year
						 (concat
						  "\\*\\|0*" (int-to-string year)
						  (if abbreviated-calendar-year
						      (concat "\\|" (int-to-string (% year 100)))
						    "")))
						(regexp
						 (concat
						  "\\(\\`\\|\^M\\|\n\\)" mark "?\\("
						  (mapconcat 'eval date-form "\\)\\(")
						  "\\)"))
						(case-fold-search t))
					     (goto-char (point-min))
					     (while (re-search-forward regexp nil t)
					       (if backup (re-search-backward "\\<" nil t))
					       (if (and (or (char-equal (preceding-char) ?\^M)
							    (char-equal (preceding-char) ?\n))
							(not (looking-at " \\|\^I")))
						   ;;  Diary entry that consists only of date.
						   (backward-char 1)
						 ;; Found a nonempty diary entry--make it visible and
						 ;; add it to the list.
						 (setq entry-found t)
						 (let ((entry-start (point))
						       (date-start))
						   (re-search-backward "\^M\\|\n\\|\\`")
						   (setq date-start (point))
						   (re-search-forward "\^M\\|\n" nil t 2)
						   (while (looking-at " \\|\^I")
						     (re-search-forward "\^M\\|\n" nil t))
						   (backward-char 1)
						   (subst-char-in-region date-start
									 (point) ?\^M ?\n t)
						   (add-to-diary-list
						    date
						    (buffer-substring-no-properties
						     entry-start (point)))))))
					   (setq d (cdr d)))
;;;; debug: Vorbereiten der Anzeige
					 (or entry-found
					     (not diary-list-include-blanks)
					     (setq diary-entries-list 
						   (nconc diary-entries-list
							  (list (list date diary-noentry-string))))) ;SMW
;;;; Dies letzte Zeile war eine Änderung von mir

					 (setq date
					       (calendar-gregorian-from-absolute
						(1+ (calendar-absolute-from-gregorian date))))
					 (setq entry-found nil)))
		    (set-buffer-modified-p diary-modified))
		(set-syntax-table old-diary-syntax-table))
	      (goto-char (point-min))
	      (run-hooks 'nongregorian-diary-listing-hook
			 'list-diary-entries-hook)
	      (if diary-display-hook
		  (run-hooks 'diary-display-hook)
		(simple-diary-display))
	      (run-hooks 'diary-hook)
	      diary-entries-list))))
;;;
;;; Funktion herauskopiert aus
;;; diary-lib.el    66330  Emacs-Lisp	/usr/share/emacs/20.3/lisp/calendar/diary-lib.el
;;;
  (defun list-diary-entries (date number)
    "Create and display a buffer containing the relevant lines in diary-file.
The arguments are DATE and NUMBER; the entries selected are those
for NUMBER days starting with date DATE.  The other entries are hidden
using selective display.

Returns a list of all relevant diary entries found, if any, in order by date.
The list entries have the form ((month day year) string specifier) where
\(month day year) is the date of the entry, string is the entry text, and
specifier is the applicability.  If the variable `diary-list-include-blanks'
is t, this list includes a dummy diary entry consisting of the empty string)
for a date with no diary entries.

After the list is prepared, the hooks `nongregorian-diary-listing-hook',
`list-diary-entries-hook', `diary-display-hook', and `diary-hook' are run.
These hooks have the following distinct roles:

    `nongregorian-diary-listing-hook' can cull dates from the diary
        and each included file.  Usually used for Hebrew or Islamic
        diary entries in files.  Applied to *each* file.

    `list-diary-entries-hook' adds or manipulates diary entries from
        external sources.  Used, for example, to include diary entries
        from other files or to sort the diary entries.  Invoked *once* only,
        before the display hook is run.

    `diary-display-hook' does the actual display of information.  If this is
        nil, simple-diary-display will be used.  Use add-hook to set this to
        fancy-diary-display, if desired.  If you want no diary display, use
        add-hook to set this to ignore.

    `diary-hook' is run last.  This can be used for an appointment
        notification function."

    (if (< 0 number)
	(let* ((original-date date);; save for possible use in the hooks
	       (old-diary-syntax-table)
	       (diary-entries-list)
	       (date-string (calendar-date-string date))
	       (d-file (substitute-in-file-name diary-file)))
	  (message "Preparing diary...")
	  (save-excursion
	    (let ((diary-buffer (find-buffer-visiting d-file)))
	      (if (not diary-buffer)
		  (set-buffer (find-file-noselect d-file t))
		(set-buffer diary-buffer)
		(if (buffer-file-name (get-buffer diary-file)) ;SMW
		    (or (verify-visited-file-modtime diary-buffer)
			(revert-buffer t t)))
		))
	    (setq selective-display t)
	    (setq selective-display-ellipses nil)
	    (setq old-diary-syntax-table (syntax-table))
	    (set-syntax-table diary-syntax-table)
	    (unwind-protect
		(let ((buffer-read-only nil)
		      (diary-modified (buffer-modified-p))
		      (mark (regexp-quote diary-nonmarking-symbol)))
		  (goto-char (1- (point-max)))
		  (if (not (looking-at "\^M\\|\n"))
		      (progn
			(forward-char 1)
			(insert-string "\^M")))
		  (goto-char (point-min))
		  (if (not (looking-at "\^M\\|\n"))
		      (insert-string "\^M"))
		  (subst-char-in-region (point-min) (point-max) ?\n ?\^M t)
		  (calendar-for-loop i from 1 to number do
				     (let ((d diary-date-forms)
					   (month (extract-calendar-month date))
					   (day (extract-calendar-day date))
					   (year (extract-calendar-year date))
					   (entry-found (list-sexp-diary-entries date)))
				       (while d
					 (let*
					     ((date-form (if (equal (car (car d)) 'backup)
							     (cdr (car d))
							   (car d)))
					      (backup (equal (car (car d)) 'backup))
					      (dayname
					       (concat
						(calendar-day-name date) "\\|"
						(substring (calendar-day-name date) 0 3) ".?"))
					      (monthname
					       (concat
						"\\*\\|"
						(calendar-month-name month) "\\|"
						(substring (calendar-month-name month) 0 3) ".?"))
					      (month (concat "\\*\\|0*" (int-to-string month)))
					      (day (concat "\\*\\|0*" (int-to-string day)))
					      (year
					       (concat
						"\\*\\|0*" (int-to-string year)
						(if abbreviated-calendar-year
						    (concat "\\|" (int-to-string (% year 100)))
						  "")))
					      (regexp
					       (concat
						"\\(\\`\\|\^M\\|\n\\)" mark "?\\("
						(mapconcat 'eval date-form "\\)\\(")
						"\\)"))
					      (case-fold-search t))
					   (goto-char (point-min))
					   (while (re-search-forward regexp nil t)
					     (if backup (re-search-backward "\\<" nil t))
					     (if (and (or (char-equal (preceding-char) ?\^M)
							  (char-equal (preceding-char) ?\n))
						      (not (looking-at " \\|\^I")))
						 ;;  Diary entry that consists only of date.
						 (backward-char 1)
					       ;; Found a nonempty diary entry--make it visible and
					       ;; add it to the list.
					       (setq entry-found t)
					       (let ((entry-start (point))
						     (date-start))
						 (re-search-backward "\^M\\|\n\\|\\`")
						 (setq date-start (point))
						 (re-search-forward "\^M\\|\n" nil t 2)
						 (while (looking-at " \\|\^I")
						   (re-search-forward "\^M\\|\n" nil t))
						 (backward-char 1)
						 (subst-char-in-region date-start
								       (point) ?\^M ?\n t)
						 (add-to-diary-list
						  date
						  (buffer-substring-no-properties
						   entry-start (point))
						  (buffer-substring-no-properties
						   (1+ date-start) (1- entry-start)))))))
					 (setq d (cdr d)))
;;;; debug: Vorbereiten der Anzeige
				       (or entry-found
					   (not diary-list-include-blanks)
					   (setq diary-entries-list 
						 (nconc diary-entries-list
							(list (list date diary-noentry-string))))) ;SMW
;;;; Dies letzte Zeile war eine Änderung von mir
				       (setq date
					     (calendar-gregorian-from-absolute
					      (1+ (calendar-absolute-from-gregorian date))))
				       (setq entry-found nil)))
		  (set-buffer-modified-p diary-modified))
	      (set-syntax-table old-diary-syntax-table))
	    (goto-char (point-min))
	    (run-hooks 'nongregorian-diary-listing-hook
		       'list-diary-entries-hook)
	    (if diary-display-hook
		(run-hooks 'diary-display-hook)
	      (simple-diary-display))
	    (run-hooks 'diary-hook)
	    diary-entries-list))))
  )


(message "Kalender geladen bis Marke 9")


(require 'cal-x)

(setq calendar-frame-parameters
  '((name . "Kalender") (minibuffer . t) (height . 10) (width . 80)
    (unsplittable . t) (vertical-scroll-bars . nil)))

(defun calendar-in-extra-window (&optional arg)
  "Start calendar and diary in separate, dedicated frames."
  (interactive "p")
  (if arg (if (= arg 1) (setq arg nil)))
  (if (not window-system)
      (calendar-basic-setup arg)
    (if (< emacs-major-version 20)
	(standard-display-european t))
    (setq display-time-24hr-format t)
    (setq display-time-day-and-date t)
    (setq calendar-setup 'calendar-in-extra-window)
    (if (frame-live-p calendar-frame) (delete-frame calendar-frame))
    ;;(if (frame-live-p diary-frame) (delete-frame diary-frame))
    (add-hook 'diary-hook 'change-diary-frame-position)
    (let ((pop-up-windows nil)
          (view-diary-entries-initially nil)
          (special-display-buffer-names nil))
      ;;
      (setq pop-up-frame-function
	    (function (lambda ()
			(make-frame pop-up-frame-alist))))
      (setq special-display-function 'special-display-popup-frame)
      ;;
      (save-window-excursion
        (save-excursion (calendar-basic-setup arg))
        (setq calendar-frame (make-frame calendar-frame-parameters))
        (run-hooks 'calendar-after-frame-setup-hooks)
        (select-frame calendar-frame)
        (if (eq 'icon (cdr (assoc 'visibility
                                  (frame-parameters calendar-frame))))
            (iconify-or-deiconify-frame))
        (display-buffer calendar-buffer)
        (set-window-dedicated-p (selected-window) 'calendar)
	;;
	;;
	;; hier werden die Feiertage geladen !!!
	(require 'op-calendar-holidaylist)
	;;
	;;
	(calendar-recenter)
	(setq calendar-setup 'calendar-in-extra-window)
	(if mark-calendar-holidays-initially
	    (mark-calendar-holidays))
	(calendar-day-message)
	(if mark-sundays (mark-calendar-days-named 7)) ;; alle Sonntage rot einfärben
	(define-key calendar-mode-map "d" 'view-diary-entries-2)
	(define-key calendar-mode-map "D" 'view-other-diary-entries-2)
        (run-hooks 'calendar-after-frame-setup-hooks)))))
(defun change-diary-frame-position ()
  (let ((calendar-frame-x-position (cdr (assoc 'left (frame-parameters (selected-frame)))))
	(calendar-frame-y-position (cdr (assoc 'top (frame-parameters (selected-frame)))))
	(calendar-frame-scroll-bar-width 2) 
	(calendar-frame-width (cdr (assoc 'width (frame-parameters (selected-frame)))))
	(diary-frame-width 2)
	(scroll-bar-width 2))
    (if (listp calendar-frame-x-position)
	(setq calendar-frame-x-position (nth 1 calendar-frame-x-position)))
    (if (listp calendar-frame-y-position)
	(setq calendar-frame-y-position (nth 1 calendar-frame-y-position)))
    (switch-to-buffer-other-frame fancy-diary-buffer)
    ;;
    ;; Suche die Zeile mit den ===== - Zeichen und bestimme die Länge
    ;; Setze dann daraus die Breite des anzuzeigenden Rahmens fest
    (goto-char (point-min))
    (if (search-forward "=============" nil t)
	(progn
	  (let ((tmp (save-excursion (beginning-of-line)(point))))
	    (end-of-line)
	    (setq diary-frame-width (1+ (- (point) tmp)))
	    (forward-char 1))))
    ;;
    ;;
    ;; setzt die Rahmenposition des diary-Fensters
    ;; in Abhängigkeit vom jeweiligen Fenstersystem
    ;;
    ;(require 'backquote)
    ;;
    ;;(setq scroll-bar-width 
    ;;	 (cdr (assoc 'scroll-bar-width (frame-parameters (selected-frame)))))
    (setq diary-frame-width (max (- calendar-frame-width scroll-bar-width) diary-frame-width))
    (cond ((equal window-system 'x)
	   (modify-frame-parameters (selected-frame)
				    `((name . "Eingetragene Termine") ; ` damin das Komma in der Liste evaluiert wird
				      (vertical-scroll-bars . t) 
				      (width . ,diary-frame-width) ; , zum evaluieren des Characters
				      (height . 22))))
	  ;; (- calendar-frame-width calendar-frame-scroll-bar-width)))))
	  ((equal window-system 'win32)
	   (modify-frame-parameters (selected-frame) 
				    `((name . "Eingetragene Termine") 
				      (vertical-scroll-bars . t) 
				      (width . ,diary-frame-width) ; , zum evaluieren des Characters
				      (height . 22)))))
    (set-frame-position (selected-frame) 
			calendar-frame-x-position 
			(+ calendar-frame-y-position 200))
    (switch-to-buffer-other-frame calendar-buffer)
    ))



(message "Kalender geladen bis Marke 10")


(defun view-diary-entries-2 (&optional args)
  "Documentation see view-diary-entries, but some features more 
for natural behavior"
  (interactive "P")
  (let ((current-frame (selected-frame))
	(current-buf (current-buffer))
	(calendar-setup-from-calendar-buffer calendar-setup))
    ;; sehe nach, ob sich einer der diary-files geändert hat
    ;;     (diese Zeilen nutzen den Seiteneffekt von find-file-noselect, dass dieses auf das
    ;;     aktuelle Synchronisation des Files und des zugehörigen Buffers sieht)
    (if (boundp 'group-diary-file) (find-file-noselect group-diary-file))
    (if (boundp 'diary-file) (find-file-noselect diary-file))
    ;;
    ;;
    ;; falls calendar-setup nil: mache eventuell ein zweites Fenster und wechsle dorthin
    (if (not calendar-setup)
	(progn
	  (setq pop-up-windows t)
	  (setq pop-up-frame-function 'switch-to-buffer)
	  (setq special-display-function 'switch-to-buffer)
	  (setq diary-hook (delq 'change-diary-frame-position diary-hook))
	  (if (one-window-p)
	      (split-window (selected-window) 8))  ;; 8 = Kalender-Höhe
	  ))
    (view-diary-entries (if (not args) 1 args))
    (let ((calbuf (current-buffer)))
      ;;
      (set-buffer (find-file-noselect group-diary-file))
      (let ((modp (buffer-modified-p)))
	(save-excursion
	  (goto-char (point-min))
	  (replace-regexp "\r" "\n"))
	(set-buffer-modified-p modp))
      ;;
      (set-buffer (find-file-noselect diary-file))
      (let ((modp (buffer-modified-p)))
	(save-excursion
	  (goto-char (point-min))
	  (replace-regexp "\r" "\n"))
	(set-buffer-modified-p modp))
      ;;

      (if window-system
	  (progn
	    (hilit-add-pattern 
	     "^Montag\\|^Dienstag\\|^Mittwoch\\|^Donnerstag\\|^Freitag\\|^Samstag\\|^Sonntag"
	     "^=+$" 'struct)
	    (hilit-repaint-command t)))
		  
      (if calendar-setup-from-calendar-buffer
	  (progn
	    (switch-to-buffer-other-frame fancy-diary-buffer)
	    (setq calendar-setup calendar-setup-from-calendar-buffer)
	    (calendar-set-mode-line "Eingetragene Termine")
	    (remove-strange-noentry-entries)
	    (local-set-key "\C-o" 'change-focus)
	    (if window-system
		(hilit-repaint-command t))
	    (switch-to-buffer-other-frame calendar-buffer))
	(switch-to-buffer calendar-buffer)
	(other-window 1)
	(switch-to-buffer fancy-diary-buffer)
	(calendar-set-mode-line "Eingetragene Termine")
	(remove-strange-noentry-entries)
	(setq calendar-setup calendar-setup-from-calendar-buffer)
	(local-set-key "\C-o" 'change-focus)
	(if window-system
	    (hilit-repaint-command t))
	(other-window -1)))
    (calendar-day-message)))


(defun view-other-diary-entries-2 (filename &optional args)
  (interactive "fTerminkalenderfile: \npAnzahl der auzuzeigenden Tage [1]: ")
  (let ((current-frame (selected-frame))
	(current-buf (current-buffer))
	(diary-file filename)
	(calendar-setup-from-calendar-buffer calendar-setup))
    ;; sehe nach, ob sich einer der diary-files geändert hat
    ;;     (diese Zeilen nutzen den Seiteneffekt von find-file-noselect, dass dieses auf das
    ;;     aktuelle Synchronisation des Files und des zugehörigen Buffers sieht)
    (if (boundp 'group-diary-file) (find-file-noselect group-diary-file))
    (if (boundp 'diary-file) (find-file-noselect diary-file))
    ;;

    ;;
    ;; falls calendar-setup nil: mache eventuell ein zweites Fenster und wechsle dorthin
    ;;
    (if (not calendar-setup)
	(progn
	  (setq pop-up-windows t)
	  (setq pop-up-frame-function 'switch-to-buffer)
	  (setq special-display-function 'switch-to-buffer)
	  (setq diary-hook (delq 'change-diary-frame-position diary-hook))
	  (if (one-window-p)
	      (split-window (selected-window) 8))  ;; 8 = Kalender-Höhe
	  ))
    
    (view-diary-entries (if (not args) 1 args))

    (let ((calbuf (current-buffer)))
      
      (set-buffer (find-file-noselect group-diary-file))
      (let ((modp (buffer-modified-p)))
	(save-excursion
	  (goto-char (point-min))
	  (replace-regexp "\r" "\n"))
	(set-buffer-modified-p modp)
	(setq calendar-setup calendar-setup-from-calendar-buffer))
      (set-buffer (find-file-noselect diary-file))
      (let ((modp (buffer-modified-p)))
	(save-excursion
	  (goto-char (point-min))
	  (replace-regexp "\r" "\n"))
	(set-buffer-modified-p modp)
	(setq calendar-setup calendar-setup-from-calendar-buffer))
      (if calendar-setup
	  (progn
	    (switch-to-buffer-other-frame fancy-diary-buffer)
	    (calendar-set-mode-line "Eingetragene Termine")
	    (remove-strange-noentry-entries)
	    (local-set-key "\C-o" 'change-focus)
	    (if window-system
		(hilit-repaint-command t))
	    (switch-to-buffer-other-frame calendar-buffer))
	(switch-to-buffer calendar-buffer)
	(other-window 1)
	(switch-to-buffer fancy-diary-buffer)
	(calendar-set-mode-line "Eingetragene Termine")
	(remove-strange-noentry-entries)
	(local-set-key "\C-o" 'change-focus)
	(if window-system
	    (hilit-repaint-command t))
	(other-window -1)))
    (calendar-day-message)))

(defun remove-strange-noentry-entries ()
  "Bugkorrektur auf der optischen Seite, nicht schön, aber ohne 
extrem großen Aufwand nicht anders zu machen
(unter bestimmten Umständen taucht manchmal ein dirary-noentry-string 
zuviel im Kalender auf. Dieser wird in dieser Subroutine entfernt.)"
	    (save-excursion
	      (toggle-read-only t)
	      (toggle-read-only)
	      (goto-char (point-min))
	      (let ((tmppnt 0))
		(while (search-forward diary-noentry-string nil t)
		  (goto-char (match-beginning 0))
		  (setq tmppnt (match-end 0))
		  (backward-char 7)
		  (if (not (looking-at "======"))
		      (progn
			(forward-line 1)
			(beginning-of-line)
			(kill-line 1))
		    (goto-char tmppnt)
		    (next-line 1)
		    (if (not (looking-at "^$"))
			(progn
			  (previous-line 1)
			  (beginning-of-line)
			  (kill-line 1))))))
	      (toggle-read-only t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Neuigkeitenbufferfenster

(define-key calendar-mode-map [menu-bar news]
  (cons "Neues" (make-sparse-keymap "Nachrichten")))

(define-key calendar-mode-map [menu-bar news news-infofile]
  '("Allgemeine Nachrichten" . view-information-file))

(defun view-information-file ()
  (interactive)
  (find-file-noselect information-file)
  (let ((pop-up-windows nil))
    (display-buffer (get-file-buffer information-file))
    (set-buffer (get-file-buffer information-file))
    (calendar-set-mode-line "Nachrichten")
    ;;
    (setq buffer-read-only t)
    (goto-char (point-min))
    ;;
    (setq my-mode-map (cons 'keymap text-mode-map))
    (local-set-key "q" '(lambda ()(interactive)(kill-buffer (current-buffer))(iconify-frame)))
    (message "q verläßt den Buffer")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; gefährlich
(define-key (current-global-map) "\C-x\C-y" 'iconify-frame)
(if (not xemacsp)
    (define-key menu-bar-tools-menu [iconify]
      '("Fenster iconifizieren" . iconify-frame)))



(message "Kalender geladen bis Marke 11")

;;;================================================================================
;;;================================================================================
;;;================================================================================
;;;================================================================================
;;;================================================================================

;
;
; Funktionen zum Erstellen des Kalenders aus dem gcal-Ausgabefile
;
;



;;; Zwei noch per Hand nachgetragene Termine in der Liste: Siebenschläfer und Johannisnacht
;)(holiday-extract-from-list '((11 20 1960) "Totensonntag (DE) [Merktag]")
;)(holiday-extract-from-list '((12 24 1960) "Heiligabend (Std) [Merktag]")
;)(holiday-extract-from-list '((12 31 1960) "Silvester (Std) [Merktag]")
;)
;(holiday-fixed 7 27 "Siebenschläfer [Merktag]")
;(holiday-fixed 6 21 "Johannisnacht [+\"Fete de la musique\"] [Merktag]")
;))

(defun insert-gcal-holidays-in-calendar (infile)
  "Macht aus einem gcal-Output-File einen emacs-Holiday-File,
  der dann durch ein eval bzw. load ausgeführt werden kann.
  der Ausgabefile ist unter \"op-project/op-calendar-holidaylist.el\"
  fest}im Code eingetragen."
  (interactive "FFilename of gcal output file: ")
  (find-file infile)
  (goto-char (point-max))
  (insert-char ?\n 3)
  (goto-char (point-min))
  ;;
  (let (mark1 mark2 feiertagsname tag monat jahr lokalfeiertag 
	      kallist-a kallist-b kallist-c nichtdeutscherfeiertag)
    ;; while-loop über alle Kalender aller Jahre
    (while (search-forward "Ewige Feiertagsliste:" nil t)
      (forward-line 2)
      (beginning-of-line)
      ;;
      ;; while-loop über die Zeilen der Feiertage der einzelnen Jahre
      (while (not (looking-at "^$"))
	;; Anfang Extraktion des Formates in eine Liste
	(beginning-of-line)
	(setq mark1 (point)) ; mark1 = Anfang der Zeile
	(re-search-forward "\\( Mo, \\| Di, \\| Mi, \\| Do, \\| Fr, \\| Sa, \\| So, \\)" 
			   (save-excursion (end-of-line)(point))
			   nil) ; Suche: Mo, Di,... - bei Fehler: breche ab
	(goto-char (match-beginning 0))
	;;
	;;
	;; zum Zeichen vor dem Wochentag
	;;
	(backward-char 1) ; Zeichen vor Mo, Di, ...
	(save-excursion
	  (backward-char)
	  ;; extrahiere den Feiertagsnamen
	  (re-search-backward "[^ \t]" nil nil)
	  (forward-char 1)
	  (setq feiertagsname (regexp-quote (buffer-substring-no-properties mark1 (point)))))
	  ;;
	  ;;
	  ;; extraire die Datumstrings
	  ;;
	(save-excursion
	  (search-forward "," nil nil)
	  (forward-word 1)
	  (backward-word 1)
	  (setq mark2 (point))
	  (setq tag (string-to-number (current-longword-quote-2)))
	  (forward-word 2)
	  (backward-char 1)
	  (setq monat (string-to-number (current-longword-quote-2)))
	  (forward-word 2)
	  (backward-char 1)
	  (setq jahr (string-to-number (current-longword-quote-2)))
	  )
	;;
	;;
	;; Suche danach, in welchem Land dieser Tag Feiertag ist -
	;; wenn er kein deutscher oder Standardfeiertag ist, dann setzte
	;; den Tag auf Kategorie "Merktag".
	;; Ebenso wird auf "Merktag" gesetzt, wenn das gcal-Zeichen 
	;; ein "-" ist
	;;
	(save-excursion
	  (beginning-of-line)
	  (re-search-forward "(\\(.*\\))" nil nil)
	  (backward-char 2)
	  (if (not (or (string= (current-longword-quote-2) "(DE)")
		       (string= (current-longword-quote-2) "(Std)")))
	      (setq nichtdeutscherfeiertag t)
	    (setq nichtdeutscherfeiertag nil)))
	;; interpretiere den String vor dem Wochentag für ausländische Tage
	(cond ((looking-at "-")
	       (setq lokalfeiertag "[Besonderer Tag im Ausland]"))
	      ((looking-at "#")  
	       (setq lokalfeiertag "[Feiertag im Ausland (im Großteil des Landes)]"))
	      ((looking-at "\\*")
	       (setq lokalfeiertag "[Feiertag im Ausland (in einem Teil des Landes)]"))
	      ((looking-at "\\+")
	       (setq lokalfeiertag "[Feiertag im Ausland]")))
	;;
	;;
	;; interpretiere den String vor dem Wochentag bei deutschen Feiertagen
	(cond ((or (looking-at "-")
		   nichtdeutscherfeiertag)
	       (if nichtdeutscherfeiertag
		   ;; Auslandsfeiertagsliste
		   (setq kallist-c (nconc kallist-c 
					   (list (concat "((" monat " " tag " " jahr ") \""
							 feiertagsname 
							 " [" lokalfeiertag "]\")\n"))))
		 ;; Inlandsmerktagsliste
		 (setq lokalfeiertag "Merktag")
		 (setq kallist-a (nconc kallist-a 
					 (list (concat "((" monat " " tag " " jahr ") \""
						       feiertagsname 
						       " [" lokalfeiertag "]\")\n"))))))
	      ((looking-at "#")
	       (if (not nichtdeutscherfeiertag)(setq lokalfeiertag "Feiertag in den meisten Bundesländern"))
	       (setq kallist-b (nconc kallist-b 
				       (list (concat "((" monat " " tag " " jahr ") \""
						     feiertagsname 
						     " [" lokalfeiertag "]\")\n")))))
	      ((looking-at "\\*")
	       (if (not nichtdeutscherfeiertag)(setq lokalfeiertag "Feiertag in einigen Bundesländern"))
	       (setq kallist-b (nconc kallist-b 
				       (list (concat "((" monat " " tag " " jahr ") \""
						     feiertagsname 
						     " [" lokalfeiertag "]\")\n")))))
	      ((looking-at "\\+")
	       (if (not nichtdeutscherfeiertag)(setq lokalfeiertag "Gesetzlicher Feiertag"))
	       (setq kallist-b (nconc kallist-b 
				       (list (concat "((" monat " " tag " " jahr ") \""
						     feiertagsname 
						     " [" lokalfeiertag "]\")\n"))))))
	;;
	;;
	;; neue Position für die while-loop
	(beginning-of-line)
	(forward-line 1)
	;;(if (looking-at "St. ")(debug))
	))
    ;;  ; einfügen vornadran
    (goto-char (point-min))
    (replace-regexp "\\(^\\)" "\\1;")
    ;; hänge an den File ein Stück gültigen Elisp-Code, 
    ;; der den Feiertagen des Emacs-Calenders entspricht
    (goto-char (point-max))
    ;;
    ;;
    ;;
    ;; setze den zu evaluierenden Code ein
    ;;
    (insert "\n\n\n\50setq merktage\n'\50")
    (while (car kallist-a)
      (insert "\50holiday-extract-from-list '" (car kallist-a) "\51")
      (setq kallist-a (cdr kallist-a)))
    ;;
    ;;
    ;; nachträglich "per Hand" eingetragene Tage
    ;;
    (insert "\n\50holiday-fixed 4 30 \"zum 1. Mai: Wallpurgisnacht [Merktag]\"\51")
    (insert "\n\50holiday-fixed 6 27 \"Siebenschläfer [Merktag]\"\51")
    (insert "\n\50holiday-fixed 6 21 \"Johannisnacht [+\\\"Fete de la musique\\\"] [Merktag]\"\51")
    ;;
    (insert "\n\51\51\n")
    ;;
    ;;
    ;;
    ;;
    (insert "\n\n\n\50setq gcal-feiertage\n'\50")
    (while (car kallist-b)
      (insert "\50holiday-extract-from-list '" (car kallist-b) "\51")
      (setq kallist-b (cdr kallist-b)))
    (insert "\51\51\n")
    ;;
    (insert "\n\n\n\50setq auslandsfeiertage\n'\50")
    (while (car kallist-c)
      (insert "\50holiday-extract-from-list '" (car kallist-c) "\51")
      (setq kallist-c (cdr kallist-c)))
    (insert "\51\51\n")
    ;;
    ;;
    ;;
    ;; zu guter letzt noch: die Kalender-Liste bekommt noch die Tage dazu,
    ;; die nicht markiert werden sollen, aber angezeigt werden sollen
    ;; (siehe Feiertagskategorie "Merktag")
    (insert "\n\n\n\50setq calendar-holidays
	    \50append calendar-holidays gcal-feiertage merktage auslandsfeiertage\51\51\n\n")
    ;;
    ;;
    (insert "\n\n\n\50provide 'op-calendar-holidaylist\51\n\n\n")
    ;;
    ;;
    ;; ändere leicht die grammatische Struktur
    (goto-char (point-min))
    (replace-string " Tag (" "-Tag (")
    ;;
    ;;
    ;; compiliere den File zum Laden
    (write-file "~walther/op-project/op-calendar-holidaylist.el")
    (byte-compile-file "~walther/op-project/op-calendar-holidaylist.el")
    ))



(message "Kalender geladen bis Marke 12")


(defun current-longword-quote-2 ()
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
    (regexp-quote (buffer-substring-no-properties begin end))))



(message "Kalender geladen bis Marke 13")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Das hier passt mir die Tabs beim Diary-Einsetzen an meinen Stil
;; (Leerzeichen vornedran und alle Diary-Blöcke zusammen)
;; im diary-File an (aus diary-ins.el)
;;
;;; gefährlich !!x		
(defun make-diary-entry (string &optional nonmarking file)
  "Insert a diary entry STRING which may be NONMARKING in FILE.
If omitted, NONMARKING defaults to nil and FILE defaults to diary-file."
  (find-file-other-window
   (substitute-in-file-name (if file file diary-file)))
  (save-excursion
    (goto-char (point-min))
    (replace-regexp "\r" "\n"))
  (goto-char (point-max))
  (if (string-match "(diary-block" string)
      (progn
	(if (re-search-backward "(diary-block" nil t)
	    (progn
	      (end-of-line)
	      (re-search-forward "^[ \t]*$" nil t)
	      (insert "\n")))))
  (if (string-match "(diary-cyclic" string)
      (progn
	(if (re-search-backward "(diary-cyclic" nil t)
	    (progn
	      (end-of-line)
	      (re-search-forward "^[ \t]*$" nil t)
	      (insert "\n")))))
  (if (string-match "(diary-anniversary" string)
      (progn
	(if (re-search-backward "(diary-anniversary" nil t)
	    (progn
	      (end-of-line)
	      (re-search-forward "^[ \t]*$" nil t)
	      (insert "\n")))))
  (if (string-match "(diary-weekday" string)
      (progn
	(if (re-search-backward "(diary-anniversary" nil t)
	    (progn
	      (end-of-line)
	      (re-search-forward "^[ \t]*$" nil t)
	      (insert "\n")))))
  (if (string-match "Sonntag\\|Montag\\|Dienstag\\|Mittwoch\\|Donnerstag\\|Freitag\\|Samstag" string)
      (insert
       (if (bolp) "" "\n")
       (if nonmarking diary-nonmarking-symbol "")
       string "\n\t\n\n")
    (if (string-match " \\* " string)
	(insert
	 (if (bolp) "" "\n")
	 (if nonmarking diary-nonmarking-symbol "")
	 string "x\t\n\n")
      (insert
       (if (bolp) "" "\n")
       (if nonmarking diary-nonmarking-symbol "")
       string ".\t\n\n")))
  (backward-char 2)
  (iso-accents-mode t))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Korrekturen zu appt.el:

;; 3 x auf Deutsch, 1 x Bug-Korrektur für mehrzeilige Anzeigen


;; Deutsch 1
(defun appt-check ()
  "Check for an appointment and update the mode line.
Note: the time must be the first thing in the line in the diary
for a warning to be issued.

The format of the time can be either 24 hour or am/pm.
Example: 

               02/23/89
                 18:00 Dinner
            
              Thursday
                11:45am Lunch meeting.

Appointments are checked every `appt-display-interval' minutes.
The following variables control appointment notification:

`appt-issue-message'
	If t, the diary buffer is checked for appointments.

`appt-message-warning-time'
	Variable used to determine if appointment message
	should be displayed.

`appt-audible'
	Variable used to determine if appointment is audible.
	Default is t.

`appt-visible'
	Variable used to determine if appointment message should be
	displayed in the mini-buffer.  Default is t.

`appt-msg-window'
	Variable used to determine if appointment message
	should temporarily appear in another window.  Mutually exclusive
	to `appt-visible'.

`appt-display-duration'
	The number of seconds an appointment message
	is displayed in another window.

`appt-disp-window-function'
    	Function called to display appointment window.  You can customize
	appt.el by setting this variable to a function different from the
	one provided with this package.
  
`appt-delete-window-function'
    	Function called to remove appointment window and buffer.  You can
	customize appt.el by setting this variable to a function different
	from the one provided with this package."

  (let* ((min-to-app -1)
	 (new-time "")
	 (prev-appt-mode-string appt-mode-string)
	 (prev-appt-display-count (or appt-display-count 0))
	 ;; Non-nil means do a full check for pending appointments
	 ;; and display in whatever ways the user has selected.
	 ;; When no appointment is being displayed,
	 ;; we always do a full check.
	 (full-check
	  (or (not appt-now-displayed)
	      ;; This is true every appt-display-interval minutes.
	      (= 0 (mod prev-appt-display-count appt-display-interval))))
	 ;; Non-nil means only update the interval displayed in the mode line.
	 (mode-line-only
	  (and (not full-check) appt-now-displayed)))

    (when (or full-check mode-line-only)
      (save-excursion

	;; Get the current time and convert it to minutes
	;; from midnight. ie. 12:01am = 1, midnight = 0.

	(let* ((now (decode-time))
	       (cur-hour (nth 2 now))
	       (cur-min (nth 1 now))
	       (cur-comp-time (+ (* cur-hour 60) cur-min)))

	  ;; At the first check in any given day, update our 
	  ;; appointments to today's list.

	  (if (or (null appt-prev-comp-time)
		  (< cur-comp-time appt-prev-comp-time))
	      (condition-case nil
		  (progn
		    (if (and view-diary-entries-initially appt-display-diary)
			(diary)
		      (let ((diary-display-hook 'appt-make-list))
			(diary))))
		(error nil)))
	  (setq appt-prev-comp-time cur-comp-time)

	  (setq appt-mode-string nil)
	  (setq appt-display-count nil)

	  ;; If there are entries in the list, and the
	  ;; user wants a message issued,
	  ;; get the first time off of the list
	  ;; and calculate the number of minutes until the appointment.

	  (if (and appt-issue-message appt-time-msg-list)
	      (let ((appt-comp-time (car (car (car appt-time-msg-list)))))
		(setq min-to-app (- appt-comp-time cur-comp-time))

		(while (and appt-time-msg-list 
			    (< appt-comp-time cur-comp-time))
		  (setq appt-time-msg-list (cdr appt-time-msg-list)) 
		  (if appt-time-msg-list
		      (setq appt-comp-time 
			    (car (car (car appt-time-msg-list))))))

		;; If we have an appointment between midnight and
		;; 'appt-message-warning-time' minutes after midnight,
		;; we must begin to issue a message before midnight.
		;; Midnight is considered 0 minutes and 11:59pm is
		;; 1439 minutes. Therefore we must recalculate the minutes
		;; to appointment variable. It is equal to the number of 
		;; minutes before midnight plus the number of 
		;; minutes after midnight our appointment is.

		(if (and (< appt-comp-time appt-message-warning-time)
			 (> (+ cur-comp-time appt-message-warning-time)
			    appt-max-time))
		    (setq min-to-app (+ (- (1+ appt-max-time) cur-comp-time))
			  appt-comp-time))

		;; issue warning if the appointment time is 
		;; within appt-message-warning time

		(when (and (<= min-to-app appt-message-warning-time)
			   (>= min-to-app 0))
		  (setq appt-now-displayed t)
		  (setq appt-display-count
			(1+ prev-appt-display-count))
		  (unless mode-line-only
		    (if appt-msg-window
			(progn
			  (setq new-time (calendar-date-string (calendar-current-date)))  ;SMW
					;(format-time-string "%a %b %e "  
					;		    (current-time)))
			  (funcall
			   appt-disp-window-function
			   min-to-app new-time
			   (car (cdr (car appt-time-msg-list))))

			  (run-at-time
			   (format "%d sec" appt-display-duration)
			   nil
			   appt-delete-window-function))
			      ;;; else

		      (if appt-visible
			  (message "%s" 
				   (car (cdr (car appt-time-msg-list)))))

		      (if appt-audible
			  (beep 1))))

		  (when appt-display-mode-line
		    (setq appt-mode-string
			  (concat  " Termin in " min-to-app " min. ")))

		  ;; When an appointment is reached,
		  ;; delete it from the list.
		  ;; Reset the count to 0 in case we display another
		  ;; appointment on the next cycle.
		  (if (= min-to-app 0)
		      (setq appt-time-msg-list
			    (cdr appt-time-msg-list)
			    appt-display-count nil)))))

	  ;; If we have changed the mode line string,
	  ;; redisplay all mode lines.
	  (and appt-display-mode-line
	       (not (equal appt-mode-string
			   prev-appt-mode-string))
	       (progn
		 (force-mode-line-update t)
		 ;; If the string now has a notification,
		 ;; redisplay right now.
		 (if appt-mode-string
		     (sit-for 0)))))))))


;; Deutsch 2
;; Display appointment message in a separate buffer.
(defun appt-disp-window (min-to-app new-time appt-msg)
  (require 'electric)

  ;; Make sure we're not in the minibuffer
  ;; before splitting the window.

  (if (equal (selected-window) (minibuffer-window))
      (if (other-window 1) 
	  (select-window (other-window 1))
	(if window-system
	    (select-frame (other-frame 1)))))
      
  (let* ((this-buffer (current-buffer))
	 (this-window (selected-window))
	 (appt-disp-buf (set-buffer (get-buffer-create appt-buffer-name))))

    (if (cdr (assq 'unsplittable (frame-parameters)))
	;; In an unsplittable frame, use something somewhere else.
	(display-buffer appt-disp-buf)
      (unless (or (special-display-p (buffer-name appt-disp-buf))
		  (same-window-p (buffer-name appt-disp-buf)))
	;; By default, split the bottom window and use the lower part.
	(appt-select-lowest-window)
	(split-window))
      (pop-to-buffer appt-disp-buf))
    (setq mode-line-format 
	  (concat "-------------------- Termin in "
		  min-to-app " Minuten. " new-time " %-"))
    (erase-buffer)
    (insert-string appt-msg)
    (shrink-window-if-larger-than-buffer (get-buffer-window appt-disp-buf t))
    (set-buffer-modified-p nil)
    (raise-frame (selected-frame))
    (select-window this-window)
    (if appt-audible
	(beep 1))))


;; Deutsch 3
;   (if window-system
;       (progn
;         (make-local-hook 'activate-menubar-hook)
;         (add-hook 'activate-menubar-hook 'cal-menu-update nil t)))
(defun cal-menu-update ()
  ;; Update the holiday part of calendar menu bar for the current display.
  (condition-case nil
      (if (eq major-mode 'calendar-mode)
          (let ((l))
            (calendar-for-loop;; Show 11 years--5 before, 5 after year of
                   ;; middle month
             i from (- displayed-year 5) to (+ displayed-year 5) do
             (setq l (cons (vector (format "des Jahres %s" i)
                                   (list (list 'lambda 'nil '(interactive)
                                               (list 'list-holidays i i)))
                                   t)
                           l)))
;SMW             (setq l (cons ["Feiertage markieren           (u)" mark-calendar-holidays t]
;SMW                           (cons ["Feiertagsmarkierung löschen" calendar-unmark t]
;SMW                                 (cons ["--" '("--") t] l))))
            (easy-menu-change '("Holidays") "Feiertage" (nreverse l)) ;SMW
;SMW            (define-key calendar-mode-map [menu-bar Holidays separator]
;SMW              '("--")) ;SMW
            (define-key calendar-mode-map [menu-bar Holidays today]
                `(,(format "Für heute (%s)"
                           (calendar-date-string (calendar-current-date) t t))
                  . cal-menu-today-holidays))
            (let ((title
                   (let ((m1 displayed-month)
                         (y1 displayed-year)
                         (m2 displayed-month)
                         (y2 displayed-year))
                     (increment-calendar-month m1 y1 -1)
                     (increment-calendar-month m2 y2 1)
                     (if (= y1 y2)
                         (format "%s-%s, %d"
                                 (calendar-month-name m1 3)
                                 (calendar-month-name m2 3)
                                 y2)
                       (format "%s, %d-%s, %d"
                               (calendar-month-name m1 3)
                               y1
                               (calendar-month-name m2 3)
                               y2)))))
              (define-key  calendar-mode-map [menu-bar Holidays 3-month]
                `(,(format "Für das aktuelle Fenster (%s)" title)
                  . list-calendar-holidays)))
            (let ((date (calendar-cursor-to-date)))
              (if date
                  (define-key calendar-mode-map [menu-bar Holidays 1-day]
                    `(,(format "Für Cursor-Datum %s"
                               (calendar-date-string date t t))
                      . calendar-cursor-holidays))))))
    ;; Try to avoid entering infinite beep mode in case of errors.
    (error (ding))))
(defun calendar-cursor-holidays ()
  "Find holidays for the date specified by the cursor in the calendar window."
  (interactive)
  (message "Checking holidays...")
  (let* ((date (calendar-cursor-to-date t))
         (date-string (calendar-date-string date))
         (holiday-list (check-calendar-holidays date))
         (holiday-string (mapconcat 'identity holiday-list ";  "))
         (msg (format "%s:  %s" date-string holiday-string)))
    (if (not holiday-list)
        (message "Keine Feiertage bekannt für %s" date-string)
      (if (<= (length msg) (frame-width))
          (message "%s" msg)
        (set-buffer (get-buffer-create holiday-buffer))
        (setq buffer-read-only nil)
        (calendar-set-mode-line date-string)
        (erase-buffer)
        (insert (mapconcat 'identity holiday-list "\n"))
        (goto-char (point-min))
        (set-buffer-modified-p nil)
        (setq buffer-read-only t)
        (display-buffer holiday-buffer)
        (message "Checking holidays...done")))))



;; Bug-Fix: auch mehrzeilige Korrekturen
(defun appt-make-list ()
  ;; We have something to do if the range of dates that the diary is
  ;; considering includes the current date.
  (let ((calendar-setup calendar-setup))
    (if (and (not (calendar-date-compare
		   (list (calendar-current-date))
		   (list original-date)))
	     (calendar-date-compare
	      (list (calendar-current-date))
	      (list (calendar-gregorian-from-absolute
		     (+ (calendar-absolute-from-gregorian original-date)
			number)))))
	(save-excursion
	  ;; Clear the appointments list, then fill it in from the diary.
	  (setq appt-time-msg-list nil)
	  (if diary-entries-list

	      ;; Cycle through the entry-list (diary-entries-list)
	      ;; looking for entries beginning with a time. If 
	      ;; the entry begins with a time, add it to the
	      ;; appt-time-msg-list. Then sort the list.

	      (let ((entry-list diary-entries-list)
		    (new-time-string ""))
		;; Skip diary entries for dates before today.
		(while (and entry-list
			    (calendar-date-compare
			     (car entry-list) (list (calendar-current-date))))
		  (setq entry-list (cdr entry-list)))
		;; Parse the entries for today.
		(while (and entry-list 
			    (calendar-date-equal 
			     (calendar-current-date) (car (car entry-list))))
		  (let ((time-string (substring (prin1-to-string 
						 (cadr (car entry-list))) 1 -1)))

		    (while (string-match
			    "[0-9]?[0-9]:[0-9][0-9]\\(am\\|pm\\)?\\(.*\n\\)*.*" ;SMW
			    time-string)
		      (let* ((appt-time-string (substring time-string
							  (match-beginning 0)
							  (match-end 0))))

			(if (< (match-end 0) (length time-string))
			    (setq new-time-string (substring time-string 
							     (+ (match-end 0) 1)
							     nil))
			  (setq new-time-string ""))

			(string-match "[0-9]?[0-9]:[0-9][0-9]\\(am\\|pm\\)?"
				      time-string)

			(let* ((appt-time (list (appt-convert-time 
						 (substring time-string
							    (match-beginning 0)
							    (match-end 0)))))
			       (time-msg (cons appt-time
					       (list appt-time-string))))
			  (setq time-string new-time-string)
			  (setq appt-time-msg-list (append appt-time-msg-list
							   (list time-msg)))))))
		  (setq entry-list (cdr entry-list)))))
	  (setq appt-time-msg-list (appt-sort-list appt-time-msg-list))

	  ;; Get the current time and convert it to minutes
	  ;; from midnight. ie. 12:01am = 1, midnight = 0,
	  ;; so that the elements in the list
	  ;; that are earlier than the present time can
	  ;; be removed.

	  (let* ((now (decode-time))
		 (cur-hour (nth 2 now))
		 (cur-min (nth 1 now))
		 (cur-comp-time (+ (* cur-hour 60) cur-min))
		 (appt-comp-time (car (car (car appt-time-msg-list)))))

	    (while (and appt-time-msg-list (< appt-comp-time cur-comp-time))
	      (setq appt-time-msg-list (cdr appt-time-msg-list)) 
	      (if appt-time-msg-list
		  (setq appt-comp-time (car (car (car appt-time-msg-list)))))))))))
  




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(message "Benutzerdefiniertes")


(setq appt-message-warning-time 15 ) ; Wieviele Minuten vorher will ich vor einem Termin gewarnt werden ?
(setq appt-display-duration 8 )      ; Wieviele Sekunden soll die Display-Zeile erinnern ?


(message "Kalender vollständig geladen")


(setq op-project-calendar-loaded t))   ; kommt ganz von vorne vom if am Anfang !!!

(provide 'op-calendar)


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
	(message "Outline blank line set to `nil}"))
    (set-variable (quote outline-blank-line) t) 
    (message "Outline blank line set to `true}"))
  (redraw-display))




;;;=============================================================================

;; braucht die Variable truncate-lines, sonst unabhängig
;;;;;;;includeforelc!
;;;;;;(load "op-xemacs-invisible")

              ;
(if gnuemacsp ;
    nil       ;
;=============;



(defun region-invisible-unsearchable ()
  (interactive)
  (save-excursion
    (add-text-properties (region-beginning) (region-end) '(invisible t))
    ;; (put-text-property (region-beginning) (region-end) 'invisible t)
    (map-extents (lambda
		   (extent ignored)
		   (set-extent-property extent 'isearch-open-invisible nil))
		 nil
		 (region-beginning) (region-end)
		 nil
		 'all-extents-closed 'invisible)))

(defun region-invisible-searchable ()
  (interactive)
  (save-excursion
    (add-text-properties (region-beginning) (region-end) '(invisible t))
    (map-extents (lambda
		   (extent ignored)
		   (set-extent-property extent 'isearch-open-invisible t))
		 nil
		 (region-beginning) (region-end)
		 nil
		 'all-extents-closed 'invisible)))

(defun current-position-visible ()
  (interactive)
  (let ((mark-beginning nil) (mark-end nil)
	(already-modified (buffer-modified-p)))
    (save-excursion
      (setq mark-beginning (- (save-excursion (re-search-backward "[\r\n]" nil t) (point)) 1))
      (setq mark-end (save-excursion (re-search-forward "[\r\n]" nil t) (point)))
      (narrow-to-region mark-beginning mark-end)
      (add-text-properties (point-min) (point-max) '(invisible nil))
      (goto-char mark-beginning)
      (while (and (re-search-forward "\r" nil t)
		  (< (point) mark-end))
	(replace-match "\n" nil nil))
      (goto-char mark-beginning)
      (widen)
      (forward-char 2)
      (column-ruler)
      )
    (set-buffer-modified-p already-modified)))
;;
;; !!
;; 
(define-key isearch-mode-map "\C-j" 'current-position-visible)


;;
;; wenn Blöcke versteckt waren: diese alle wieder anzeigen
(defun show-invisible ()
  (interactive)
  (save-excursion
    (add-text-properties (save-excursion (beginning-of-buffer) (point)) ; Buffer-Anfang
			 (save-excursion (end-of-buffer) (point))       ; Buffer-Ende
			 '(invisible nil))
    (map-extents (lambda
		   (extent ignored)
		   (set-extent-property extent 'isearch-open-invisible t))
		 nil
		 (save-excursion (beginning-of-buffer) (point)) (save-excursion (end-of-buffer) (point))
		 nil
		 'all-extents-closed 'invisible)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Hilfsfunktionen


(defun region-invisible-searchable-s-e (start end)
  (add-text-properties start end '(invisible t))
  (map-extents (lambda
		 (extent ignored)
		 (set-extent-property extent 'isearch-open-invisible t))
	       nil
	       start end
	       nil
	       'all-extents-closed 'invisible))

;;;;;;;;;


(defun current-position-selecive-display-invisible-to-extent-invisible ()
  (let ((mark-beginning nil) (mark-end nil))
    ;; Region setzen
    (save-excursion
      (setq mark-beginning (save-excursion
			     (re-search-backward "\r\\|^" nil t) (+ 1 (point))))
      (setq mark-end (save-excursion 
		       (re-search-forward "\n" nil t)
		       (- (point) 1)))
      ;; Extent verändern
      (map-extents (lambda (extent ignored)
		     (set-extent-property extent 'invisible t)
		     )
		   nil
		   mark-beginning mark-end
		   nil
		   'all-extents-closed 'invisible)
      )))
;;    (add-text-properties mark-beginning mark-end '(invisible t))))
  
;;
;; ALLE nicht sichbare Anteile verstecken
(defun make-selective-display-hidden-invisible ()
  (let (tmp)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\r" nil t)
	(forward-char 1)
;;	(region-invisible-searchable-s-e (point) (save-excursion
;;					    (re-search-forward "\n" nil t)
;;					    (point)))
	(current-position-selecive-display-invisible-to-extent-invisible)
;;	(goto-char (save-excursion
;;		     (re-search-forward "\n" nil t)
;;		     (point)))
	))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  Anfangsfunktionen
;;;
;;;
;;; wenn auch nur einmal: alles zum Anfang mit der Option "isearch-open-invisible" markieren
(defun all-faces-with-isearch-open-invisisble ()
  (save-excursion
    (add-text-properties (save-excursion (goto-char (point-min))(point)) ; Buffer-Anfang
			 (save-excursion (goto-char (point-max))(point)) ; Buffer-Ende
			 '(isearch-open-invisible t))
    (map-extents (lambda
		   (extent ignored)
		   (set-extent-property extent 'isearch-open-invisible t))
		 nil
		 (save-excursion (goto-char (point-min))(point))
		 (save-excursion (goto-char (point-max))(point))
		 nil
		 'all-extents-closed 'isearch-open-invisible)))

;;
;; ...und ausführen:
;; An den Anfang von op-main an den Anfang bald nach dem Laden, noch bevor etwas zusammengefaltet wird
;;
(all-faces-with-isearch-open-invisisble)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;(setq isearch-mode-hook nil)
(setq isearch-mode-hook 'op-mode-isearch-prepare)
(defun op-mode-isearch-prepare ()
  (save-excursion
    (setq global-isearch-begin-point (point))
    (setq selective-display nil)
    (setq truncate-lines-tmp-save truncate-lines)
    (setq truncate-lines nil)))


;;;(setq isearch-mode-end-hook nil)
(setq isearch-mode-end-hook 'op-mode-isearch-restore)
(defun op-mode-isearch-restore ()
  (if (string= mode-name "Logbuch") ; ist op-mode
      (progn
	(save-excursion
	  (setq truncate-lines truncate-lines-tmp-save)
	  (setq selective-display t)
	  (if (and 
	       (not (= (point) global-isearch-begin-point))
	       ;;(and (not (backward-char (length isearch-string)))
	       ;;	 (looking-at isearch-string))
	       )
	      (progn
		(save-excursion
		  (re-search-backward "[\n\r]" nil t)
		  (forward-char 1)
		  (show-entry-hide))
		;? besser oder nicht? (recenter)
		;(column-ruler)
		))
	  ))))



;=============;
       )      ; Ende des ifs am Anfang: op-isearch gilt in der aktuellen Version nur für den XEmacs
              ;


;;;;;;;includeforelc!
;;;;;;(load "isearch-outline")

;;; ISEARCH-OUTLINE.EL --- allow isearch to ignore text hidden by outline

;; Copyright (C) 1996 Stephen Eglen

;; mit interessanten Veränderungen durch SMW

;; Author: Stephen Eglen <stephene@cogs.susx.ac.uk>
;; Maintainer: Stephen Eglen <stephene@cogs.susx.ac.uk>
;; Created: 13 Oct 1996
;; Version: 1.0
;; Keywords: isearch outline

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Outline-mode hides text within a buffer to create an outline of
;; your document.  Isearch normally matches text, regardless of
;; whether it is currently visible or not.  This code allows you to
;; toggle isearch so that it can ignore text that matches the search
;; if it is currently hidden.

;; It works by redefining isearch-search, so if isearch-search changes
;; in the future, this file will also need changing.

;;; Installation

;; Put the following code in .emacs
;; (require 'noutline)
;; (require 'isearch-outline)
;; (define-key isearch-mode-map "\C-o" 'isearch-toggle-outlines)

;; Note that this code requires noutline.el rather than outline.el to be
;; used.  By default, text is matched by isearch regardless of whether
;; it is visible or not.  If you want the default to be that you
;; ignore hidden text, set the following:
;; (setq isearch-outlines nil)

;;; Compatibility.
;; Tested on emacs 19.34. noutline.el not distributed with emacs
;; 19.27, but noutline.el loads ok in 19.27, and thus package works
;; ok.

;; Unfortunately, it doesnt work with xemacs 19.14. noutline.el is not
;; provided in the xemacs distribution, and the noutline.el from emacs
;; 19.34 does not load in xemacs due to keymap errors.


;;; Code:

(defconst isearch-outline-version (substring "$Revision: 1.1 $" 11 -2)
  "$Id: isearch-outline.el,v 1.1 1996/10/13 17:50:53 stephene Exp stephene $

Report bugs to: Stephen Eglen <stephene@cogs.susx.ac.uk>")


(defvar isearch-outlines t
  "Nil means isearch finds text only if visible.   Otherwise, text will
be found regardless of hidden or visible.  This value can be toggled
by isearch-toggle-outlines which can be bound to C-o.")

(defvar isearch-sje-repeat nil
  "Set by isearch-search to control matching of hidden text.")

(defun isearch-toggle-outlines ()
  "Toggle searching of hidden text within isearch."
  (interactive)
  (setq isearch-outlines (not isearch-outlines))
  (if isearch-outlines
      (progn
	(fset 'isearch-search 'isearch-search-also-invisible)  ;(symbol-function 
	(message "isearch searches also invisible"))
    (fset 'isearch-search 'isearch-search-only-visible)
    (message "isearch searches only visible"))
  ;(isearch-update)
  )


;; This is a modification of the function provided by isearch.el.  All
;; I have added is a simple while loop controlled by
;; isearch-sje-repeat, which decides whether we need to do another
;; search if we are in hidden text.

(defun isearch-search-only-visible ()
  ;; Do the search with the current search string.
  (isearch-message nil t)
  (condition-case lossage
      (let ((inhibit-quit nil)
	    (case-fold-search isearch-case-fold-search))
	(if isearch-regexp (setq isearch-invalid-regexp nil))
	(setq isearch-within-brackets nil)
	(setq isearch-sje-repeat t)
	(while isearch-sje-repeat
	  (setq isearch-success
		(funcall
		 (cond (isearch-word
			(if isearch-forward
			    'word-search-forward 'word-search-backward))
		       (isearch-regexp
			(if isearch-forward
			    're-search-forward 're-search-backward))
		       (t
			(if isearch-forward 'search-forward 'search-backward)))
		 isearch-string nil t))

	  ;; modification by sje
	  ;; this could be replaced by a function to allow more 
	  ;; general conditions to be tested.  If no function is given,
	  ;; default action is to set isearch-sje-repeat to nil.
	  (if (or isearch-outlines
		  (not isearch-success)
		  (outline-prev-char-visible))
	      (setq isearch-sje-repeat nil))
	  
	  ) ; while
	    
	(setq isearch-just-started nil)
	(if isearch-success
	    (setq isearch-other-end
		  (if isearch-forward (match-beginning 0) (match-end 0)))))

    (quit (isearch-unread ?\C-g)
	  (setq isearch-success nil))

    (invalid-regexp 
     (setq isearch-invalid-regexp (car (cdr lossage)))
     (setq isearch-within-brackets (string-match "\\`Unmatched \\["
						 isearch-invalid-regexp))
     (if (string-match
	  "\\`Premature \\|\\`Unmatched \\|\\`Invalid "
	  isearch-invalid-regexp)
	 (setq isearch-invalid-regexp "incomplete input")))
    (error
     ;; stack overflow in regexp search.
     (setq isearch-invalid-regexp (car (cdr lossage)))))

  (if isearch-success
      nil
    ;; Ding if failed this time after succeeding last time.
    (and (nth 3 (car isearch-cmds))
	 (ding))
    (goto-char (nth 2 (car isearch-cmds)))))


(defun isearch-search-also-invisible ()
  ;; Do the search with the current search string.
  (isearch-message nil t)
  (isearch-fix-case)
  (condition-case lossage
      (let ((inhibit-quit nil)
	    (case-fold-search isearch-case-fold-search)
	    (retry t))
	(if isearch-regexp (setq isearch-invalid-regexp nil))
	(setq isearch-within-brackets nil)
	(while retry
	  (setq isearch-success
		(funcall
		 (cond (isearch-word
			(if isearch-forward
			    'word-search-forward 'word-search-backward))
		       (isearch-regexp
			(if isearch-forward
			    're-search-forward 're-search-backward))
		       (t
			(if isearch-forward 'search-forward 'search-backward)))
		 isearch-string nil t))
	  ;; Clear RETRY unless we matched some invisible text
	  ;; and we aren't supposed to do that.
	  (if (or (eq search-invisible t)
		  (not isearch-success)
		  (bobp) (eobp)
		  (= (match-beginning 0) (match-end 0))
		  (not (isearch-range-invisible
			(match-beginning 0) (match-end 0))))
	      (setq retry nil)))
	(setq isearch-just-started nil)
	(when isearch-success
	  (setq isearch-other-end
		(if isearch-forward (match-beginning 0) (match-end 0)))
	  (and isearch-hide-immediately
	       (isearch-restore-invisible-extents (match-beginning 0)
						  (match-end 0)))))

    (quit (setq unread-command-events (nconc unread-command-events
					     (character-to-event (quit-char))))
	  (setq isearch-success nil))

    (invalid-regexp
     (setq isearch-invalid-regexp (car (cdr lossage)))
     (setq isearch-within-brackets (string-match "\\`Unmatched \\["
						 isearch-invalid-regexp))
     (if (string-match
	  "\\`Premature \\|\\`Unmatched \\|\\`Invalid "
	  isearch-invalid-regexp)
	 (setq isearch-invalid-regexp (gettext "incomplete input"))))
    (error
     ;; stack overflow in regexp search.
     (setq isearch-invalid-regexp (car (cdr lossage)))))

  (if isearch-success
      nil

    ;; If we're being run inside a keyboard macro, then the call to
    ;; ding will signal an error (to terminate the macro).  We must
    ;; turn off isearch-mode first, so that we aren't still in isearch
    ;; mode after the macro exits.  Note that isearch-recursive-edit
    ;; must not be true if a keyboard macro is executing.
    (if (and executing-kbd-macro (not defining-kbd-macro))
	(progn
	  (isearch-done)
	  (ding nil 'isearch-failed)))

    ;; Ding if failed this time after succeeding last time.
    (and (nth 3 (car isearch-cmds))
	 (ding nil 'isearch-failed))
    (goto-char (nth 2 (car isearch-cmds)))))



(defun outline-prev-char-visible ()
  "Return t if current character is visible."
  (interactive)
  (save-excursion
    (backward-char)
    (outline-visible)			;defined in noutline.el
    ))
  

(provide 'isearch-outline)

;;; ISEARCH-OUTLINE.EL ends here
(define-key isearch-mode-map "\C-o" 'isearch-toggle-outlines)

;;;;;;;includeforelc!
;;;;;;(load "outlines")



;;<http://www.damtp.cam.ac.uk/user/sje30/emacs/outlines.el>

;;; outlines.el --- show the outlines of a file in separate buffer

;; Copyright (C) 1997 Stephen Eglen

;; Author: Stephen Eglen <stephen@anc.ed.ac.uk>
;; Maintainer: Stephen Eglen <stephen@anc.ed.ac.uk>
;; Created: 05 Jul 1997
;; Keywords: outlines
;; location: http://www.anc.ed.ac.uk/~stephen/emacs
;; RCS: $Id: outlines.el,v 1.4 1999/04/08 09:07:40 stephen Exp $
 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This library allows you to view the outlines of a source buffer in
;; a separate buffer.  It allows you to see the structure of the
;; outlines in an easier way than using the outline function
;; `hide-body'.  This has similar functionality to `occur'.

;;; Installation
;; (require 'outlines)
;;
;; To see the outlines, do M-x outlines-show.  Its best to bind this to a key, 
;; for example, you can bind to either outline-mode or outline-minor-mode:
;;
;;(define-key outline-minor-mode-map
;;  (concat outline-minor-mode-prefix "o") 'outlines-show)

;; To see the corresponding line of the source buffer, press return or
;; the middle mouse button on the line in the *Outlines* buffer.  To
;; automatically see the corresponding source entries as you move
;; across the lines in *Outlines*, use `fm-start' from fm.el (from my
;; website):
;;
;; (add-hook 'outlines-mode-hook 'fm-start)
;;
;; (You may have to press `f' to activate the highlighting.)  As you
;; move across lines in *Outlines*, the corresponding line in the
;; source buffer is highlighted.  To move to the next (previous)
;; heading of the same depth, press C-up (C-down).

;; Viewing different depths
;;
;; By default, all outlines are shown in the *Outlines* buffer.  To
;; restrict viewing to just outlines up to and including level N,
;; supply N as a numeric argument to outlines-show.  
;; e.g. C-u 3 M-x outlines-show will show outlines only up to level 3.
;; The value 0 means show all outlines.

;; Updating the *Outlines* buffer
;; 
;; If your source buffer changes, press "g" to update the *Outlines*
;; buffer.  If you also want to change the depth of outlines shown,
;; either press "N g" (where N is 0,1,2,3,4 or 5) or "M-N g" (the meta
;; key and any number N followed by g).

;;; Viewing latex documents.

;; If you use AUC TeX, support is available for viewing outlines of
;; LaTeX documents.  However, rather than using this package, I highly
;; recommend using reftex.el for generating and navigating your latex
;; documents.  Reftex should be distributed with new versions of
;; Emacs, or get it from: http://www.strw.leidenuniv.nl/~dominik/Tools/

;;; How it works.
;; We use the text property `line' attached to each line of the
;; *Outlines* buffer to indicate the corresponding line of the source
;; file (with a marker).  The `depth' property tells us the depth of the
;; outline.

;;; Versions.
;; This should work under Emacs 20 and XEmacs.  If you are using Emacs
;; 19, you probably need to explicitly load noutlines.el before
;; loading this package -- in which case, just add:
;; (require 'noutline)
;; to your .emacs before loading this file.

;;; User variables.
;; Check out the boolean variables outlines-number, outlines-hide-out
;; and outlines-flatten.

;;; TODO
;;  showing the depth in the *Outlines* buffer?

;; Using an indirect-buffer would be nicer, but this is not yet in
;; XEmacs suite.

;;; Code:

;; Must use noutline in Emacs  get outline-on-heading-p.
;(if (string-match "XEmacs" (emacs-version))
;    (require 'outline)
;  ;; else Emacs
;  (require 'noutline))

(require 'outline)

;;; User variables
(defvar outlines-number nil
  "*Non-nil to show the depth of current entry.")

(defvar outlines-hide-out nil
  "*Non-nil to hide the outline matched by `outline-regexp'.")

(defvar outlines-flatten nil
  "*Non-nil means to ignore the depth of outline.")

;;; Internal variables

(defvar outlines-mode-map ())
;; name of buffer where outlines last run.
(defvar outlines-buffer nil)

;; max depth allowed: 0 means allow all depths.
(defvar outlines-max-depth 0)

;; (setq outlines-mode-map nil) ;just for testing
(if outlines-mode-map
    ()
  (setq outlines-mode-map (make-sparse-keymap))
  (define-key outlines-mode-map [mouse-2] 'outlines-mouse-goto-line)
  (define-key outlines-mode-map "\C-c\C-c" 'outlines-goto-line)
  (define-key outlines-mode-map "\C-m" 'outlines-goto-line)
  (define-key outlines-mode-map "0" 'outlines-set-depth)
  (define-key outlines-mode-map "1" 'outlines-set-depth)
  (define-key outlines-mode-map "2" 'outlines-set-depth)
  (define-key outlines-mode-map "3" 'outlines-set-depth)
  (define-key outlines-mode-map "4" 'outlines-set-depth)
  (define-key outlines-mode-map "5" 'outlines-set-depth)
  (define-key outlines-mode-map (read-kbd-macro "C-<down>")
    'outlines-forward-same-depth)
  (define-key outlines-mode-map (read-kbd-macro "C-<up>")
    'outlines-backward-same-depth)
  (define-key outlines-mode-map "g" 'revert-buffer))

(defun outlines-set-depth ()
  "Set the value of `outlines-max-depth'."
  (interactive)
  (setq outlines-max-depth (- last-input-char 48)))

(defun outlines-mode ()
  "Major mode for output from \\[outlines-show].

  \\{outlines-mode-map}"
  (kill-all-local-variables)
  (use-local-map outlines-mode-map)
  (setq major-mode 'outlines-mode)
  (setq mode-name "Outlines")
  (make-local-variable 'revert-buffer-function)
  (setq outlines-max-depth 0)
  (setq revert-buffer-function 'outlines-revert-function)
  (run-hooks 'outlines-mode-hook)
)

(defun outlines-revert-function (ignore1 ignore2)
  "Revert the *Outlines* buffer.
Use a prefix argument to specify a maximum depth of outline."

  (let ((pos (marker-position (get-text-property (point) 'line))))
    (set-buffer outlines-buffer)
    (outlines-show (if current-prefix-arg
		       current-prefix-arg
		     outlines-max-depth)
		   pos))) 

(defun outlines-show (&optional arg pos)
  "Show the outlines in the *Outlines* buffer."
  (interactive "P")
  (let (
	(opbuf (get-buffer-create "*Outlines*"))
	(buffer (current-buffer))
	(p (point))  ;SMW
	(beg nil)
	(end nil)
	(regexp outline-regexp)
	(fname buffer-file-truename)
	(depth nil)
	(text nil)
	(pt (or pos (point)))
	(this-line (point-min))
	(this-pt (point-min))
	(last-line nil)
	(last-pt nil)
	(s nil)
	(outlines-latex (and 
			 ;; AUC TeX specific.
			 (fboundp 'LaTeX-outline-level)
			 (eq major-mode 'latex-mode)))
	)
    (save-excursion

      ;; write some stuff in the *Outlines* buffer before starting.
      (set-buffer opbuf)
      (outlines-mode)
      (delete-region (point-min) (point-max))
      (setq outlines-buffer buffer)
      (if arg
	  (setq outlines-max-depth arg))
      (insert (format "Outlines depth %d for %s\n"  
		      (if outlines-max-depth
			  outlines-max-depth
			0)
		      fname ))

      ;; Parse the source buffer.
      (set-buffer buffer)
      (goto-char (point-min))

      (while (not (eobp))
	(if (outline-on-heading-p) 
	    (progn

	      (cond (
		     
		     outlines-latex
		     (setq depth (LaTeX-outline-level)))
		    ( t
		      (setq depth (outline-level))))

	      (if (or (eq outlines-max-depth 0)
		      (<= depth outlines-max-depth))
		  ;; show the outline.
		  (progn
		    (beginning-of-line) 
		    (setq tem (make-marker))
		    (set-marker tem (point))
		    (if outlines-latex
			;; do the latex stuff.
			(progn
			  (search-forward "{")
			  (setq beg (point))
			  (backward-char 1)
			  (forward-list 1)
			  (backward-char 1)			  
			  (setq end (point)))
		      ;; else not latex.
		      (if outlines-hide-out
			  (forward-char depth))
		      (setq beg (point))
		      (beginning-of-line)
		      (end-of-line)
		      (setq end (point))
		      )

		    ;; extract the text and insert it.
		    (setq text (buffer-substring beg end))
		    ;;(forward-line -1)
		    ;; add space to the string to indicate depth:
		    (setq text (concat 
				(if outlines-flatten
				    " "
				  (concat
				    (make-string depth ? ) 
				    (if outlines-number depth)))
				" "
				text "\n"))
		    (put-text-property 0 (length text) 'line tem text)
		    (put-text-property 0 (length text) 'depth depth text)
		    
		    ;; put the info in the *Outlines* buffer
		    (set-buffer opbuf)
		    (insert text)
		    (set-buffer buffer)
		    )
		)))
	(outline-next-heading)
	      )
      (goto-char p)  ;; SMW
      (switch-to-buffer opbuf)
      (setq outlines-buffer buffer)

      (goto-char (point-min))

      ;; lets try to position the point appropriately.
      (forward-line 1)
      (while (and (not s)
		  (< (point) (point-max)))
	(setq last-line this-line
	      this-line (marker-position (get-text-property (point) 'line))
	      last-pt this-pt
	      this-pt (point))
	(if (and (>= pt last-line)
		 (<= pt this-line))
	    (setq s  (if (< (- pt last-line) (- this-line pt))
			 last-pt
		       this-pt)))
	(forward-line 1)
	)
      (if s (goto-char s)
	(message "couldn't place at start"))

      )))

;; copied from occur-mode-mouse-goto in replace.el
(defun outlines-mouse-goto-line (event)
  "Visit outline under mouse in source buffer."
  (interactive "e")
  (let (buffer pos line)
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-end event))))
      (save-excursion
	(goto-char (posn-point (event-end event)))
	(setq line (get-text-property (point) 'line)))
      )
    (pop-to-buffer outlines-buffer)
    (goto-char (marker-position line))
    ))

(defun outlines-goto-line ()
  "Visit outline under point in source buffer."
  (interactive)
  (let ((line (get-text-property (point) 'line)))
    (pop-to-buffer outlines-buffer)
    (goto-char (marker-position line))
    ))

(defun outlines-forward-same-depth ()
  "Move to next outline with same depth."
  (interactive)
  (let 
      ( (depth (get-text-property (point) 'depth))
	found
	trydepth)
    (save-excursion
      (while (and (not (eobp))
		  (not found))
	(forward-line 1)
	(setq trydepth (get-text-property (point) 'depth))
	(if (equal trydepth depth)
	    (setq found (point)))))
    (if found
	(goto-char found)
      (message "no next of same depth"))))

(defun outlines-backward-same-depth ()
  "Move to previous outline with same depth."
  (interactive)
  (let 
      ( (depth (get-text-property (point) 'depth))
	found
	trydepth)
    (save-excursion
      (while (and (not (bobp))
		  (not found))
	(forward-line -1)
	(setq trydepth (get-text-property (point) 'depth))
	(if (equal trydepth depth)
	    (setq found (point)))))
    (if found
	(goto-char found)
      (message "no previous of same depth"))))

(provide 'outlines)

;;; outlines.el ends here



;; glyphs: einfachstes Einfügen von Bildern
;;;;;;;includeforelc!
;;;;;;(load "op-glyph-images")

              ;
(if gnuemacsp ;
    nil       ;
;=============;
;;;;
;;;;
;;;; (progn
;;;;  (if (featurep 'gtk)
;;;;      (set-console-type-image-conversion-list
;;;;       'gtk
;;;;       `(,@(if (featurep 'xpm) '(("\\.xpm\\'" [xpm :file nil] 2)))
;;;; 	   ("\\.xbm\\'" [xbm :file nil] 2)
;;;; 	   ,@(if (featurep 'xpm) '(("\\`/\\* XPM \\*/" [xpm :data nil] 2)))
;;;; 	   ,@(if (featurep 'xface) '(("\\`X-Face:" [xface :data nil] 2)))
;;;; 	   ,@(if (featurep 'gif) '(("\\.gif\\'" [gif :file nil] 2)
;;;; 				   ("\\`GIF8[79]" [gif :data nil] 2)))
;;;; 	   ,@(if (featurep 'jpeg) '(("\\.jpe?g\\'" [jpeg :file nil] 2)))
;;;; 	   ;; all of the JFIF-format JPEG's that I've seen begin with
;;;; 	   ;; the following.  I have no idea if this is standard.
;;;; 	   ,@(if (featurep 'jpeg) '(("\\`\377\330\377\340\000\020JFIF"
;;;; 				     [jpeg :data nil] 2)))
;;;; 	   ,@(if (featurep 'png) '(("\\.png\\'" [png :file nil] 2)))
;;;; 	   ,@(if (featurep 'png) '(("\\`\211PNG" [png :data nil] 2)))
;;;; 	   ("" [autodetect :data nil] 2))))
;;;;  (cond ((featurep 'xpm)
;;;;	 (set-glyph-image frame-icon-glyph
;;;;			  (concat "../etc/" "xemacs-icon3.xpm")
;;;;			  'global 'gtk)
;;;;	 (set-glyph-image xemacs-logo
;;;;			  (concat "../etc/"
;;;;				  (if emacs-beta-version
;;;;				      "xemacs-beta.xpm"
;;;;				    "xemacs.xpm"))
;;;;			  'global 'gtk))
;;;;	(t
;;;;	 (set-glyph-image xemacs-logo
;;;;			  "XEmacs <insert spiffy graphic logo here>"
;;;;			  'global 'gtk)))
;;;;  (set-glyph-image octal-escape-glyph "\\")
;;;;  (set-glyph-image control-arrow-glyph "^")
;;;;  (set-glyph-image invisible-text-glyph " ...")
;;;;  )


;;     ;; Use this function to insert a glyph at the left edge of point in the
;;     ;; current buffer.  Any existing glyph at this location is replaced.
;;     (defun insert-glyph (gl)
;;       "Insert a glyph at the left edge of point."
;;       (let ( (prop 'myimage)        ;; myimage is an arbitrary name, chosen
;;                                     ;; to (hopefully) not conflict with any
;;                                     ;; other properties.  Change it if
;;                                     ;; necessary.
;;              extent )
;;         ;; First, check to see if one of our extents already exists at
;;         ;; point.  For ease-of-programming, we are creating and using our
;;         ;; own extents (multiple extents are allowed to exist/overlap at the
;;         ;; same point, and it's quite possible for other applications to
;;         ;; embed extents in the current buffer without your knowledge).
;;         ;; Basically, if an extent, with the property stored in "prop",
;;         ;; exists at point, we assume that it is one of ours, and we re-use
;;         ;; it (this is why it is important for the property stored in "prop"
;;         ;; to be unique, and only used by us).
;;         (if (not (setq extent (extent-at (point) (current-buffer) prop)))
;;           (progn
;;             ;; If an extent does not already exist, create a zero-length
;;             ;; extent, and give it our special property.
;;             (setq extent (make-extent (point) (point) (current-buffer)))
;;             (set-extent-property extent prop t)
;;             ))
;;         ;; Display the glyph by storing it as the extent's "begin-glyph".
;;         (set-extent-property extent 'begin-glyph gl)
;;         ))
     
;;     ;; You can then use this function like:
;;     (insert-glyph (make-glyph [jpeg :file "/tmp/file1.jpg"]))
;;     ;; This will insert the glyph at point.
     
;;     ;; Here's an example of how to insert two glyphs side-by-side, at point
;;     ;; (using the above code):
;;     (progn
;;       (insert-glyph (make-glyph [jpeg :file "/tmp/file1.jpg"]))
;;       ;; Create a new extent at point.  We can't simply call "insert-glyph",
;;       ;; as "insert-glyph" will simply replace the first glyph with the
;;       ;; second.
;;       (setq extent (make-extent (point) (point) (current-buffer)))
;;       ;; Here, we're only setting the 'myimage property in case we need
;;       ;; to later identify/locate/reuse this particular extent.
;;       (set-extent-property extent 'myimage t)
;;       (set-extent-property extent 'begin-glyph
;;                            (make-glyph [jpeg :file "/tmp/file2.jpg"]))
;;       )


;;(defun about-with-face (string face)
;;  (let ((ext (make-extent 0 (length string) string)))
;;    (set-extent-property ext 'duplicable t)
;;    (set-extent-property ext 'unique t)
;;    (set-extent-property ext 'start-open t)
;;    (set-extent-property ext 'end-open t)
;;    (set-extent-face ext face))
;;  string)





(defun insert-glyph (gl)
  "Insert a glyph at the left edge of point."
  (let ( (prop 'myimage)        ;; myimage is an arbitrary name, chosen
	 ;; to (hopefully) not conflict with any
	 ;; other properties.  Change it if
	 ;; necessary.
	 extent )
    ;; First, check to see if one of our extents already exists at
    ;; point.  For ease-of-programming, we are creating and using our
    ;; own extents (multiple extents are allowed to exist/overlap at the
    ;; same point, and it's quite possible for other applications to
    ;; embed extents in the current buffer without your knowledge).
    ;; Basically, if an extent, with the property stored in "prop",
    ;; exists at point, we assume that it is one of ours, and we re-use
    ;; it (this is why it is important for the property stored in "prop"
    ;; to be unique, and only used by us).
    (if (not (setq extent (extent-at (point) (current-buffer) prop)))
	(progn
	  ;; If an extent does not already exist, create a zero-length
	  ;; extent, and give it our special property.
	  (setq extent (make-extent (point) (point) (current-buffer)))
	  (set-extent-property extent prop t)
	  ))
    ;; Display the glyph by storing it as the extent's "begin-glyph".
    (set-extent-property extent 'begin-glyph gl)
    ))



;;(image-specifier-p (make-image-specifier [jpeg :file "c:\\tmp\\nettesWerbeMailBild.jpg"]))

;; (glyph-property 
;; (glyph-property-instance
;; (setq tmptmp (make-glyph [jpeg :file "c:\\tmp\\nettesWerbeMailBild.jpg"]))
;; (glyph-width tmptmp)
;; (glyph-heigh tmptmp)
;; (insert-glyph tmptmp)



(defun insert-image-as-glyph--temporarily (picture)
  (interactive "fPicture to include [for this session only!]: ")
  (let (tmpglyph type-of-picture)
    (setq type-of-picture (substring picture 
				     (+ 1 (string-match "\\..*$" picture -5)) ; die 5 da ...
				     ;; ... die XEmacs-File-Spezifikation von Bildern nicht mehr als 4 Buchstaben haben 
				     ;; (z.B. .tiff, .pmg etc.)
				     nil))
    ;; z.Zt. unterstützte Filetypen
    ;;`xbm'
    ;;`xpm'
    ;;`xface'
    ;;`gif'
    ;;`jpeg'
    ;;`png'
    ;;`tiff'
    (cond ((string= type-of-picture "jpg")
	   ;; abfangen, wenn .jpg anstatt .jpeg
	   (setq tmpglyph (make-glyph (vector 'jpeg ':file picture))))
	  ((string= type-of-picture "jpeg")
	   (setq tmpglyph (make-glyph (vector 'jpeg ':file picture))))
	  ((string= type-of-picture "tif")
	   ;; oder .tif anstatt .tiff
	   (setq tmpglyph (make-glyph (vector 'tiff ':file picture))))
	  ((string= type-of-picture "tiff")
	   (setq tmpglyph (make-glyph (vector 'tiff ':file picture))))
	  ((string= type-of-picture "xbm")
	   (setq tmpglyph (make-glyph (vector 'xbm ':file picture))))
	  ((string= type-of-picture "xpm")
	   (setq tmpglyph (make-glyph (vector 'xpm ':file picture))))
	  ((string= type-of-picture "xface")
	   (setq tmpglyph (make-glyph (vector 'xface ':file picture))))
	  ((string= type-of-picture "gif")
	   (setq tmpglyph (make-glyph (vector 'gif ':file picture))))
	  ((string= type-of-picture "png")
	   (setq tmpglyph (make-glyph (vector 'png ':file picture))))
	  ((string= type-of-picture "png")
	   (setq tmpglyph (make-glyph (vector 'png ':file picture))))
	  ((string= type-of-picture "png")
	   (setq tmpglyph (make-glyph (vector 'png ':file picture))))
	  ;; sonst wie gewünscht und automatisch nach Endung des Files (BUG)
	  ;; ((setq tmpglyph (make-glyph (vector (make-symbol type-of-picture) ':file picture))))
	  )
    (insert-glyph tmpglyph)
    ;; (insert-glyph (make-glyph [jpeg :file picture]))
    ;; (insert-glyph (make-glyph [jpeg :file "c:\\tmp\\nettesWerbeMailBild.jpg"])
    (insert-char ?  )))
;;(insert-image "c:/tmp/nettesWerbeMailBild.jpg")



;=============;
       )      ; Ende des ifs am Anfang: op-isearch gilt in der aktuellen Version nur für den XEmacs
              ;



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
;;;;;;(load "op-indent")

;;;
;;; Indentierendes
;;;
;
;;; Die nächsten Zeilen sind
;;; wegen ernster Probleme aus text-mode.el herausgenommen worden.
;;; War: (defun indented-text-mode () ...)
;;;
(defun indented-text-mode-without-varkill ()
  "Major mode for editing text with indented paragraphs.
In this mode, paragraphs are delimited only by blank lines.
You can thus get the benefit of adaptive filling
 (see the variable `adaptive-fill-mode').
\\{indented-text-mode-map}
Turning on `indented-text-mode' calls the value of the variable
`text-mode-hook', if that value is non-nil."
;;  (interactive)
;;  (kill-all-local-variables)
;;  (use-local-map text-mode-map)  ; wirft alle fremden Tastenkombinationen raus
  (define-abbrev-table 'text-mode-abbrev-table ())
  (setq local-abbrev-table text-mode-abbrev-table)
  (set-syntax-table text-mode-syntax-table)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'indent-relative-maybe)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
;;  (use-local-map indented-text-mode-map) ; macht Tastatur und den Menü-Balken zu Schrott
  (setq mode-name "OP Indented Text")
  (setq major-mode 'indented-text-mode)
  (run-hooks 'text-mode-hook 'indented-text-mode-hook)
  ) 
(load "text-mode")
(indented-text-mode-without-varkill)


(defvar toggle-re-indent-place nil)
(make-local-variable 'toggle-re-indent-place)
(defvar toggle-re-indent-place_emptyHeadlines nil)
(make-local-variable 'toggle-re-indent-place_emptyHeadlines)
(defvar am-i-in-the-same-line 1)
(make-local-variable 'am-i-in-the-same-line)
(defvar am-i-in-the-same-line_emptyHeadlines 1)
(make-local-variable 'am-i-in-the-same-line_emptyHeadlines)

;;; braucht das Laden des indented-text-mode, in diesem Code hier
;;; aufgerufen als indented-text-mode-without-varkill (s.o.)

(defun re-indent-for-tab-command ()
  "Kompliziertes Indent für Outline- oder Project-Mode des Emacs.
-- unter einer Outline-Überschrift: Cursor nach der Anzahl der Sterne setzen
-- unter Freizeichen: an das Ende der Freizeichen (ignoriert dazwischen-
   liegende Newlines)
-- bei nur Newlines: kein Indent
-- Buchstaben am Zeilenanfang: normale Indentierung
-- Zeichen am Zeilenanfang: Indentierung nach dem ersten Leerraum
-- Pfeil (==>) in der da überliegenden Zeile: Einsetzen von vier Blanks
-- Minuszeichen (-) in der darüberliegenden Zeile: Einsetzen von 2 Blanks

-- Toggeln der Cursor-Positionen bei mehrmaligem Tippen zwischen
   Indentierung und Zeilenende
-- automatisches Tabulatorisieren der Leerzeichen (tabify)

Diese Funktion schaltet automatisch den indented-text-mode mit ein,
siehe auch unter der Dokumentation dieser Funktion"
;war: unter einer Outline-Überschrift: Cursor unter erste Schrift nach Sternen
;;;
;;; Ist die darüberliegende Zeile eine Zeile mit Buchstaben am Anfang ?
  (let ((before-change-functions nil)(after-change-functions nil))

  (if (not (fboundp 'this-is-column-mode))
      (defun this-is-column-mode (dum) nil))  ; gefährlich, Effekt noch nicht ganz abgeschätzt
	   
  (if (not (this-is-column-mode 'dum))
      (progn
	(let ((one-up-is-headline 0)(one-up-is-arrowline 0)(this-is-a-headline nil)(one-up-is-intendetforarrow-line 0))
	  (if (looking-at "[ \t]*$")
	      (setq am-i-in-the-same-line -1))
	  (save-excursion
	    (previous-line 1)
	    (beginning-of-line)
	    (setq point-save1 (point))
	    (if (not (looking-at "$"))
		(re-search-forward "^[^ \t]" nil t))
	    (setq point-save2 (- (point) 1)))
	  (if (= point-save1 point-save2)
	      (setq one-up-is-headline 1)
;;;
;;; Wenn keine Headline: Ist die darüberliegende Zeile eine Zeile mit einem Pfeil (==>) am Anfang ?
	    (save-excursion
	      (previous-line 1)
	      (beginning-of-line)
	      (if (looking-at "^[ \t]+==>")
		  (setq one-up-is-arrowline 1)))
;;; oder ist die darüberliegende Zeile eine Zeile mit einem Minus (-) am Anfang ?
	    (save-excursion
	      (previous-line 1)
	      (beginning-of-line)
	      (if (looking-at "^[ \t]*-")
		  (setq one-up-is-arrowline 2))))
;;;
;;; Falls in der gleichen Zeile: Toggeln zwischen sinnvollem Punkt und Ende der Zeile
	  (save-excursion
	    (beginning-of-line)
	    (if (= am-i-in-the-same-line (point))
		(setq toggle-re-indent-place (not toggle-re-indent-place))
	      (setq toggle-re-indent-place nil)
	      (setq am-i-in-the-same-line (point))))
	  (if toggle-re-indent-place
	      (end-of-line)
	    (let ((point-save1 (point))(point-save2)(save-position-for-short)(save-for-short))
	      (beginning-of-line)
;;;
;;; Ist die aktuelle Zeile eine Zeile mit Sonderzeichen am Anfang ?
;;	      (if (looking-at "[][!@#$\%^&\*()_+=|\\~`{};:'\"?/>.<,-]")
	      (if (looking-at "[][@$\%^&\*)_+|!\\~};:/>.,]") ; ohne '=', '#' und '-' sowie 
;;					                      ; verschiedene Sonderzeichen
;;	      (if (looking-at "[\*#]")
		  (progn
;;;
;;; Wenn ja: sind wir auf einem ^*-Pattern (Outline-Überschrift) ?
		    (if (looking-at "^\\*")
			;; Wenn ja: indent gemäß der Anzahl der Sterne
			(setq this-is-a-headline t))
;;; sonst indent-for-tab-command nach dem ersten Leerzeichen
		    (beginning-of-line)
		    (save-excursion
		      (setq save-position-for-short (point))
		      (re-search-forward "[ \t]+" nil t)
		      (beginning-of-line)
		      (if (= save-position-for-short (point))
			  (setq save-for-short t)))
		    (if save-for-short
			(re-search-forward "[ \t]+" nil t) ; --- here
		      (end-of-line)
		      (indent-for-tab-command)))
;;;
;;; Wenn nein, dann {
;;;
;;; Ist die aktuelle Zeile eine Leerzeile ? Wenn nein, dann alle Leerzeichen vornedran loeschen
		(if (not (and (looking-at "$")
			      (= one-up-is-headline 0)))
		    (progn
		      (beginning-of-line)
		      (setq point-save1 (point))
		      (re-search-forward "[^ \t]\\|$\\|^[^ \t]" nil t)

		      (if (not (= point-save1 (point)))
			  (progn
			    (backward-char 1)
			    (kill-line 0)))))))
;;;
;;; Wenn dieses eine Überschriftenzeile ist: setze zusätzliche Tabs ein
;;;
	    (if this-is-a-headline
		(progn
		  (save-excursion
		    (beginning-of-line)
		    (setq point-save1 (point))
		    (while (looking-at "\\*")
		      (forward-char 1))
		    (setq point-save2 (point))
		    ;; suche ersten Blank oder Ende der Zeile
		    (while (not (looking-at "[ \t]\\|$"))
		      (forward-char 1))
		    (if (>= (current-column) tab-width)
			(setq point-save2 (- point-save2 (/ (current-column) tab-width))))
		    (if (looking-at "[ \t]*$")
			(progn
			  (if (= (- (- point-save2 point-save1) 1) 0)
			      (insert-char ?  1)
			    (insert-char ?	(- (- point-save2 point-save1) 1)))
			  (if (not (looking-at "$"))
			      (kill-region (point) (save-excursion (end-of-line)(point)))))))
		  (if (or (save-excursion (beginning-of-line) (looking-at "^\\*+\\w*[ \t\n\r]*$"))
			  (save-excursion (beginning-of-line) (looking-at "[ \t\n\r]*$")))
		      (progn
			(if (= am-i-in-the-same-line_emptyHeadlines (save-excursion (beginning-of-line) (point)))
			    (setq toggle-re-indent-place_emptyHeadlines (not toggle-re-indent-place_emptyHeadlines))
			  (setq toggle-re-indent-place_emptyHeadlines t)
			  (save-excursion (beginning-of-line) (setq am-i-in-the-same-line_emptyHeadlines (point))))
			(if toggle-re-indent-place_emptyHeadlines
			    (end-of-line)
			  (beginning-of-line)
			  (re-search-forward "\\*+" nil t)))))
;;;
;;; Wenn die vorhergehende Zeile eine Überschrift ist, dann indentieren nach dem ersten Space,
;;; sonst normales indent-for-tab-command
	      (if (= one-up-is-headline 0)
		  (progn
;;;		  
;;; Falls die darueberliegende Zeile eine Zeile mit Pfeil (==>) ist (und sie nicht selbst einen
;;; Pfeil am Anfang hat), setze 4 Blanks ein, sonst normales Indent
		    (if (or (= one-up-is-arrowline 0)
			    (looking-at "==>")
			    (looking-at "-"))
			(progn
			  (indent-for-tab-command)
;;; Stelle erst einmal fest, ob die darüberliegende Zeile mit einem Blank anfängt: Voraussetzung für
;;; das Löschen von Zeichen, um alle Pfeile untereinander liegen zu haben
			  (save-excursion
			    (previous-line 1)
			    (backward-char 1)
			    (if (and (looking-at " ")
				     (not (save-excursion (beginning-of-line)(looking-at "  "))))
				(setq one-up-is-intendetforarrow-line 1)))
			  (if (and (looking-at "==>")
				   (= one-up-is-arrowline 0)
				   (= one-up-is-intendetforarrow-line 1))
			      (progn
				(if (not (looking-at "^  \\|^"))
				    (backward-delete-char-untabify 1))
				(if (not (looking-at "^  \\|^"))
				    (backward-delete-char-untabify 1))
				(if (not (looking-at "^  \\|^"))
				    (backward-delete-char-untabify 1))
				(if (not (looking-at "^  \\|^"))
				    (backward-delete-char-untabify 1))))
			  (if (and (looking-at "-")
				   (= one-up-is-arrowline 0)
				   (= one-up-is-intendetforarrow-line 1))
			      (progn
				(if (not (looking-at "^  \\|^"))
				    (backward-delete-char-untabify 1))
				(if (not (looking-at "^  \\|^"))
				    (backward-delete-char-untabify 1)))))
		      (indent-for-tab-command)
		      (if (= one-up-is-arrowline 1)
			  (insert-char ?  4))
		      (if (= one-up-is-arrowline 2)
			  (insert-char ?  2))))
		(previous-line 1)
		(beginning-of-line)
		(setq point-save1 (point))
		(while (looking-at "\\*")
		  (forward-char 1))
		(setq point-save2 (point))
		(next-line 1)
		(beginning-of-line)
		(insert-char ?	(- (- point-save2 point-save1) 1))
		(indent-for-tab-command)
		;; wenn obendrüber eine Headline mit nur einem Stern ist, landet man hier
		;; -- dann einfach zwei Blanks einsetzen
		(if (looking-at "^") ; am Zeilenanfang
		    (if (save-excursion (previous-line 1)
					(beginning-of-line)
					(looking-at "\\(\*[^*]\\)\\|-"))
			(insert "  ")))
		)))))
    (insert-char ?  tab-width))))
;;; }
;;;
;;; aufrufende Funktion ist hier
;;;
(defun re-indent ()
  "Kompliziertes Indent für Outline- oder Project-Mode des Emacs.
-- unter einer Outline-Überschrift: Cursor nach der Anzahl der Sterne setzen
-- unter Freizeichen: an das Ende der Freizeichen (ignoriert dazwischen-
   liegende Newlines)
-- bei nur Newlines: kein Indent
-- Buchstaben am Zeilenanfang: normale Indentierung
-- Zeichen am Zeilenanfang: Indentierung nach dem ersten Leerraum
-- Pfeil (==>) in der da überliegenden Zeile: Einsetzen von vier Blanks
-- Minuszeichen (-) in der darüberliegenden Zeile: Einsetzen von 2 Blanks

-- Toggeln der Cursor-Positionen bei mehrmaligem Tippen zwischen
   Indentierung und Zeilenende
-- Beim ersten Drücken am Zeilenende einer Zeile: nur indentieren
-- automatisches Tabulatorisieren der Leerzeichen (tabify)"
  (interactive)
  ;; wenn am Zeilenende: schau nach, ob sich die Indentierung durch zweimaliges drücken ändert
  (if (not
       (save-excursion
	 (beginning-of-line)
	 (or (looking-at "^\\*+\\w*[ \t\n\r]*$")
	     (looking-at "[ \t\n\r]*$"))))
      (if (looking-at "$")
	  (let ((p (point)))
	    (re-indent-for-tab-command)
	    (end-of-line)
	    ;; wenn nein, dann wieder an den Zeilenanfang, sonst so lassen
	    (if (= p (point))
		(re-indent-for-tab-command)))
	(re-indent-for-tab-command))
    (re-indent-for-tab-command)))

(define-key (current-local-map) "\C-i" 're-indent)


(defun newline-and-re-indent ()
  (interactive)
  (let ((before-change-functions nil)(after-change-functions nil))
;;
;; die nächsten beiden Zeilen ab emacs 19.29
;;
    (if (<= (current-column) (current-fill-column))
	nil
      ;;
      (if ;(and 
	  (not (looking-at "^"))
	  ;    (looking-at "$"))
	  (save-excursion
	    (progn
	      (backward-char 1)
	      (while (looking-at "[ \t]+$")
		(backward-char 1))
	      (if (not (looking-at "^"))
		  (progn
		    (forward-char 1)
		    ;(fixup-whitespace) ; kann man, muss man aber nicht 
		                        ;(die letzen Whitespaces der umgebrochenen Zeile werden verändert)
		    (re-search-backward "[ \t]" nil t)
		    (forward-char 1)
		    (insert-char ?\n 1))
		))))
      (setq toggle-re-indent-place nil)
      (save-excursion
	  (re-indent-for-tab-command)
	  (re-indent-for-tab-command)))))

(make-local-variable 'auto-fill-function)
(setq auto-fill-function 'newline-and-re-indent)
;;
;; Vor emacs 19.29:
;;(define-key (current-local-map) "\C-m" 'newline-and-re-indent)



(defun op-indent-region (&optional rb re)
  "Indentiert eine komplette Region mit re-indent-for-tab-command nach den Regeln
des OP-Werzeugs"
  (interactive)
  (let ((region-beginning (region-beginning))
	(region-end (region-end))
	(lastline (+ 1 (count-lines (point-min) (region-end)))))
    (save-excursion
      (goto-char region-beginning)
      (while (<= (count-lines (point-min) (point))
		 lastline)
	(if (looking-at "^$")
	    (progn
	      (re-indent-for-tab-command)
	      (next-line 1))
	  (end-of-line)
	  (force-indent)
	  (re-indent)
	  (next-line 1))))))

(defun op-softindent-region ()
  "Indentiert eine komplette Region mit re-indent-for-tab-command nach den Regeln
des OP-Werzeugs. Nimmt allerdings Rücksicht auf eingelesene Files, die in der 
Regel nicht den Indentierungen des OP-Modes unterliegen."
  (interactive)
  (let ((region-beginning (region-beginning))(region-end (region-end))
	(lastline (+ 1 (count-lines (point-min) (region-end)))))
    (save-excursion
      (goto-char region-beginning)
      (while (<= (count-lines (point-min) (point))
		 lastline)
	(beginning-of-line)
	(re-indent-for-tab-command)
	(next-line 1)))))


(defun force-indent ()
  (interactive)
  (beginning-of-line)
  (if (looking-at "^$")
      nil
    (let ((before-change-functions nil)(after-change-functions nil)(am-i-in-the-same-line 0))
      (if (re-search-forward "\\*+[A-Za-z0-9]*\\([ \t]\\|$\\)" (save-excursion (end-of-line)(point)) t)
	  (progn
	    (if (looking-at "$") (insert-char ?  1))
	    (backward-char 1)
	    (insert-char ?\n 1)
	    (previous-line 1)
	    (re-indent)
	    (re-search-forward "[\n]\\([ \t]*\\)" (save-excursion (next-line 1)(end-of-line)(- (point) 1)) t)
	    (replace-match "" nil nil)
	    (re-indent))
	(beginning-of-line)
	(insert-char ?  3)
	(insert-char ?\n 1)
	(previous-line 1)
	(re-indent)
	(re-search-forward "[\n]\\([ \t]*\\)" (save-excursion (next-line 1)(end-of-line)(- (point) 1)) t)
	(replace-match "" nil nil)
	(end-of-line)
	(re-indent)
	(end-of-line)))))
(define-key (current-local-map) "\M-;" 'force-indent)




(defun indent-like-last-headline (&optional arg)
  "Indent the same level like the headline before. With ARG, indent one level more"
  (interactive "P") 
  (let ((headline)(before-change-functions nil)(after-change-functions nil))
    (save-excursion
      (outline-back-to-heading)
      (setq heading (buffer-substring (point) (re-search-forward "\\*+" nil t))))
    (beginning-of-line)
    (insert-string heading)
    (if arg
	(progn
	  (if (= (car arg) 4)
	      (progn
		(beginning-of-line)
		(insert-string "*")))
	  (if (> (car arg) 4)
	      (progn
		(beginning-of-line)
		(if (looking-at "\\*\\*")
		    (delete-char 1))))))
    (re-indent)
    (re-indent)
    (re-indent)
    ))
(define-key (current-local-map) "\C-c\C-i" 'indent-like-last-headline)







(defun op-fill-paragraph-region ()
  (interactive)
  (save-excursion
    (let ((tmp-region-end nil))
      (setq tmp-region-end (save-excursion 
			     (goto-char (if (< (region-beginning)(region-end))(region-end)(region-beginning)))
			     (end-of-line)
			     (point)))
      (goto-char (if (< (region-beginning)(region-end))(region-beginning)(region-end)))
      (beginning-of-line)
      (newline)
      (previous-line 1)
      (save-match-data
	(re-indent))
      (beginning-of-line)
      (narrow-to-region (point) (1+ tmp-region-end))
      (fill-paragraph-or-region nil)
      (goto-char (point-min))
      (kill-line)
      (goto-char (point-max))
      (delete-char -1)
      (widen)
)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;
;;; schalte die auto-fill-function aus für das newline, da die beiden Funktionen sich überschneiden
(define-key (current-local-map) "\C-m" '(lambda()(interactive)(let((auto-fill-function nil))(newline))))

(if xemacsp
    (define-key (current-local-map) [(meta space)] 'fixup-whitespace)
  (define-key (current-local-map) [M-SPC] 'fixup-whitespace))

(define-key (current-local-map) "\M-q" 'op-fill-paragraph-region)

(define-key (current-local-map) "\M-\C-i" '(lambda()(interactive)(insert-char ?\C-i 1)))

(provide 'op-indent)



;; zur Kontrolle
(message "2")

;
; Hideif-Mode in abgeänderter Weise
;
;; laden des geänderten op-hideif (= hideif.el)- Modes
;;;;;;;includeforelc!
;;;;;;(load "op-hideif")

;;; hide-ifdef-mode.el --- hides selected code within ifdef.

;; Copyright (C) 1988, 1994 Free Software Foundation, Inc.

;; Author: Dan LaLiberte <liberte@a.cs.uiuc.edu>
;; Maintainer: FSF
;; Keywords: c, outlines

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; To initialize, toggle the hide-ifdef minor mode with
;;
;; M-x hide-ifdef-mode
;;
;; This will set up key bindings and call hide-ifdef-mode-hook if it
;; has a value.  To explicitly hide ifdefs using a buffer-local
;; define list (default empty), type
;;
;; M-x hide-ifdefs  or C-c @ h
;;
;; Hide-ifdef suppresses the display of code that the preprocessor wouldn't
;; pass through.  The support of constant expressions in #if lines is 
;; limited to identifiers, parens, and the operators: &&, ||, !, and
;; "defined".  Please extend this.
;;
;; The hidden code is marked by ellipses (...).  Be
;; cautious when editing near ellipses, since the hidden text is
;; still in the buffer, and you can move the point into it and modify
;; text unawares.  If you don't want to see the ellipses, set 
;; selective-display-ellipses to nil.  But this can be dangerous.
;; You can make your buffer read-only while hide-ifdef-hiding by setting
;; hide-ifdef-read-only to a non-nil value.  You can toggle this 
;; variable with hide-ifdef-toggle-read-only (C-c @ C-q).
;;
;; You can undo the effect of hide-ifdefs by typing
;;
;; M-x show-ifdefs  or C-c @ s
;;
;; Use M-x hide-ifdef-define (C-c @ d) to define a symbol.
;; Use M-x hide-ifdef-undef (C-c @ u) to undefine a symbol.
;;
;; If you define or undefine a symbol while hide-ifdef-mode is in effect,
;; the display will be updated.  Only the define list for the current
;; buffer will be affected.  You can save changes to the local define
;; list with hide-ifdef-set-define-alist.  This adds entries 
;; to hide-ifdef-define-alist.
;;
;; If you have defined a hide-ifdef-mode-hook, you can set
;; up a list of symbols that may be used by hide-ifdefs as in the
;; following example:
;;
;; (setq hide-ifdef-mode-hook
;;      '(lambda ()
;;	 (if (not hide-ifdef-define-alist)
;;	     (setq hide-ifdef-define-alist
;;		  '((list1 ONE TWO)
;;		    (list2 TWO THREE)
;;		    )))
;;	 (hide-ifdef-use-define-alist 'list2) ; use list2 by default
;;	 ))
;;
;; You can call hide-ifdef-use-define-alist (C-c @ u) at any time to specify
;; another list to use.
;;
;; To cause ifdefs to be hidden as soon as hide-ifdef-mode is called,
;; set hide-ifdef-initially to non-nil.
;;
;; If you set hide-ifdef-lines to t, hide-ifdefs hides all the #ifdef lines.
;; In the absence of highlighting, that might be a bad idea.  If you set
;; hide-ifdef-lines to nil (the default), the surrounding preprocessor
;; lines will be displayed.  That can be confusing in its own
;; right.  Other variations on display are possible, but not much
;; better.
;;
;; You can explicitly hide or show individual ifdef blocks irrespective
;; of the define list by using hide-ifdef-block and show-ifdef-block.
;;
;; You can move the point between ifdefs with forward-ifdef, backward-ifdef,
;; up-ifdef, down-ifdef, next-ifdef, and previous-ifdef.
;;
;; If you have minor-mode-alist in your mode line (the default) two labels
;; may appear.  "Ifdef" will appear when hide-ifdef-mode is active.  "Hiding"
;; will appear when text may be hidden ("hide-ifdef-hiding" is non-nil).
;;
;; Written by Brian Marick, at Gould, Computer Systems Division, Urbana IL.
;; Extensively modified by Daniel LaLiberte (while at Gould).
;;
;; You may freely modify and distribute this, but keep a record
;; of modifications and send comments to:
;; 	 liberte@a.cs.uiuc.edu  or  ihnp4!uiucdcs!liberte
;; I will continue to upgrade hide-ifdef-mode
;; with your contributions.

;;; Code:

(require 'cc-mode)

(defgroup hide-ifdef nil
  "Hide selected code within `ifdef'."
  :group 'c)

(defvar hide-ifdef-mode-submap nil
  "Keymap used with Hide-Ifdef mode.")

(defvar hide-ifdef-mode-map nil
  "Keymap used with Hide-Ifdef mode.")

(defconst hide-ifdef-mode-prefix-key "\C-c@"
  "Prefix key for all Hide-Ifdef mode commands.")

;; Set up the submap that goes after the prefix key.
(if hide-ifdef-mode-submap
    ()				; Don't redefine it.
  (if xemacsp
      (setq hide-ifdef-mode-submap (make-sparse-keymap))
    (setq hide-ifdef-mode-submap (make-keymap)))
  (define-key hide-ifdef-mode-submap "d" 'hide-ifdef-define)
  (define-key hide-ifdef-mode-submap "u" 'hide-ifdef-undef)
  (define-key hide-ifdef-mode-submap "D" 'hide-ifdef-set-define-alist)
  (define-key hide-ifdef-mode-submap "U" 'hide-ifdef-use-define-alist)

  (define-key hide-ifdef-mode-submap "h" 'hide-ifdefs)
  (define-key hide-ifdef-mode-submap "s" 'show-ifdefs)
  (define-key hide-ifdef-mode-submap "\C-d" 'hide-ifdef-block)
  (define-key hide-ifdef-mode-submap "\C-s" 'show-ifdef-block)

  (define-key hide-ifdef-mode-submap "\C-q" 'hide-ifdef-toggle-read-only)
  ;;
  ;; SMW
  ;;
  (if gnuemacsp
      (let ((where (where-is-internal 'toggle-read-only '(keymap) t)))
	(if where
	    (define-key hide-ifdef-mode-submap
	      where
	      'hide-ifdef-toggle-outside-read-only))))
  )

;; Set up the mode's main map, which leads via the prefix key to the submap.
(if hide-ifdef-mode-map
    ()
  (setq hide-ifdef-mode-map (make-sparse-keymap))
  (define-key hide-ifdef-mode-map hide-ifdef-mode-prefix-key
    hide-ifdef-mode-submap))

(defvar hide-ifdef-mode nil
  "Non-nil when hide-ifdef-mode is activated.")

(defvar hide-ifdef-hiding nil
  "Non-nil when text may be hidden.")

;; Arrange to use the mode's map when the mode is enabled.
(or (assq 'hide-ifdef-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
          (cons (cons 'hide-ifdef-mode hide-ifdef-mode-map)
                minor-mode-map-alist)))

(or (assq 'hide-ifdef-hiding minor-mode-alist)
    (setq minor-mode-alist
          (cons '(hide-ifdef-hiding " Hiding")
                minor-mode-alist)))

(or (assq 'hide-ifdef-mode minor-mode-alist)
    (setq minor-mode-alist
          (cons '(hide-ifdef-mode " Ifdef")
                minor-mode-alist)))

;; fix c-mode syntax table so we can recognize whole symbols.
(defvar hide-ifdef-syntax-table
  (copy-syntax-table c-mode-syntax-table)
  "Syntax table used for tokenizing #if expressions.")

(modify-syntax-entry ?_ "w" hide-ifdef-syntax-table)
(modify-syntax-entry ?& "." hide-ifdef-syntax-table)
(modify-syntax-entry ?\| "." hide-ifdef-syntax-table)

;;;###autoload
(defun hide-ifdef-mode (arg)
  "Toggle Hide-Ifdef mode.  This is a minor mode, albeit a large one.
With ARG, turn Hide-Ifdef mode on if arg is positive, off otherwise.
In Hide-Ifdef mode, code within #ifdef constructs that the C preprocessor
would eliminate may be hidden from view.  Several variables affect
how the hiding is done:

hide-ifdef-env
	An association list of defined and undefined symbols for the
	current buffer.  Initially, the global value of `hide-ifdef-env'
	is used.

hide-ifdef-define-alist
	An association list of defined symbol lists.  
        Use `hide-ifdef-set-define-alist' to save the current `hide-ifdef-env'
        and `hide-ifdef-use-define-alist' to set the current `hide-ifdef-env'
        from one of the lists in `hide-ifdef-define-alist'.

hide-ifdef-lines
	Set to non-nil to not show #if, #ifdef, #ifndef, #else, and
	#endif lines when hiding.

hide-ifdef-initially
	Indicates whether `hide-ifdefs' should be called when Hide-Ifdef mode
	is activated.

hide-ifdef-read-only
	Set to non-nil if you want to make buffers read only while hiding.
	After `show-ifdefs', read-only status is restored to previous value.

\\{hide-ifdef-mode-map}"

  (interactive "P")
  (make-local-variable 'hide-ifdef-mode)
  (setq hide-ifdef-mode
	(if (null arg)
	    (not hide-ifdef-mode)
	  (> (prefix-numeric-value arg) 0)))
  
  (force-mode-line-update)

  (if hide-ifdef-mode
      (progn
	; inherit global values
	(make-local-variable 'hide-ifdef-env)
	(setq hide-ifdef-env (default-value 'hide-ifdef-env))

	(make-local-variable 'hide-ifdef-hiding)
	(setq hide-ifdef-hiding (default-value 'hide-ifdef-hiding))

	(make-local-variable 'hif-outside-read-only)
	(setq hif-outside-read-only buffer-read-only)

	(run-hooks 'hide-ifdef-mode-hook)

	(if hide-ifdef-initially
	    (hide-ifdefs)
	  (show-ifdefs))
	(message "Enter Hide-Ifdef mode")
	)
     ; else end hide-ifdef-mode
    (if hide-ifdef-hiding
	(show-ifdefs))
    (message "Exit Hide-Ifdef mode")
    ))
  

;; from outline.el with docstring fixed.
(defun hif-outline-flag-region (from to flag)
  "Hides or shows lines from FROM to TO, according to FLAG.
If FLAG is \\n (newline character) then text is shown, while if FLAG is \\^M
\(control-M) the text is hidden."
  (let ((modp (buffer-modified-p)))
    (unwind-protect (progn
		      (subst-char-in-region from to
			      (if (= flag ?\n) ?\^M ?\n)
			      flag t) )
      (set-buffer-modified-p modp))
    ))

(defun hif-show-all ()
  "Show all of the text in the current buffer."
  (interactive)
  (hif-outline-flag-region (point-min) (point-max) ?\n))

;; By putting this on after-revert-hook, we arrange that it only
;; does anything when revert-buffer avoids turning off the mode.
;; (That can happen in VC.)
(defun hif-before-revert-function ()
  (and hide-ifdef-mode hide-ifdef-hiding
       (hide-ifdefs t)))
(add-hook 'after-revert-hook 'hif-before-revert-function)

(defun hide-ifdef-region (start end)
  "START is the start of a #if or #else form.  END is the ending part.
Everything including these lines is made invisible."
  (hif-outline-flag-region start end ?\^M)
  )

(defun hif-show-ifdef-region (start end)
  "Everything between START and END is made visible."
  (hif-outline-flag-region start end ?\n)
  )



;===%%SF%% evaluation (Start)  ===

;; It is not useful to set this to anything but `eval'.
;; In fact, the variable might as well be eliminated.
(defvar hide-ifdef-evaluator 'eval
  "The function to use to evaluate a form.
The evaluator is given a canonical form and returns t if text under
that form should be displayed.")

(defvar hif-undefined-symbol nil
  "...is by default considered to be false.")

(defvar hide-ifdef-env nil
  "An alist of defined symbols and their values.")


(defun hif-set-var (var value)
  "Prepend (var value) pair to hide-ifdef-env."
  (setq hide-ifdef-env (cons (cons var value) hide-ifdef-env)))


(defun hif-lookup (var)
;  (message "hif-lookup %s" var)
  (let ((val (assoc var hide-ifdef-env)))
    (if val
	(cdr val)
      hif-undefined-symbol)))

(defun hif-defined (var)
  (hif-lookup var)
  ; when #if expressions are fully supported, defined result should be 1
  ;  (if (assoc var  hide-ifdef-env)
  ;      1
  ;    nil)
)


;===%%SF%% evaluation (End)  ===



;===%%SF%% parsing (Start)  ===
;;;  The code that understands what ifs and ifdef in files look like.

(defconst hif-cpp-prefix "\\(^\\|\r\\)[ \t]*###[ \t]*")
(make-variable-buffer-local 'hif-cpp-prefix)
(defconst hif-ifndef-regexp (concat hif-cpp-prefix "nicht[ \t]+"))
(make-variable-buffer-local 'hif-ifndef-regexp)
(defconst hif-ifx-regexp (concat hif-cpp-prefix "\\(nicht\\)*[ \t]+"))
(make-variable-buffer-local 'hif-ifx-regexp)
(defconst hif-else-regexp (concat hif-cpp-prefix "dagegen[ \t]+jedoch[ \t]+nicht"))
(make-variable-buffer-local 'hif-else-regexp)
(defconst hif-endif-regexp (concat hif-cpp-prefix "bis[ \t]+hierher"))
(make-variable-buffer-local 'hif-endif-regexp)
(defconst hif-ifx-else-endif-regexp (concat hif-ifx-regexp "\\|" hif-else-regexp "\\|" hif-endif-regexp))
(make-variable-buffer-local 'hif-ifx-else-endif-regexp)


(defun hif-infix-to-prefix (token-list)
  "Convert list of tokens in infix into prefix list"
;  (message "hif-infix-to-prefix: %s" token-list)
  (if (= 1 (length token-list))
      (` (hif-lookup (quote (, (car token-list)))))
    (hif-parse-if-exp token-list))
  )

; pattern to match initial identifier, !, &&, ||, (, or ).
; Added ==, + and -: garyo@avs.com 8/9/94
(defconst hif-token-regexp "^\\(&&\\|||\\|[!=]=\\|!\\|[()+-]\\|[<>]=?\\|\\w+\\)")
(defconst hif-end-of-comment "\\*/")


(defun hif-tokenize (expr-string)
  "Separate string into a list of tokens"
  (let ((token-list nil)
	(expr-start 0)
	(expr-length (length expr-string))
	(current-syntax-table (syntax-table)))
    (unwind-protect
	(progn
	  (set-syntax-table hide-ifdef-syntax-table)
	  (while (< expr-start expr-length) 
;	    (message "expr-start = %d" expr-start) (sit-for 1)
	    (cond
	     ((string-match "^[ \t]+" expr-string expr-start)
	      ;; skip whitespace
	      (setq expr-start (match-end 0))
	      ;; stick newline in string so ^ matches on the next string-match
	      (aset expr-string (1- expr-start) ?\n))

	     ((string-match "^/\\*" expr-string expr-start)
	      (setq expr-start (match-end 0))
	      (aset expr-string (1- expr-start) ?\n)
	      (or
	       (string-match hif-end-of-comment
			     expr-string expr-start) ; eat comment
	       (string-match "$" expr-string expr-start)) ; multi-line comment
	      (setq expr-start (match-end 0))
	      (aset expr-string (1- expr-start) ?\n))

	     ((string-match "^//" expr-string expr-start)
	      (string-match "$" expr-string expr-start)
	      (setq expr-start (match-end 0)))

	     ((string-match hif-token-regexp expr-string expr-start)
	      (let ((token (substring expr-string expr-start (match-end 0))))
		(setq expr-start (match-end 0))
		(aset expr-string (1- expr-start) ?\n)
;		(message "token: %s" token) (sit-for 1)
		(setq token-list
		      (cons
		       (cond
			((string-equal token "||") 'or)
			((string-equal token "&&") 'and)
			((string-equal token "==") 'equal)
			((string-equal token "!=") 'hif-notequal)
			((string-equal token "!")  'not)
			((string-equal token "defined") 'hif-defined)
			((string-equal token "(") 'lparen)
			((string-equal token ")") 'rparen)
			((string-equal token ">") 'hif-greater)
			((string-equal token "<") 'hif-less)
			((string-equal token ">=") 'hif-greater-equal)
			((string-equal token "<=") 'hif-less-equal)
			((string-equal token "+") 'hif-plus)
			((string-equal token "-") 'hif-minus)
			(t (intern token)))
		       token-list))))
	     (t (error "Bad #if expression: %s" expr-string)))))
      (set-syntax-table current-syntax-table))
    (nreverse token-list)))

;;;-----------------------------------------------------------------
;;; Translate C preprocessor #if expressions using recursive descent.
;;; This parser is limited to the operators &&, ||, !, and "defined".
;;; Added ==, !=, +, and -.  Gary Oberbrunner, garyo@avs.com, 8/9/94

(defun hif-parse-if-exp (token-list)
  "Parse the TOKEN-LIST.  Return translated list in prefix form."
  (hif-nexttoken)
  (prog1
      (hif-expr)
    (if token ; is there still a token?
	(error "Error: unexpected token: %s" token))))

(defun hif-nexttoken ()
  "Pop the next token from token-list into the let variable \"token\"."
  (setq token (car token-list))
  (setq token-list (cdr token-list))
  token)

(defun hif-expr ()
  "Parse an expression as found in #if.
       expr : term | expr '||' term."
  (let ((result (hif-term)))
    (while (eq  token 'or)
      (hif-nexttoken)
      (setq result (list 'or result (hif-term))))
  result))

(defun hif-term ()
  "Parse a term : eq-expr | term '&&' eq-expr."
  (let ((result (hif-eq-expr)))
    (while (eq token 'and)
      (hif-nexttoken)
      (setq result (list 'and result (hif-eq-expr))))
    result))

(defun hif-eq-expr ()
  "Parse an eq-expr : math | eq-expr `=='|`!='|`<'|`>'|`>='|`<=' math."
  (let ((result (hif-math))
	(eq-token nil))
    (while (memq token '(equal hif-notequal hif-greater hif-less
			       hif-greater-equal hif-less-equal))
      (setq eq-token token)
      (hif-nexttoken)
      (setq result (list eq-token result (hif-math))))
    result))

(defun hif-math ()
  "Parse an expression with + or - and simpler things.
       math : factor | math '+|-' factor."
  (let ((result (hif-factor))
	(math-op nil))
    (while (or (eq  token 'hif-plus) (eq token 'hif-minus))
      (setq math-op token)
      (hif-nexttoken)
      (setq result (list math-op result (hif-factor))))
  result))
  
(defun hif-factor ()
  "Parse a factor: '!' factor | '(' expr ')' | 'defined(' id ')' | id."
  (cond
    ((eq token 'not)
     (hif-nexttoken)
     (list 'not (hif-factor)))

    ((eq token 'lparen)
     (hif-nexttoken)
     (let ((result (hif-expr)))
       (if (not (eq token 'rparen))
	   (error "Bad token in parenthesized expression: %s" token)
	 (hif-nexttoken)
	 result)))

    ((eq token 'hif-defined)
     (hif-nexttoken)
     (if (not (eq token 'lparen))
	 (error "Error: expected \"(\" after \"defined\""))
     (hif-nexttoken)
     (let ((ident token))
       (if (memq token '(or and not hif-defined lparen rparen))
	   (error "Error: unexpected token: %s" token))
       (hif-nexttoken)
       (if (not (eq token 'rparen))
	   (error "Error: expected \")\" after identifier"))
       (hif-nexttoken)
       (` (hif-defined (quote (, ident))))
       ))

    (t ; identifier
      (let ((ident token))
	(if (memq ident '(or and))
	    (error "Error: missing identifier"))
	(hif-nexttoken)
	(` (hif-lookup (quote (, ident))))
	))
    ))

(defun hif-mathify (val)
  "Treat VAL as a number: if it's t or nil, use 1 or 0."
  (cond ((eq val t)
	 1)
	((null val)
	 0)
	(t val)))

(defun hif-plus (a b)
  "Like ordinary plus but treat t and nil as 1 and 0."
  (+ (hif-mathify a) (hif-mathify b)))
(defun hif-minus (a b)
  "Like ordinary minus but treat t and nil as 1 and 0."
  (- (hif-mathify a) (hif-mathify b)))
(defun hif-notequal (a b)
  "Like (not (equal A B)) but as one symbol."
  (not (equal a b)))
(defun hif-greater (a b)
  "Simple comparison."
  (> (hif-mathify a) (hif-mathify b)))
(defun hif-less (a b)
  "Simple comparison."
  (< (hif-mathify a) (hif-mathify b)))
(defun hif-greater-equal (a b)
  "Simple comparison."
  (>= (hif-mathify a) (hif-mathify b)))
(defun hif-less-equal (a b)
  "Simple comparison."
  (<= (hif-mathify a) (hif-mathify b)))
;;;----------- end of parser -----------------------


(defun hif-canonicalize ()
  "When at beginning of #ifX, returns a Lisp expression for its condition."
  (save-excursion
    (let ((negate (looking-at hif-ifndef-regexp)))
      (re-search-forward hif-ifx-regexp)
      (let* ((expr-string
	      (buffer-substring (point)
				(progn (skip-chars-forward "^\n\r") (point))))
	     (expr (hif-infix-to-prefix (hif-tokenize expr-string))))
;	(message "hif-canonicalized: %s" expr)
	(if negate
	    (list 'not expr)
	  expr)))))


(defun hif-find-any-ifX ()
  "Move to next #if..., or #ifndef, at point or after."
;  (message "find ifX at %d" (point))
  (prog1
      (re-search-forward hif-ifx-regexp (point-max) t)
    (beginning-of-line)))


(defun hif-find-next-relevant ()
  "Move to next #if..., #else, or #endif, after the current line."
;  (message "hif-find-next-relevant at %d" (point))
  (end-of-line)
  ; avoid infinite recursion by only going to beginning of line if match found
  (if (re-search-forward hif-ifx-else-endif-regexp (point-max) t)
      (beginning-of-line)))

(defun hif-find-previous-relevant ()
  "Move to previous #if..., #else, or #endif, before the current line."
;  (message "hif-find-previous-relevant at %d" (point))
  (beginning-of-line)
  ; avoid infinite recursion by only going to beginning of line if match found
  (if (re-search-backward hif-ifx-else-endif-regexp (point-min) t)
     (beginning-of-line)))


(defun hif-looking-at-ifX ()		;; Should eventually see #if
  (looking-at hif-ifx-regexp))
(defun hif-looking-at-endif ()
  (looking-at hif-endif-regexp))
(defun hif-looking-at-else ()
  (looking-at hif-else-regexp))



(defun hif-ifdef-to-endif ()
  "If positioned at #ifX or #else form, skip to corresponding #endif."
;  (message "hif-ifdef-to-endif at %d" (point)) (sit-for 1)
  (hif-find-next-relevant)
  (cond ((hif-looking-at-ifX)
	 (hif-ifdef-to-endif) ; find endif of nested if
	 (hif-ifdef-to-endif)) ; find outer endif or else
	((hif-looking-at-else)
	 (hif-ifdef-to-endif)) ; find endif following else
	((hif-looking-at-endif)
	 'done)
	(t
	 (error "Mismatched #ifdef #endif pair"))))


(defun hif-endif-to-ifdef ()
  "If positioned at #endif form, skip backward to corresponding #ifX."
;  (message "hif-endif-to-ifdef at %d" (point))
  (let ((start (point)))
    (hif-find-previous-relevant)
    (if (= start (point))
	(error "Mismatched #ifdef #endif pair")))
  (cond ((hif-looking-at-endif)
	 (hif-endif-to-ifdef) ; find beginning of nested if
	 (hif-endif-to-ifdef)) ; find beginning of outer if or else
	((hif-looking-at-else)
	 (hif-endif-to-ifdef))
	((hif-looking-at-ifX)
	 'done)
	(t)))			; never gets here


(defun forward-ifdef (&optional arg)
  "Move point to beginning of line of the next ifdef-endif.
With argument, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (if (< arg 0)
      (backward-ifdef (- arg)))
  (while (< 0 arg)
    (setq arg (- arg))
    (let ((start (point)))
      (if (not (hif-looking-at-ifX))
	  (hif-find-next-relevant))
      (if (hif-looking-at-ifX)
	  (hif-ifdef-to-endif)
	(goto-char start)
	(error "No following #ifdef")
	))))


(defun backward-ifdef (&optional arg)
  "Move point to beginning of the previous ifdef-endif.
With argument, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (if (< arg 0)
      (forward-ifdef (- arg)))
  (while (< 0 arg)
    (setq arg (1- arg))
    (beginning-of-line)
    (let ((start (point)))
      (if (not (hif-looking-at-endif))
	  (hif-find-previous-relevant))
      (if (hif-looking-at-endif)
	  (hif-endif-to-ifdef)
	(goto-char start)
	(error "No previous #ifdef")))))


(defun down-ifdef ()
  "Move point to beginning of nested ifdef or else-part."
    (interactive)
    (let ((start (point)))
      (hif-find-next-relevant)
      (if (or (hif-looking-at-ifX) (hif-looking-at-else))
	  ()
	(goto-char start)
	(error "No following #ifdef"))))


(defun up-ifdef ()
  "Move point to beginning of enclosing ifdef or else-part."
  (interactive)
  (beginning-of-line)
  (let ((start (point)))
    (if (not (hif-looking-at-endif))
	(hif-find-previous-relevant))
    (if (hif-looking-at-endif)
	(hif-endif-to-ifdef))
      (if (= start (point))
	  (error "No previous #ifdef"))))

(defun next-ifdef (&optional arg)
  "Move to the beginning of the next #ifX, #else, or #endif.
With argument, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (if (< arg 0)
      (previous-ifdef (- arg)))
  (while (< 0 arg)
    (setq arg (1- arg))
    (hif-find-next-relevant)
    (if (eolp)
	(progn
	  (beginning-of-line)
	  (error "No following #ifdefs, #elses, or #endifs")))))

(defun previous-ifdef (&optional arg)
  "Move to the beginning of the previous #ifX, #else, or #endif.
With argument, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (if (< arg 0)
      (next-ifdef (- arg)))
  (while (< 0 arg)
    (setq arg (1- arg))
    (let ((start (point)))
      (hif-find-previous-relevant)
      (if (= start (point))
	  (error "No previous #ifdefs, #elses, or #endifs")
	))))


;===%%SF%% parsing (End)  ===


;===%%SF%% hide-ifdef-hiding (Start)  ===


;;; A range is a structure with four components:
;;; ELSE-P	True if there was an else clause for the ifdef.
;;; START	The start of the range. (beginning of line)
;;; ELSE	The else marker (beginning of line)
;;;			Only valid if ELSE-P is true.
;;; END		The end of the range.  (beginning of line)

(defun hif-make-range (else-p start end &optional else)
  (list else-p start else end))

(defun hif-range-else-p (range)  (elt range 0))
(defun hif-range-start (range) (elt range 1))
(defun hif-range-else (range) (elt range 2))
(defun hif-range-end (range) (elt range 3))



;;; Find-Range
;;; The workhorse, it delimits the #if region.  Reasonably simple:
;;; Skip until an #else or #endif is found, remembering positions.  If
;;; an #else was found, skip some more, looking for the true #endif.

(defun hif-find-range ()
  "Returns a Range structure describing the current #if region.
Point is left unchanged."
;  (message "hif-find-range at %d" (point))
  (save-excursion
    (beginning-of-line)
    (let ((start (point))
	  (else-p nil)
	  (else nil)
	  (end nil))
      ;; Part one.  Look for either #endif or #else.
      ;; This loop-and-a-half dedicated to E. Dijkstra.
      (hif-find-next-relevant)
      (while (hif-looking-at-ifX)		; Skip nested ifdef
	(hif-ifdef-to-endif)
	(hif-find-next-relevant))
      ;; Found either a #else or an #endif.
      (cond ((hif-looking-at-else)
	     (setq else-p t)
	     (setq else (point)))
	    (t
	     (setq end (point)) ; (save-excursion (end-of-line) (point))
	     ))
      ;; If found #else, look for #endif.
      (if else-p
	  (progn
	    (hif-find-next-relevant)
	    (while (hif-looking-at-ifX)	; Skip nested ifdef
	      (hif-ifdef-to-endif)
	      (hif-find-next-relevant))
	    (if (hif-looking-at-else)
		(error "Found two elses in a row?  Broken!"))
	    (setq end (point))  ; (save-excursion (end-of-line) (point))
	    ))
      (hif-make-range else-p start end else))))

	  
;;; A bit slimy.
;;; NOTE:  If there's an #ifdef at the beginning of the file, we can't
;;; hide it.  There's no previous newline to replace.  If we added
;;; one, we'd throw off all the counts.  Feh.

(defun hif-hide-line (point)
  "Hide the line containing point.  Does nothing if `hide-ifdef-lines' is nil."
  (if hide-ifdef-lines
      (save-excursion
	(goto-char point)
	(let ((modp (buffer-modified-p)))
	  (unwind-protect
	      (progn
		(beginning-of-line)
		(if (not (= (point) 1))
		    (hide-ifdef-region (1- (point)) (point))))
	    (set-buffer-modified-p modp))
	  ))
    ))
		  

;;;  Hif-Possibly-Hide
;;;  There are four cases.  The #ifX expression is "taken" if it
;;;  the hide-ifdef-evaluator returns T.  Presumably, this means the code
;;;  inside the #ifdef would be included when the program was
;;;  compiled.  
;;;
;;;  Case 1:  #ifX taken, and there's an #else.
;;;	The #else part must be hidden.  The #if (then) part must be
;;;	processed for nested #ifX's.
;;;  Case 2:  #ifX taken, and there's no #else.
;;;	The #if part must be processed for nested #ifX's.
;;;  Case 3:  #ifX not taken, and there's an #else.
;;;	The #if part must be hidden.  The #else part must be processed
;;;	for nested #ifs.
;;;  Case 4:  #ifX not taken, and there's no #else.
;;;	The #ifX part must be hidden.
;;;
;;;  Further processing is done by narrowing to the relevant region
;;;  and just recursively calling hide-ifdef-guts.
;;;
;;;  When hif-possibly-hide returns, point is at the end of the
;;;  possibly-hidden range.

(defun hif-recurse-on (start end)
  "Call `hide-ifdef-guts' after narrowing to end of START line and END line."
  (save-excursion
    (save-restriction
      (goto-char start)
      (end-of-line)
      (narrow-to-region (point) end)
      (hide-ifdef-guts))))

(defun hif-possibly-hide ()
  "Called at #ifX expression, this hides those parts that should be hidden.
It uses the judgement of `hide-ifdef-evaluator'."
;  (message "hif-possibly-hide") (sit-for 1)
    (let ((test (hif-canonicalize))
	  (range (hif-find-range)))
;      (message "test = %s" test) (sit-for 1)
      
      (hif-hide-line (hif-range-end range))
      (if (funcall hide-ifdef-evaluator test)
	  (cond ((hif-range-else-p range) ; case 1
		 (hif-hide-line (hif-range-else range))
		 (hide-ifdef-region (hif-range-else range) 
				    (1- (hif-range-end range)))
		 (hif-recurse-on (hif-range-start range)
				 (hif-range-else range)))
		(t ; case 2
		 (hif-recurse-on (hif-range-start range)
				 (hif-range-end range))))
	(cond ((hif-range-else-p range) ; case 3
	       (hif-hide-line (hif-range-else range))
	       (hide-ifdef-region (hif-range-start range)
				  (1- (hif-range-else range)))
	       (hif-recurse-on (hif-range-else range)
			       (hif-range-end range)))
	      (t ; case 4
	       (hide-ifdef-region (point)
				  (1- (hif-range-end range))))
	      ))
      (hif-hide-line (hif-range-start range))	; Always hide start.
      (goto-char (hif-range-end range))
      (end-of-line)
      ))



(defun hide-ifdef-guts ()
  "Does most of the work of `hide-ifdefs'.
It does not do the work that's pointless to redo on a recursive entry."
;  (message "hide-ifdef-guts")
  (save-excursion
    (goto-char (point-min))
    (while (hif-find-any-ifX)
      (hif-possibly-hide))))

;===%%SF%% hide-ifdef-hiding (End)  ===


;===%%SF%% exports (Start)  ===

;;;###autoload
(defcustom hide-ifdef-initially nil
  "*Non-nil means call `hide-ifdefs' when Hide-Ifdef mode is first activated."
  :type 'boolean
  :group 'hide-ifdef)

;;;###autoload
(defcustom hide-ifdef-read-only nil
  "*Set to non-nil if you want buffer to be read-only while hiding text."
  :type 'boolean
  :group 'hide-ifdef)

(defvar hif-outside-read-only nil
  "Internal variable.  Saves the value of `buffer-read-only' while hiding.")

;;;###autoload
(defcustom hide-ifdef-lines nil
  "*Non-nil means hide the #ifX, #else, and #endif lines."
  :type 'boolean
  :group 'hide-ifdef)

(defun hide-ifdef-toggle-read-only ()
  "Toggle hide-ifdef-read-only."
  (interactive)
  (setq hide-ifdef-read-only (not hide-ifdef-read-only))
  (message "Hide-Read-Only %s"
	   (if hide-ifdef-read-only "ON" "OFF"))
  (if hide-ifdef-hiding
      (setq buffer-read-only (or hide-ifdef-read-only hif-outside-read-only)))
  (force-mode-line-update))

(defun hide-ifdef-toggle-outside-read-only ()
  "Replacement for `toggle-read-only' within Hide-Ifdef mode."
  (interactive)
  (setq hif-outside-read-only (not hif-outside-read-only))
  (message "Read only %s"
	   (if hif-outside-read-only "ON" "OFF"))
  (setq buffer-read-only
	(or (and hide-ifdef-hiding hide-ifdef-read-only)
	    hif-outside-read-only)
	)
  (force-mode-line-update))

      
(defun hide-ifdef-define (var)
  "Define a VAR so that #ifdef VAR would be included."
  (interactive "SDefine what? ")
  (hif-set-var var 1)
  (if hide-ifdef-hiding (hide-ifdefs)))

(defun hide-ifdef-undef (var)
  "Undefine a VAR so that #ifdef VAR would not be included."
  (interactive "SUndefine what? ")
  (hif-set-var var nil)
  (if hide-ifdef-hiding (hide-ifdefs)))


(defun hide-ifdefs (&optional nomsg)
  "Hide the contents of some #ifdefs.  
Assume that defined symbols have been added to `hide-ifdef-env'.  
The text hidden is the text that would not be included by the C
preprocessor if it were given the file with those symbols defined.

Turn off hiding by calling `show-ifdefs'."

  (interactive)
  (message "Hiding...")
  (setq hif-outside-read-only buffer-read-only)
  (if (not hide-ifdef-mode)
      (hide-ifdef-mode 1)) ; turn on hide-ifdef-mode
  (if hide-ifdef-hiding
      (show-ifdefs))			; Otherwise, deep confusion.
  (let ((inhibit-read-only t))
    (setq selective-display t)
    (setq hide-ifdef-hiding t)
    (hide-ifdef-guts))
  (setq buffer-read-only (or hide-ifdef-read-only hif-outside-read-only))
  (or nomsg
      (message "Hiding done")))


(defun show-ifdefs ()
  "Cancel the effects of `hide-ifdef': show the contents of all #ifdefs."
  (interactive)
  (setq buffer-read-only hif-outside-read-only)
  (setq selective-display nil)	; defaults
  (let ((inhibit-read-only t))
    (hif-show-all))
  (setq hide-ifdef-hiding nil))


(defun hif-find-ifdef-block ()
  "Utility for hide and show `ifdef-block'.
Set top and bottom of ifdef block."
  (let (max-bottom)
  (save-excursion
    (beginning-of-line)
    (if (not (or (hif-looking-at-else) (hif-looking-at-ifX)))
	(up-ifdef))
    (setq top (point))
    (hif-ifdef-to-endif)
    (setq max-bottom (1- (point))))
  (save-excursion
    (beginning-of-line)
    (if (not (hif-looking-at-endif))
	(hif-find-next-relevant))
    (while (hif-looking-at-ifX)
      (hif-ifdef-to-endif)
      (hif-find-next-relevant))
    (setq bottom (min max-bottom (1- (point))))))
  )


(defun hide-ifdef-block ()
  "Hide the ifdef block (true or false part) enclosing or before the cursor."
  (interactive)
  (if (not hide-ifdef-mode)
      (hide-ifdef-mode 1))
  (setq selective-display t)
  (let (top bottom (inhibit-read-only t))
    (hif-find-ifdef-block) ; set top and bottom - dynamic scoping
    (hide-ifdef-region top bottom)
    (if hide-ifdef-lines
	(progn
	  (hif-hide-line top)
	  (hif-hide-line (1+ bottom))))
    (setq hide-ifdef-hiding t))
  (setq buffer-read-only (or hide-ifdef-read-only hif-outside-read-only)))


(defun show-ifdef-block ()
  "Show the ifdef block (true or false part) enclosing or before the cursor."
  (interactive)
  (let ((inhibit-read-only t))
    (if hide-ifdef-lines
	(save-excursion
	  (beginning-of-line)
	  (hif-show-ifdef-region (1- (point)) (progn (end-of-line) (point))))

      (let (top bottom)
	(hif-find-ifdef-block)
	(hif-show-ifdef-region (1- top) bottom)))))


;;;  definition alist support

(defvar hide-ifdef-define-alist nil
  "A global assoc list of pre-defined symbol lists")

(defun hif-compress-define-list (env)
  "Compress the define list ENV into a list of defined symbols only."
  (let ((defs (mapcar '(lambda (arg)
			 (if (hif-lookup (car arg)) (car arg)))
		      env))
	(new-defs nil))
    (while defs
      (if (car defs)
	  (setq new-defs (cons (car defs) new-defs)))
      (setq defs (cdr defs)))
    new-defs))

(defun hide-ifdef-set-define-alist (name)
  "Set the association for NAME to `hide-ifdef-env'."
  (interactive "SSet define list: ")
  (setq hide-ifdef-define-alist
	(cons (cons name (hif-compress-define-list hide-ifdef-env))
	      hide-ifdef-define-alist)))

(defun hide-ifdef-use-define-alist (name)
  "Set `hide-ifdef-env' to the define list specified by NAME."
  (interactive "SUse define list: ")
  (let ((define-list (assoc name hide-ifdef-define-alist)))
    (if define-list
	(setq hide-ifdef-env
	      (mapcar '(lambda (arg) (cons arg t))
		      (cdr define-list)))
      (error "No define list for %s" name))
    (if hide-ifdef-hiding (hide-ifdefs))))

(provide 'hideif)

;;; hideif.el ends here



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
;;;;;;(load "op-outline-mode-emacs19")  ;; nix anderes als der outline-mode OHNE die 

;;; outline.el --- outline mode commands for Emacs

;; Copyright (C) 1986, 1993, 1994 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: outlines

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package is a major mode for editing outline-format documents.
;; An outline can be `abstracted' to show headers at any given level,
;; with all stuff below hidden.  See the Emacs manual for details.

;;; Code:

;; Jan '86, Some new features added by Peter Desnoyers and rewritten by RMS.
  
(defvar outline-regexp nil
  "*Regular expression to match the beginning of a heading.
Any line whose beginning matches this regexp is considered to start a heading.
The recommended way to set this is with a Local Variables: list
in the file it applies to.  See also outline-heading-end-regexp.")

;; Can't initialize this in the defvar above -- some major modes have
;; already assigned a local value to it.
(or (default-value 'outline-regexp)
    (setq-default outline-regexp "[*\^L]+"))
  
(defvar outline-heading-end-regexp "[\n\^M]"
  "*Regular expression to match the end of a heading line.
You can assume that point is at the beginning of a heading when this
regexp is searched for.  The heading ends at the end of the match.
The recommended way to set this is with a \"Local Variables:\" list
in the file it applies to.")

(defvar outline-mode-prefix-map nil)

(if outline-mode-prefix-map
    nil
  (setq outline-mode-prefix-map (make-sparse-keymap))
  (define-key outline-mode-prefix-map "\C-n" 'outline-next-visible-heading)
  (define-key outline-mode-prefix-map "\C-p" 'outline-previous-visible-heading)
  (define-key outline-mode-prefix-map "\C-i" 'show-children)
  (define-key outline-mode-prefix-map "\C-s" 'show-subtree)
  (define-key outline-mode-prefix-map "\C-d" 'hide-subtree)
  (define-key outline-mode-prefix-map "\C-u" 'outline-up-heading)
  (define-key outline-mode-prefix-map "\C-f" 'outline-forward-same-level)
  (define-key outline-mode-prefix-map "\C-b" 'outline-backward-same-level)
  (define-key outline-mode-prefix-map "\C-t" 'hide-body)
  (define-key outline-mode-prefix-map "\C-a" 'show-all)
  (define-key outline-mode-prefix-map "\C-c" 'hide-entry)
  (define-key outline-mode-prefix-map "\C-e" 'show-entry)
  (define-key outline-mode-prefix-map "\C-l" 'hide-leaves)
  (define-key outline-mode-prefix-map "\C-k" 'show-branches)
  (define-key outline-mode-prefix-map "\C-q" 'hide-sublevels)
  (define-key outline-mode-prefix-map "\C-o" 'hide-other))

(defvar outline-mode-menu-bar-map nil)
(if outline-mode-menu-bar-map
    nil
  (setq outline-mode-menu-bar-map (make-sparse-keymap)))


(defvar outline-mode-map nil "")

(if outline-mode-map
    nil
  (setq outline-mode-map (nconc (make-sparse-keymap) text-mode-map))
  (define-key outline-mode-map "\C-c" outline-mode-prefix-map)
  (define-key outline-mode-map [menu-bar] outline-mode-menu-bar-map))

(defvar outline-minor-mode nil
  "Non-nil if using Outline mode as a minor mode of some other mode.")
(make-variable-buffer-local 'outline-minor-mode)
(put 'outline-minor-mode 'permanent-local t)
(or (assq 'outline-minor-mode minor-mode-alist)
    (setq minor-mode-alist (append minor-mode-alist
				   (list '(outline-minor-mode " Outl")))))

(defvar outline-font-lock-keywords
  '(;; Highlight headings according to the level.
    ("^\\(\\*+\\)[ \t]*\\(.+\\)?[ \t]*$"
     (1 font-lock-string-face)
     (2 (let ((len (- (match-end 1) (match-beginning 1))))
	  (or (cdr (assq len '((1 . font-lock-function-name-face)
			       (2 . font-lock-keyword-face)
			       (3 . font-lock-comment-face))))
	      font-lock-variable-name-face))
	nil t))
    ;; Highlight citations of the form [1] and [Mar94].
    ("\\[\\([A-Z][A-Za-z]+\\)*[0-9]+\\]" . font-lock-type-face))
  "Additional expressions to highlight in Outline mode.")

;;;###autoload
(defun outline-mode ()
  "Set major mode for editing outlines with selective display.
Headings are lines which start with asterisks: one for major headings,
two for subheadings, etc.  Lines not starting with asterisks are body lines. 

Body text or subheadings under a heading can be made temporarily
invisible, or visible again.  Invisible lines are attached to the end 
of the heading, so they move with it, if the line is killed and yanked
back.  A heading with text hidden under it is marked with an ellipsis (...).

Commands:\\<outline-mode-map>
\\[outline-next-visible-heading]   outline-next-visible-heading      move by visible headings
\\[outline-previous-visible-heading]   outline-previous-visible-heading
\\[outline-forward-same-level]   outline-forward-same-level        similar but skip subheadings
\\[outline-backward-same-level]   outline-backward-same-level
\\[outline-up-heading]   outline-up-heading		    move from subheading to heading

\\[hide-body]	make all text invisible (not headings).
\\[show-all]	make everything in buffer visible.

The remaining commands are used when point is on a heading line.
They apply to some of the body or subheadings of that heading.
\\[hide-subtree]   hide-subtree	make body and subheadings invisible.
\\[show-subtree]   show-subtree	make body and subheadings visible.
\\[show-children]   show-children	make direct subheadings visible.
		 No effect on body, or subheadings 2 or more levels down.
		 With arg N, affects subheadings N levels down.
\\[hide-entry]	   make immediately following body invisible.
\\[show-entry]	   make it visible.
\\[hide-leaves]	   make body under heading and under its subheadings invisible.
		     The subheadings remain visible.
\\[show-branches]  make all subheadings at all levels visible.

The variable `outline-regexp' can be changed to control what is a heading.
A line is a heading if `outline-regexp' matches something at the
beginning of the line.  The longer the match, the deeper the level.

Turning on outline mode calls the value of `text-mode-hook' and then of
`outline-mode-hook', if they are non-nil."
  (interactive)
  (kill-all-local-variables)
  (setq selective-display t)
  (use-local-map outline-mode-map)
  (setq mode-name "Outline")
  (setq major-mode 'outline-mode)
  (define-abbrev-table 'text-mode-abbrev-table ())
  (setq local-abbrev-table text-mode-abbrev-table)
  (set-syntax-table text-mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat paragraph-start "\\|\\("
				outline-regexp "\\)"))
  ;; Inhibit auto-filling of header lines.
  (make-local-variable 'auto-fill-inhibit-regexp)
  (setq auto-fill-inhibit-regexp outline-regexp)
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate (concat paragraph-separate "\\|\\("
				   outline-regexp "\\)"))
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(outline-font-lock-keywords t))
  (make-local-variable 'change-major-mode-hook)
  (add-hook 'change-major-mode-hook 'show-all)
  (run-hooks 'text-mode-hook 'outline-mode-hook))

(defvar outline-minor-mode-prefix "\C-c@"
  "*Prefix key to use for Outline commands in Outline minor mode.
The value of this variable is checked as part of loading Outline mode.
After that, changing the prefix key requires manipulating keymaps.")

(defvar outline-minor-mode-map nil)
(if outline-minor-mode-map
    nil
  (setq outline-minor-mode-map (make-sparse-keymap))
  (define-key outline-minor-mode-map [menu-bar]
    outline-mode-menu-bar-map)
  (define-key outline-minor-mode-map outline-minor-mode-prefix
    outline-mode-prefix-map))

(or (assq 'outline-minor-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'outline-minor-mode outline-minor-mode-map)
		minor-mode-map-alist)))

;;;###autoload
(defun outline-minor-mode (&optional arg)
  "Toggle Outline minor mode.
With arg, turn Outline minor mode on if arg is positive, off otherwise.
See the command `outline-mode' for more information on this mode."
  (interactive "P")
  (setq outline-minor-mode
	(if (null arg) (not outline-minor-mode)
	  (> (prefix-numeric-value arg) 0)))
  (if outline-minor-mode
      (progn
	(setq selective-display t)
	(run-hooks 'outline-minor-mode-hook))
    (setq selective-display nil))
  ;; When turning off outline mode, get rid of any ^M's.
  (or outline-minor-mode
      (outline-flag-region (point-min) (point-max) ?\n))
  (force-mode-line-update))

(defvar outline-level 'outline-level
  "Function of no args to compute a header's nesting level in an outline.
It can assume point is at the beginning of a header line.")

;; This used to count columns rather than characters, but that made ^L
;; appear to be at level 2 instead of 1.  Columns would be better for
;; tab handling, but the default regexp doesn't use tabs, and anyone
;; who changes the regexp can also redefine the outline-level variable
;; as appropriate.
(defun outline-level ()
  "Return the depth to which a statement is nested in the outline.
Point must be at the beginning of a header line.  This is actually
the number of characters that `outline-regexp' matches."
  (save-excursion
    (looking-at outline-regexp)
    (- (match-end 0) (match-beginning 0))))

(defun outline-next-preface ()
  "Skip forward to just before the next heading line.
If there's no following heading line, stop before the newline
at the end of the buffer."
  (if (re-search-forward (concat "[\n\^M]\\(" outline-regexp "\\)")
			 nil 'move)
      (goto-char (match-beginning 0)))
  (if (memq (preceding-char) '(?\n ?\^M))
      (forward-char -1)))

(defun outline-next-heading ()
  "Move to the next (possibly invisible) heading line."
  (interactive)
  (if (re-search-forward (concat "[\n\^M]\\(" outline-regexp "\\)")
			 nil 'move)
      (goto-char (1+ (match-beginning 0)))))

(defun outline-back-to-heading ()
  "Move to previous heading line, or beg of this line if it's a heading.
Only visible heading lines are considered."
  (beginning-of-line)
  (or (outline-on-heading-p)
      (re-search-backward (concat "^\\(" outline-regexp "\\)") nil t)
      (error "before first heading")))

(defun outline-on-heading-p ()
  "Return t if point is on a (visible) heading line."
  (save-excursion
    (beginning-of-line)
    (and (bolp)
	 (looking-at outline-regexp))))

(defun outline-end-of-heading ()
  (if (re-search-forward outline-heading-end-regexp nil 'move)
      (forward-char -1)))

(defun outline-next-visible-heading (arg)
  "Move to the next visible heading line.
With argument, repeats or can move backward if negative.
A heading line is one that starts with a `*' (or that
`outline-regexp' matches)."
  (interactive "p")
  (if (< arg 0)
      (beginning-of-line)
    (end-of-line))
  (or (re-search-forward (concat "^\\(" outline-regexp "\\)") nil t arg)
      (error ""))
  (beginning-of-line))

(defun outline-previous-visible-heading (arg)
  "Move to the previous heading line.
With argument, repeats or can move forward if negative.
A heading line is one that starts with a `*' (or that
`outline-regexp' matches)."
  (interactive "p")
  (outline-next-visible-heading (- arg)))

(defun outline-flag-region (from to flag)
  "Hides or shows lines from FROM to TO, according to FLAG.
If FLAG is `\\n' (newline character) then text is shown,
while if FLAG is `\\^M' (control-M) the text is hidden."
  (let (buffer-read-only)
    (subst-char-in-region from to
			  (if (= flag ?\n) ?\^M ?\n)
			  flag t)))

(defun hide-entry ()
  "Hide the body directly following this heading."
  (interactive)
  (outline-back-to-heading)
  (outline-end-of-heading)
  (save-excursion
   (outline-flag-region (point) (progn (outline-next-preface) (point)) ?\^M)))

(defun show-entry ()
  "Show the body directly following this heading."
  (interactive)
  (save-excursion
   (outline-flag-region (point) (progn (outline-next-preface) (point)) ?\n)))

(defun hide-body ()
  "Hide all of buffer except headings."
  (interactive)
  (hide-region-body (point-min) (point-max)))

(defun hide-region-body (start end)
  "Hide all body lines in the region, but not headings."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (if (outline-on-heading-p)
	  (outline-end-of-heading))
      (while (not (eobp))
	(outline-flag-region (point)
			     (progn (outline-next-preface) (point)) ?\^M)
	(if (not (eobp))
	    (progn
	      (forward-char
	       (if (looking-at "[\n\^M][\n\^M]")
		   2 1))
	      (outline-end-of-heading)))))))

(defun show-all ()
  "Show all of the text in the buffer."
  (interactive)
  (outline-flag-region (point-min) (point-max) ?\n))

(defun hide-subtree ()
  "Hide everything after this heading at deeper levels."
  (interactive)
  (outline-flag-subtree ?\^M))

(defun hide-leaves ()
  "Hide all body after this heading at deeper levels."
  (interactive)
  (outline-back-to-heading)
  (outline-end-of-heading)
  (hide-region-body (point) (progn (outline-end-of-subtree) (point))))

(defun show-subtree ()
  "Show everything after this heading at deeper levels."
  (interactive)
  (outline-flag-subtree ?\n))

(defun hide-sublevels (levels)
  "Hide everything but the top LEVELS levels of headers, in whole buffer."
  (interactive "p")
  (if (< levels 1)
      (error "Must keep at least one level of headers"))
  (setq levels (1- levels))
  (save-excursion
    (goto-char (point-min))
    ;; Keep advancing to the next top-level heading.
    (while (or (and (bobp) (outline-on-heading-p))
	       (outline-next-heading))
      (let ((end (save-excursion (outline-end-of-subtree) (point))))
	;; Hide everything under that.
	(outline-flag-region (point) end ?\^M)
	;; Show the first LEVELS levels under that.
	(if (> levels 0)
	    (show-children levels))
	;; Move to the next, since we already found it.
	(goto-char end)))))

(defun hide-other ()
  "Hide everything except for the current body and the parent headings."
  (interactive)
  (hide-sublevels 1)
  (let ((last (point))
	(pos (point)))
    (while (save-excursion
	     (and (re-search-backward "[\n\r]" nil t)
		  (eq (following-char) ?\r)))
      (save-excursion
	(beginning-of-line)
	(if (eq last (point))
	    (progn
	      (outline-next-heading)
	      (outline-flag-region last (point) ?\n))
	  (show-children)
	  (setq last (point)))))))

(defun outline-flag-subtree (flag)
  (save-excursion
    (outline-back-to-heading)
    (outline-end-of-heading)
    (outline-flag-region (point)
			  (progn (outline-end-of-subtree) (point))
			  flag)))

(defun outline-end-of-subtree ()
  (outline-back-to-heading)
  (let ((opoint (point))
	(first t)
	(level (funcall outline-level)))
    (while (and (not (eobp))
		(or first (> (funcall outline-level) level)))
      (setq first nil)
      (outline-next-heading))
    (if (memq (preceding-char) '(?\n ?\^M))
	(progn
	  ;; Go to end of line before heading
	  (forward-char -1)
	  (if (memq (preceding-char) '(?\n ?\^M))
	      ;; leave blank line before heading
	      (forward-char -1))))))

(defun show-branches ()
  "Show all subheadings of this heading, but not their bodies."
  (interactive)
  (show-children 1000))

(defun show-children (&optional level)
  "Show all direct subheadings of this heading.
Prefix arg LEVEL is how many levels below the current level should be shown.
Default is enough to cause the following heading to appear."
  (interactive "P")
  (setq level
	(if level (prefix-numeric-value level)
	  (save-excursion
	    (outline-back-to-heading)
	    (let ((start-level (funcall outline-level)))
	      (outline-next-heading)
	      (if (eobp)
		  1
		(max 1 (- (funcall outline-level) start-level)))))))
  (save-excursion
    (save-restriction
      (outline-back-to-heading)
      (setq level (+ level (funcall outline-level)))
      (narrow-to-region (point)
			(progn (outline-end-of-subtree)
			       (if (eobp) (point-max) (1+ (point)))))
      (goto-char (point-min))
      (while (and (not (eobp))
		  (progn
		    (outline-next-heading)
		    (not (eobp))))
	(if (<= (funcall outline-level) level)
	    (save-excursion
	      (outline-flag-region (save-excursion
				     (forward-char -1)
				     (if (memq (preceding-char) '(?\n ?\^M))
					 (forward-char -1))
				     (point))
				   (progn (outline-end-of-heading) (point))
				   ?\n)))))))

(defun outline-up-heading (arg)
  "Move to the heading line of which the present line is a subheading.
With argument, move up ARG levels."
  (interactive "p")
  (outline-back-to-heading)
  (if (eq (funcall outline-level) 1)
      (error ""))
  (while (and (> (funcall outline-level) 1)
	      (> arg 0)
	      (not (bobp)))
    (let ((present-level (funcall outline-level)))
      (while (not (< (funcall outline-level) present-level))
	(outline-previous-visible-heading 1))
      (setq arg (- arg 1)))))

(defun outline-forward-same-level (arg)
  "Move forward to the ARG'th subheading at same level as this one.
Stop at the first and last subheadings of a superior heading."
  (interactive "p")
  (outline-back-to-heading)
  (while (> arg 0)
    (let ((point-to-move-to (save-excursion
			      (outline-get-next-sibling))))  
      (if point-to-move-to
	  (progn
	    (goto-char point-to-move-to)
	    (setq arg (1- arg)))
	(progn
	  (setq arg 0)
	  (error ""))))))

(defun outline-get-next-sibling ()
  "Move to next heading of the same level, and return point or nil if none."
  (let ((level (funcall outline-level)))
    (outline-next-visible-heading 1)
    (while (and (> (funcall outline-level) level)
		(not (eobp)))
      (outline-next-visible-heading 1))
    (if (< (funcall outline-level) level)
	nil
      (point))))
	
(defun outline-backward-same-level (arg)
  "Move backward to the ARG'th subheading at same level as this one.
Stop at the first and last subheadings of a superior heading."
  (interactive "p")
  (outline-back-to-heading)
  (while (> arg 0)
    (let ((point-to-move-to (save-excursion
			      (outline-get-last-sibling))))
      (if point-to-move-to
	  (progn
	    (goto-char point-to-move-to)
	    (setq arg (1- arg)))
	(progn
	  (setq arg 0)
	  (error ""))))))

(defun outline-get-last-sibling ()
  "Move to next heading of the same level, and return point or nil if none."
  (let ((level (funcall outline-level)))
    (outline-previous-visible-heading 1)
    (while (and (> (funcall outline-level) level)
		(not (bobp)))
      (outline-previous-visible-heading 1))
    (if (< (funcall outline-level) level)
	nil
        (point))))

(provide 'outline)

;;; outline.el ends here
	;; unnötige Menübalkenbelegung
	;; else
)
;;;;;;;includeforelc!
;;;;;;(load "op-outline-mode-emacs20+")  ;; nix anderes als der outline-mode alt (der neue hat mir zu viele Bugs),

;;; outline.el --- outline mode commands for Emacs

;; Copyright (C) 1986, 1993, 1994, 1995, 1997 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: outlines

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package is a major mode for editing outline-format documents.
;; An outline can be `abstracted' to show headers at any given level,
;; with all stuff below hidden.  See the Emacs manual for details.

;;; Code:



(defcustom selective-display-ellipses t
  "If you set the variable selective-display-ellipses to nil, the three dots at the
end of a line that precedes invisible lines do not appear. There is no visible
indication of the invisible lines. This variable becomes local automatically
when set.")

(defgroup outlines nil
  "Support for hierarchical outlining."
  :prefix "outline-"
  :group 'editing)

(defcustom outline-regexp nil
  "*Regular expression to match the beginning of a heading.
Any line whose beginning matches this regexp is considered to start a heading.
Note that Outline mode only checks this regexp at the start of a line,
so the regexp need not (and usually does not) start with `^'.
The recommended way to set this is with a Local Variables: list
in the file it applies to.  See also `outline-heading-end-regexp'."
  :type '(choice regexp (const nil))
  :group 'outlines)

;; Can't initialize this in the defvar above -- some major modes have
;; already assigned a local value to it.
(or (default-value 'outline-regexp)
    (setq-default outline-regexp "[*\^L]+"))

(defcustom outline-heading-end-regexp "\n"
  "*Regular expression to match the end of a heading line.
You can assume that point is at the beginning of a heading when this
regexp is searched for.  The heading ends at the end of the match.
The recommended way to set this is with a `Local Variables:' list
in the file it applies to."
  :type 'regexp
  :group 'outlines)


;(add-hook 'outline-view-change-hook 'recenter)


(defvar outline-mode-prefix-map nil)

(if outline-mode-prefix-map
    nil
  (if xemacsp
      (setq outline-mode-prefix-map (make-keymap))
    (setq outline-mode-prefix-map (make-sparse-keymap)))
  (define-key outline-mode-prefix-map "@" 'outline-mark-subtree)
  (define-key outline-mode-prefix-map "\C-n" 'outline-next-visible-heading)
  (define-key outline-mode-prefix-map "\C-p" 'outline-previous-visible-heading)
  (define-key outline-mode-prefix-map "\C-i" 'show-children)
  (define-key outline-mode-prefix-map "\C-s" 'show-subtree)
  (define-key outline-mode-prefix-map "\C-d" 'hide-subtree)
  (define-key outline-mode-prefix-map "\C-u" 'outline-up-heading)
  (define-key outline-mode-prefix-map "\C-f" 'outline-forward-same-level)
  (define-key outline-mode-prefix-map "\C-b" 'outline-backward-same-level)
  (define-key outline-mode-prefix-map "\C-t" 'hide-body)
  (define-key outline-mode-prefix-map "\C-a" 'show-all)
  (define-key outline-mode-prefix-map "\C-c" 'hide-entry)
  (define-key outline-mode-prefix-map "\C-e" 'show-entry)
  (define-key outline-mode-prefix-map "\C-l" 'hide-leaves)
  (define-key outline-mode-prefix-map "\C-k" 'show-branches)
  (define-key outline-mode-prefix-map "\C-q" 'hide-sublevels)
  (define-key outline-mode-prefix-map "\C-o" 'hide-other)
  (define-key outline-mode-prefix-map "\C- " 'outline-toggle-children)
  (define-key outline-mode-prefix-map "\C-^" 'outline-move-subtree-up)
  (define-key outline-mode-prefix-map "\C-v" 'outline-move-subtree-down)
  (define-key outline-mode-prefix-map [(control ?<)] 'outline-promote)
  (define-key outline-mode-prefix-map [(control ?>)] 'outline-demote)
  (define-key outline-mode-prefix-map "\C-m" 'outline-insert-heading)
)



(defvar outline-mode-menu-heading
  '("Headings"
    ["Up" outline-up-heading t]
    ["Next" outline-next-visible-heading t]
    ["Previous" outline-previous-visible-heading t]
    ["Next Same Level" outline-forward-same-level t]
    ["Previous Same Level" outline-backward-same-level t]
    ["New heading" outline-insert-heading t]
    ["Copy to kill ring" outline-headers-as-kill :active (region-active-p)]
    ["Toggle Children" outline-toggle-children t]
    ["Move subtree up" outline-move-subtree-up t]
    ["Move subtree down" outline-move-subtree-down t]
    ["Promote subtree" outline-promote t]
    ["Demote subtree" outline-demote t]))

(defvar outline-mode-menu-show
  '("Show"
    ["Show All" show-all t]
    ["Show Entry" show-entry t]
    ["Show Branches" show-branches t]
    ["Show Children" show-children t]
    ["Show Subtree" show-subtree t]))

(defvar outline-mode-menu-hide
  '("Hide"
    ["Hide Leaves" hide-leaves t]
    ["Hide Body" hide-body t]
    ["Hide Entry" hide-entry t]
    ["Hide Subtree" hide-subtree t]
    ["Hide Other" hide-other t]
    ["Hide Sublevels" hide-sublevels t]))



(defvar outline-mode-menu-bar-map nil)
(if outline-mode-menu-bar-map
    nil
  (if xemacsp
      (setq outline-mode-menu-bar-map (make-keymap))
  (setq outline-mode-menu-bar-map (make-sparse-keymap))))

(defconst outline-mode-map nil "")

(if outline-mode-map
    nil
  (if xemacsp
      (setq outline-mode-map (copy-keymap text-mode-map)) ;;Xemacs: korrekt??
    (setq outline-mode-map (nconc (make-sparse-keymap) text-mode-map)))
  (define-key outline-mode-map "\C-c" outline-mode-prefix-map)
  (define-key outline-mode-map [menu-bar] outline-mode-menu-bar-map))

(defcustom outline-minor-mode nil
  "Non-nil if using Outline mode as a minor mode of some other mode."
  :type 'boolean
  :group 'outlines)
(make-variable-buffer-local 'outline-minor-mode)
(or (assq 'outline-minor-mode minor-mode-alist)
    (setq minor-mode-alist (append minor-mode-alist
				   (list '(outline-minor-mode " Outl")))))

(defvar outline-font-lock-keywords
  '(;;
    ;; Highlight headings according to the level.
    (eval . (list (concat "^" outline-regexp ".+")
		  0 '(or (cdr (assq (outline-font-lock-level)
				    '((1 . font-lock-function-name-face)
				      (2 . font-lock-variable-name-face)
				      (3 . font-lock-keyword-face)
				      (4 . font-lock-builtin-face)
				      (5 . font-lock-comment-face)
				      (6 . font-lock-constant-face)
				      (7 . font-lock-type-face)
				      (8 . font-lock-string-face))))
			 font-lock-warning-face)
		  nil t)))
  "Additional expressions to highlight in Outline mode.")

(defun outline-font-lock-level ()
  (let ((count 1))
    (save-excursion
      (outline-back-to-heading)
      (condition-case nil
	  (while (not (bobp))
	    (outline-up-heading 1)
	    (setq count (1+ count)))
	(error (message "count stop in outline-font-lock-level"))))
    count))

(defvar outline-view-change-hook nil
  "Normal hook to be run after outline visibility changes.")

(defvar outline-mode-hook t
  "*This hook is run when outline mode starts.")

(defvar outline-blank-line t
  "*Non-nil means to leave unhidden blank line before heading.")

;;;###autoload
(defun outline-mode ()
  "Set major mode for editing outlines with selective display.
Headings are lines which start with asterisks: one for major headings,
two for subheadings, etc.  Lines not starting with asterisks are body lines.

Body text or subheadings under a heading can be made temporarily
invisible, or visible again.  Invisible lines are attached to the end
of the heading, so they move with it, if the line is killed and yanked
back.  A heading with text hidden under it is marked with an ellipsis (...).

Commands:\\<outline-mode-map>
\\[outline-next-visible-heading]   outline-next-visible-heading      move by visible headings
\\[outline-previous-visible-heading]   outline-previous-visible-heading
\\[outline-forward-same-level]   outline-forward-same-level        similar but skip subheadings
\\[outline-backward-same-level]   outline-backward-same-level
\\[outline-up-heading]   outline-up-heading		    move from subheading to heading

\\[hide-body]	make all text invisible (not headings).
\\[show-all]	make everything in buffer visible.
\\[hide-sublevels]  make only the first N levels of headers visible.

The remaining commands are used when point is on a heading line.
They apply to some of the body or subheadings of that heading.
\\[hide-subtree]   hide-subtree	make body and subheadings invisible.
\\[show-subtree]   show-subtree	make body and subheadings visible.
\\[show-children]   show-children	make direct subheadings visible.
		 No effect on body, or subheadings 2 or more levels down.
		 With arg N, affects subheadings N levels down.
\\[hide-entry]	   make immediately following body invisible.
\\[show-entry]	   make it visible.
\\[hide-leaves]	   make body under heading and under its subheadings invisible.
		     The subheadings remain visible.
\\[show-branches]  make all subheadings at all levels visible.

The variable `outline-regexp' can be changed to control what is a heading.
A line is a heading if `outline-regexp' matches something at the
beginning of the line.  The longer the match, the deeper the level.

Turning on outline mode calls the value of `text-mode-hook' and then of
`outline-mode-hook', if they are non-nil."
  (interactive)
  ;; SMW!!  (kill-all-local-variables)
  (use-local-map outline-mode-map)
  (setq mode-name "Outline")
  (setq major-mode 'outline-mode)
  (define-abbrev-table 'text-mode-abbrev-table ())
  (setq local-abbrev-table text-mode-abbrev-table)
  (set-syntax-table text-mode-syntax-table)
  (make-local-variable 'line-move-ignore-invisible)
  (setq line-move-ignore-invisible t)
  ;; Cause use of ellipses for invisible text.
  (add-to-invisibility-spec '(outline . t))
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat paragraph-start "\\|\\("
				outline-regexp "\\)"))
  ;; Inhibit auto-filling of header lines.
  (make-local-variable 'auto-fill-inhibit-regexp)
  (setq auto-fill-inhibit-regexp outline-regexp)
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate (concat paragraph-separate "\\|\\("
				   outline-regexp "\\)"))
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(outline-font-lock-keywords t))
  (make-local-variable 'change-major-mode-hook)
  (add-hook 'change-major-mode-hook 'show-all)
  (run-hooks 'text-mode-hook 'outline-mode-hook))

(defcustom outline-minor-mode-prefix "\C-c@"
  "*Prefix key to use for Outline commands in Outline minor mode.
The value of this variable is checked as part of loading Outline mode.
After that, changing the prefix key requires manipulating keymaps."
  :type 'string
  :group 'outlines)

(defvar outline-minor-mode-map nil)
(if outline-minor-mode-map
    nil
  (setq outline-minor-mode-map (make-sparse-keymap))
  (define-key outline-minor-mode-map [menu-bar]
    outline-mode-menu-bar-map)
  (define-key outline-minor-mode-map outline-minor-mode-prefix
    outline-mode-prefix-map))

(or (assq 'outline-minor-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'outline-minor-mode outline-minor-mode-map)
		minor-mode-map-alist)))

;;;###autoload
(defun outline-minor-mode (&optional arg)
  "Toggle Outline minor mode.
With arg, turn Outline minor mode on if arg is positive, off otherwise.
See the command `outline-mode' for more information on this mode."
  (interactive "P")
  (setq outline-minor-mode
	(if (null arg) (not outline-minor-mode)
	  (> (prefix-numeric-value arg) 0)))
  (if outline-minor-mode
      (progn
	(make-local-hook 'change-major-mode-hook)
	;; Turn off this mode if we change major modes.
	(add-hook 'change-major-mode-hook
		  '(lambda () (outline-minor-mode -1))
		  nil t)
	(make-local-variable 'line-move-ignore-invisible)
	(setq line-move-ignore-invisible t)
	;; Cause use of ellipses for invisible text.
	(add-to-invisibility-spec '(outline . t))
	(run-hooks 'outline-minor-mode-hook))
    (setq line-move-ignore-invisible nil)
    ;; Cause use of ellipses for invisible text.
    (remove-from-invisibility-spec '(outline . t)))
  ;; When turning off outline mode, get rid of any outline hiding.
  (or outline-minor-mode
      (show-all))
  (force-mode-line-update))

(defcustom outline-level 'outline-level
  "*Function of no args to compute a header's nesting level in an outline.
It can assume point is at the beginning of a header line."
  :type 'function
  :group 'outlines)

(defvar outline-heading-alist ()
  "Alist associating a heading for every possible level.
Each entry is of the form (HEADING . LEVEL).
This alist is used two ways: to find the heading corresponding to
a given level and to find the level of a given heading.
If a mode or document needs several sets of outline headings (for example
numbered and unnumbered sections), list them set by set and sorted by level
within each set.  For example in texinfo mode:

     (setq outline-heading-alist
      '((\"@chapter\" . 2) (\"@section\" . 3) (\"@subsection\" . 4)
           (\"@subsubsection\" . 5)
        (\"@unnumbered\" . 2) (\"@unnumberedsec\" . 3)
           (\"@unnumberedsubsec\" . 4)  (\"@unnumberedsubsubsec\" . 5)
        (\"@appendix\" . 2) (\"@appendixsec\" . 3)...
           (\"@appendixsubsec\" . 4) (\"@appendixsubsubsec\" . 5) ..))

Instead of sorting the entries in each set, you can also separate the
sets with nil.")
(make-variable-buffer-local 'outline-heading-alist)



;; This used to count columns rather than characters, but that made ^L
;; appear to be at level 2 instead of 1.  Columns would be better for
;; tab handling, but the default regexp doesn't use tabs, and anyone
;; who changes the regexp can also redefine the outline-level variable
;; as appropriate.

;;; Auskommentiert, da Buggy (SMW, 26.7.07)
;,(defun outline-level ()
;,  "Return the depth to which a statement is nested in the outline.
;,Point must be at the beginning of a header line.
;,This is actually either the level specified in `outline-heading-alist'
;,or else the number of characters matched by `outline-regexp'."
;,  (or (cdr (assoc (match-string 0) outline-heading-alist))
;,      (- (match-end 0) (match-beginning 0))))
;;; Ersatzfunktion SMW
(defun outline-level ()
  "Return the depth to which a statement is nested in the outline.
Point must be at the beginning of a header line.  This is actually
the number of characters that `outline-regexp' matches."
  (save-excursion
    (looking-at outline-regexp)
    (- (match-end 0) (match-beginning 0))))

(defun outline-next-preface ()
  "Skip forward to just before the next heading line.
If there's no following heading line, stop before the newline
at the end of the buffer."
  (if (re-search-forward (concat "\n\\(?:" outline-regexp "\\)")
			 nil 'move)
      (goto-char (match-beginning 0)))
  (if (and (bolp) (or outline-blank-line (eobp)) (not (bobp)))
      (forward-char -1)))

(defun outline-next-heading ()
  "Move to the next (possibly invisible) heading line."
  (interactive)
  (if (re-search-forward (concat "\n\\(" outline-regexp "\\)")
			 nil 'move)
      (goto-char (1+ (match-beginning 0)))))

(defsubst outline-visible ()
  "Non-nil if the character after point is visible."
  (not (get-char-property (point) 'invisible)))

(defun outline-previous-heading ()
  "Move to the previous (possibly invisible) heading line."
  (interactive)
  (re-search-backward (concat "^\\(?:" outline-regexp "\\)")
		      nil 'move))

(defsubst outline-invisible-p (&optional pos)
  "Non-nil if the character after point is invisible."
  (get-char-property (or pos (point)) 'invisible))

(defun outline-visible ()
  (not (outline-invisible-p)))
(make-obsolete 'outline-visible 'outline-invisible-p)

(defun outline-back-to-heading (&optional invisible-ok)
  "Move to previous heading line, or beg of this line if it's a heading.
Only visible heading lines are considered, unless INVISIBLE-OK is non-nil."
  (beginning-of-line)
  (or (outline-on-heading-p invisible-ok)
      (let (found)
	(save-excursion
	  (while (not found)
	    (or (re-search-backward (concat "^\\(?:" outline-regexp "\\)")
				    nil t)
		(error "before first heading"))
	    (setq found (and (or invisible-ok (not (outline-invisible-p)))
			     (point)))))
	(goto-char found)
	found)))

(defun outline-on-heading-p (&optional invisible-ok)
  "Return t if point is on a (visible) heading line.
If INVISIBLE-OK is non-nil, an invisible heading line is ok too."
  (save-excursion
    (beginning-of-line)
    (and (bolp) (or invisible-ok (not (outline-invisible-p)))
	 (looking-at outline-regexp))))

(defun outline-insert-heading ()
  "Insert a new heading at same depth at point."
  (interactive)
  (let ((head (save-excursion
		(condition-case nil
		    (outline-back-to-heading)
		  (error (outline-next-heading)))
		(if (eobp)
		    (or (caar outline-heading-alist) "")
		  (match-string 0)))))
    (unless (or (string-match "[ \t]\\'" head)
		(not (string-match (concat "\\`\\(?:" outline-regexp "\\)")
				   (concat head " "))))
      (setq head (concat head " ")))
    (unless (bolp) (end-of-line) (newline))
    (insert head)
    (unless (eolp)
      (save-excursion (newline-and-indent)))
    (run-hooks 'outline-insert-heading-hook)))

(defun outline-invent-heading (head up)
  (save-match-data
    ;; Let's try to invent one by repeating or deleting the last char.
    (let ((new-head (if up (substring head 0 -1)
                      (concat head (substring head -1)))))
      (if (string-match (concat "\\`\\(?:" outline-regexp "\\)")
                        new-head)
          ;; Why bother checking that it is indeed higher/lower level ?
          new-head
        ;; Didn't work, so ask what to do.
        (read-string (format "%s heading for `%s': "
                             (if up "Parent" "Demoted") head)
                     head nil nil)))))

(defun outline-promote (&optional children)
  "Promote headings higher up the tree.
If prefix argument CHILDREN is given, promote also all the children.
If the region is active in `transient-mark-mode', promote all headings
in the region."
  (interactive
   (list (if (and zmacs-regions (region-active-p)) 'region
	   (outline-back-to-heading)
	   (if current-prefix-arg nil 'subtree))))
  (cond
   ((eq children 'region)
    (outline-map-region 'outline-promote (region-beginning) (region-end)))
   (children
    (outline-map-region 'outline-promote
			(point)
			(save-excursion (outline-get-next-sibling) (point))))
   (t
    (outline-back-to-heading t)
    (let* ((head (match-string-no-properties 0))
	   (level (save-match-data (funcall outline-level)))
	   (up-head (or (outline-head-from-level (1- level) head)
			;; Use the parent heading, if it is really
			;; one level less.
			(save-excursion
			  (save-match-data
			    (outline-up-heading 1 t)
			    (and (= (1- level) (funcall outline-level))
				 (match-string-no-properties 0))))
                        ;; Bummer!! There is no lower level heading.
                        (outline-invent-heading head 'up))))

      (unless (rassoc level outline-heading-alist)
	(push (cons head level) outline-heading-alist))

      (replace-match up-head nil t)))))

(defun outline-demote (&optional children)
  "Demote headings lower down the tree.
If prefix argument CHILDREN is given, demote also all the children.
If the region is active in `transient-mark-mode', demote all headings
in the region."
  (interactive
   (list (if (and zmacs-regions (region-active-p)) 'region
	   (outline-back-to-heading)
	   (if current-prefix-arg nil 'subtree))))
  (cond
   ((eq children 'region)
    (outline-map-region 'outline-demote (region-beginning) (region-end)))
   (children
    (outline-map-region 'outline-demote
			(point)
			(save-excursion (outline-get-next-sibling) (point))))
   (t
    (let* ((head (match-string-no-properties 0))
	   (level (save-match-data (funcall outline-level)))
	   (down-head
	    (or (outline-head-from-level (1+ level) head)
		(save-excursion
		  (save-match-data
		    (while (and (progn (outline-next-heading) (not (eobp)))
				(<= (funcall outline-level) level)))
		    (when (eobp)
		      ;; Try again from the beginning of the buffer.
		      (goto-char (point-min))
		      (while (and (progn (outline-next-heading) (not (eobp)))
				  (<= (funcall outline-level) level))))
		    (unless (eobp)
		      (looking-at outline-regexp)
		      (match-string-no-properties 0))))
                ;; Bummer!! There is no higher-level heading in the buffer.
                (outline-invent-heading head nil))))

      (unless (rassoc level outline-heading-alist)
	(push (cons head level) outline-heading-alist))
      (replace-match down-head nil t)))))

(defun outline-head-from-level (level head &optional alist)
  "Get new heading with level LEVEL from ALIST.
If there are no such entries, return nil.
ALIST defaults to `outline-heading-alist'.
Similar to (car (rassoc LEVEL ALIST)).
If there are several different entries with same new level, choose
the one with the smallest distance to the assocation of HEAD in the alist.
This makes it possible for promotion to work in modes with several
independent sets of headings (numbered, unnumbered, appendix...)"
  (unless alist (setq alist outline-heading-alist))
  (let ((l (rassoc level alist))
	ll h hl l2 l2l)
    (cond
     ((null l) nil)
     ;; If there's no HEAD after L, any other entry for LEVEL after L
     ;; can't be much better than L.
     ((null (setq h (assoc head (setq ll (memq l alist))))) (car l))
     ;; If there's no other entry for LEVEL, just keep L.
     ((null (setq l2 (rassoc level (cdr ll)))) (car l))
     ;; Now we have L, L2, and H: see if L2 seems better than L.
     ;; If H is after L2, L2 is better.
     ((memq h (setq l2l (memq l2 (cdr ll))))
      (outline-head-from-level level head l2l))
     ;; Now we have H between L and L2.
     ;; If there's a separator between L and H, prefer L2.
     ((memq h (memq nil ll))
      (outline-head-from-level level head l2l))
     ;; If there's a separator between L2 and H, prefer L.
     ((memq l2 (memq nil (setq hl (memq h ll)))) (car l))
     ;; No separator between L and L2, check the distance.
     ((< (* 2 (length hl)) (+ (length ll) (length l2l)))
      (outline-head-from-level level head l2l))
     ;; If all else fails, just keep L.
     (t (car l)))))

(defun outline-map-region (fun beg end)
  "Call FUN for every heading between BEG and END.
When FUN is called, point is at the beginning of the heading and
the match data is set appropriately."
  (save-excursion
    (setq end (copy-marker end))
    (goto-char beg)
    (when (re-search-forward (concat "^\\(?:" outline-regexp "\\)") end t)
      (goto-char (match-beginning 0))
      (funcall fun)
      (while (and (progn
		    (outline-next-heading)
		    (< (point) end))
		  (not (eobp)))
	(funcall fun)))))

;; Vertical tree motion

(defun outline-move-subtree-up (&optional arg)
  "Move the currrent subtree up past ARG headlines of the same level."
  (interactive "p")
  (outline-move-subtree-down (- arg)))

(defun outline-move-subtree-down (&optional arg)
  "Move the currrent subtree down past ARG headlines of the same level."
  (interactive "p")
  (let ((movfunc (if (> arg 0) 'outline-get-next-sibling
		   'outline-get-last-sibling))
	(ins-point (make-marker))
	(cnt (abs arg))
	(tmp-string "")
	beg end folded)
    ;; Select the tree
    (outline-back-to-heading)
    (setq beg (point))
    (save-match-data
      (save-excursion (outline-end-of-heading)
		      (setq folded (outline-invisible-p)))
      (outline-end-of-subtree))
    (if (= (char-after) ?\n) (forward-char 1))
    (setq end (point))
    ;; Find insertion point, with error handling
    (goto-char beg)
    (while (> cnt 0)
      (or (funcall movfunc)
	  (progn (goto-char beg)
		 (error "Cannot move past superior level")))
      (setq cnt (1- cnt)))
    (if (> arg 0)
	;; Moving forward - still need to move over subtree
	(progn (outline-end-of-subtree)
	       (if (= (char-after) ?\n) (forward-char 1))))
    (move-marker ins-point (point))
    (setq tmp-string (buffer-substring beg end))
    (delete-region beg end)
    (insert tmp-string)
    (goto-char ins-point)
    (if folded (hide-subtree))
    (move-marker ins-point nil)))

(defun outline-end-of-heading ()
  (if (re-search-forward outline-heading-end-regexp nil 'move)
      (forward-char -1)))

(defun outline-next-visible-heading (arg)
  "Move to the next visible heading line.
With argument, repeats or can move backward if negative.
A heading line is one that starts with a `*' (or that
`outline-regexp' matches)."
  (interactive "p")
  (if (< arg 0)
      (beginning-of-line)
    (end-of-line))
  (while (and (not (bobp)) (< arg 0))
    (while (and (not (bobp))
		(re-search-backward (concat "^\\(?:" outline-regexp "\\)")
				    nil 'move)
		(outline-invisible-p)))
    (setq arg (1+ arg)))
  (while (and (not (eobp)) (> arg 0))
    (while (and (not (eobp))
		(re-search-forward (concat "^\\(?:" outline-regexp "\\)")
				   nil 'move)
		(outline-invisible-p (match-beginning 0))))
    (setq arg (1- arg)))
  (beginning-of-line))

(defun outline-previous-visible-heading (arg)
  "Move to the previous heading line.
With argument, repeats or can move forward if negative.
A heading line is one that starts with a `*' (or that
`outline-regexp' matches)."
  (interactive "p")
  (outline-next-visible-heading (- arg)))

(defun outline-mark-subtree ()
  "Mark the current subtree in an outlined document.
This puts point at the start of the current subtree, and mark at the end."
  (interactive)
  (let ((beg))
    (if (outline-on-heading-p)
	;; we are already looking at a heading
	(beginning-of-line)
      ;; else go back to previous heading
      (outline-previous-visible-heading 1))
    (setq beg (point))
    (outline-end-of-subtree)
    (push-mark (point) nil t)
    (goto-char beg)))


(defun outline-discard-extents (&optional beg end)
  "Clear BEG and END of overlays whose property NAME has value VAL.
Overlays might be moved and/or split.
BEG and END default respectively to the beginning and end of buffer."
  (unless beg (setq beg (point-min)))
  (unless end (setq end (point-max)))
  (if (< end beg)
      (setq beg (prog1 end (setq end beg))))
  (save-excursion
    (map-extents 
     #'(lambda (ex ignored) 
	 (if (< (extent-start-position ex) beg)
	    (if (> (extent-end-position ex) end)
		(progn
		  (set-extent-endpoints (copy-extent ex)
					(extent-start-position ex) beg)
		  (set-extent-endpoints ex end (extent-end-position ex)))
	      (set-extent-endpoints ex (extent-start-position ex) beg)))
	  (if (> (extent-end-position ex) end)
	      (set-extent-endpoints ex end (extent-end-position ex))
	       (delete-extent ex)))
     (current-buffer) beg end nil 'end-closed 'outline)))

;;; SMW !!! wegen der Kompatibilität zum project-mode: Einfach Überladen der Funktion, 
;;;     (wird im project-mode nochmals überladen!)
(defun outline-discard-extents (from to)
  "Delete hideshow extents in region defined by FROM and TO."
  (when (< to from)
    (setq from (prog1 to (setq to from))))
  (map-extents #'(lambda (ex ignored) (delete-extent ex))
	       (current-buffer) from to nil 'end-closed 'outline))

(defun outline-flag-region (from to flag)
  "Hide or show lines from FROM to TO, according to FLAG.
If FLAG is nil then text is shown, while if FLAG is t the text is hidden."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char from)
      (end-of-line)
      (outline-discard-overlays (point) to 'outline)
      (if flag
          (let ((o (make-overlay (point) to)))
            (overlay-put o 'invisible 'outline)
	    (overlay-put o 'isearch-open-invisible
			 'outline-isearch-open-invisible)))))
  (run-hooks 'outline-view-change-hook))
  


;; Function to be set as an outline-isearch-open-invisible' property
;; to the overlay that makes the outline invisible (see
;; `outline-flag-region').
(defun outline-isearch-open-invisible (overlay)
  (save-excursion
    (goto-char (overlay-start overlay))
    (show-entry)))


;; Exclude from the region BEG ... END all overlays
;; which have PROP as the value of the `invisible' property.
;; Exclude them by shrinking them to exclude BEG ... END,
;; or even by splitting them if necessary.
;; Overlays without such an `invisible' property are not touched.
(defun outline-discard-overlays (beg end prop)
  (if (< end beg)
      (setq beg (prog1 end (setq end beg))))
  (save-excursion
    (let ((overlays (overlays-in beg end))
	  o
	  o1)
      (while overlays
	(setq o (car overlays))
	(if (eq (overlay-get o 'invisible) prop)
	    ;; Either push this overlay outside beg...end
	    ;; or split it to exclude beg...end
	    ;; or delete it entirely (if it is contained in beg...end).
	    (if (< (overlay-start o) beg)
		(if (> (overlay-end o) end)
		    (progn
		      (setq o1 (outline-copy-overlay o))
		      (move-overlay o1 (overlay-start o1) beg)
		      (move-overlay o end (overlay-end o)))
		  (move-overlay o (overlay-start o) beg))
	      (if (> (overlay-end o) end)
		  (move-overlay o end (overlay-end o))
		(delete-overlay o))))
	(setq overlays (cdr overlays))))))

;; Make a copy of overlay O, with the same beginning, end and properties.
(defun outline-copy-overlay (o)
  (let ((o1 (make-overlay (overlay-start o) (overlay-end o)
			  (overlay-buffer o)))
	(props (overlay-properties o)))
    (while props
      (overlay-put o1 (car props) (nth 1 props))
      (setq props (cdr (cdr props))))
    o1))

(defun hide-entry ()
  "Hide the body directly following this heading."
  (interactive)
  (outline-back-to-heading)
  (outline-end-of-heading)
  (save-excursion
   (outline-flag-region (point) (progn (outline-next-preface) (point)) t)))

(defun show-entry ()
  "Show the body directly following this heading.
Show the heading too, if it is currently invisible."
  (interactive)
  (save-excursion
    (outline-back-to-heading t)
    (outline-flag-region (1- (point))
			 (progn (outline-next-preface) (point)) nil)))

(defun hide-body ()
  "Hide all of buffer except headings."
  (interactive)
  (hide-region-body (point-min) (point-max)))

(defun hide-region-body (start end)
  "Hide all body lines in the region, but not headings."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (if (outline-on-heading-p)
	  (outline-end-of-heading))
      (while (not (eobp))
	(outline-flag-region (point)
			     (progn (outline-next-preface) (point)) t)
	(if (not (eobp))
	    (progn
	      (forward-char
	       (if (looking-at "\n\n")
		   2 1))
	      (outline-end-of-heading)))))))

(defun show-all ()
  "Show all of the text in the buffer."
  (interactive)
  (outline-flag-region (point-min) (point-max) nil))

(defun hide-subtree ()
  "Hide everything after this heading at deeper levels."
  (interactive)
  (outline-flag-subtree t))

(defun hide-leaves ()
  "Hide all body after this heading at deeper levels."
  (interactive)
  (outline-back-to-heading)
  (outline-end-of-heading)
  (hide-region-body (point) (progn (outline-end-of-subtree) (point))))

(defun show-subtree ()
  "Show everything after this heading at deeper levels."
  (interactive)
  (if (not (looking-at "\\*")) ; only if outline regex is standard
      (outline-up-heading 0))
  (outline-flag-subtree nil))

(defun outline-show-heading ()
  "Show the current heading and move to its end."
  (outline-flag-region (- (point)
 			  (if (bobp) 0
 			    (if (and outline-blank-line
                                     (eq (char-before (1- (point))) ?\n))
 				2 1)))
		       (progn (outline-end-of-heading) (point))
		       nil))

(defun hide-sublevels (levels)
  "Hide everything but the top LEVELS levels of headers, in whole buffer."
  (interactive "p")
  (if (< levels 1)
      (error "Must keep at least one level of headers"))
  (setq levels (1- levels))
  (save-excursion
    (goto-char (point-min))
    ;; Keep advancing to the next top-level heading.
    (while (or (and (bobp) (outline-on-heading-p))
	       (outline-next-heading))
      (let ((end (save-excursion (outline-end-of-subtree) (point))))
	;; Hide everything under that.
	(outline-flag-region (point) end t)
	;; Show the first LEVELS levels under that.
	(if (> levels 0)
	    (show-children levels))
	;; Move to the next, since we already found it.
	(goto-char end)))))


;;;SMW ff
;;; Angleich der Verhaltensweise an den emacs19 - war für meinen Geschmack auch besser
;;; - diese hier ist deshalb auch aus dem emacs19 
; (defun hide-other ()
;   "Hide everything except for the current body and the parent headings."
;   (interactive)
;   (hide-sublevels 1)
;   (let ((last (point))
; 	(pos (point)))
;     (while (save-excursion
; 	     (and (re-search-backward "[\n\r]" nil t)
; 		  (eq (following-char) ?\r)))
;       (save-excursion
; 	(beginning-of-line)
; 	(if (eq last (point))
; 	    (progn
; 	      (outline-next-heading)
; 	      (outline-flag-region last (point) ?\n))
; 	  (show-children)
; 	  (setq last (point)))))))
;;;SMW
;;; und hier das Orginal vom emacs20
; (defun hide-other ()
;   "Hide everything except current body and parent and top-level headings."
;   (interactive)
;   (hide-sublevels 1)
;   (save-excursion
;     (outline-back-to-heading t)
;     (show-entry)
;     (while (condition-case nil (progn (outline-up-heading 1) t) (error nil))
;       (outline-flag-region (1- (point))
; 			   (save-excursion (forward-line 1) (point))
; 			   nil))))
(defun hide-other ()
  "Hide everything except current body and parent and top-level headings."
  (interactive)
  (hide-sublevels 1)
  (let (outline-view-change-hook)
    (save-excursion
      (outline-back-to-heading t)
      (show-entry)
      (while (condition-case nil (progn (outline-up-heading 1 t) (not (bobp)))
	       (error nil))
	(outline-flag-region (max 1 (1- (point)))
			     (save-excursion (forward-line 1) (point))
			     nil))))
  (run-hooks 'outline-view-change-hook))



(defun outline-toggle-children ()
  "Show or hide the current subtree depending on its current state."
  (interactive)
  (save-excursion
    (outline-back-to-heading)
    (if (not (outline-invisible-p (point-at-eol)))
	(hide-subtree)
      (show-children)
      (show-entry))))

(defun outline-flag-subtree (flag)
  (save-excursion
    (outline-back-to-heading)
    (outline-end-of-heading)
    (outline-flag-region (point)
			  (progn (outline-end-of-subtree) (point))
			  flag)))

(defun outline-end-of-subtree ()
  (outline-back-to-heading)
  (let ((first t)
	(level (funcall outline-level)))
    (while (and (not (eobp))
		(or first (> (funcall outline-level) level)))
      (setq first nil)
      (outline-next-heading))
    (if (bolp)
	(progn
	  ;; Go to end of line before heading
	  (forward-char -1)
          (if (and outline-blank-line (bolp))
 	      ;; leave blank line before heading
 	      (forward-char -1))))))

(defun show-branches ()
  "Show all subheadings of this heading, but not their bodies."
  (interactive)
  (show-children 1000))

(defun show-children (&optional level)
  "Show all direct subheadings of this heading.
Prefix arg LEVEL is how many levels below the current level should be shown.
Default is enough to cause the following heading to appear."
  (interactive "P")
  (setq level
	(if level (prefix-numeric-value level)
	  (save-excursion
	    (outline-back-to-heading)
	    (let ((start-level (funcall outline-level)))
	      (outline-next-heading)
	      (if (eobp)
		  1
		(max 1 (- (funcall outline-level) start-level)))))))
  (save-excursion
    (save-restriction
      (outline-back-to-heading)
      (setq level (+ level (funcall outline-level)))
      (narrow-to-region (point)
			(progn (outline-end-of-subtree)
			       (if (eobp) (point-max) (1+ (point)))))
      (goto-char (point-min))
      (while (and (not (eobp))
		  (progn
		    (outline-next-heading)
		    (not (eobp))))
	(if (<= (funcall outline-level) level)
	    (save-excursion
	      (outline-flag-region (save-excursion
				     (forward-char -1)
				     (if (bolp)
					 (forward-char -1))
				     (point))
				   (progn (outline-end-of-heading) (point))
				   nil)))))))


(defun outline-up-heading (arg &optional invisible-ok)
  "Move to the visible heading line of which the present line is a subheading.
With argument, move up ARG levels.
If INVISIBLE-OK is non-nil, also consider invisible lines."
  (interactive "p")
  (and (eq this-command 'outline-up-heading)
       (or (eq last-command 'outline-up-heading) (push-mark)))
  (outline-back-to-heading invisible-ok)
  (let ((start-level (funcall outline-level)))
    (if (eq start-level 1)
	(error "Already at top level of the outline"))
    (while (and (> start-level 1) (> arg 0) (not (bobp)))
      (let ((level start-level))
	(while (not (or (< level start-level) (bobp)))
	  (if invisible-ok
	      (outline-previous-heading)
	    (outline-previous-visible-heading 1))
	  (setq level (funcall outline-level)))
	(setq start-level level))
      (setq arg (- arg 1))))
  (looking-at outline-regexp))

(defun outline-forward-same-level (arg)
  "Move forward to the ARG'th subheading at same level as this one.
Stop at the first and last subheadings of a superior heading."
  (interactive "p")
  (outline-back-to-heading)
  (while (> arg 0)
    (let ((point-to-move-to (save-excursion
			      (outline-get-next-sibling))))
      (if point-to-move-to
	  (progn
	    (goto-char point-to-move-to)
	    (setq arg (1- arg)))
	(progn
	  (setq arg 0)
	  (error "No following same-level heading"))))))

(defun outline-get-next-sibling ()
  "Move to next heading of the same level, and return point or nil if none."
  (let ((level (funcall outline-level)))
    (outline-next-visible-heading 1)
    (while (and (not (eobp)) (> (funcall outline-level) level))
      (outline-next-visible-heading 1))
    (if (or (eobp) (< (funcall outline-level) level))
	nil
      (point))))

(defun outline-backward-same-level (arg)
  "Move backward to the ARG'th subheading at same level as this one.
Stop at the first and last subheadings of a superior heading."
  (interactive "p")
  (outline-back-to-heading)
  (while (> arg 0)
    (let ((point-to-move-to (save-excursion
			      (outline-get-last-sibling))))
      (if point-to-move-to
	  (progn
	    (goto-char point-to-move-to)
	    (setq arg (1- arg)))
	(progn
	  (setq arg 0)
	  (error "No previous same-level heading"))))))

(defun outline-get-last-sibling ()
  "Move to previous heading of the same level, and return point or nil if none."
  (let ((level (funcall outline-level)))
    (outline-previous-visible-heading 1)
    (while (and (> (funcall outline-level) level)
		(not (bobp)))
      (outline-previous-visible-heading 1))
    (if (< (funcall outline-level) level)
	nil
        (point))))

(defun outline-headers-as-kill (beg end)
  "Save the visible outline headers in region at the start of the kill ring.

Text shown between the headers isn't copied.  Two newlines are
inserted between saved headers.  Yanking the result may be a
convenient way to make a table of contents of the buffer."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((buffer (current-buffer))
	    start end)
	(with-temp-buffer
	  (with-current-buffer buffer
	    ;; Boundary condition: starting on heading:
	    (when (outline-on-heading-p)
	      (outline-back-to-heading)
	      (setq start (point)
		    end (progn (outline-end-of-heading)
			       (point)))
	      (insert-buffer-substring buffer start end)
	      (insert "\n\n")))
	  (let ((temp-buffer (current-buffer)))
	    (with-current-buffer buffer
	      (while (outline-next-heading)
		(unless (outline-invisible-p)
		  (setq start (point)
			end (progn (outline-end-of-heading) (point)))
		  (with-current-buffer temp-buffer
		    (insert-buffer-substring buffer start end)
		    (insert "\n\n"))))))
	  (kill-new (buffer-string)))))))

;;; outline.el ends here
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
		  (if (not (bobp))(backward-char 1))
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
;;;;;;(load "op-hide")

;;;
;;; Blendet alle Zeilen, die ein bestimmes Pattern matchen, aus.
;;; geschrieben am 23.11.1994 von Matthias Kapffer und Stefan Walther
;;;



;;; ----------------------------------------------------------------------------------------------------
;;; ----------------------------------------------------------------------------------------------------
;;; ----------------------------------------------------------------------------------------------------
;;; Benutzerdefiniertes (für andere Regexpressions -1 in -2 und "\\*+E|\\*+FW" austauschen)
;;;


(defun op-hide-at-end-of-show ()
;;
;; which headline signs (i.g.  ***E ) have to be shown?
;;
;; 		   ["Toggle Everything In Region [...]" ausblend-0 t]
;; 		   ["Toggle *E[rledigtes] / *F[ällt]W[eg] in region " ausblend-1 t]
;; 		   ["Toggle *S[tändiges] / *W[iederkehrendes] in region " ausblend-2 t]
;; 		   ["Toggle *V[erschobenes] in region " ausblend-3 t]
;; 		   ["Toggle *I[n]B[earbeitung] in region " ausblend-4 t]
;; 		   ["Toggle *T[ermin] in region " ausblend-6 t]
;; 		   ["Toggle *R[essourcen] in region " ausblend-5 t]
;; 		   ["Markierung   *D   für  D[ringendes] \t(ohne Funktion)" nil t]
    (if ausblend-toggle-status-1
	(hide-lines-matching-regexpr-end-mainheadlines "\\*+E\\|\\*+FW" (point-min) (point-max)))
    (if ausblend-toggle-status-2
	(hide-lines-matching-regexpr-end-mainheadlines "\\*+W\\|\\*+S" (point-min) (point-max)))
    (if ausblend-toggle-status-3
	(hide-lines-matching-regexpr-end-mainheadlines "\\*+V" (point-min) (point-max)))
    (if ausblend-toggle-status-4
	(hide-lines-matching-regexpr-end-mainheadlines "\\*+I" (point-min) (point-max)))
    (if ausblend-toggle-status-5
	(hide-lines-matching-regexpr-end "[ \t]*#+R" (point-min) (point-max)))
    (if ausblend-toggle-status-6
	(hide-lines-matching-regexpr-end "[ \t]*#+T" (point-min) (point-max))))


(defvar ausblend-toggle-status-1 nil)
(make-local-variable 'ausblend-toggle-status-1)
(defun ausblend-1 ()
  (interactive)
  (if xemacsp (setq mark-active (mark)))
  (hide-lines-matching-regexpr-mainheadlines "\\*+E\\|\\*+FW" 'ausblend-toggle-status-1
					     (if mark-active (region-beginning)(point-min))
					     (if mark-active (region-end)(point-max))))
(define-key outline-mode-prefix-map "1" 'ausblend-1)


(defvar ausblend-toggle-status-2 nil)
(make-local-variable 'ausblend-toggle-status-2)
(defun ausblend-2 ()
  (interactive)
  (if xemacsp (setq mark-active (mark)))
  (hide-lines-matching-regexpr-mainheadlines "\\*+W\\|\\*+S" 'ausblend-toggle-status-2
			       (if mark-active (region-beginning)(point-min))
			       (if mark-active (region-end)(point-max))))
(define-key outline-mode-prefix-map "2" 'ausblend-2)


(defvar ausblend-toggle-status-3 nil)
(make-local-variable 'ausblend-toggle-status-3)
(defun ausblend-3 ()
  (interactive)
  (if xemacsp (setq mark-active (mark)))
  (hide-lines-matching-regexpr-mainheadlines "\\*+V" 'ausblend-toggle-status-3
			       (if mark-active (region-beginning)(point-min))
			       (if mark-active (region-end)(point-max))))
(define-key outline-mode-prefix-map "3" 'ausblend-3)


(defvar ausblend-toggle-status-4 nil)
(make-local-variable 'ausblend-toggle-status-4)
(defun ausblend-4 ()
  (interactive)
  (if xemacsp (setq mark-active (mark)))
  (hide-lines-matching-regexpr-mainheadlines "\\*+I" 'ausblend-toggle-status-4
			       (if mark-active (region-beginning)(point-min))
			       (if mark-active (region-end)(point-max))))
(define-key outline-mode-prefix-map "4" 'ausblend-4)


(defvar ausblend-toggle-status-5 nil)
(make-local-variable 'ausblend-toggle-status-5)
(defun ausblend-5 ()
  (interactive)
  (if xemacsp (setq mark-active (mark)))
  (hide-lines-matching-regexpr "[ \t]*#+R" 'ausblend-toggle-status-5
			       (if mark-active (region-beginning)(point-min))
			       (if mark-active (region-end)(point-max))))
(define-key outline-mode-prefix-map "5" 'ausblend-5)


(defvar ausblend-toggle-status-6 nil)
(make-local-variable 'ausblend-toggle-status-6)
(defun ausblend-6 ()
  (interactive)
  (if xemacsp (setq mark-active (mark)))
  (hide-lines-matching-regexpr "[ \t]*#+T" 'ausblend-toggle-status-6
			       (if mark-active (region-beginning)(point-min))
			       (if mark-active (region-end)(point-max))))
(define-key outline-mode-prefix-map "6" 'ausblend-6)


(defun set-show-ausblend-all ()
  (interactive)
  (if xemacsp (setq mark-active (mark)))  
  (setq ausblend-toggle-status-1 t)
  (hide-lines-matching-regexpr-mainheadlines "\\*+E\\|\\*+FW" 'ausblend-toggle-status-1
					     (if mark-active (region-beginning)(point-min))
					     (if mark-active (region-end)(point-max)))
  (setq ausblend-toggle-status-2 t)
  (hide-lines-matching-regexpr-mainheadlines "\\*+W\\|\\*+S" 'ausblend-toggle-status-2
					     (if mark-active (region-beginning)(point-min))
					     (if mark-active (region-end)(point-max)))
  (setq ausblend-toggle-status-3 t)
  (hide-lines-matching-regexpr-mainheadlines "\\*+V" 'ausblend-toggle-status-3
					     (if mark-active (region-beginning)(point-min))
					     (if mark-active (region-end)(point-max)))
  (setq ausblend-toggle-status-4 t)
  (hide-lines-matching-regexpr-mainheadlines "\\*+I" 'ausblend-toggle-status-4
					     (if mark-active (region-beginning)(point-min))
					     (if mark-active (region-end)(point-max)))
  (setq ausblend-toggle-status-5 t)
  (hide-lines-matching-regexpr "[ \t]*#+R" 'ausblend-toggle-status-5
			       (if mark-active (region-beginning)(point-min))
			       (if mark-active (region-end)(point-max)))
  (setq ausblend-toggle-status-6 t)
  (hide-lines-matching-regexpr "[ \t]*#+T" 'ausblend-toggle-status-6
			       (if mark-active (region-beginning)(point-min))
			       (if mark-active (region-end)(point-max))))
(define-key outline-mode-prefix-map "0" 'set-show-ausblend-all)




;;;
;;; ----------------------------------------------------------------------------------------------------
;;; ----------------------------------------------------------------------------------------------------
;;; ----------------------------------------------------------------------------------------------------
;;;
;;; Mode-Funktionen



(defun hide-lines-matching-regexpr-mainheadlines (regexpr hidden-toggle-pat1-status startpkt ende)
  (save-excursion
    (let ((before-change-functions nil)(after-change-functions nil))
      (setq selective-display nil)
      (if xemacsp (setq mark-active (mark)))
      (if mark-active (narrow-to-region startpkt ende))
      (goto-char (point-min))
      (if (save-excursion (re-search-forward regexpr nil t))
	  (progn
	    (let ((already-modified (buffer-modified-p)))
	      (goto-char (point-min))
	      (if (symbol-value hidden-toggle-pat1-status)
		  ;; ausklappen  
		  (let ((p1))
		    (while (re-search-forward (concat "\r\\(" regexpr "\\)") nil t)
		      (setq p1 (point))
		      (replace-match "\n\\1" nil nil)
		      ;; bis zur naechsten Ueberschrift
		      (if (not (re-search-forward "[\n\r]\\*" nil t))
			  (goto-char (point-max)))
		      (backward-char 2)
		      ;; und jetzt alles ersetzen
		      (while (re-search-backward "\r" p1 t)
			(replace-match "\n" nil nil))))
		;; einklappen
		(let ((p1))
		  (while (re-search-forward (concat "\n\\(" regexpr "\\)") nil t)
		    (setq p1 (point))
		    (replace-match "\r\\1" nil nil)
		    ;; bis zur naechsten Ueberschrift
		    (if (not (re-search-forward "[\n\r]\\*" nil t))
			(goto-char (point-max)))
		    (backward-char 2)
		    (while (re-search-backward "\n" p1 t)
		      (replace-match "\r" nil nil))
		    )))
	      (set-buffer-modified-p already-modified))))
      (if xemacsp (setq mark-active (mark)))
      (if mark-active (widen))
      (if window-system
	  (hilit-repaint-command t))
      (setq selective-display t)
      (set hidden-toggle-pat1-status (not (symbol-value hidden-toggle-pat1-status))))))



(defun hide-lines-matching-regexpr-end-mainheadlines (regexpr startpkt ende)
  (let ((before-change-functions nil)(after-change-functions nil))
    (save-excursion
      (goto-char (point-min))
      (if (save-excursion (re-search-forward regexpr nil t))
	  (progn
	    (setq selective-display nil)
	    (let ((already-modified (buffer-modified-p))(tmp))
	      (while (re-search-forward (concat "\n\\(" regexpr "\\)") nil t)
		(replace-match "\r\\1" nil nil)
		(if (< (point-min) (match-beginning 0))
		    (setq tmp (- (match-beginning 0) 1))
		  (setq tmp (match-beginning 0)))
		(if (re-search-forward "[\n\r]\\*" nil t)
		    nil
		  (goto-char (point-max)))
		(backward-char 3)
		(while (re-search-backward "\n" tmp t)
		  (replace-match "\r" nil nil)))
	      (set-buffer-modified-p already-modified))
	    (setq selective-display t))))))




(defun hide-lines-matching-regexpr (regexpr hidden-toggle-pat1-status startpkt ende)
  (let ((before-change-functions nil)(after-change-functions nil))
    (save-excursion
      (setq selective-display nil)
      (goto-char (point-min))
      (if xemacsp (setq mark-active (mark)))
      (if mark-active (narrow-to-region startpkt ende))
      (if (save-excursion (re-search-forward regexpr nil t))
	  (progn
	    (let ((already-modified (buffer-modified-p)))
	      (if (symbol-value hidden-toggle-pat1-status)
		  (while (re-search-forward (concat "\r\\(" regexpr "\\)") nil t)
		    (replace-match "\n\\1" nil nil))
		(while (re-search-forward (concat "\n\\(" regexpr "\\)") nil t)
		  (replace-match "\r\\1" nil nil)))
	      (set-buffer-modified-p already-modified))))
      (if xemacsp (setq mark-active (mark)))
      (if mark-active (widen))
      (if window-system
	  (hilit-repaint-command t))
      (setq selective-display t)
      (set hidden-toggle-pat1-status (not (symbol-value hidden-toggle-pat1-status))))))



(defun hide-lines-matching-regexpr-end (regexpr startpkt ende)
  (let ((before-change-functions nil)(after-change-functions nil))
    (save-excursion
      (goto-char (point-min))
      (if (save-excursion (re-search-forward regexpr nil t))
	  (progn
	    (setq selective-display nil)
	    (let ((already-modified (buffer-modified-p)))
	      (while (re-search-forward (concat "\n\\(" regexpr "\\)") nil t)
		(replace-match "\r\\1" nil nil))
	      (set-buffer-modified-p already-modified))
	    (setq selective-display t))))))





;;;
;;; Mache Report-File: Alle Ellipsen werden in einen extra Buffer geschaufelt und vernichtet, zusätzlich
;;; noch sonst ein C-artige Kommentare und (find-file-other-window ... ...)
;;;
(defun make-report ()
  (interactive)
  (save-excursion
    (if xemacsp (setq mark-active (mark)))
    (kill-all-this-regexpr "\r.*"
			   (if mark-active (region-beginning)(point-min))
			   (if mark-active (region-end)(point-max)))
    (other-window 1)
    (mark-whole-buffer)
    (remove-simple-re-comments 1 "\n[ \t]*(find-file-other-window" ")[ \t]*\n" )
    (mark-whole-buffer)
    (remove-simple-re-comments 1 "(find-file-other-window" ")" )
    (mark-whole-buffer)
    (remove-simple-re-comments 1 "\n[ \t]*(insert-file" ")[ \t]*\n" )
    (mark-whole-buffer)
    (remove-simple-re-comments 1 "(insert-file" ")" )
    (if (boundp 'perlIsCallableFromEmacs)
	(if perlIsCallableFromEmacs 
	    (progn
	      (mark-whole-buffer)
	      (remove-/*-comments 1))))
    (other-window -1)))



(defun kill-ellipses ()
  (interactive)
  (save-excursion
    (if xemacsp (setq mark-active (mark)))
    (kill-all-this-regexpr "\r.*"
			   (if mark-active (region-beginning)(point-min))
			   (if mark-active (region-end)(point-max)))))


(defun kill-all-this-regexpr (regexpr startpkt ende)
  (save-excursion
    (setq selective-display t)
    (let ((already-modified (buffer-modified-p)))
      (copy-to-buffer "Removed--still_to_save" startpkt ende)
      (split-window-vertically)
      (other-window 1)
      (switch-to-buffer "Removed--still_to_save")
      (replace-regexp (concat "\n" regexpr) "")
      (replace-regexp regexpr "")
      (other-window -1)
      (set-buffer-modified-p already-modified))))





(defun show-ifdefs-in-region ()
  (interactive)
  (if xemacsp (setq mark-active (mark)))
  (show-ifdefs-in-region-sub (if mark-active (region-beginning)(point-min))
			 (if mark-active (region-end)(point-max)))
  (end-of-show))
(define-key (current-local-map) "\M-c\M-s" 'show-ifdefs-in-region)

(defun show-ifdefs-in-region-sub (startpkt ende)
  (save-excursion
    (setq selective-display nil)
    (let ((already-modified (buffer-modified-p))(before-change-functions nil)(after-change-functions nil))
      (narrow-to-region startpkt ende)
      (goto-char (point-min))
      (show-ifdefs)
      (widen)
      (setq selective-display t)
      (set-buffer-modified-p already-modified))))




(defun hide-ifdefs-in-region ()
  (interactive)
  (if xemacsp (setq mark-active (mark)))
  (hide-ifdefs-in-region-sub (if mark-active (region-beginning)(point-min))
			 (if mark-active (region-end)(point-max))))
(define-key (current-local-map) "\M-c\M-h" 'hide-ifdefs-in-region)

(defun hide-ifdefs-in-region-sub (startpkt ende)
  (save-excursion
    (setq selective-display t)
    (let ((already-modified (buffer-modified-p))(before-change-functions nil)(after-change-functions nil))
      (narrow-to-region startpkt ende)
      (goto-char (point-min))
      (show-ifdefs)
      (hide-ifdefs)
      (widen)
      (setq selective-display t)
      (set-buffer-modified-p already-modified))))





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
;;;;;;(load "op-crypt")




(setq crypt-encryption-type 'des)
(setq crypt-confirm-password t)
(setq crypt-encrypted-disable-auto-save nil)
(setq crypt-encoded-disable-auto-save nil)
(require 'crypt++)


(defun find-file-crypted (filename &optional force)
  "Edit file FILENAME.
Switch to a buffer visiting file FILENAME *without* extention .crypt,
creating one if none already exists.
  Filters under certain conditions Logbuch-Buffer-Files, prefix argument
forces loading in case of refusion."
  (interactive "FFind file: ")
  ;;
  ;; Überprüfung, ob das crypt-Kommando auf diesem Rechner ist, wenn nein, dann Fehlermeldung
  (switch-to-buffer "*ASYNC*")
  (shell-command (concat " test -n \"`type -path "
			 (prin1-to-string crypt-encryption-type)
			 "`\" && echo yes") t)
  (if (not (looking-at "yes"))
      (progn
	(kill-buffer (current-buffer))
	(error "crypt command not on this machine"))
    (kill-buffer (current-buffer)))
  ;;
  ;;
  (string-match "\\(.*/\\)*" (prin1-to-string filename t))
  (let* ((save-string-tmp (match-end 0))
	 (end-of-namestring (string-match "\\.crypt" (prin1-to-string filename t)))
	 (new-buffer-name (substring (prin1-to-string filename t) save-string-tmp end-of-namestring)))
    (if (get-buffer new-buffer-name)
	(switch-to-buffer new-buffer-name)
      (if (get-buffer (substring (prin1-to-string filename t) save-string-tmp))
	  (error "Buffer error: Only shaddow buffer %s is there: Please correct (i.g. by deleting this buffer ?)" filename)
	(if (and (boundp 'logbuch-zeitobjekt)
		 (boundp 'logbuch-projectdoc-buffer)
		 (boundp 'logbuch-buffer))
	    ;; angeforderter File ist nicht der Logbuch-File
	    (if (not (string= (expand-file-name (file-name-nondirectory filename))
			      (expand-file-name (file-name-nondirectory logbuch-projectdoc-buffer))))
		(switch-to-buffer (find-file-noselect filename))
	      ;; falls doch: der Logbuch-buffer war noch nicht aufgerufen (logbuch-buffer==nil)
	      (if (not logbuch-buffer)
		  (switch-to-buffer (find-file-noselect filename))
		;; --- und hier wird ignoriert falls kein Prefix-Argument <> 1 vorliegt
		(if (not (string= (prin1-to-string (prefix-numeric-value force))
				  "1"))
		    (switch-to-buffer (find-file-noselect filename)))
		))
	  (switch-to-buffer (find-file-noselect filename)))))))



(defun save-buffer-crypted ()
  (interactive)
  (if (buffer-modified-p)
      
      ;; Dieser Absatz: Wechselwirkung aus dem File op-includer.el (Nachprüfen, ob alle #include-Statements ausgelagert sind)
      (save-excursion
	;;
	;; Überprüfung, ob das crypt-Kommando auf diesem Rechner ist, wenn nein, dann Fehlermeldung
	(switch-to-buffer "*ASYNC*")
	(shell-command (concat " test -n \"`type -path "
			       (prin1-to-string crypt-encryption-type)
			       "`\" && echo yes") t)
	(if (not (looking-at "yes"))
	    (progn
	      (kill-buffer (current-buffer))
	      (error "crypt command not on this machine"))
	  (kill-buffer (current-buffer)))
	;;
	;;
	(goto-char (point-min))
	(if (search-forward "end_include_statement" nil t)
	    (if (not
		 (y-or-n-p "Sie haben registriert, dass es noch nicht ausgelagerte #include-Files in diesem File gibt? "))
		(error "Speichervorgang abgebrochen")))
	(let* ((current-buf (prin1-to-string (current-buffer) t))
	       (newname (concat current-buf ".crypt")))
	  (switch-to-buffer newname t)
	  (toggle-read-only -1)
	  (erase-buffer)
	  (switch-to-buffer current-buf)
	  (widen)
	  (copy-to-buffer newname (point-min) (point-max))
	  (switch-to-buffer newname t)
	  (goto-char (point-min))
	  (replace-regexp "\r" "\n")
	  (crypt-encrypted-mode 1)
	  (auto-save-mode 0)
	  (if (file-exists-p (concat newname "~"))
	      (copy-file (concat newname "~") (concat newname ".bak") t))
	  (message "This might take a short while...")
	  (write-file newname)
	  (auto-save-mode 1)
	  (toggle-read-only 1)
	  (switch-to-buffer current-buf)
	  (set-buffer-modified-p nil)))))



(defun write-file-crypted (name &optional noninteractive)
;;;
;;; if you are changing something here: keep attention and look at the file
;;; op-project.crypt.el
;;;
  (interactive
   (list (if buffer-file-name
	     (read-file-name "Write file (without .crypt): "
			     nil nil nil nil)
	   (read-file-name "Write file (without .crypt): "
			   (cdr (assq 'default-directory
				      (buffer-local-variables)))
			   nil nil (buffer-name)))))
  (require 'op-crypt "op-crypt")
  ;;
  ;; Überprüfung, ob das crypt-Kommando auf diesem Rechner ist, wenn nein, dann Fehlermeldung
  (switch-to-buffer "*ASYNC*")
  (shell-command (concat " test -n \"`type -path "
			 (prin1-to-string crypt-encryption-type)
			 "`\" && echo yes") t)
  (if (not (looking-at "yes"))
      (progn
	(kill-buffer (current-buffer))
	(error "crypt command not on this machine"))
    (kill-buffer (current-buffer)))
  ;;
  ;;
  ;; Dieser Absatz: Wechselwirkung aus dem File op-includer.el (Nachprüfen, ob alle #include-Statements ausgelagert sind)
  (save-excursion
    (goto-char (point-min))
    (if (search-forward "end_include_statement" nil t)
	(if (not
	     (y-or-n-p "Sie haben registriert, dass es noch nicht ausgelagerte #include-Files in diesem File gibt? "))
	  (error "Speichervorgang abgebrochen")))
    (let* ((newname (concat name ".crypt")) (current-buf (current-buffer)) (position (point))
	   (fnndn (file-name-nondirectory name)) (fnndnn (file-name-nondirectory newname)))
      (if (and (not (string= (buffer-name) fnndn))
	       (get-buffer fnndn))
	  (if (y-or-n-p "buffer alredy exists: overwrite ? ")
	      (progn
		(kill-buffer fnndn)
		(if (get-buffer fnndnn)
		    (kill-buffer fnndnn)))
	    (error "canceling function \"write-file-crypted\"")))
      (switch-to-buffer fnndnn t)
      (erase-buffer)
      (switch-to-buffer current-buf)
      (widen)
      (if (file-exists-p (concat newname "~"))
	  (copy-file (concat newname "~") (concat newname ".bak") t))
      (copy-to-buffer fnndnn (point-min) (point-max))
      (switch-to-buffer fnndnn t)
      (goto-char (point-min))
      (replace-regexp "\r" "\n")
      (crypt-encrypted-mode 1)
      (auto-save-mode 0)
      (write-file newname)
      (auto-save-mode 1)
      (if (not (string= (buffer-name current-buf) fnndn))
	  (progn
	    (let* ((bfn (buffer-file-name)) (bftn nil))
	      (if (fboundp 'buffer-file-truename)
		  (setq bftn (buffer-file-truename)))
	      (switch-to-buffer current-buf)
	      (set-buffer-modified-p nil)
	      (copy-to-buffer fnndn (point-min) (point-max))
	      (switch-to-buffer fnndn)
	      (set-buffer-modified-p nil)
	      (setq buffer-file-name
		    (substring bfn 0 (string-match "\\.crypt$" bfn)))
	      (if bftn
		  (setq buffer-file-truename
			(substring bftn 0 (string-match "\\.crypt$" bftn)))))
	    ;; Lösche den Orginal-Schattenbuffer (falls da)
	    (if (get-buffer (concat (buffer-name current-buf) ".crypt"))
		(kill-buffer (concat (buffer-name current-buf) ".crypt")))
	    ;; Lösche den Orginal-Buffer
	    (kill-buffer current-buf)
	    (goto-char position))
	(let ((bfn (buffer-file-name)) (bftn nil))
	  (if (fboundp 'buffer-file-truename)
	      (setq bftn (buffer-file-truename)))
	  (switch-to-buffer current-buf)
	  (set-buffer-modified-p nil)
	  (setq buffer-file-name
		(substring bfn 0 (string-match "\\.crypt$" bfn)))
	  (if bftn
	      (setq buffer-file-truename
		    (substring bftn 0 (string-match "\\.crypt$" bftn))))))
      (setq use-keymap-for-crypted-files t)
      (if (not noninteractive)
	  (progn
	    (project-mode)
	    (if window-system
		(hilit-repaint-command t)))))))



(defun write-region-crypted (name crypted &optional kill)
  (interactive
   (list
    (let ((transient-mark-mode transient-mark-mode))
      (setq transient-mark-mode t)
      (exchange-point-and-mark)
      (exchange-point-and-mark)
      (if buffer-file-name
	  (read-file-name "Write region to file (without extention .crypt): "
			  nil nil nil nil)
	(read-file-name "Write region to file (without extention .crypt): "
			(cdr (assq 'default-directory
				   (buffer-local-variables)))
			nil nil (buffer-name))))
    (yes-or-no-p "Do you want to write the file crypted ?	" )))
  ;;
  ;; Überprüfung, ob das crypt-Kommando auf diesem Rechner ist, wenn nein, dann Fehlermeldung
  (switch-to-buffer "*ASYNC*")
  (shell-command (concat " test -n \"`type -path "
			 (prin1-to-string crypt-encryption-type)
			 "`\" && echo yes") t)
  (if (not (looking-at "yes"))
      (progn
	(kill-buffer (current-buffer))
	(error "crypt command not on this machine"))
    (kill-buffer (current-buffer)))
  ;;
  ;;
  (if xemacsp (setq mark-active (mark)))
  (if kill
      (kill-region (if mark-active (region-beginning)(point-min))
		   (if mark-active (region-end)(point-max)))
    (copy-region-as-kill (if mark-active (region-beginning)(point-min))
			 (if mark-active (region-end)(point-max))))
  (save-excursion
    (switch-to-buffer (generate-new-buffer "tmp"))
    (yank)
    (if crypted
	(progn
	  (write-file-crypted name)
	  (kill-buffer-crypted))
      (crypt-encrypted-mode 0)
      (write-file name)
      (kill-buffer (current-buffer)))))

      
(defun kill-buffer-crypted (&optional dummy)
  (interactive "sTo kill current buffer please hit [Return]")
  (let ((current-buf (current-buffer)))
    (if (boundp 'logbuch-buffer)
	(if (eq current-buf logbuch-buffer)
	    (setq logbuch-buffer nil)))
    (if (get-buffer (concat (prin1-to-string current-buf t) ".crypt"))
	(kill-buffer (concat (prin1-to-string current-buf t) ".crypt")))
    (kill-buffer current-buf)))


(defun ask-for-crypt-save (&optional yesorno)
  (interactive "sDo you want to save the file crypted ? (yes or no) [no]: ")
  (if (string= yesorno "yes")
      (progn
	(save-buffer-crypted)
	(message (concat "Maybe there is still an uncrypted file --- be careful !" )))
    (crypt-encrypted-mode 0)
    (save-buffer-with-control)))


(defun ask-for-crypt-write (name crypted)
  (interactive
   (list (if buffer-file-name
	     (read-file-name "Write to file (without extention .crypt): "
				 nil nil nil nil)
	   (read-file-name "Write to file (without extention .crypt): "
			   (cdr (assq 'default-directory
				      (buffer-local-variables)))
			   nil nil (buffer-name)))
	 (yes-or-no-p "Do you want to write the file crypted ?	" )))
  (if crypted
      (progn
	(write-file-crypted name)
	(setq use-keymap-for-crypted-files t))
    (crypt-encrypted-mode 0)
    (write-file name)
    (setq use-keymap-for-crypted-files nil)))


(defun recover-op-file (file)
  "Visit file FILE, but get contents from its last auto-save file."
  (interactive "FRecover current OP file [ please type return or ^G ]: ")
  (setq file (expand-file-name file))
  (if use-keymap-for-crypted-files
      (setq filecrypt (concat (file-relative-name file) ".crypt"))
    (setq filecrypt (file-relative-name file)))
  (if (auto-save-file-name-p (file-name-nondirectory filecrypt))
      (error "%s is an auto-save file" filecrypt))
  (let ((file-name (let ((buffer-file-name filecrypt))
		     ;; hier so gemacht wegen eines Bugs mit dem AFS-File-System
		     (make-auto-save-file-name))))
    (cond ((if (file-exists-p filecrypt)
	       (not (file-newer-than-file-p file-name filecrypt))
	     (not (file-exists-p file-name)))
	   (error "Auto-save file %s not current or not newer than current file" file-name))
	  ((save-window-excursion
	     (if (not (eq system-type 'vax-vms))
		 (with-output-to-temp-buffer "*Directory*"
		   (buffer-disable-undo standard-output)
		   (call-process "ls" nil standard-output nil
				 (if (file-symlink-p file) "-lL" "-l")
				 file file-name)))
	     (yes-or-no-p (format "Recover auto save file %s? " file-name)))
	   (switch-to-buffer (file-name-nondirectory file))
	   (let ((buffer-read-only nil))
	     (erase-buffer)
	     (insert-file-contents file-name t)
	     ;;
	     ;; falls der File noch den auto-file-name-Namen trägt: ändere den zurück
	     (if (auto-save-file-name-p buffer-file-name)
		 (progn
		   (string-match "^#\\(.*\\)#$" buffer-file-name)
		   (setq buffer-file-name (match-string 1))))
	     ;;
	     (if window-system
		 (hilit-repaint-command t)))
	   (after-find-file nil nil t))
	  (t (error "Recover-file cancelled.")))))



;---------------------------------------------------------------------------------------------------

;;;
;;; Aktivieren der jeweiligen BUFFER-lokalen Keymap durch eine Minor-Mode-List
;;;
;;; ist die Variable use-keymap-for-crypted-files t, wird eine zusätzliche Keymap aktiviert. Damit kann
;;; erreichen, daß eine Key-Belegung Buffer-Lokal ist: use-keymap-for-crypted-files muß Buffer-Lokal sein.
;;; Hier wird das dafür verwendet, daß gekryptete und ungekryptete Files unterschiedliche Speichermecha-
;;; nismen verwenden. Dieser Code hier wäre ohne die Hilfe von Matthias Kapffer nicht zum Laufen gekommen.
;;;

(local-set-key "\C-x\C-w" 'ask-for-crypt-write)
(local-set-key "\C-x\C-s" 'save-buffer-with-control)

(defvar project-mode-crypted-keymap (make-sparse-keymap))
(define-key project-mode-crypted-keymap "\C-x\C-s" 'save-buffer-crypted)
(define-key project-mode-crypted-keymap "\C-xk" 'kill-buffer-crypted)
(define-key project-mode-crypted-keymap "\C-x\C-f" 'find-file-crypted)

(setq minor-mode-map-alist
      (cons
       (cons 'use-keymap-for-crypted-files project-mode-crypted-keymap)
       minor-mode-map-alist))


(provide 'op-crypt)





;; zur Kontrolle
(message "4")


;;; ----------------------------------------------------------------------------------------
;;; ----------------------------------------------------------------------------------------
;;; ----------------------------------------------------------------------------------------



;;;
;;;
;;; SCHNITTSTELLENPROGRAMMIERUNGEN ZU ANDEREN FILES
;;;
;;;

;;;;;;;includeforelc!
;;;;;;(load "op-xfig&corell-analyser")




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


;;;;;;;includeforelc!
;;;;;;(load "op-mpx-analyser")





(defun mpx-msp-text-extractor (withcomments current-buff)
  "Extracts from the Microsoft Project 4.0 mpx-file the text
entries; with positive Argument: inserts comment also,
with - as Argument: Outline-Outfit without comments,
negative number as argument: Outline-Outfit with comments"
  (if (eq withcomments '-)
      (setq withcomments '(0)))
  (if (listp withcomments)
      (setq withcomments (car withcomments)))
  ;; zuerst: reduzieren des MPX-Files auf die wesentichen Zeilen: 60,70.71
  (if withcomments
      (if (> (abs withcomments) 0)
	  (occur "^60;\\|^70;\\|^71;")
	(occur "^60;\\|^70;"))
    (occur "^60;\\|^70;"))
  (switch-to-buffer "*Occur*")
  (toggle-read-only t)
  (toggle-read-only)
  (load "op-indent")
  (beginning-of-buffer)
  (kill-line)
  
  ;; Aufbau: ersten Zeile beschreibt die Textfelder, alle weiteren den Aufbau des Files, beides im .cvs-Format
  
  ;; Initialisiere die Variablen, die für die Eintragung verwendet werden
  (let ((meilenstein "") (prozentfertig "") (vorgangsname "") (anfang)(ende)
	(vorgangsnummer 0) (kontaktperson "") (unterprojekt "") (einrueckung ""))
    (goto-char (point-min))
    (while (re-search-forward ":70;" nil t) ; laufe den File ab
      ;; Projektzeilen mit "70" bearbeiten
      (progn
	
	;;
	;;
	;; Setze die Variable "Einmalige Nr." in Zeile 60 zu "Vorgangsnummer"
	(setq vorgangsnummer     (finde_Feld "Einmalige Nr."    ":60;"))
	
	;;
	;;
	(setq vorgangsname	 (finde_Feld "Name"		":60;"))
	
	;;
	;;
	;; für die Schachtelungstiefe: die Gliederungsebene
	(setq einrueckung	 (finde_Feld "Gliederungsebene"	":60;"))
	
	;;
	;;
	(setq dauer		 (finde_Feld "Dauer"		":60;"))
	;; Wenn Meilenstein, dann setze String
	(if (or (string= dauer "0d")
		(string= dauer "0t")) (setq meilenstein "[Meilenstein]  ") (setq meilenstein ""))
	
	;;
	;;
	;; für die Prozentzahlen, wieviel erledigt ist
	(setq prozentfertig	 (finde_Feld "% Abgeschlossen"	":60;"))
	
	;;
	;;
	(setq kontaktperson	 (finde_Feld "Kontaktperson"	":60;"))
	
	;;
	;;
	;; zum Ermitteln, ob hier ein Unterprojektfile eingebaut wurde
	(setq unterprojekt       (finde_Feld "Teilprojektdatei" ":60;"))
	(if unterprojekt
	    (if (not (string= unterprojekt ""))
		(setq unterprojekt (concat "|| [Unterprojekt]  "
					   unterprojekt
					   " || ")))
	  (setq unterprojekt ""))
	
	;;
	;;
	;; ermitteln des Anfangs- und des Endtermines
	(setq anfang	         (finde_Feld "Anfang"		":60;"))
	(setq ende               (finde_Feld "Ende"		":60;"))
	
	
	;;
	;; =============================================================================
	;;
	;; Aufbau des Projektfiles - Zeile für Zeile
	
	;; Vorgangsname
	(beginning-of-line)
	(kill-line t)			; alles was in der Zeile ist löschen
	(insert "\n")
	(backward-char 1)
	;;
	(if vorgangsname (insert vorgangsname))
	;;
	;; setze Unterprojekt ein
	(beginning-of-line)
	(insert unterprojekt)
	;;
	;; setze Meilenstein ein
	(beginning-of-line)
	(insert meilenstein)
	;;
	;; als Merker zur späteren Verwendung: die Einrückungstiefe als extra Punkt
	(end-of-line)
	(if einrueckung 
	    (insert "  :Einrückung:" einrueckung ":")
	  (insert "  :Einrückung:-1:"))
	
	;;---
	;; setze unter der Hauptzeile eine Beschreibungszeile ein
	(end-of-line)
	(insert "\n {")
	(if vorgangsnummer (insert "Nr." vorgangsnummer))
	;;
	;; setze Prozentzahl ein (nach dem Semikolon)
	(end-of-line)
	(if prozentfertig  (insert "; " prozentfertig " erledigt"))
	;;
	(end-of-line)
	(if anfang (insert "; Anfangstermin: " anfang))
	;;
	(end-of-line)
	(if ende (insert ", fertigzustellen bis: " ende))
	;;
	(end-of-line)
	(insert "}")
	;;
	;;

	(end-of-line)
	(if kontaktperson 
	    (if (not (or (string= kontaktperson "NV")
			 (string= kontaktperson "")))
		(insert "\n {Verantwortlich: " kontaktperson "}")))
	  
	)))				; durch alle 70er-Zeilen durch 
  
  
  
  ;; Kommentarzeilen, wenn gewünscht (d.h. 71er-Zeilen wurden im Occur oben gewünscht!)
  (goto-char (point-min))
  (while (re-search-forward ":71;" nil t)  ; jetzt über die Kommentarzeilen laufen - sofern vorhanden 
    (end-of-line)
    (re-search-backward ":71;" nil t)
    (kill-line 0)
    (delete-char 4)
    (insert "\t"))
  
  ;;;=======================================================================================
  ;;;
  ;;; Hier sollte der Occur folgende Struktur haben: 
  ;;; Hauptzeilen fangen bei Position 1 der jeweiligen Zeile an
  ;;; Die Einrücktiefe (Indentierung/Gliederungsebene) ist am Ende dieser Zeile eingetragen
  ;;; Prozessspezifizierende Merkmale sind, soweit gewünscht, nach einem Freizeichen 
  ;;;        in geschweiften Klammern in extra nachfolgenden Zeilen
  ;;; MPX-Kommentare sind nach einem TAB in einer extra Zeile
  ;;;
  
  (goto-char (point-min))
  (forward-line 1)			; die 60er-Zeile überspringen


  (let ((indent-anzahl 0) (tmp ""))
    (while (re-search-forward "^[^ \t]" nil t) ; so lange es Hauptthemenzeilen gibt
      ;; 
      ;; jetzt hole die Einrückungstiefe aus dem letzten Ausdruck in den Hauptüberschriften: sollte immer vorhanden sein
      (re-search-forward ":Einrückung:" (save-excursion (end-of-line)(point)) nil) ; :Einrückung:-1:
      (setq indent-anzahl (string-to-int (buffer-substring-no-properties (point)
									 (save-excursion (re-search-forward ":"
													    (save-excursion (end-of-line)(point))
													    nil)
											 (1- (point))))))
      ;;
      ;; indent-anzahl hat nun die Einrückungstiefe
      ;;
      
      ;; die Anzahl von * entsprechend der Indentierung eintragen
      (beginning-of-line)
      (insert-char ?* indent-anzahl)
      ;;
      ;; wenn schon zu 100% erledigt: setze noch ein "E" hintendran
      ;; (setzt hier voraus, dass die Beschreibungszeile der Hauptüberschrift folgt)
      (save-excursion
	(next-line 1)
	(if (search-forward "100%" (save-excursion (end-of-line)(point)) t)
	    (setq tmp "E")
	  (setq tmp "")))
      (insert tmp)

      (insert-OP-indents indent-anzahl)
      
      ;; jetzt: gibt es Beschreibungs- und Kommentarzeilen (erkennbar am führenden TAB)? Diese werden indentiert.
      (next-line 1)
      (beginning-of-line)
      (while (looking-at "[ \t]")
	(if (looking-at " ")		;entweder es steht auf einem Freizeichen
	    (progn
	      (insert-OP-indents indent-anzahl)
	      (delete-char 1)
	      (if (= indent-anzahl 1) (insert-char ?  1)))
	  (if (looking-at "\t")		; oder auf einem Tab
	      (progn
		(delete-char 1)		; Tab löschen
		(if (and (not (looking-at "-"))
			 (not (looking-at "==>")))
		    (insert "§§§- ")	; §§§-Zeichen als Markierung 
		  (insert "§§§"))
		(beginning-of-line)
		(insert-OP-indents indent-anzahl)
		(if (= indent-anzahl 1) (insert-char ?  1)))))
	(next-line 1)
	(beginning-of-line))

      ))

  ;; die oberste Zeile (60er) löschen
  (goto-char (point-min))
  (kill-line 1)
  (newline)

    ;;;=======================================================================================
  ;; jetzt: Nachbearbeitung des Files


  ;; zuerst: Kommentarzeilen verarbeiten
  ;;
  (goto-char (point-min))
  ;; umbrechen
  (while (re-search-forward "§§§" nil t)
    (end-of-line)
    ;; Zeilen des Kommentars wiederherstellen
    (while (re-search-backward "" (save-excursion (beginning-of-line)(point)) t)
      (replace-match "\n" nil nil)
      (backward-char 1)))
  ;; von den Begriffen :Einrückung: und §§§ reinigen
  (goto-char (point-min))
  (while (re-search-forward ":Einrückung:*.[0-9]+:" nil t)
    (replace-match "" nil nil))
  (goto-char (point-min))
  (while (re-search-forward "§§§" nil t)
    (replace-match "" nil nil))
  ;; nachsehen, welche Zeilen zu lang sind und dann eventuell umbrechen
  (goto-char (point-min))
  (end-of-line)
  ;;
  (while (not (eobp))
    (beginning-of-line)
    ;; gehe zum Zeilenende oder zu Spalte 111 -- umständlich und langsam, jedoch notwendig 
    ;; wegen eventueller Tabulatoren (1 Char = 8 Plätze)
    (while (and (not (eolp))
		(< (current-column) 111))
      (if (not (eolp))(forward-char 1)))
    (if (>= (current-column) 111)
	(if (re-search-backward "[ \t]" (save-excursion (beginning-of-line)(point)) t)
	    (newline)
	  (end-of-line)
	  (if (not (eobp)) (next-line 1)))
      ;; steht jetzt am Ende der Zeile
      (if (not (eobp)) (next-line 1))))

  ;; jetzt noch sauber indentieren
  (goto-char (point-min))
  (end-of-line)
  (re-search-forward "^\\*" nil t)
  (while (not (eobp))
    (beginning-of-line)
    (when (not (looking-at "^\\*")) (insert " ")(beginning-of-line))
    (if (not (bobp))(re-indent-for-tab-command))
    (end-of-line)
    (if (not (eobp))(next-line 1)))

  ;; füge Leerzeilen ein zwischen den einzelnen Paragraphen
  (goto-char (point-min))
  (while (re-search-forward "^\\*" nil t)
    (beginning-of-line)
    (newline)
    (end-of-line))
  
  ;; Aufräumarbeiten
  (goto-char (point-min))
  (setq truncate-lines t)
  ;; Lösche doppelte Newlines heraus
  (trim-buffer)
  (goto-char (point-min))
  (while (re-search-forward "\n\n+" nil t)
    (replace-match "\n\n" nil nil))
  ;;
  (tabify (point-min)(point-max))
  ;;
  ;; Leerzeichen einfügen an das Ende des Files
  (goto-char (point-max))
  (insert-char ?\n 5)
  ;;
  (op-mpx-rehilit)			; BUG
  (mpx-menu-bar)
  ;;
  (delete-window)
  (switch-to-buffer current-buff)
  (split-window-vertically 8)
  (switch-to-buffer "*Occur*")
  (toggle-read-only t)
  (toggle-read-only)
  (rename-buffer "*mpx*" t)
  (local-set-key "\C-c\C-c" 'op-jump-to-line)
  (if (featurep 'xemacs)
      (local-set-key [button2] 'op-jump-to-line-at-mouse-position)
    (local-set-key [mouse-2] 'op-jump-to-line-at-mouse-position))
  (local-set-key "\C-c\C-s" 'xop-sort-reference-lines)
  (local-set-key "q" 'delete-window)
  (beginning-of-buffer)
)

      

(defun insert-OP-indents (indent-anzahl)
  (if (= indent-anzahl 1)
      (insert-char ?  1)
    (insert-char ?\t
		 (- indent-anzahl
		    ;; tab-Weite modulo Anzahl
		    (floor (/ indent-anzahl tab-width))
		    1))))



;;; alter Code zum Aufheben... vielleicht noch gebraucht
;		  (goto-char save-start)
;		  (if (not (looking-at "-\\|[0-9]+\\."))
;		      (progn
;			(insert " ==> ")
;			(setq arrowinserted 5))
;		    (setq arrowinserted 0))
;		  (set-mark (point))
;		  (goto-char (+ save-end arrowinserted))
;		  ;;
;		  (insert-char ?\n 1)
;		  (backward-char 2)
;		  (beginning-of-line)
;		  (if (= (point) save-start)
;		      (force-indent)
;		    (op-indent-region))
;		  (end-of-line)
;		  (setq save-end (point))
;		  ;;
;		  ;; anstatt (fill-paragraph): Umbrechen der zu langen Textzeilen im OP-Modus
;		  (while (and (>= (point) save-start)
;			      (<= (point) save-end))
;		    (if (< (save-excursion
;			     (beginning-of-line)
;			     (save-excursion
;			       (goto-char save-end)
;			       (untabify save-start save-end)
;			       (end-of-line)
;			       (setq save-end (point)))
;			     (forward-char (min fill-column
;						(save-excursion (end-of-line) (current-column))))
;			     (if (not (looking-at "$"))
;				 (progn
;				   (forward-word 1)
;				   (if (not (looking-at "$"))
;				       (forward-char 1))))
;			     (current-column))
;			   (save-excursion
;			     (end-of-line)
;			     (current-column)))
;			(progn
;			  (beginning-of-line)
;			  (save-excursion
;			    (goto-char save-end)
;			    (untabify save-start save-end)
;			    (setq save-end (point)))
;			  (forward-char (min fill-column
;					     (save-excursion (end-of-line) (current-column))))
;			  (if (not (looking-at "$"))
;			      (progn
;				(forward-word 1)
;				(if (not (looking-at "$"))
;				    (forward-char 1))
;				(newline-and-re-indent)
;				(setq save-end (point)))))
;		      (previous-line 1)))
;		  ;; end (fill-paragraph)
;		  ;; Leerzeichen abräumen
;		  (next-line 1))
;	      (next-line 1)))))



(defun op-mpx-rehilit ()
  (interactive)
  (if window-system
      (progn
	(if (x-display-color-p)
	    (progn
	      (hilit-set-mode-patterns
	       'indented-text-mode
	       '(
					;("/\\*" "\\*/" msg-from)   ; wie C-Kommentare
		 ;; Doppelzeilenkombination für golden unterlegt
		 ("/\\*\\|\\*/" nil msg-separator)
		 ("/\\*\\|\\*/" nil msg-quote)

		 ;;	   ("->" "[ \t\r]\\|$" jargon-keyword)
		 ;; ("->" nil label)
		 ;;	   ("->" "[ \t\r]\\|$" string)

		 ;; Dreizeilenkombination für Dunkelgrün
		 ("#[^TR]" ".#\\|[\r\n]" msg-quote)
		 ("#[^TR]" ".#\\|[\r\n]" struct)
		 ("#[^TR]" ".#\\|[\r\n]" msg-quote)

		 ;; Doppelzeilenkombination für golden unterlegt
					;("#T" ".#\\|[\r\n]" msg-separator)
					;("#T" ".#\\|[\r\n]" msg-quote)
		 ("#T" ".#\\|$" jargon-keyword)

		 ("#R" ".#\\|[\r\n]" comment)

		 ("\\<\\(Info\\|INFO\\|info\\)\\>.*" nil struct)

		 ("\\[Meilenstein\\]" nil define)
		 ("|| \\[Unterprojekt\\]" " ||" error)
		 ("#include[ \t]+\"" "\"" define)
		 ("[\n\r]#end_include_statement" nil define)

		 ;; speziell für MPX-MSP
		 ("{Nr\\." "}" goldenrod)
		 ("{Verantwortlich" "}" goldenrod)

		 ("^\\(\\*E\\|\\*FW\\).*" "[\r\n]" summary-deleted) ; Erledigtes in Hauptüberschrift
		 ("^\\(\\*\\*+E\\|\\*+FW\\).*" "[\r\n]" keyword) ; Erledigtes in Unterüberschrift
								 ;(generelle Doku: in op-menus.el)
		 ("^\\(\\*+V\\|\\*+S\\).*" "[\r\n]" include)	 ; Verschobenes und ständig wiederkehrendes
		 ("^\\*+\\-+" "[\r\n]" string)			 ; *---- - Zeilen
		 ("^\\*[^*EFVDSR].*" "[\r\n]" msg-header)	 ; Hauptüberschrift ; F != FW !!!
		 ("^\\*\\*+[^*EFVDSR].*" "[\r\n]" defun)	 ; Unterüberschriften
		 ("^\\*+D" "[\r\n]" error)			 ; Dringendes
		 )))
	  (hilit-set-mode-patterns
	   'indented-text-mode
	   '(
	     ("==>" nil string)
	     ("^\\*+E.*\\|^\\*+FW.*\\|^\\*+V.*" "[\r\n]" keyword)
	     ("#[^#]" ".#\\|[\r\n]" msg-from)
	     ("\\<\\(Info\\|INFO\\|info\\)\\>.*" "[\r\n]" glob-struct)
	     ("^\\*+\\-+" "[\r\n]" msg-from)
	     ("^\\*" "[\r\n]" defun)
	     )))

	;;(setq hilit-face-check nil) ; Fonds werden ja meistens nicht extra geändert, sonst hilft auch noch C-u S-C-l
	(setq hilit-inhibit-rebinding t) ; wenn der Server nur wenige Farben hat, geht sonst manchmal Yank nicht mehr
	(setq hilit-auto-rehighlight 10000) ; Anzahl der automatisch upgedateten Zeilen bei Benutzung des X-Servers

	;; auch der hilit-mode kommt nicht mit dem (setq selective-display t) zurecht
	(setq hilit-auto-rehighlight-fallback '(20000 . 10000))
	(make-variable-buffer-local 'hilit-auto-rehighlight-fallback)

	(hilit-repaint-command t)
	)))


(defun mpx-headlines-only ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\n[ \t]*{.*}" nil t)
      (replace-match "" nil nil))))


(defun mpx-ueberschriften-abgleich ()
  "Im erzeugten *mpx*-Buffer werden alle Überschriftenzeilen
mit denen im Projektfile nach Abgleich durchsucht. Eindeutigkeit
der Zeilen ist Voraussetzung."
  (interactive)
  (let ((ptrpossv))
    (if window-system
	(require 'hilit19.variation))
    (while (re-search-forward "*+" nil t)
      (if (op-jump-to-line)
	  (progn
	    ;; Zeile existiert
	    (while (progn
		     ;; zurück zum mpx-Buffer
		     (other-window 1)
		     (beginning-of-line)
		     (save-match-data
		       (re-search-forward "*+" nil t))
		     (setq ptrpossv (buffer-substring-no-properties (point)(1+ (point))))
		     ;; zurück zum Projekt-Buffer
		     (other-window -1)
		     (beginning-of-line)
		     (save-match-data
		       (re-search-forward "*+" nil t))
		     (if (not (looking-at ptrpossv))
			 (progn
			   (recenter)
			   (other-window 1)
			   (recenter)
			   (if window-system
			       (hilit-region-set-face (save-excursion (beginning-of-line)
								      (point))
						      (save-excursion (end-of-line)
								      (point))
						      'highlight))
			   (other-window -1)
			   (if window-system
			       (hilit-region-set-face (1- (match-beginning 0))(match-end 0) 'highlight))
			   (if (if window-system
				   (y-or-n-p "Abgleichen ? [SPC=y] ")
				 (string= (read-string "Abgleichen ? (y or n): ") "y"))
			       (progn
				 (delete-char 1)
				 (insert ptrpossv)
				 (if (not (looking-at "[ \t]"))
				     (insert-char ?  1))
				 ))
			   (other-window 1)
			   (if window-system
			       (hilit-unhighlight-region (save-excursion (beginning-of-line)
									 (point))
							 (save-excursion (end-of-line)
									 (point))
							 'highlight))
			   (other-window -1)
			   (if window-system
			       (hilit-unhighlight-region (1- (match-beginning 0))(match-end 0)))))
		     ;; wenn mehr als einmal die gleiche Zeile auftaucht
		     (other-window -1)
		     (op-jump-to-line))))
	    ;(other-window -1))
	(if window-system
	    (hilit-region-set-face (save-excursion
				     (beginning-of-line)
				     (point))
				   (save-excursion
				     (end-of-line)
				     (point))
				   'highlight))
	(recenter)
	(other-window 1)
	(recenter)
	(other-window -1)
	(if (if window-system
		(y-or-n-p "Diese Zeile ist nicht im Projektfile: mit NV Markieren ? [SPC=y] ")
	      (string= (read-string
			"Diese Zeile ist nicht im Projektfile: mit NV Markieren ? [SPC=y] ") "y"))
	    (progn
	      (beginning-of-line)
	      (insert "NV: ")
	      (end-of-line))
;	  (if window-system
;	      (hilit-unhighlight-region (save-excursion
;					  (beginning-of-line)
;					  (point))
;					(save-excursion
;					  (end-of-line)
;					  (point))
;					'highlight))
	  (next-line 1))
	)
      (end-of-line))))



(defun mpx-abgleich ()
  "Gleicht *mpx*-Buffer mit Projektkontrollbuffer ab. Beide Buffer müssen in
zwei verschiedenen Rahmen stehen, Aufruf im *mpx*-Buffer"
  (interactive)
  (goto-char (point-min))
  (other-window 1)
  (show-really-everything)
  (goto-char (point-min))
  (other-window -1)
  (mpx-ueberschriften-abgleich)
  (goto-char (point-min))
  (other-window 1)
  (goto-char (point-min))
  (other-window -1)
  (mpx-headlines-only)
  (goto-char (point-min))
  (other-window 1)
  (goto-char (point-min))
  (let ((tmpbuf (current-buffer)))
    (other-window -1)
    (ediff-buffers (current-buffer) tmpbuf)))




(defun mpx-text-extractor (&optional withcomments)
  "Extracts from Microsoft Project the mpx-file at the current
cursor position the text entries; with Argument: inserts comments also"
  (interactive "P")
  (let ((find-file-not-found-hooks find-file-not-found-hooks) (current-buffer (current-buffer)))
    (add-hook 'find-file-not-found-hooks
	      (function (lambda ()
			  (error "Keinen solchen File gefunden: %s" buffer-file-name))))
    (find-file (current-longword))
    (message "MPX nach Standard MS Project 4.0/95/98")
    (mpx-msp-text-extractor withcomments current-buffer))
  )


(defun mpx-import (file &optional withcomments)
  "Extracts from Microsoft Project or CA SuperProject the mpx-file at the current
cursor position the text entries; with Argument: inserts comments also"
  (interactive "fMPX-File: \nnMit Kommentarzeilenverarbeitung (1) oder ohne (0) ?: ")
  (find-file file)
  (message "MPX nach Standard MS Project 4.0/95/98")
  (mpx-msp-text-extractor withcomments (file-name-nondirectory buffer-file-name))
  )
(define-project-mode-key  "\M-m" 'mpx-text-extractor)


(setq selective-display t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun mpx-menu-bar ()
  (defvar mpx-mode-menu-bar-map (make-sparse-keymap))
  ;;
  (define-key mpx-mode-menu-bar-map [MPX]
    (cons "MPX" (make-sparse-keymap "MPX")))
  ;;
  (define-key mpx-mode-menu-bar-map [MPX op-mpx-rehilit]
    '("Neu Einfärben" . op-mpx-rehilit))
  (define-key mpx-mode-menu-bar-map [MPX xop-sort-reference-lines]
    '("Zeilen sortieren" . xop-sort-reference-lines))
  (define-key mpx-mode-menu-bar-map [MPX op-jump-to-line]
    '("Zu Zeile in Projektkontrollfile in anderem Fenster" . op-jump-to-line))
  (define-key mpx-mode-menu-bar-map [MPX mpx-ueberschriften-abgleich]
    '("MPX Überschriftenabgleich mit Projektkontrollfile in anderem Fenster: M-x mpx-ueberschriften-abgleich" . dummy))
  (define-key mpx-mode-menu-bar-map [MPX mpx-headlines-only]
    '("MPX: nur noch Überschriften" . mpx-headlines-only))
  (define-key mpx-mode-menu-bar-map [MPX mpx-abgleich]
    '("MPX-ABGLEICH mit Projektkontrollfile in anderem Fenster: M-x mpx-abgleich" . dummy))
  ;;
  ;;  (define-key mpx-mode-menu-bar-map [MPX mpx-tescht]
  ;;	'("Test" . tescht))
  ;;
  ;;
  (define-key (current-local-map) [menu-bar] mpx-mode-menu-bar-map))



;;(defun tescht ()
;;  (interactive)
;;  (setq window-system nil)
;;    (y-or-n-p "test "))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;
;;;
;;;  Allgemeine Lib zum Bearbeiten von Semikolon-basierten .mpx-Dateien (das ausgelaufene "Microsoft Project Exchange"-Format)
;;;  SMW, 17.9.2004
;;;


;;  Hauptfunktion: finde_Feld



(defun zaehle_Semikolons_rueckwaerts ()
  (let ((Anzahl_gueltiger_Semikolons 0) (svpnt 0) (m1 0)(m2 0) (in nil))
    (save-excursion
      ;; sehe zuerst nach, ob sich der Cursor direkt im String befindet
      (setq svpnt (point))
      (beginning-of-line)
      (while (re-search-forward "\\([^\\]\"\\).*\\([^\\]\"\\)" (save-excursion (end-of-line)(point)) t)
	(backward-char 1)
	(setq m2 (point))
	(re-search-backward "[^\\]\"" (save-excursion (beginning-of-line)(point)) t)
	(setq m1 (1+ (point)))
	(if (and (<= m1 svpnt)
		 (>= m2 svpnt)) ; der Punkt liegt im String!!
	    (setq in t))
	(goto-char m2))
      (goto-char svpnt))
      (if in (progn
	       (re-search-backward "[^\\]\"" (save-excursion (beginning-of-line)(point)) t)
	       (forward-char 1)))
      ;;
      ;;
      (while (re-search-backward ";\\|\\([^\\]\"\\)" (save-excursion (beginning-of-line)(point)) t)  ; sucht rückwärts nach Semikolon oder Anführungsstrich
	(if (looking-at "[^\\]\"") ; steht auf einem Anführungsstrich
	    (progn
	      (re-search-backward "[^\\]\"" (save-excursion (beginning-of-line)(point)) t) ; suche das Pendant zum Anführungsstrich
	      (forward-char 1)
	      (re-search-backward ";" (save-excursion (beginning-of-line)(point)) t)  ; suche den Anführungsstrich vor dem Anführungsstrich
	      (if (save-excursion (forward-char 1) (not (looking-at "\"")))           ; das dem Semikolon folgende Zeichen ist kein Anführungsstrich: dann Fehler
		  (error "Fehler beim Parsen in \"zaehle_Semikolons_rueckwaerts\": beim Stringparsen des CSV-Files Anführungsstrichproblem - siehe CSV-File"))))
	(setq Anzahl_gueltiger_Semikolons (1+ Anzahl_gueltiger_Semikolons)))
      (goto-char svpnt)
      Anzahl_gueltiger_Semikolons))
;Teststring;Teststring;"Test; ; \" ;string";Teststring  xxx  Sollergebnis vom Kreuz: 4 !!!



;Zellenformat:         Test " mit einem ; im String ", mit Semikolon ; pur, mit ;; und mit ;";"
;ergibt CSV-Format:    "Test "" mit einem ; im String "", mit Semikolon ; pur, mit ;; und mit ;"";"""
;das gleiche über 3 Zellen:
;                      "Test "" mit einem ; im String "", mit Semikolon ; pur, mit ;; und mit ;"";""";"Test "" mit einem ; im String "", mit Semikolon ; pur, mit ;; und mit ;"";""";"Test "" mit einem ; im String "", mit Semikolon ; pur, mit ;; und mit ;"";"""

;und die MPX-Datei aus MS Project mit dem gleichen Sting in der Zelle:
;                      70;1;1;"Test  mit einem ; im String , mit Semikolon ; pur, mit ;; und mit ;;";1t;Nein;1;0t;;Mo 20.09.04;Mo 20.09.04;Mo 20.09.04;Mo 20.09.04;Mo 20.09.04;Mo 20.09.04;0t;0t;0ft;0%;NV;NV;NV;NV;So früh wie möglich;;NV;NV;So 19.09.04;0h;0h;0h;0,00 DM;0,00 DM;0,00 DM;0,00 DM;0,00 DM;1;Mittel;Nein;Nein;Nein;;;;;;;;;;;0,00 DM;0,00 DM;0,00 DM;0t;0t;0t;Nein;Nein;Nein;Nein;Nein;Nein;Nein;Nein;Nein;Nein;Nein;0;0;0;0;0;
;d.h. alle Mittelstrings werden im MPX-File gestrippt
 



(defun wandere_Semikolons_vorwaerts (arg)
  (interactive)
  "Setzt den Cursor um die Anzahl von Semikolons nach vorne, die im Argument angegeben werden.
Der Cursor bleibt bei dem Zeichen nach dem Semikolon stehen. Erfolgt keine Positionsveränderung,
ist das Rückgabeargument nil, kann der Cursor nur um weniger verrückt werden als im Argument angegeben, 
wird die maximal mögliche Zahl nach vorne verrückt."
  (if (and (looking-at ";") (not (eolp))) (forward-char 1))
  (let ((Anzahl_gueltiger_Semikolons 0) (pnt (point)))
    (while (> arg Anzahl_gueltiger_Semikolons)
      (progn
	(re-search-forward ";\\|\"" (save-excursion (end-of-line)(point)) t)  ; sucht vorwärts nach Semikolon oder Anführungsstrich
	(backward-char 1)
	(if (looking-at "\"") ; steht auf einem Anführungsstrich
	    (progn
	      (re-search-forward "[^\\]\"" (save-excursion (end-of-line)(point)) t) ; suche das Pendant zum Anführungsstrich
	      (backward-char 1)
	      (re-search-forward ";" (save-excursion (end-of-line)(point)) t)       ; suche den Anführungsstrich vor dem Anführungsstrich
	      (if (save-excursion (backward-char 2) (not (looking-at "\"")))       ; das dem Semikolon vorangehende Zeichen ist kein Anführungsstrich: dann Fehler
		  (error "Fehler beim Parsen in \"zaehle_Semikolons_rueckwaerts\": beim Stringparsen des CSV-Files Anführungsstrichproblem - siehe CSV-File"))))
	(setq Anzahl_gueltiger_Semikolons (1+ Anzahl_gueltiger_Semikolons))
	(forward-char 1)))
    (if (not (save-excursion (backward-char 1) (looking-at ";"))) (backward-char 1))
    (if (= (1- pnt) (point)) 
	(progn (forward-char 1)
	       (setq Anzahl_gueltiger_Semikolons nil)))
    Anzahl_gueltiger_Semikolons))




(defun finde_Feld (FeldStringDefinition BeschreibungszeilenSuchmuster)
  "Macht folgendes: Sucht nach oben mit dem Muster BeschreibungszeilenSuchmuster eine 
Zeile, sucht in dieser Zeile den Beschreibungsstring \"FeldStringDefinition\", zählt
die Feld-Stelle, an der sich dieses Feld befindet, und wandert dann in der aktuellen Zeile
an diese Feldstelle. Der Rückgabewert ist der Inhalt dieser Feldstelle.
Beispielsfile:

Beschreibungszeile;Beschreibungsfeld_1;Beschreibungsfeld_2;Beschreibungsfeld_3;Beschreibungsfeld_4
Inhaltszeile;Inhalt1;Inhalt2;Inhalt3;Inhalt4
Inhaltszeile;AndererInhalt1;AndererInhalt2;AndererInhalt3;AndererInhalt4

Wenn man jetzt z.B. auf \"AndererInhalt1\" steht und 
(finde_Feld \"Beschreibungsfeld_3\" \"Beschreibungszeile\")
aufruft ist der Rückgabewert \"AndererInhalt3\"." 
  (interactive)
  (let ((feldposition 0))
    (save-excursion
      (if (not (re-search-backward BeschreibungszeilenSuchmuster nil t))  ; wenn Beschreibungszeile nicht gefunden wird: nil
	  (progn
            (condition-case nil
	        (message (concat "Beschreibungzeile " BeschreibungszeilenSuchmuster " nicht hochwärts im File gefunden!"))
	     (error (concat "Beschreibungzeile mit Prozentzeichen-Beschreibungszeilen-Suchmuster nicht hochwärts im File gefunden!")))
	    nil)
	;; Beschreibungzeile wurde gefunden
	(beginning-of-line)
	(if (not (search-forward FeldStringDefinition nil t))  ; Muster wurde in der Beschreibungszeile nicht gefunden
	    (progn
              (condition-case nil
	          (message (concat "Suchmuster " FeldStringDefinition "in Beschreibungzeile " BeschreibungszeilenSuchmuster " nicht gefunden!"))
	        (error (concat "Beschreibungzeile mit Prozentzeichen-Beschreibungszeilen-Suchmuster nicht hochwärts im File gefunden!")))
	      nil)
	  ;; Beschreibungszeilenfeld wurde in Beschreibungszeile gefunden
	  (backward-char 1)
	  ;; Bestimme die Position des Feldes
	  (setq feldposition (zaehle_Semikolons_rueckwaerts)))))
    ;; Hier sollte nun die Feldposition bekannt sein
    (if (not (= feldposition 0))
	(progn
	  (beginning-of-line)
	  ;;
          (if (not (save-excursion (beginning-of-line)(looking-at ";")))
	      (wandere_Semikolons_vorwaerts feldposition)
	    (wandere_Semikolons_vorwaerts (1- feldposition)))
	  ;; an dieser Stelle sollte der Cursor am Anfang des Feldinhaltes, der gesucht wird, stehen.
          ;;
	  ;;
	  ;; nun: Ermittlung des Rückgabewertes:
	  ;; Um Probleme konsistent zu halten, wird hier die Funktion wandere_Semikolons_vorwaerts noch einmal genutzt
	  (let ((p1) (p2))
	    (setq p1 (point))
	    (save-excursion
	      (beginning-of-line)
	      ;;
	      (if (not (save-excursion (beginning-of-line)(looking-at ";")))
		  (wandere_Semikolons_vorwaerts (1+ feldposition))
		(wandere_Semikolons_vorwaerts feldposition))
	      ;;
	      (setq p2 (1- (point)))
	      (when (= p1 (1+ p2)) 
		(end-of-line)
		(setq p2 (point))))
            ;; wenn das Feld leer ist, ist der folgende Ausdruck ein Semikolon, sonst der String
	    (if (string= (buffer-substring-no-properties p1 p2 (current-buffer)) ";")
		""
	      (buffer-substring-no-properties p1 p2 (current-buffer)))
	    )))))
;;Beschreibungszeile;Beschreibungsfeld_1;Beschreibungsfeld_2;Beschreibungsfeld_3;Beschreibungsfeld_4
;;Inhaltszeile;Inhalt1;Inhalt2;Inhalt3;Inhalt4
;;Inhaltszeile;AndererInhalt1;AndererInhalt2;AndererInhalt3;AndererInhalt4
;Beschreibungszeile;Beschreibungsfeld_1;Beschreibungsfeld_2;Beschreibungsfeld_3;Beschreibungsfeld_4
;Inhaltszeile;Inhalt1;Inhalt2;Inhalt3;Inhalt4
;Inhaltszeile;AndererInhalt1;AndererInhalt2;AndererInhalt3;AndererInhalt4




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;includeforelc!
;;;;;;(load "op-dia-syncer")

;;; =================================================================================================
;;;
;;;  Teil des OP-Modes: Einsetzen und Synchronisation des OP-Modes mit dem XML-Abspeicherungsfile
;;;  des Programmes Dia  --  damit bei der Projektplanung eine erleichterte h,Ad(Bndige Synchronisation
;;;  zwischen dem Dia-File und dem OP-Mode-File m,Av(Bglich wird
;;;
;;;  Geschrieben 28.9.2004 als Erg,Ad(Bnzung zum OP-Mode von SMW
;;;
;;;

;;;  f,A|(Bnf Nutzerfunktionen werden hier definiert:
;;;   (dia-inserter FILE boolean)
;;;   (dia-inserter-from-file)
;;;   (dia-ID-synchronisator FILE)
;;;   (next-dia-tag)
;;;   (previous-dia-tag)

;;; =================================================================================================




(define-key (current-local-map) "\M-c\M-n\M-n" 'next-dia-tag)
(define-key (current-local-map) "\M-c\M-n\M-p" 'previous-dia-tag)
(define-key (current-local-map) "\M-c\M-n\M-s" 'dia-ID-synchronisator)
(define-key (current-local-map) "\M-c\M-n\M-i" 'dia-inserter-from-file)





(defun dia-string-searcher-forward ()
  "Sucht aus dem XML-File des Programmes Dia die nchste Textpassagen-Position heraus
und gibt diese als (evtl. mehrzeiligen) R$(1vU(Bkgabestring heraus. Wurde keine Passage
gefunden ist die Funktion nil."
  (let ((string-start 0)(string nil))
;;;
;;; <dia:object type="Standard - Text" version="0" id="O12">
;;;
    (if (re-search-forward "\<dia:object[ \t]*type[ \t]*=[ \t]*\".*Text.*\".*\".*\\id[ \t]*=[ \t]*\"O.*\".*\>" nil t) 
					; <- Eindeutige Identifikation von Text-Objekten
	;; bei den restlichen Objekten geht der Code einfach von einer Einhaltung der XML-Struktur aus
	(if (re-search-forward "\<dia:attribute[ \t]*name[ \t]*=[ \t]*\"text\"\>" nil t)
	    (if (re-search-forward "\<dia:composite[ \t]*type[ \t]*=[ \t]*\"text\"\>" nil t)
		(if (re-search-forward "\<dia:attribute[ \t]*name[ \t]*=[ \t]*\"string\"\>" nil t)
		    (if (search-forward "<dia:string>" nil t)
			(progn
			  (setq string-start (point))
			  (search-forward "</dia:string>" nil nil)
			  (goto-char (match-beginning 0))
			  (setq string (buffer-substring-no-properties (1+ string-start)   ; 1+ wegen des dia-typischen Lattenkreuzes
								       (1- (point))))))))) ; 1- wegen des dia-typischen Lattenkreuzes
      string)))


(defun dia-id-searcher-backward ()
  "Nach dem Aufruf von (dia-string-searcher-forward) steht der Cursor auf dem Text. 
Diese Funktion sucht den letzten Text-ID-Tag heraus und gibt ihn zur$(1vU(Bk."
  (let ((idstring ""))
;;;
;;; <dia:object type="Standard - Text" version="0" id="O12">
;;;
    (save-excursion
      (if (re-search-backward "\<dia:object[ \t]*type[ \t]*=[ \t]*\".*Text.*\".*\".*id[ \t]*=[ \t]*\"O.*\".*\>" nil t) 
					; <- Eindeutige Identifikation von Text-Objekten
	  ;; Schnellprogrammierung, da das mit dem Gruppierung hat nicht geklappt, warum auch immer
	  (progn
	    (end-of-line)
	    (re-search-backward "[0-9]" (save-excursion (beginning-of-line)(point)) nil) ; steht auf einem string
	    (while (looking-at "[0-9]")
	      (setq idstring (concat (buffer-substring-no-properties (point) (1+ (point))) idstring))
	      (backward-char 1))
	    ;; idstring enth,Ad(Blt hier die ID des Dia-Strings
	    ))
      idstring)))


(defun op-dia-id-searcher-backward ()
  "Gibt im OP-Mode-File die ID des Dia-Tags heraus - nil wenn nicht innerhalb der Dia-Tags.
Diese Funktion sucht den letzten Text-ID-Tag heraus und gibt ihn zur,A|(Bck."
  (let ((idstring ""))
;;;
;;; <dia id=123> Text </dia>
;;;
    (save-excursion
      (if (re-search-backward "\<dia[ \t]*id[ \t]*=.*\>" nil t)
					; <- Eindeutige Identifikation von Dia-Objekten im OP-Mode
	  ;; Schnellprogrammierung, da das mit dem Gruppierung hat nicht geklappt, warum auch immer
	  (progn
	    (re-search-forward "[0-9]" (save-excursion (end-of-line)(point)) nil) ; gibt einen Fehler wenn kein ID-Tag da ist
	    (backward-char 1)
	    (while (looking-at "[0-9]")
	      (setq idstring (concat idstring (buffer-substring-no-properties (point) (1+ (point)))))
	      (forward-char 1)))))
	    ;; idstring ent,Ad(Bllt hier die ID des Dia-Strings
    idstring))




;(defun dia-inserter (file &optional killemptylines)
;  "Setzt die Textpassagen aus dem XML-File des Programmes Dia (abgespeichert in 
;nicht komprimierter Form!) in OP-Mode-Form an die aktuelle Cursorposition ein.
;Argument FILE f$(1vd(B den Dia-File, Argument KILLEMPTYLINES steuert bei nil das 
;Einsetzen leere Zeilen von Dia zu OP-Mode zu verhindern."
;  (interactive "fDia file: ")
;  (let ((string "") (current-buff (current-buffer)) (idstring "") (tagpoint (point)))
;    (find-file file)
;    (switch-to-buffer (file-name-nondirectory file))
;    (beginning-of-buffer)
;    (if (setq string (dia-string-searcher-forward))
;	(progn
;	  ;; l,Av(Bsche wenn nicht vermieden alle Leerzeilen aus dem String
;	  (if (not killemptylines)
;	      (while (posix-string-match "^[ \t\n\r]+" string)
;		(setq string (concat (substring string 0 (match-beginning 0)) (substring string (match-end 0) nil)))))
;	  (setq idstring (dia-id-searcher-backward))
;	  ;;
;	  (switch-to-buffer current-buff)
;	  (end-of-line)
;	  (newline)
;	  (insert "* Textbereiche aus dem XML-Code des Programmes \"Dia\", File: " file "\n" )
;	  (insert (concat "\n**\t" "<dia id=" idstring ">" string "</dia>" "\n"))
;	  ;; indentiere OP-Mode-gem,Ad_(B
;	  (progn
;	    (setq tagpoint (+ 2 (point)))
;	    (search-backward "<dia" nil t)
;	      (end-of-line)
;	      (next-line 1)
;	      (while (and (not (eobp))
;			  (<= (point) tagpoint))
;		(beginning-of-line)
;		(when (not (looking-at "^\\*")) (insert " ")(beginning-of-line))
;		;;(re-indent-for-tab-command)
;		(if (not (bobp))(re-indent-for-tab-command))
;		(end-of-line)
;		(if (not (eobp))(next-line 1))))
;	  (switch-to-buffer (file-name-nondirectory file))
;	  ;;
;	  ;;
;	  (while (setq string (dia-string-searcher-forward))
;	    (setq idstring (dia-id-searcher-backward))
;	    ;; l,Av(Bsche wenn nicht vermieden alle Leerzeilen aus dem String
;	    (if (not killemptylines)
;		(while (posix-string-match "^[ \t\n\r]+" string)
;		  (setq string (concat (substring string 0 (match-beginning 0)) (substring string (match-end 0) nil)))))
;	    ;;
;	    (switch-to-buffer current-buff)
;	    (insert (concat "\n**\t" "<dia id=" idstring ">" string "</dia>" "\n"))
;	    (switch-to-buffer (file-name-nondirectory file)))
;	  (switch-to-buffer current-buff))
;      (switch-to-buffer current-buff))
;    ))



(defun dia-inserter-from-file ()
"Extracts from the dia file at the current cursor position the text entries"
  (interactive)
  (let (find-file-not-found-hooks find-file-not-found-hooks)
    (add-hook 'find-file-not-found-hooks
	      (function (lambda ()
			  (error "Keinen solchen File gefunden: %s" buffer-file-name))))
    (let ((current-buff (current-buffer)))
      (dia-inserter (current-longword)))))




(defun dia-inserter (file &optional killemptylines)
  "Setzt die ZUS,AD(BZLICHEN Textpassagen aus dem XML-File des Programmes Dia (abgespeichert in 
nicht komprimierter Form!) in OP-Mode-Form an die aktuelle Cursorposition ein.
Argument FILE f$(1vd(B den Dia-File, Argument KILLEMPTYLINES steuert bei nil das 
Einsetzen leere Zeilen von Dia zu OP-Mode zu verhindern."
  (interactive "fDia file: ")
  (let ((string "") (current-buff (current-buffer)) (idstring "") (indent-lines-counter (point)) (already nil))
    (find-file file)
    (switch-to-buffer (file-name-nondirectory file))
    (beginning-of-buffer)
    ;;
    ;; suche den ersten Sting im Dia-File
    (if (setq string (dia-string-searcher-forward))
	(progn
	  ;; l,Av(Bsche wenn nicht vermieden alle Leerzeilen aus dem String
	  (if (not killemptylines)
	      (while (posix-string-match "^[ \t\n\r]+" string)
		(setq string (concat (substring string 0 (match-beginning 0)) (substring string (match-end 0) nil)))))
	  (setq idstring (dia-id-searcher-backward))
	  ;;
	  ;; zur,A|(Bck zum OP-Mode-Buffer, string enth,Ad(Blt den Dia-String, idstring die ID
	  (switch-to-buffer current-buff)
	  (end-of-line)
	  ;;
	  ;; Haupt$(1vT(Berschrift einsetzen
	  (insert "\n* Neue Textbereiche aus dem XML-Code des Programmes \"Dia\", ")
	  (if (fboundp 'insert-date) (progn (insert-date) (insert ", ")) "")
	  (insert "File: " file "\n" )
	  ;;
	  ;;
	  ;; Dia-Inhalte einsetzen
	  (while (progn
		   ;; l,Av(Bsche wenn nicht vermieden alle Leerzeilen aus dem String
		   (if (not killemptylines)
		       (while (posix-string-match "^[ \t\n\r]+" string)
			 (setq string (concat (substring string 0 (match-beginning 0)) (substring string (match-end 0) nil)))))
		   ;;
		   ;; wenn Tag-ID im OP-Mode noch nicht vorhanden ist, dann setze den String ein
		   ;; - daher hier bestimmen, ob der ID-Tag schon da ist: Variable already
		   (save-excursion
		     (beginning-of-buffer)
		     (if (re-search-forward (concat "\<dia[ \t]*id[ \t]*=[ \t]*" idstring "[ \t]*\>") nil t) ; OP-Mode-Dia-Tag
			 (setq already t)
		       (setq already nil)))
		   
		   ;;
		   ;;

		   (debug)
		   (if (not already)
		       ;;
		       ;; wenn der Dia-Tag noch nicht vorhanden ist, f$(1vY(Be den neu gefundenen als OP-Mode-berschrift ein
		       ;;
		       (progn
			 (insert (concat "\n**\t" "<dia id=" idstring ">" string "</dia>" "\n"))
			 ;; indentiere OP-Mode-gem
			 (progn
			   (setq indent-lines-counter (count-lines (save-excursion (re-search-backward "<dia[ \t]" nil t) (point))
								   (point)))
			   (re-search-backward "<dia[ \t]" nil t)
			   (end-of-line)
			   (next-line 1)
			   ;; laufe nun die Anzahl der Zeilen nach unten ab und indentiere sie:
			   ;; die Variable indent-lines-counter ist der Zeilenzeiler
			   (setq indent-lines-counter (1- indent-lines-counter))
			   (while (and (not (eobp))
				       (<= 0 indent-lines-counter))
			     (beginning-of-line)
			     (when (not (looking-at "^\\*")) (insert " ")(beginning-of-line))
			     (if (not (bobp))(re-indent-for-tab-command))
			     (end-of-line)
			     (if (not (eobp))(next-line 1))
			     (setq indent-lines-counter (1- indent-lines-counter))
			     )
			   (previous-line 1)))
		     ;;
		     ;; und wenn er schon vorhanden ist und der Taginhalt NICHT bereinstimmt, frage nach einer Synchronisation
		     ;;
		     ;; Vergleiche den Tag-Inhalt
		     ;;     zuerst die ID im op-File suchen
		     (switch-to-buffer current-buff)
		     (goto-char (point-min))
		     (if (search-forward (concat "<dia id=" idstring ">") nil t)
			 ;; hat den Tag im op-file gefunden, daher jetzt den Text vergleichen
			 ;; (muss gefunden werden!!)
			 (progn
			   (backward-char 1)
			   (if (save-excursion
				 (search-forward string (save-excursion (re-search-forward "<dia[ \t]" nil t) (point)) t))
			       ;; hat den Text identisch gefunden, alles in Ordnung
			       t
			     ;; wenn nicht: den String zur h,Ad(Bndigen Synchronisation anbieten
			     (dia-ID-synchronisator file)
			     ))
		       
		       (error "op-dia-syncer.el: das sollte aber gefunden werden"))
		     (switch-to-buffer (file-name-nondirectory file))
		     



		     ; (if (string= string diaopsstring



		     ; wenn Tag nicht gleich: h,Ad(Bndige Synchronisation




		     ; Erg,Ad(Bnzung zur h,Ad(Bndigen Synchronisation: a->b, b->a, q f$(1vd(B quit im dia-XML-Buffer




		     )
		   
		   (switch-to-buffer (file-name-nondirectory file))
		   (if (setq string (dia-string-searcher-forward))
		       (progn
			 (setq idstring (dia-id-searcher-backward))
			 (switch-to-buffer current-buff))
		     (switch-to-buffer current-buff)
		     nil)))
	  ))))


;(dia-inserter "c:/temp/Diagramm1.dia" nil)
;(dia-inserter "c:/temp/Diagramm1.dia" t)






(defun dia-ID-synchronisator (file)
  "Sucht, wenn im OP-Mode-File an einer <dia id=.*>-Passagen stehend, den XML-File von Dia heraus
und bietet in einem extra Fenster den Tag zur hndigen Synchronisation an. 
Die Sync-Suche des Dia-Files im Argument mit Hilfe der Dia-ID-Tags."
  (interactive "fDia file: ")
  (let ((idstring "") (tmppnt (point)) (tvar 0))

    ;; nachsehen, ob man sich tatschlich innerhalb eines Dia-Tags im OP-Mode befindet
    (split-window-vertically 10)

    ;; wenn man direkt auf einem Tag steht: gehe in eine tagfreie Zone
    (if (< (setq tvar (- (point)
			 (save-excursion (search-backward "\<" nil t) (point))))
	   3)
	(backward-char tvar))
    ;; oder wenn man in dem Tag im Bereich des Wortes id= steht
    (if (<= (setq tvar (- (point)
			  (save-excursion (search-backward "<dia" nil t) (point))))
	    9)
	(backward-char tvar))
    
    (if (re-search-backward "\<dia\\|\</dia" nil t)
	(if (looking-at "\</dia") ; steht nicht zwischen zwei Tags
	    (progn
	      (goto-char tmppnt)
	      (message "not in <dia id=...> tag")
	      (delete-window)
	      (other-window 1)
	      nil)
	  (goto-char tmppnt)
	  (setq idstring (op-dia-id-searcher-backward))
	  (find-file file)
	  (beginning-of-buffer)
	  (if (re-search-forward (concat "\<dia:object[ \t]*type[ \t]*=[ \t]*\".*Text.*\".*\".*\\id[ \t]*=[ \t]*\"O" idstring ".*\".*\>") nil t)
	      (progn
		(beginning-of-line)
		(dia-string-searcher-forward) 
		(re-search-backward "\<dia:string\>#" (save-excursion (beginning-of-line)(point)) nil)
		(re-search-forward "\<dia:string\>#" (save-excursion (end-of-line)(point)) nil)
		(recenter))
	    (delete-window)
	    (other-window 1)
	    (message "no equivalent tag found")))
      (delete-window)
      (other-window 1)
      (message "Parser error: Current cursor position is not inside a <dia...> </dia> tag")
      nil)))




(defun next-dia-tag ()
  (interactive)
  (if (not (re-search-forward "\<dia[ \t]*id[ \t]*=.*\>" nil t))
					; <- Eindeutige Identifikation von Dia-Objekten im OP-Mode
      (message "no Dia tag found")))

(defun previous-dia-tag ()
  (interactive)
  (if (re-search-backward "\<dia[ \t]*id[ \t]*=.*\>" nil t)
					; <- Eindeutige Identifikation von Dia-Objekten im OP-Mode
	(re-search-forward "\<dia[ \t]*id[ \t]*=.*\>" nil t)
      (message "no Dia tag found")))





(defun dia-textline-synchronisator (file)
  "Sucht im OP-Mode-File die markierten <dia.*>-Passagen und bietet die beiden in einem extra Fenster 
zur hndigen Synchronisation an. 
Die Sync-Suche des Dia-Files im Argument mit Hilfe einer Zeilensuche - das heit mit Hilfe identischer 
Textstrings innerhalb eines Dia-Tags im Dia-File.

Dies ist *keine* zuverl,Ad(Bssige Synchronisation.
"
  ; auskommentiert, damit nicht aktiv sichtbar ;(interactive "fDia file: ")
  (let ((syncline "") (tmppnt 0) (current-buff (current-buffer)) (tpoint1 0) (tpoint2 0) (current-point (point)))
    (find-file file)
    (switch-to-buffer current-buff)
    (beginning-of-buffer)
    ;;
    ;; suche Tag
    (while (search-forward "<dia" nil t)
      (setq tmppnt (save-excursion (search-forward "</dia>" nil nil) (point)))
      ;;
      ;; suche die Textzeile zwischen den Tags einzeln ab, suche diese im Dia-File und frage nach, ob 
      ;; und in welche Richtung diese synchronisiert werden soll. 
      ;; Die Textzeilen werden _einzeln_ synchronisiert, da eventuell im OP-Mode Formatierungen ge,Ad(Bndert werden.
      ;;
      ;;
      ;; suche die Zeile im Tag
      (while (and (> tmppnt (save-excursion (end-of-line)(point)))
		  (re-search-forward "[^ \t]" 
				     (min (save-excursion (end-of-line)(point))
					  (save-excursion
					    (if (search-forward "</dia>" (save-excursion (end-of-line)(point)) t)
						(- (point) 6)
					      999999)))
				     t)
		  )
	(progn
	  (backward-char 1)
	  (setq syncline (buffer-substring-no-properties (point) (min (save-excursion (end-of-line)(point))
								      (save-excursion
									(if (search-forward "</dia>" (save-excursion (end-of-line)(point)) t)
									    (- (point) 6)
									  999999)))))
	  ;; hier sollte syncline den Inhalt zwischen den Tags enthalten - von der Zeile, in der der Cursor steht
	  ;;
	  ;;
	  
	  ;; suche im Dia-File nach dem Inhalt der Variablen "syncline"
	  (switch-to-buffer (file-name-nondirectory file))
	  (beginning-of-buffer)
	  (if (search-forward syncline nil t)
	      (progn
		;; extra Fenster mit Dia-File
		(split-window-vertically 10)
		(beginning-of-line)
		(recenter)
		(other-window 1)
		;; wieder zum Ursprungsfenster, Cursor-Position im <dia.*>-Bereich
		(split-window-vertically 10)
		(beginning-of-line)
		(recenter)
		(switch-to-buffer current-buff)
		(other-window 1)
		)
	    )
	  (switch-to-buffer current-buff)
	  (goto-char current-point)
)))))






;;;;;;;includeforelc!
;;;;;;(load "csv")

;;; csv.el --- Functions for reading/parsing csv files
;;
;;  Copyright (C) 2001 by Ulf Jasper
;;
;;  Author:     Ulf Jasper <ulf.jasper@web.de>
;;  Filename:   csv.el
;;  Created:    August 19 2001
;;  Keywords:   util
;;  Version:    $Id: csv.el,v 1.2 2001/08/30 20:09:50 ulf Exp $
;;  Time-stamp: "30. August 2001, 22:09:47 (ulf)"
;;
;;
;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.
;;
;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
;;
;;; Commentary:
;; 
;;  Main routine is `csv-parse-buffer' which takes a buffer containing a
;;  csv (Comma Separated Value) file and converts its contents into a list
;;  of alists. The first line of the csv file is interpreted as the list of
;;  keys. For example:
;;
;;  Key1,Key 2,"Key3"
;;  Value1a,Value1b,"Value1c"
;;  Value2a,Value2b,"Very long Value
;;  2c"
;;
;;  gets translated into
;;
;;  ((("Key1" "Value1a") ("Key 2" "Value1b") ("Key3" "Value1c")) 
;;   (("Key1" "Value2a") ("Key 2" "Value2b") ("Key3" "Very long Value
;;    2c")))
;;
;;  The function `csv-insert-contents' demonstrates how to use
;;  `csv-parse-buffer'.
;;
;;  This package has been tested on Emacs 20.7.1 and XEmacs 21.1.12.
;;
;;; History:
;; 
;;  1.0  First version
;;  
;;  1.1  First.first version
;;       automatically add missing entries for lines that are too short
;;

;;; Code:
(defun csv-parse-buffer (&optional buffer coding-system)
  "
BUFFER = csv buffer."
  (interactive)
  (let ((result nil)
	(keylist nil)
	(go-ahead t)
	(coding-system-for-read coding-system))
    (save-window-excursion
      (if buffer
	  (switch-to-buffer buffer)
	(setq buffer (current-buffer)))
      (beginning-of-buffer)
      ;; get the header line
      (setq keylist (csv-read-line))
      (next-line 1)
      ;; get all the content lines
      (while go-ahead
	(setq result (cons (csv-read-line keylist) result))
	(next-line 1)
	(end-of-line)
	(if (eobp)
	    (setq go-ahead nil))))
    (setq result (reverse result))
    ;;(csv-force-complete-lines keylist result)
    result))

(defun csv-read-line (&optional keylist)
  "Parses a single csv line. 
If KEYLIST is not nil an alist is returned, using the keys from the keylist. 
Otherwise just the list of entries is returned."
  (let ((line-contents nil)
	(match1 "")
	(match2 "")
	(match "")
	(matchstart1 0)
	(matchstart2 0)
	(matchend1 0)
	(matchend2 0)
	(index 0)
	(go-ahead t))
    (beginning-of-line)
    (setq line-contents nil)
    (while go-ahead
      (setq matchstart1 nil
	    matchstart2 nil)
      ;; try for quoted entry
      (save-excursion
	(when (re-search-forward
	       "\\(^\\|,\\)\"\\(\\([^\"]\\|\n\\|\\\\\"\\)*\\)\"\\(,\\|,?$\\)"
	       nil t)
	  (setq matchstart1 (match-beginning 0))
	  (setq matchend1 (+ 1 (match-end 2)))
	  (setq match1 (match-string 2))))
      ;; try unquoted
      (save-excursion
	(when (re-search-forward "\\(^\\|,\\)\\([^,\n]*\\)\\(,\\|,?$\\)"
				 nil t)
	  (setq matchstart2 (match-beginning 0))
	  (setq matchend2 (match-end 2))
	  (setq match2 (match-string 2))))
      ;; check whether quoted or unquoted fits better
      (setq match nil)
      (if matchstart1
	  (if matchstart2
	      (if (<= matchstart1 matchstart2)
		  (progn
		    (setq match match1)
		    (goto-char matchend1))
		(setq match match2)
		(goto-char matchend2))
	    (setq match match1)
	    (goto-char matchend1))
	(when matchstart2
	  (setq match match2)
	  (goto-char matchend2)))
      ;; check whether we found something
      (if (not match)
	  (setq go-ahead nil)
	(if (not keylist)
	    (setq line-contents (cons match line-contents))
	  (let ((key (nth index keylist)))
	    (setq line-contents (cons (cons key match) line-contents))))
	)
      (setq index (+ 1 index))
      (if (eolp) (setq go-ahead nil)))
    ;; fill up
    (while (< index (length keylist))
      (let ((key (nth index keylist)))
	(setq line-contents (cons (cons key "") line-contents)))
      (setq index (+ 1 index)))
    ;; finally reverse result -- for readability
    (reverse line-contents)))

;; (defun csv-force-complete-lines (keylist contentlist) 
;;   (mapcar (lambda (line)
;; 	    (mapcar (lambda (key)
;; 		      (or (assoc key line)
;; 			  (cons key "not found")))
;; 		    keylist))
;; 	  contentlist))


(defun csv-insert-contents (contentlist)
  "Inserts the contents of a csv file -- sample for using `csv-parse-buffer'. 
CONTENTSLIST gives a list of alists as returned by `csv-parse-buffer'."
  (interactive)
  (message "hallo")
   (mapcar (lambda (line)
	     (insert "-----\n")
	     (mapcar (lambda (i)
		       (insert (format "\"%s\" = \"%s\"\n" (car i) (cdr i)))
		       )
		     line))
	   contentlist)
)
  
(defun csv-test ()
  "Test routine -- don't care."
;SMW  (interactive)
  (let* ((b (current-buffer))
	 (tb (get-buffer-create "*csv*")))
    (switch-to-buffer-other-window tb)
    (insert "asdf")
    (erase-buffer)
    (beginning-of-buffer)
    (csv-insert-contents (csv-parse-buffer b))
    (switch-to-buffer tb)))


(provide 'csv)

;;; csv.el ends here

;;;;;;;includeforelc!
;;;;;;(load "op-csv")


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






;;;;;;;includeforelc!
;;;;;;(load "op-BF")





(defun insert_BF ()
  (interactive)
  (let ((erledigt-feld nil)(termin-feld "")(tmp nil))
    (goto-char 1)
    (while (re-search-forward "^\\*+" nil t)
      ;; mache die Analyse der BF-Felder
      (save-excursion
	;; nachsehen, ob dies eine mehrzeilige Überschrift ist, wenn ja, dann setzen 
	;; an das Ende der Mehrzeiler
	(end-of-line)
	(backward-char 1)
	(while (looking-at "\n[^*]")
	  (next-line 1)
	  (end-of-line)
	  (backward-char 1))
	;; jetzt die BF-Kommentarzeilen lesen und auslesen (falls vorhanden)
	(if (search-backward "(" (save-excursion (beginning-of-line)(point)) t)
	    (progn
	      (save-excursion
		;; wenn das Done-Feld gesetzt ist, dann ersetze dieses durch das "E"-Feld des OP-Modes
		(if (re-search-forward "Done" (save-excursion (end-of-line)(point)) t)
		    (progn
		      (setq erledigt-feld t)
		      (if (looking-at ",")
			  (forward-char 1))
		      (if (looking-at " ")
			  (forward-char 1))
		      (backward-kill-word 1))))
	      (end-of-line)
	      (search-backward "(" (save-excursion (beginning-of-line)(point)) t)
	      (save-excursion
		;; wenn das Termin-Feld gesetzt ist, dann setze das ersetze dieses durch das "E"-Feld des OP-Modes
		(if (re-search-forward "due" (save-excursion (end-of-line)(point)) t)
		    (progn
		      (setq termin-feld (buffer-substring (point) (+ (point) 11)))
		      (backward-char 4)
		      (kill-word 4)
		      (save-excursion
			(end-of-line)
			(insert-char ?\n 1)
			(re-indent)
			(insert "#T" termin-feld)))))
	      (end-of-line)
	      (search-backward "(" (save-excursion (beginning-of-line)(point)) t)
	      (save-excursion
		;; wenn das Termin-Feld gesetzt ist, dann setze das ersetze dieses durch das "E"-Feld des OP-Modes
		(if (re-search-forward "To do" (save-excursion (end-of-line)(point)) t)
		    (progn
		      (if (looking-at ",")
			  (forward-char 1))
		      (if (looking-at " ")
			  (forward-char 1))
		      (backward-kill-word 2))))
	      ;; reformatiere ein eventuell noch vorhandenes Prioritätsfeld
	      (end-of-line)
	      (search-backward "(" (save-excursion (beginning-of-line)(point)) t)
	      ;; dazu: eventuell noch von vorher vorhandenes Feld löschen (nach hinten in der Zeile gehen)
	      (save-excursion
					;Muster: (p1, )
		(while (re-search-backward "(p[0-9]+,[ ]*)" (save-excursion (beginning-of-line)(point)) t)
		  (replace-match "")))
	      (save-excursion
					;Muster: (p1, 
		(while (re-search-backward "(p[0-9]+,[ ]*" (save-excursion (beginning-of-line)(point)) t)
		  (replace-match "")))
	      (save-excursion
					;Muster: (p1  )
		(while (re-search-backward "(p[0-9]+[ ]*)" (save-excursion (beginning-of-line)(point)) t)
		  (replace-match "")))
	      (save-excursion
					;Muster: (p1
		(while (re-search-backward "(p[0-9]+" (save-excursion (beginning-of-line)(point)) t)
		  (replace-match "")))
	      ;; dann das aktuelle Feld reformatieren (nach vorne in der Zeile gehen)
	      (end-of-line)
	      (search-backward "(" (save-excursion (beginning-of-line)(point)) t)
	      (save-excursion
					;Muster: (p1,  )
		(if (re-search-forward "(p\\([0-9]+\\),[ ]*)" (save-excursion (end-of-line)(point)) t)
		    
		    (replace-match "(p\\1)")
					;Muster: (p1, 
		  (if (re-search-forward "(p\\([0-9]+\\),[ ]*" (save-excursion (end-of-line)(point)) t)
		      (replace-match "(p\\1)")
					;Muster: (p1  )
		    (if (re-search-forward "(p\\([0-9]+\\)[ ]*)" (save-excursion (end-of-line)(point)) t)
			(replace-match "(p\\1)")
					;Muster: (p1
		      (if (re-search-forward "(p\\([0-9]+\\)[ ]*" (save-excursion (end-of-line)(point)) t)
			  (replace-match "(p\\1)"))))))
	      )))
      ;; Einsetzen des "E"- (== erledigt-) Feldes
      (beginning-of-line)
      (re-search-forward "\\*+" (save-excursion (end-of-line)(point)) t) ; ****
      (if erledigt-feld 
	  (progn
	    (insert-char ?E 1)
	    (setq erledigt-feld nil)
	    ;; wenn ein E eingesetzt wurde und dies eine Hauptüberschrift ist (d.h. nur ein Stern),
	    ;; dann noch ein Freizeichen dazu
	    (save-excursion
	      (beginning-of-line)
	      (if (looking-at "^\\*[^*]")
		  (setq tmp t)))
	    (if tmp (insert-char ? 1))(setq tmp nil)))
      )
    ;; BF-Notizfeld zum normalen Mehrzeileneintrag uner des entsprechenden Überschrift machen
    (goto-char 1)
    (while (search-forward "<Note: " nil t)
      (replace-match "<Notiz: "))
    ))


(defun from_BF ()
  (interactive)
  (let ((tfilename))
    ;; setzt den Filenamen um von der Extention .txt auf die Extention .op - wenn .op
    ;; schon existiert, dann mache nicht weiter, um ein Überschreiben zu verhindern
    (setq tfilename (concat
		     (substring buffer-file-name 0 (string-match "\\.txt$" buffer-file-name))
		     ".op"))
    (if (not (file-exists-p tfilename))
	(progn
	  ;;
	  ;; erst einmal die Grundfunktionen aud BF übernehmen: Datum, Done, ...
	  (insert_BF)
	  ;;
	  ;;
	  ;; mache eine Rekonstruktion der Zeilen durch Ersetzen der Sonderzeichen
	  (debug)
	  (goto-char 1)
	  (while (re-search-forward "+" nil t)
	    (replace-match "\n"))
	  (goto-char 1)
	  (while (re-search-forward "+" nil t)
	    (replace-match "\n"))
 	  (goto-char 1)
	  (while (re-search-forward "\n" nil t) ; neu kreierte Punkte in BrainForest
	    (replace-match ""))
 	  (goto-char 1)
	  (while (re-search-forward "" nil t)
	    (replace-match "\n"))
 	  (goto-char 1)
	  (while (re-search-forward "" nil t)
	    (replace-match "\n"))
	  (goto-char 1)
	  (while (re-search-forward "$" nil t)
	    (replace-match "\n"))
	  (goto-char 1)
	  (while (re-search-forward "+" nil t)
	    (replace-match ""))
	  ;; jetzt: frage für jeden speziellen Punkt, ob er nicht vielleicht neu ist:
	  (goto-char 1)
	  (while (re-search-forward "^\\*+\\w" nil t)
	    (progn
	      (backward-char 1)
	      (if (not (looking-at "[-+EVDSR]"))
		  (if (not (looking-at "FW"))
		      (progn
			(isearch-highlight 
			 (save-excursion (beginning-of-line)(point))
			 (save-excursion (end-of-line)(point)))
			(if (y-or-n-p "Format headline ? ")
			    (progn
			      (insert-char ?\n 1)
			      (backward-char 1)
			      (re-indent)
			      (end-of-line)
			      (delete-char 1)
			    (isearch-dehighlight nil))        
			    ))))))
	  ;; formatiere eventuell neu hinzugekommene Zeilen
	  ;; jetzt: frage für jeden speziellen Punkt
	  (goto-char 1)
	  (re-search-forward "^\\*" nil t)   ; erste Zeile mit Outline-Überschrift-Charakter
	  (while (re-search-forward "^[^* \t]." nil t)
	  ;;    (if (y-or-n-p "Indent OP-Mode like ? ")
		  (progn
		    (beginning-of-line)
		    (re-indent)
		    (next-line 1)
		    (beginning-of-line)))
					;)
;;; 	  ;; jetzt noch indents nachholen, die durch die Baumstruktur vielleicht verloren gegangen sind
;;; 	  (goto-char 1)
;;; 	  (while (re-search-forward "^\t" nil t)
;;; 	    (re-indent))
	  ;;
	  ;; File-Sicherung und löschen des Files
	  (if (yes-or-no-p (concat "Save file to " tfilename " ? "))
	      (progn
		(write-file tfilename)
		(revert-buffer nil t)
		(kill-buffer (current-buffer))
		(find-file tfilename))
	    (if (y-or-n-p "Revert buffer? ")
		(revert-buffer nil t)))
	  )
      (error "Filename exists already with extention .op"))))


(defun to_BF ()
  (interactive)
  (let ((tfilename))
    (goto-char 1)
    ;; setzt den Filenamen um von der Extention .op auf die Extention .txt - auf diesem File
    ;; wird gearbeitet (zum Schutz des .op-Files)
    (setq tfilename (concat
		     (substring buffer-file-name 0 (string-match "\\.op$" buffer-file-name))
		     ".txt"))
    (if (not (file-exists-p tfilename))  ; wenn .txt schon existiert: Abbruch
	(progn
	  ;; zum .txt-File
	  (copy-file buffer-file-name tfilename)
	  (find-file tfilename)
	  ;; nehme alle nicht-"outline-headings" durch  zu der Überschrift dazu
	  (while (re-search-forward "^[ \t]" nil t)
	    (if (not (eobp))
		(progn
		  (backward-char 2)
		  (delete-char 1)
		  (insert-char ? 1)
		  (if (not (eobp))
		      (forward-char 1)))))
	  ;; ersetze alle reinen Freizeilen durch das Zeichen , damit diese bei der 
	  ;; Rekonstruktion wieder herstellbar sind
	  (trim-buffer)
	  (goto-char 1)
	  (while (and (re-search-forward "^$" nil t)
		      (not (eobp)))
	    (progn
	      (delete-char 1)
	      (if (not (= (point) 1))
		  (backward-char 1))
	      (insert-char ? 1)
	      (if (not (eobp))
		  (forward-char 1))))
	  ;; setze oben eine Synchronisationszeile ein, zur Kennzeichnung und um 
	  ;; BrainForest zu zwingen, die Headlines eine Stufe zu tief einzusetzen -
	  ;; die Stufe-1-Überschriften bleiben sonst nicht in Outline-Form erhalten
	  (goto-char 1)
	  (if (not (looking-at "\n*emacs OP-Mode"))
	      (insert (concat "emacs OP-Mode-File zur Synchronisation in BF: \"" buffer-file-name "\"\n")))
	  (save-buffer)
	  (kill-buffer (current-buffer)))
      (error "Filename exists already with extention .txt"))))





;; Gift Botulinum, Fa. Botox gegen Migräne
;; man ist nicht, sondern man tut so als wenn man wäre
;; es ist ein Puzzle, das muss man sich zusammenbauen aus den eigenen Bildern und denen der anderen





;; hier war der File für das Programm BrainForest Pro, das auf dem Palm läuft

;; zur Kontrolle
(message "5")

;;;
;;; Die nächsten Zeilen laden den Hyperbole-Mode
;;;
;;;;;;;includeforelc!
;;;;;;(load "op-hyper/sql-mode")

;;; sql-mode.el --- Mode for editing and testing SQL.

;; Copyright (C) 1994 Rob Riepel.

;; Author: Rob Riepel <riepel@networking.stanford.edu>
;; Maintainer: Rob Riepel <riepel@networking.stanford.edu>
;; Keywords: sql isql sqlplus sybase oracle

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; LCD Archive Entry:
;; sql-mode|Rob Riepel|riepel@networking.stanford.edu|
;; Mode for editing and testing SQL.|
;; 12-Feb-94|Version: 2.0|~/modes/sql-mode.el.Z|

;;; Commentary:

;;  This file is implements a SQL-mode for emacs.  The code is a hacked up
;;  version of Lynn Slater's sql.el.  Some of the comments were pinched from
;;  Jim Lange's sqlplus.  Both are available from the elisp archives.
;;
;;  SQL-mode is for editing and testing SQL statements in a standard text
;;  buffer.  SQL-mode turns on abbrev-mode with abbreviations for common SQL
;;  keywords.  The most useful feature of SQL-mode is sending SQL statements
;;  to a SQL interpreter.
;;
;;  The following commands can be added to a global initialization file or
;;  to any user's .emacs file to conveniently use SQL-mode.
;;
;;    (autoload 'sql "sql-mode"
;;      "Start the interactive SQL interpreter in a new buffer." t)
;;
;;    (autoload 'sql-mode "sql-mode"
;;      "Mode for editing SQL files and running a SQL interpetror." t)
;;
;;    (setq auto-mode-alist (cons '("\\.sql$" . sql-mode) auto-mode-alist))
;;
;;  Use describe-mode while in sql-mode for further instructions.
;;
;;
;;  A Note to Oracle SQL*Plus users:
;;
;;    This mode was originally put together for Sybase isql, without
;;    any provisions for SQL*Plus.  If you find the SQL*Plus support
;;    deficient, please feel free to hack it to pieces and send your
;;    changes to the maintainer.  Or, you might want to check out Jim
;;    Lange's sqlplus, which was written specifically for SQL*plus.

;;; Code:


;;;  Revision Information

(defconst sql-revision "$Revision: 2.5 $")


;;;  Variables -

(defconst sql-emacs18-p (string-lessp emacs-version "19"))
(defconst sql-emacs19-p (not sql-emacs18-p))

(defvar sql-buffer-name "" "Buffer where SQL commands are run.")
(defvar sql-buffer-prefix "*" "Beginning of SQL buffer name.")
(defvar sql-buffer-postfix "*" "End of SQL buffer name.")

(defvar sql-command "isql" "SQL interpreter program.")
(defvar sql-server "" "SQL server name.")
(defvar sql-username "" "SQL interpreter username.")
(defvar sql-process-name "" "SQL interpreter process name.")

(defvar sql-magic-go t "*If non-NIL, submit SQL when 'go' is entered.")
(defvar sql-magic-semicolon t "*If non-NIL, submit SQL when a semicolon is entered.")

(defvar sql-mode-abbrev-table nil "Abbrev table used in SQL mode buffers.")
(defvar sql-mode-map nil "Keymap used in SQL mode.")

(defvar sql-output-separator "@--"
  "String printed between sets of SQL command output.")

(defvar sql-separator-regexp "[ \n\t]*go\\b\\|;[ \t]*$"  ; "go" isql (Sybase)
  "Regexp used to seperate groups of SQL statements.")   ; ";" sqlplus (Oracle)
   ;; alternative version -- "^[ \t]*go\\b\\|;[ \t]*$"

(defvar sql-terminator "\ngo"
  "String to send to the SQL interpreter to initiate execution.")


;;;  Markers -

(defvar sql-buffer-mark (make-marker)
  "Marks the current SQL command in the SQL output buffer.")

(make-variable-buffer-local 'sql-buffer-mark)

(defvar sql-region-beginning-mark (make-marker)
  "Marks the beginning of the region to sent to the SQL process.")
(defvar sql-region-end-mark (make-marker)
  "Marks the end of the region to sent to the SQL process.")


;;;  SQL-mode Keymap -

(if (not sql-mode-map)
    (progn
      (setq sql-mode-map (make-sparse-keymap))
      (define-key sql-mode-map "O"        'sql-magic-go)
      (define-key sql-mode-map "o"        'sql-magic-go)
      (define-key sql-mode-map ";"        'sql-magic-semicolon)
      (define-key sql-mode-map "\C-cg"    'sql-goto-error)
      (define-key sql-mode-map "\C-c\C-e" 'sql-buffer-erase)
      (define-key sql-mode-map "\C-cb"    'sql-buffer-bottom)
      (define-key sql-mode-map "\C-ct"    'sql-buffer-top)
      (define-key sql-mode-map "\C-cp"    'sql-buffer-prev-command)
      (define-key sql-mode-map "\C-cn"    'sql-buffer-next-command)
      (define-key sql-mode-map "\C-c\C-w" 'sql-buffer-display-window)
      (define-key sql-mode-map "\C-c\C-l" 'sql-buffer-redisplay-current)
      (define-key sql-mode-map "\C-c\C-b" 'sql-buffer-scroll-right)
      (define-key sql-mode-map "\C-c\C-f" 'sql-buffer-scroll-left)
      (define-key sql-mode-map "\C-c\C-p" 'sql-buffer-scroll-down)
      (define-key sql-mode-map "\C-c\C-n" 'sql-buffer-scroll-up)
      (define-key sql-mode-map "\C-c\C-i" 'sql-send-interrupt)
      (define-key sql-mode-map "\C-c\C-r" 'sql-send-region)
      (define-key sql-mode-map "\C-c\C-c" 'sql-send-current)
      (define-key sql-mode-map "\C-c\C-u" 'sql-set-user)))


;;;  SQL-mode Abbreviations -

(progn
  (define-abbrev-table 'sql-mode-abbrev-table ())
  (define-abbrev sql-mode-abbrev-table "arc" "archivelog"   nil)
  (define-abbrev sql-mode-abbrev-table "s"   "select"       nil)
  (define-abbrev sql-mode-abbrev-table "f"   "from"         nil)
  (define-abbrev sql-mode-abbrev-table "fr"  "from"         nil)
  (define-abbrev sql-mode-abbrev-table "w"   "where"        nil)
  (define-abbrev sql-mode-abbrev-table "o"   "order by"     nil)
  (define-abbrev sql-mode-abbrev-table "nu"  "number"       nil)
  (define-abbrev sql-mode-abbrev-table "da"  "date"         nil)
  (define-abbrev sql-mode-abbrev-table "co"  "connect"      nil)
  (define-abbrev sql-mode-abbrev-table "be"  "between"      nil)
  (define-abbrev sql-mode-abbrev-table "sy"  "synonym"      nil)
  (define-abbrev sql-mode-abbrev-table "tr"  "trigger"      nil)
  (define-abbrev sql-mode-abbrev-table "up"  "update"       nil)
  (define-abbrev sql-mode-abbrev-table "ins" "insert"       nil)
  (define-abbrev sql-mode-abbrev-table "gr"  "grant"        nil)
  (define-abbrev sql-mode-abbrev-table "gra" "grant all to" nil)
  (define-abbrev sql-mode-abbrev-table "pu"  "public"       nil)
  (define-abbrev sql-mode-abbrev-table "un"  "unique"       nil)
  (define-abbrev sql-mode-abbrev-table "cl"  "cluster"      nil)
  (define-abbrev sql-mode-abbrev-table "we"  "whenever"     nil)
  (define-abbrev sql-mode-abbrev-table "ta"  "table"        nil)
  (define-abbrev sql-mode-abbrev-table "pr"  "priviledges"  nil)
  (define-abbrev sql-mode-abbrev-table "dr"  "drop"         nil)
  (define-abbrev sql-mode-abbrev-table "ro"  "rollback"     nil)
  (define-abbrev sql-mode-abbrev-table "rb"  "rollback"     nil)
  (define-abbrev sql-mode-abbrev-table "tr"  "transaction"  nil)
  (define-abbrev sql-mode-abbrev-table "us"  "using"        nil)
  (define-abbrev sql-mode-abbrev-table "u"   "using"        nil))


;;;  SQL-mode

(defun sql-mode nil
  "
Mode for editing SQL files and running a SQL interpreter (such as
'isql' (from Sybase) or 'sqlplus' (from Oracle)).  Entry into this
mode runs the hook 'sql-mode-hook'.  It also enables abbrev-mode,
with abbreviations for SQL keywords.  Use '\\[list-abbrevs]' for a
full list.

Use 'M-x sql-mode' to invoke SQL-mode for the current buffer.

Use 'M-x sql' to start the SQL interpreter.  Or, use '\\[sql-set-user]'
(sql-set-user) which sets the current user for the SQL interpreter
and starts it if necessary.  You can run the SQL interpreter
multiple times (as different users) and switch between them using
'\\[sql-set-user]' (sql-set-user).

SQL mode automatically sends SQL to the SQL interpreter when you
enter a SQL command terminator ('go' or ';').  Set sql-magic-go
and/or sql-magic-semicolon to NIL to disable this automatic
submission feature.

Existing SQL can be sent to the SQL interpreter using '\\[sql-send-current]'
(sql-send-current).  Just position the cursor on or near the SQL
statement you wish to send and press '\\[sql-send-current]' to run it and
display the results.

SQL-mode starts configured to interact with isql from Sybase.  Oracle
users can switch to the sqlplus configuration using 'M-x sql-oracle',
or by adding a call to sql-oracle to their sql-mode-hook function.

Mode Specific Bindings:

\\{sql-mode-map} "

  (interactive)
  (setq major-mode 'sql-mode)
  (setq mode-name "SQL")
  (use-local-map sql-mode-map)
  (setq local-abbrev-table sql-mode-abbrev-table)
  (abbrev-mode 1)
  (setq abbrev-all-caps 1)
  (run-hooks 'sql-mode-hook))


;;;  SQL-mode Settings for Specific Vendors

(defun sql-sybase nil
  "Configure SQL-mode for Sybase ``isql'' interaction."
  (interactive)
  (setq sql-terminator "\ngo")
  (setq sql-command "isql")
  (fset 'sql-start-interpreter 'sql-start-isql))

(defun sql-oracle nil
  "Configure SQL-mode for Oracle ``sqlplus'' interaction."
  (interactive)
  (setq sql-terminator ";")
  (setq sql-command "sqlplus")
  (fset 'sql-start-interpreter 'sql-start-sqlplus)
  (cond (sql-emacs18-p
	 (and (featurep 'gmhist)
	      (put 'sql-get-server-hist 'initial-hist (list ""))))
	(t
	 (setq sql-get-server-hist (make-list 2 "")))))

(defun sql-start-isql nil
  "Start up the Sybase isql program."
  (start-process sql-process-name sql-buffer-name sql-command "-w"
                 "2048" "-n" "-S" sql-server "-U" sql-username "-P"
                 (sql-ange-ftp-read-passwd "Password: ")))

(defun sql-start-sqlplus nil
  "Start up the Oracle sqlplus program."
  (start-process
   sql-process-name sql-buffer-name sql-command
   (concat sql-username "/" (sql-ange-ftp-read-passwd "Password: ")
	   (if (string= sql-server "") "" (concat "@" sql-server)))))


;;;  Utilitities

(defun sql-echo-in-buffer (buffer-name string &optional force-display)
  "Displays string in the named buffer, creating the buffer if needed.
If force-display is true, the buffer will appear if not already shown."
  (let ((buffer (get-buffer-create buffer-name)))
    (if force-display (display-buffer buffer))
    (set-buffer buffer)
    (goto-char (point-max))
    (insert string)
    (if force-display
	(set-window-point (get-buffer-window buffer-name) (point-max)))))

(defun sql-verify-buffer nil
  "Generates reasonable error messages abouut the SQL connection."
  (if (not (get-buffer sql-buffer-name))
      (error "No SQL output buffer!  Use 'M-x sql' to initialize the SQL interpreter."))
  (if (not (get-buffer-process sql-buffer-name))
      (error "Buffer '%s' is not talking to anybody!" sql-buffer-name)))

(defun sql-send-strings (strings)
  "Sends strings to the SQL process.
Also shows the string at the top of the SQL output buffer."
  (sql-verify-buffer)
  (sql-echo-in-buffer sql-buffer-name
		      (concat "\n" sql-output-separator "\n\n"))
  (sql-buffer-bottom)
  (sql-buffer-mark-current)
  (sql-echo-in-buffer sql-buffer-name (apply 'concat strings))
  (sql-echo-in-buffer sql-buffer-name "\n\n")
  (let ((string (apply 'concat strings))
	(process  (get-buffer-process sql-buffer-name)))
    (send-string process (concat string "\n"))
    (if (eq (current-buffer) (process-buffer process))
	(set-marker (process-mark process) (point))))
  (sql-buffer-redisplay-current))

(defun sql-mark-current nil
  "Marks the current SQL for sending to the SQL process.
Marks are placed around a region defined by matching pairs
of the expression listed in 'sql-seperater-regexp."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (or (and (re-search-backward sql-separator-regexp nil t)
             (goto-char (match-end 0)))
        (goto-char (point-min)))
    (skip-chars-forward " \t\n")
    (setq sql-region-beginning-mark (copy-marker (point)))
    (re-search-forward sql-separator-regexp)
    (setq sql-region-end-mark (copy-marker (match-beginning 0)))))


;;;  Transmission Commands

(defun sql-send-region (start end)
  "Send a region to the SQL process."
  (interactive "r")
  (save-excursion
    (sql-send-strings (list (buffer-substring start end) sql-terminator))))

(defun sql-send-current nil
  "Send the current SQL command(s) to the SQL process."
  (interactive)
  (sql-mark-current)
  (sql-send-region sql-region-beginning-mark sql-region-end-mark))


(defun sql-magic-go (arg)
  "Insert ``o'' and submit the current SQL to the interpreter."
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  (and sql-magic-go (save-excursion
                      (beginning-of-line)
                      (and (looking-at "go\\b") (sql-send-current)))))

(defun sql-magic-semicolon (arg)
  "Insert semicolon and submit the current SQL to the interpreter."
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  (and sql-magic-semicolon (sql-send-current)))


;;;  SQL-Output Buffer Operations -

(defun sql-show-buffer (&optional fcn &rest args)
  "Makes the SQL output buffer visible in the other window."
  (interactive)
  (sql-verify-buffer)
  (let ((window  (selected-window)))
    (if (not (eq (window-buffer window) (get-buffer sql-buffer-name)))
	(switch-to-buffer-other-window sql-buffer-name))
    (if fcn (condition-case nil (apply fcn args) (error nil)))
    (select-window window)))

(fset 'sql-buffer-display-window 'sql-show-buffer)

(defun sql-buffer-scroll-up nil
  "Scroll-up in the SQL output buffer window."
  (interactive)
  (sql-show-buffer 'scroll-up))

(defun sql-buffer-scroll-down nil
  "Scroll-down in the SQL output buffer window."
  (interactive)
  (sql-show-buffer 'scroll-down))

(defun sql-buffer-scroll-left (num)
  "Scroll-left in the SQL output buffer window."
  (interactive "p")
  (sql-show-buffer 'scroll-left (* num (/ (window-width) 2))))

(defun sql-buffer-scroll-right (num)
  "Scroll-right in the SQL output buffer window."
  (interactive "p")
  (sql-show-buffer 'scroll-right (* num (/ (window-width) 2))))

(defun sql-buffer-mark-current nil
  "Mark the current position in the SQL output window."
  (sql-show-buffer 'sql-buffer-make-mark))

(defun sql-buffer-make-mark nil
  "Set the sql-buffer-marker."
  (setq sql-buffer-mark (copy-marker (point))))

(defun sql-buffer-redisplay-current nil
  "Go to the current sql-buffer-mark."
  (interactive)
  (sql-show-buffer 'sql-goto-mark))

(defun sql-goto-mark nil
  (goto-char sql-buffer-mark)
  (recenter 0))

(defun sql-buffer-top nil
  "Goto the top of the SQL output buffer."
  (interactive)
  (sql-show-buffer 'sql-beginning-of-buffer))

(defun sql-beginning-of-buffer nil (goto-char (point-min)))

(defun sql-buffer-bottom nil
  "Goto the bottom of the SQL output buffer."
  (interactive)
  (sql-show-buffer 'sql-end-of-buffer))

(defun sql-end-of-buffer nil (goto-char (point-max)) (recenter -1))

(defun sql-buffer-erase nil
  "Clear the SQL output buffer."
  (interactive)
  (sql-show-buffer 'erase-buffer))

(defun sql-buffer-next-command nil
  "Search for the next command in the SQL output buffer."
  (interactive)
  (sql-show-buffer 'sql-next-command))

(defun sql-next-command nil
  "Search for the next command in the SQL output buffer."
  (cond ((re-search-forward  sql-output-separator nil t)
	 (forward-line 2)
	 (recenter 0))
	(t (beep) (message "No more commands."))))

(defun sql-buffer-prev-command nil
  "Search for the previous command in the SQL output buffer."
  (interactive)
  (sql-show-buffer 'sql-previous-command))

(defun sql-previous-command nil
  "Search for the previous command in the SQL output buffer."
  (let ((start (point)))
    (re-search-backward  sql-output-separator nil t)
    (cond ((re-search-backward  sql-output-separator nil t)
	   (forward-line 2)
	   (recenter 0))
	  (t
	   (message "No more commands.") (beep)
	   (goto-char start)))))

(defun sql-send-interrupt nil
  "Send an interrupt the the SQL interpreter process."
  (interactive)
  (interrupt-process sql-process-name))


;;;  Miscellaneous

(defun sql-goto-error (n)
  "Moves to the n'th line in the most recently executed SQL."
  (interactive "NLine number of error: ")
  (goto-char sql-region-beginning-mark)
  (forward-line (1- n)))

(defun sql-insert-gos nil
  "Inserts 'go' statements between each apparent block of SQL code."
  (interactive)
  (while (not (eobp))
    (forward-line 1)
    (if (and (looking-at "[a-z]") (not (looking-at "go")))
	(progn (insert "go\n")))))


;;;  SQL Interpreter

(defun sql nil
  "Start the interactive SQL interpreter in a new buffer."
  (interactive)
  (sql-set-server-and-user)
  (get-buffer-create sql-buffer-name)            ; move to the buffer.
  (or (get-buffer-process sql-buffer-name)       ; already got a process?
   (progn			                 ; no, start it up!
     (set-buffer sql-buffer-name)
     (setq truncate-lines t)
     (sql-start-interpreter)))
  (set-buffer sql-buffer-name)
  (sql-show-buffer))

;; Start with Sybase isql
(fset 'sql-start-interpreter 'sql-start-isql)

(defun sql-set-server-and-user nil
  "Set server, username, process, and buffer for the SQL interpreter."
  (interactive)
  (call-interactively 'sql-get-server)
  ;; skip username query if ``server/user'' was input.
  (let ((stroke (string-match "/" sql-server)))
    (cond (stroke
	   (setq sql-username (substring sql-server (1+ stroke) nil))
	   (setq sql-server (substring sql-server 0 stroke)))
	  (t
	   (call-interactively 'sql-get-username))))
  (setq sql-process-name (if (string= "" sql-server) sql-username
                           (concat sql-server "/" sql-username)))
  (setq sql-buffer-name
        (concat sql-buffer-prefix sql-process-name sql-buffer-postfix)))

(defun sql-get-server (server)
  "Set the SQL server name."
  (interactive (list (sql-string-prompt "Server: " 'sql-get-server-hist 2)))
  (setq sql-server server))

(defun sql-get-username (username)
  "Set the SQL server username."
  (interactive (list (sql-string-prompt "Username: " 'sql-get-username-hist 2)))
  (setq sql-username username))

(defun sql-string-prompt (prompt &optional symbol offset)
  "Read a string using PROMPT.
With optional HIST-SYMBOL and HIST-OFFSET, read with history."
  (cond
   (sql-emacs19-p
    (cond
     (symbol
      (let ((initial nil) (history nil))
	(if (and offset
		 (setq initial (nth (- offset 1) (symbol-value symbol))))
	    (setq history (cons symbol offset))
	  (setq history symbol))
	(read-from-minibuffer prompt initial nil nil history)))
     (t (read-string prompt))))
   (t
    (if (and symbol (featurep 'gmhist))
	(read-with-history-in symbol prompt)
      (read-string prompt)))))

(defun sql-ange-ftp-read-passwd (prompt &optional default)
  "Read a password from the user. Echos a . for each character typed.
End with RET, LFD, or ESC. DEL or C-h rubs out.  ^U kills line.
Optional DEFAULT is password to start with."
  (let ((pass (if default default ""))
	(c 0)
	(echo-keystrokes 0)
	(cursor-in-echo-area t))
    (while (and (/= c ?\r) (/= c ?\n) (/= c ?\e))
      (message "%s%s"
	       prompt
	       (make-string (length pass) ?.))
      (setq c (read-char))
      (if (= c ?\C-u)
	  (setq pass "")
	(if (and (/= c ?\b) (/= c ?\177))
	    (setq pass (concat pass (char-to-string c)))
	  (if (> (length pass) 0)
	      (setq pass (substring pass 0 -1))))))
    (sql-ange-ftp-repaint-minibuffer)
    (substring pass 0 -1)))

(setq sql-ange-ftp-tmp-keymap (make-sparse-keymap))
(define-key sql-ange-ftp-tmp-keymap "\C-m" 'exit-minibuffer)

(defun sql-ange-ftp-repaint-minibuffer nil
  "Gross hack to set minibuf_message = 0, so that the contents of the
minibuffer will show."
  (if (eq (selected-window) (minibuffer-window))
      (if (fboundp 'allocate-event)
	  ;; lemacs
	  (let ((unread-command-event (character-to-event ?\C-m
							  (allocate-event)))
		(enable-recursive-minibuffers t))
	    (read-from-minibuffer "" nil sql-ange-ftp-tmp-keymap nil))
	;; v18 GNU Emacs
	(let ((unread-command-char ?\C-m)
	      (enable-recursive-minibuffers t))
	  (read-from-minibuffer "" nil sql-ange-ftp-tmp-keymap nil)))))

(fset 'sql-set-user 'sql)


;;;  Histification

(cond
 (sql-emacs18-p

  ;; Emacs version 18, must use gmhist for history -
  ;; See if gmhist is already loaded, otherwise, attempt to load it.
  ;; If it's not available, do nothing.

  (cond
   ((or (featurep 'gmhist) (load "gmhist" t t))
    (put 'sql-get-server-hist 'no-default t)
    (put 'sql-get-server-hist 'cursor-end t)
    (put 'sql-get-server-hist 'backup 2)
    (put 'sql-get-server-hist 'initial-hist
	 (list (or (getenv "DSQUERY") "SYBASE")))

    (put 'sql-get-username-hist 'no-default t)
    (put 'sql-get-username-hist 'cursor-end t)
    (put 'sql-get-username-hist 'backup 2)
    (put 'sql-get-username-hist 'initial-hist (list (user-login-name))))))

 (t

  ;; Emacs version 19, use built-in history -

  (setq sql-get-server-hist (make-list 2 (or (getenv "DSQUERY") "SYBASE")))
  (setq sql-get-username-hist (make-list 2 (user-login-name)))))


(provide 'sql-mode)

;;; sql-mode.el ends here


;;;;;;;includeforelc!
;;;;;;(load "op-hyper/op-hyper")

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
;;   ->Name* sucht in File "Name"
;; 
;   ->>FILE:../op-project.el*defun     ; mehrmals auslösen gibt jeweils das nächste Muster
;   ->>FILE:../op-project.el*defun->
;   ->>FILE:defun
;   ->>FILE:defun->
;   ->>FILE:../op-project.el*          ; Sucht nur den anderen File aus
;   ->>defun                           ; Defaultbutton: sucht im gleichen File das Wort "defun"
;   ->>defun                           ; Defaultbutton: sucht im gleichen File das Wort "defun"
;   ->>defun->                         ; dito, setzt den Cursor aber in ein anderes Fenster
;   ->FILE:../op-project.el*defun      ; mehrmals auslösen gibt jeweils das nächste Muster
;   ->FILE:../op-project.el*defun->
;   ->FILE:defun
;   ->FILE:defun->
;   ->FILE:../op-project.el*           ; Sucht nur den anderen File aus
;   ->defun                            ; Defaultbutton: sucht im gleichen File das Wort "defun"
;   ->defun                            ; Defaultbutton: sucht im gleichen File das Wort "defun"
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
;   ->>FILE:"a test"->                 ; Blanks usw. sind erlaubt, doch nur in Anführungsstrichen  ("a test")
;   ->>FILE:"\* test"                  ; Achtgeben: Sterne gehören zu den Regexen, müssen daher gequotet werden
;   ->>FILE:"\* test"->
;   * test    $ test                   ; nur zum Testen
;
;   ->>A:Müller                        ; private Kurzform für ->FILE:adressen.op*"name", Adressenfile als Variable hier im Code
;
;   ->**Ueberschriftentest             ; steht ein * am Anfang des Musters, werden die Sterne nicht als Regexe interpretiert,
;                                      ;    sondern als "Uberschriftengerechtes Muster" (hier: `**	Ueberschriftentest')
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
;   ->EVAL:"date --version"  
;   ->EVAL:"gcc -v"
;   ->EVAL:xfig*test.fig
;   ->EVAL:"xfig -bg red"*test.fig    ; Argumente in Anführungsstrichen, Ausführungspunkt NUR vor dem 
;                                     ; ersten Blank (beim "->EVAL:xxxx"
;   ->EVAL:"xfig -bg red"*test.fig&   ; wie eben
;   ->FILE:op-hyper.el*defun;EVAL:"date --version"  
;   ->FILE:op-hyper.el*defun->;EVAL:"date --version"  
;				      ; bei Kettenargumenten: EVAL immer an den Schluß,
;                                     ;  andere Funktionen immer mit *-Zeichen. Nur je ein Buttontyp ist erlaubt.
;   ->EVAL:test.fig                   ; Defaultextention ist einprogrammiert (siehe oben)
;   ->!date                           ; Defaultbuttons
;   ->!"date --version"
;   ->!cat*op-hyper.el
;  
;;
;; Beispiele REF:
;;
;   ->REF:../op-hyper/op-hyper.el*1button      ; Muster ist hier "1button", kann aber beliebig sein
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
;   ->REF-R:../op-hyper/op-hyper.el*2button->      ; Muster ist hier "2button", kann aber beliebig sein
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


(defvar adressenfile "~/projects/adressen.op")
(make-variable-buffer-local 'adressenfile)



(defun current-thislongword ()
;;; ACHTUNG !!! siehe auch Funktion "current-longword-quote" im File op-columns.el !!!!
;;; Sucht alle EVAL-Button-Knöpfe sauber aus (siehe Beispiele und Kommentare bei "defun EVAL:func"
;;; unten), auch mit Anführungsstrichen.
  (interactive)
  (save-excursion
    (let ((begin) (end) (temp))
      (save-excursion
	(if (re-search-backward "[ \t\r]\\|^" nil t)
	    (progn
	      (if (looking-at "[ \t\r]")
		  (forward-char 1))))
	(setq begin (point))
	(if (re-search-forward "[ \t\r]\\|$" nil t)     
	    (progn
	      (backward-char 1)
	      (if (not (looking-at "[ \t\r\n]"))
		       (forward-char 1))))
	(setq end (point)))
      (setq temp (buffer-substring-no-properties begin end))
      (if (and (string-match "\"" temp)
	       (not (string-match "\".*\"" temp)))
	  (progn
	    (goto-char end)
	    (re-search-forward "\"\\|$" nil t)
	    (if (re-search-forward "[ \t\r]\\|$" nil t)
		(progn
		  (backward-char 1)
		  (if (not (looking-at "[ \t\r\n]"))
			   (forward-char 1))))
	    (setq end (point))))
      (buffer-substring-no-properties begin end))))
	  


(defvar current-point-tmp1 0)
(make-variable-buffer-local 'current-point-tmp1)
(defvar current-buffer-tmp1 (current-buffer))
(make-variable-buffer-local 'current-point-tmp1)
(defvar called-via-OP-button nil)
(make-variable-buffer-local 'called-via-OP-button)
(defun select-hyper-func (&optional stop-recursive)
  (interactive)
  (let ((command (current-thislongword)))
    (if (string-match "->[>]?[^ \t]*\\(SQL:\\|FILE:\\|A:\\|FUNC:\\|EVAL:\\|REF:\\|REF-R\\)" command)
	(progn
	  (setq called-via-OP-button t)
	  
	  ;; ---------- SQL-Button
	  ;; suche nach ->...SQL:func und führe func im SQL-Modus aus
	  ;; erst: Suche nach "->SQL:file*evalstartpunkt"
	  (if (string-match "\\(-\>[^ \t]*SQL:\\)\\(.*\\*\\)\\([^ \t\r;]*\\)\\(;?\\)" command)
	      (progn
		(SQL:func (substring command (match-beginning 2) (- (match-end 2) 1))
			  (substring command (match-end 2) (match-beginning 4))))
	    ;; oder auch: suche nach ->SQL:file
	    (if (string-match "-\>[^ \t]*SQL:" command)
		(SQL:func (substring command (match-end 0)) nil)))
	  
	  ;; ---------- FILE-Button
	  ;; suche nach ->>...FILE:file*RegexStelle
	  (if (string-match "\\(-\>\>[^ \t]*FILE:\\)\\(.*\\*\\)\\([^ \t\r;]*\\)\\(;?\\)" command)
	      (FILE:func (substring command (match-beginning 2) (- (match-end 2) 1))
			 (concat ">" (substring command (match-end 2) (match-beginning 4))))
	    ;; oder auch: suche nach ->>FILE:RegexStelle (dann im aktuellen File)
	    (if (string-match "-\>\>[^ \t]*FILE:" command)
		(FILE:func nil (concat ">"(substring command (match-end 0))))
	      ;; suche nach ->...FILE:file*RegexStelle
	      (if (string-match "\\(-\>[^ \t]*FILE:\\)\\(.*\\*\\)\\([^ \t\r;]*\\)\\(;?\\)" command)
		  (FILE:func (substring command (match-beginning 2) (- (match-end 2) 1))
			     (substring command (match-end 2) (match-beginning 4)))
		;; oder auch: suche nach ->FILE:RegexStelle (dann im aktuellen File)
		(if (string-match "-\>[^ \t]*FILE:" command)
		    (FILE:func nil (substring command (match-end 0)))))))
	  
	  ;; ---------- A-Button (für den eigenen Adressenfile) (Kurzform für ->FILE:adressen.op*"Name"
	  ;; suche nach ->...A:RegexStelle
	  (if (string-match "-\>[^ \t]*A:" command)
	      (FILE:func adressenfile (substring command (match-end 0))))

	  ;; ---------- FUNC-Button
	  ;; suche nach ->...FUNC:funcfile*func, sehe im funcfile nach, was func machen soll
	  ;;    und führe dieses dann aus
	  (if (string-match "\\(-\>[^ \t]*FUNC:\\)\\(.*\\*\\)\\([^ \t\r;]*\\)\\(;?\\)" command)
	      (progn
		(FUNC:func (substring command (match-beginning 2) (- (match-end 2) 1))
			   (substring command (match-end 2) (match-beginning 4))))
	    ;; oder nehme dafür den Default-Funcfile: .op-navigate
	    (if (string-match "-\>[^ \t]*FUNC:" command)
		(FUNC:func nil (substring command (match-end 0)))))
	  
	  ;; ---------- EVAL-Button
	  ;; suche nach ->...EVAL:programm*file, ruft Programm auf, um damit File auszuführen
	  (if (string-match "\\(-\>[^ \t]*EVAL:\\)\\(.*\\*\\)\\([^ \t\r;]*\\)\\(;?\\)" command)
	      (progn
		(EVAL:func (substring command (match-beginning 2) (- (match-end 2) 1))
			   (substring command (match-end 2) (match-beginning 4))))
	    ;; oder nach EVAL:file, dieses sucht sich 
	    (if (string-match "-\>[^ \t]*EVAL:" command)
		(EVAL:func nil (substring command (match-end 0)))))
	  
	  ;; ---------- REF-Button
	  ;; suche nach ->...REF:file*RegexStelle
	  (if (string-match "\\(-\>[^ \t]*REF:\\)\\(.*\\*\\)\\([^ \t\r;]*\\)\\(;?\\)" command)
	      (progn
		(REF:func (substring command (match-beginning 2) (- (match-end 2) 1))
			  (substring command (match-end 2) (match-beginning 4))))
	    ;; oder auch: suche nach ->REF:RegexStelle (dann im aktuellen File)
	    (if (string-match "-\>[^ \t]*REF:" command)
		(REF:func nil (substring command (match-end 0)))))
	  
	  ;; ---------- REF-R-Button (R für rekursiv)
	  ;; suche nach ->...REF-R:file*RegexStelle: Führe neu gefundenen Button sogleich wieder aus
	  (if (not stop-recursive)  ; interaktiv gerufen
	      (progn
		;; suche Position von REF-R
		(save-excursion
		  (let ((suchende (re-search-forward "[ \t\r]\\$" nil t)) (suchanfang))
		    (backward-char 1)
		    (setq suchanfang (re-search-backward "[ \t\r]\\|^" nil t))
		    (if (search-forward "REF-R" suchende t)
			(progn
			  (search-backward "REF-R" suchanfang t)
			  (setq current-point-tmp1 (point))
			  (setq current-buffer-tmp1 (current-buffer))))))))
	  ;; wenn nicht interaktiv gerufen und nicht im selben Buffer am selben Punkt
	  (if (not (and stop-recursive
			(eq current-point-tmp1 (point))
			(eq current-buffer-tmp1 (current-buffer))))
	      (if (string-match "\\(-\>[^ \t]*REF-R:\\)\\(.*\\*\\)\\([^ \t\r;]*\\)\\(;?\\)" command)
		  (progn
		    (REF-R:func (substring command (match-beginning 2) (- (match-end 2) 1))
				(substring command (match-end 2) (match-beginning 4))))
		;; oder auch: suche nach ->REF-R:RegexStelle (dann im aktuellen File)
		(if (string-match "-\>[^ \t]*REF-R:" command)
		    (REF-R:func nil (substring command (match-end 0)))))
	    (message "Returned to current button: stop")
	    (delete-other-windows)
	    (isearch-dehighlight t)
	    (recenter)))
      
      
      ;; ---------- Default-Button 
      ;;
      ;; erstmal: ->etwashier
      ;;
      (if (string-match "^-\>" command)
	  ;; Suche nach ->!programm oder ->!programm*file  ( entspricht EVAL: )
	  (if (string-match "-\>!" command)
	      (if (string-match "\\(-\>!\\)\\(.*\\*\\)" command)
		  (progn
		    (EVAL:func (substring command (match-beginning 2) (- (match-end 2) 1))
			       (substring command (match-end 2))))
		;; oder auch: suche nach ->!programm
		(if (string-match "-\>!" command)
		    ;;(nur ->file*RegexStelle oder nur ->RegexStelle
		    (EVAL:func nil (substring command (match-end 0)))))
	    ;; oder nach ->>file*RegexStelle
	    (if (string-match "\\(-\>\>\\)\\(.*\\*\\)" command)
		(FILE:func (substring command (match-beginning 2) (- (match-end 2) 1))
			   (concat ">" (substring command (match-end 2) (match-beginning 4))))
	      ;; oder auch: suche nach ->>RegexStelle (dann im aktuellen File)
	      (if (string-match "-\>\>" command)
		  (FILE:func nil (concat ">" (substring command (match-end 0))))
		;; suche nach ->file*RegexStelle
		(if (string-match "\\(-\>\\)\\(.*\\*\\)" command)
		    (FILE:func (substring command (match-beginning 2) (- (match-end 2) 1))
			       (substring command (match-end 2) (match-beginning 4)))
		  ;; mit / im Link: daraus wird ein FILE: extrapoliert
		  (if (or (string-match "\\(-\>\\)\\(.*/.*\\)" command)
			  ;; mit . mit Nachbuchstabe im Link: daraus wird ein FILE: extrapoliert
			  (string-match "\\(-\>\\)\\(.*\\.+.*\\)" command))
		      (FILE:func (substring command (match-beginning 2) (match-end 2))
				 (substring command (match-end 2) (match-beginning 4)))
		    ;; oder auch: suche nach ->RegexStelle (dann im aktuellen File)
		    (if (string-match "-\>" command)
			(FILE:func nil (substring command (match-end 0)))))))))

;	    (if (string-match "\\(-\>\\)\\(.*\\*\\)" command)
;		(FILE:func (substring command (match-beginning 2) (- (match-end 2) 1))
;			   (substring command (match-end 2)))
;	      ;; oder auch: suche nach ->RegexStelle (dann im aktuellen File)
;	      (if (string-match "-\>" command)
;		  (FILE:func nil (substring command (match-end 0))))))
	(message "not recognised as a button")))
    (setq called-via-OP-button nil)
    ))



(defun SQL:func (file startpunkt)
  (let ((pointsv (point-min)) (current-buf (current-buffer)))
;    (if startpunkt
;	(setq startpunkt (regexp-quote startpunkt)))
    (if (get-buffer (file-name-nondirectory file))
	(switch-to-buffer (file-name-nondirectory file))
      (if (not (file-exists-p file))
	  (progn
	    (delete-other-windows)
	    (error "File not found  " file))
	(if (file-exists-p file)
	    (find-file file)
	  (if (file-exists-p (concat file ".crypt"))
	      (find-file (concat file ".crypt"))))))
    (goto-char (point-min))
    ;; wenn Startpunkt im Buffer angegeben ist,
    (if startpunkt
	(progn
	  (re-search-forward startpunkt nil t)
	  ;; dann eval ab dort -
	  (setq pointsv (point))
	  ;; bis zum "END"-statement
	  (if (re-search-forward "END" nil t)
	      (backward-char (length "END"))
	    ;; oder bis zum Ende des Files
	    (goto-char (point-max)))
	  ;; ansonsten: sowieso bis zum Ende
	  (narrow-to-region pointsv (point)))
      (goto-char (point-max)))
;;;    (SQLeval buffer)
    (sql-send-region pointsv (point))
    (widen)
    (switch-to-buffer current-buf)))
;;
;; Beispiele
;;  
;   ->SQL:sqlfile.sql*startpunkt
;   ->SQL:sqlfile2.sql
;
  




(defvar am-i-at-point 0)
(make-local-variable 'am-i-at-point)
(defun FILE:func (file regex)
  ;; Verhalten: Beim ersten Betätigen: Suche ersten Verweis. Ist schon ein Fenster offen: 
  ;; siehe nach, ob sich der Cursor bewegt hat. Wenn nicht: Suche nächsen Verweis.
  (let ((pattern regex) (current-buf (current-buffer)) (was-one-window nil) (originalpattern)
	(no-headline-search t) (setto-other-window nil) (einsplus 0))
;    (setq regex (regexp-quote regex))
    (if (one-window-p)
	(progn
	  (split-window)
	  (setq was-one-window t)))
    (other-window 1)
    ;; ist ein Pfeil am Ende des Ausdrucks von "file" ? "pattern" ist "regex" ohne "->" am Ende.
    (if file
    (if (string-match "-\>$" file)
	(progn
	  (setq file (substring file 0 -2))
	  (setq setto-other-window 1))
    (if (string-match "-$" file)
	(progn
	  (setq file (substring file 0 -1))
	  (setq setto-other-window 2)))))
    ;; --
    (if file
	(if (get-buffer (file-name-nondirectory file))
	    (progn
	      (switch-to-buffer (file-name-nondirectory file))
	      (if was-one-window
		  (setq am-i-at-point 0)))
	  (if (file-exists-p file)
	      (find-file file)
	    (if (file-exists-p (concat file ".crypt"))
		(find-file (concat file ".crypt"))
	      (delete-other-windows)
	      (error "File not found  " file)))
	  (setq am-i-at-point 0))
      (switch-to-buffer current-buf))
    ;; ist ein Pfeil am Ende des Ausdrucks ? "pattern" ist "regex" ohne "->" am Ende.
    (if (string-match "-\>$" regex)
	(setq pattern (substring regex 0 -2)))
    (if (string-match "-$" regex)
	(setq pattern (substring regex 0 -1)))
    ;; wenn am Ende des Musters ein zweiter . ist, dann streiche diesen weg
    (if (string-match ".*\\..*\\.$" regex)
	(progn
	  (setq pattern (substring regex 0 -1))
	  (setq einsplus 1)))
    ;; schaue nach, ob pattern Anführungsstriche enthält ( wie "Muster" )
    (if (string-match "^\".*\"$" pattern)
	;; wenn ja: ersten und letzen Character wegstreichen (sollten die Anführungsstriche sein)
	(setq pattern (substring pattern 1 -1))
      ;; das gleiche auch, wenn das Muster aussieht wie ->>"Muster" (für detailierte Suche) 
      (if (string-match "^>\".*\"$" pattern)
	  (setq pattern (concat ">" (substring regex 2 -1))))) ;; das ">" (von "->" + ">" = "->>") wird 
                                                               ;; einfach an das geschnittene 
					                       ;; Muster weiterverarbeitet
    (setq originalpattern pattern)
    ;;
    ;;
    ;;
    ;; wenn im Pattern ein * am Anfang steht, ist der Stern nicht als regex gemeint, sondern zum Suchen einer
    ;; Hauptüberschrift. * steht dann für eine Überschrift erster Ordnung, ** für zweite Ordnung u.s.w..
    (if (string-match "^\\*" pattern)
	(progn
	  ;;(setq pattern (substring pattern 2))
	  (string-match "\\([*]+\\)\\(.*\\)" pattern)
	  (setq pattern
		(concat
		 (regexp-quote (match-string 1 pattern))
		 ".?.?"   ; was nach den Sternen der Ueberschriften kommt (E fuer erledigt, D fuer dringend,...)
		          ; aber auch eventuell Blanks
		 ;; rechne die Anzahl der Sterne in Tabs um
		 (if (< 1 (length (match-string 1 pattern)))
		     (concat (make-string (if (< 0 (- (length (match-string 1 pattern)) 3))
					      (- (length (match-string 1 pattern)) 3)
					    0) ?	))
		   ;; oder, wenn nur ein Stern: ein Blank
		   (make-string 1 ? ))
		 ;; Muster als vollständiges Wort
		 (concat "\\<" (match-string 2 pattern) "\\>"))))
      ;; ansonsten: wenn das Muster ->> lautet, dann lasse es fast so, wie es ist, ansonsten suche nach Überschriften
      (if (string-match "^>" pattern)
	  ;; entferne das erste ">" im Muster
	  (if (string= pattern ">")
	      (setq pattern (substring pattern 1))
	    ;; suche nur ganzes Wort, keine Teilstrings !
	    (setq pattern (concat "\\<" (substring pattern 1) "\\>"))
	    (setq originalpattern pattern))
	;; mache ein Überschriften-Suchmuster daraus:
	;; ->> sucht alles, -> sucht Überschriften mit dem Muster als vollständigem Wort
	(setq originalpattern pattern)
	(if (not (string= pattern ""))
	    (progn
	      ;; Überschriftenmuster und kein Pfeil (->) im Muster vornedran
	      (setq pattern (concat "[*]+.*" "[ \t(<{[]" pattern "\\>"))
	      (setq no-headline-search nil)))))
    ;; Wenn kein Verweis auf ein anderes Fenster stattfindet ("->"), dann
    ;; ist u.U. eine Mehrfachsuche sehr nützlich. Dazu:
    ;; Wenn schon mal gesucht wurde und der Cursor im gesuchten Buffer 
    ;; nicht bewegt wurde: nicht zum Anfang zurück, sondern weitersuchen
    (if (= am-i-at-point (point))
	(forward-char 1)
      (goto-char (point-min)))
    ;; suche nach regex, aber verbiete Suche nach ->...regex (damit Knöpfe sich nicht selbst finden)
    ;;
    ;; ja ja ich weiß, der nächste Teil könnte sehr viel schöner geschrieben werden (wie so manches hier),
    ;; ich habe ihn aber so gelassen, da er sonst für meinen Geschmack unlesbar dicht geworden wäre.
    ;;
    (let ((tmp 0))
      (progn
	(message "searching...")
	(while
	    ;; wenn er das Muster "pattern" findet ohne einen Pfeil "->" vornedran
	    (if (or
		 (re-search-forward (concat "^" pattern) nil t)
		 (and
		  no-headline-search
		  (re-search-forward (concat "\\([ \t\r]\\|^\\)\\([^ \t\r]*\\)" pattern) nil t)
		  (not (string-equal "->" (buffer-substring-no-properties (match-beginning 2) (+ 2 (match-beginning 2)))))))
		(progn
		  (message " ")
		  ;; dann dieses hier
		  (re-search-backward originalpattern nil t)
		  (save-match-data
		    (forward-char 1)
		    (backward-char 1)
		    (if (fboundp 'outline-visible)
			(if (not (outline-visible))
			    (progn
			      (save-excursion
				(forward-char 1)
				(backward-char 1)
				(if (re-search-backward "[*]" nil t)
				    (if (fboundp 'show-entry-hide)
					(show-entry-hide t)
				      (if (fboundp 'show-entry)
					  (show-entry))))))))
		    (recenter)) ; Marker !!! und noch einmal gleich ein paar Zeilen weiter
		  (isearch-highlight (match-beginning 0)(+ (match-end 0) einsplus))
		  ;; mit -> am Ende des Strings: wechsle das Fenster
		  (setq am-i-at-point (point))
		  (if (not (string-match "-\>$" regex))
		      (other-window -1)
		    (if (not (string-match "-$" regex))
			(progn
			  (other-window -1)
			  (delete-window))))
		  nil)
	      ;; wenn er es nicht findet, sehe nach, ob er an diesem Punkt schon mal war.
	      (if (not (= tmp (point)))
		  ;; wenn nein, suche weiter
		  (setq tmp (point))
		;; sonst: mache den Pfeil -> zu einem Pfeil ->> und das ganze nochmal
		;;
		;;
		;;
		;; wenn er das Muster "pattern" findet ohne einen Pfeil "->" vornedran
		(if (or
		     (re-search-forward (concat "^" originalpattern) nil t)
		     (and
		      (re-search-forward (concat "\\([ \t\r]\\|^\\)\\([^ \t\r]*\\)" originalpattern) nil t)
		      (not (string-equal "->" 
					 (buffer-substring-no-properties 
					  (match-beginning 2) 
					  (+ 2 (match-beginning 2)))))))
		    (progn
		      (message " ")
		      ;; dann dieses hier
		      (re-search-backward originalpattern nil t)
		      (save-match-data
			(forward-char 1)
			(backward-char 1)
			(if (fboundp 'outline-visible)
			    (if (not (outline-visible))
				(progn
				  (save-excursion
				    (if (re-search-backward "^\\*" nil t)
					(if (fboundp 'show-entry-hide)
					    (show-entry-hide t)
					  (if (fboundp 'show-entry)
					      (show-entry))))))))
			(recenter))
		      (isearch-highlight (match-beginning 0)(+ (match-end 0) einsplus))
		      ;; mit -> am Ende des Strings: wechsle das Fenster
		      (setq am-i-at-point (point))
		      (if (not (string-match "-\>$" regex))
			  (other-window -1)
			(if (not (string-match "-$" regex))
			    (progn
			      (other-window -1)
			      (delete-window))))
		      nil)
		  ;; wenn er es nicht findet, sehe nach, ob er an diesem Punkt schon mal war.
		  (if (not (= tmp (point)))
		      ;; wenn nein, suche weiter
		      (setq tmp (point))
		    ;; sonst: mache den Pfeil -> zu einem Pfeil ->> und das ganze nochmal
		    ;;
		    ;;
		    ;;
		    ;;
		    (message "not found")
		    (message "Didn't find string or pattern")
		    (beep)
		    (delete-window)
		    (recenter)
		    (isearch-dehighlight t)
		    nil)))))))
    (if setto-other-window
	(progn
	  (other-window 1)
	  (if (= setto-other-window 2)
	      (progn
		(other-window -1)
		(delete-window)))))
))
;;
;; Beispiele
;;     ->defun  defun
;   ->FILE:../op-project.el*defun
;   ->>FILE:../op-project.el*defun->
;   ->>FILE:defun
;   ->>FILE:defun->
;





;;;
;;; Die nächste Funktion ist gut, wenn man die Pfeile -> und ->> scharf getrennt haben will:
;;; -> NUR in Überschriften, ->> überall. Dazu müssen die Funktionen FILE:func und FILE:func2 
;;; getauscht werden. 
;;;
;
;(defvar am-i-at-point 0)
;(make-local-variable 'am-i-at-point)
;(defun FILE:func2 (file regex)
;  ;; Verhalten: Beim ersten Betätigen: Suche ersten Verweis. Ist schon ein Fenster offen: 
;  ;; siehe nach, ob sich der Cursor bewegt hat. Wenn nicht: Suche nächsen Verweis.
;  (let ((pattern regex) (current-buf (current-buffer)) (was-one-window nil) (originalpattern)
;	(no-headline-search t))
;;    (setq regex (regexp-quote regex))
;    (if (one-window-p)
;	(progn
;	  (split-window)
;	  (setq was-one-window t)))
;    (other-window 1)
;    (if file
;	(if (get-buffer (file-name-nondirectory file))
;	    (progn
;	      (switch-to-buffer (file-name-nondirectory file))
;	      (if was-one-window
;		  (setq am-i-at-point 0)))
;	  (if (file-exists-p file)
;	      (find-file file)
;	    (if (file-exists-p (concat file ".crypt"))
;		(find-file (concat file ".crypt"))
;             (delete-other-windows)
;	      (error "File not found  " file))
;	  (setq am-i-at-point 0))
;      (switch-to-buffer current-buf))
;    ;; ist ein Pfeil am Ende des Ausdrucks ? "pattern" ist "regex" ohne "->" am Ende.
;    (if (string-match "-\>$" regex)
;	(setq pattern (substring regex 0 -2)))
;    (if (string-match "-$" regex)
;	(setq pattern (substring regex 0 -1)))
;    ;; schaue nach, ob pattern Anführungsstriche enthält ( wie "Muster" )
;    (if (string-match "^\".*\"$" pattern)
;	;; wenn ja: ersten und letzen Character wegstreichen (sollten die Anführungsstriche sein)
;	(setq pattern (substring pattern 1 -1))
;      ;; das gleiche auch, wenn das Muster aussieht wie ->>"Muster" (für detailierte Suche) 
;      (if (string-match "^>\".*\"$" pattern)
;	  (setq pattern (concat ">" (substring regex 2 -1))))) ;; das ">" (von "->" + ">" = "->>") wird 
;                                                               ;; einfach an das geschnittene 
;					                       ;; Muster weiterverarbeitet
;    (setq originalpattern pattern)
;    ;; wenn im Pattern ein * am Anfang steht, ist der Stern nicht als regex gemeint, sondern zum Suchen einer
;    ;; Hauptüberschrift. * steht dann für eine Überschrift erster Ordnung, ** für zweite Ordnung u.s.w..
;    (if (string-match "^\\*" pattern)
;	(progn
;	  ;;(setq pattern (substring pattern 2))
;	  (string-match "\\([*]+\\)\\(.*\\)" pattern)
;	  (setq pattern
;		(concat
;		 (regexp-quote (match-string 1 pattern))
;		 ".?.?"   ; was nach den Sternen der Ueberschriften kommt (E fuer erledigt, D fuer dringend,...)
;		          ; aber auch eventuell Blanks
;		 ;; rechne die Anzahl der Sterne in Tabs um
;		 (if (< 1 (length (match-string 1 pattern)))
;		     (concat (make-string (if (< 0 (- (length (match-string 1 pattern)) 3))
;					      (- (length (match-string 1 pattern)) 3)
;					    0) ?	))
;		   ;; oder, wenn nur ein Stern: ein Blank
;		   (make-string 1 ? ))
;		 ;; Muster als vollständiges Wort
;		 (concat "\\<" (match-string 2 pattern) "\\>"))))
;      ;; ansonsten: wenn das Muster ->> lautet, dann lasse es fast so, wie es ist, ansonsten suche nach Überschriften
;      (if (string-match "^>" pattern)
;	  ;; entferne das erste ">" im Muster
;	  (if (string= pattern ">")
;	      (setq pattern (substring pattern 1))
;	    ;; suche nur ganzes Wort, keine Teilstrings !
;	    (setq pattern (concat "\\<" (substring pattern 1) "\\>"))
;	    (setq originalpattern pattern))
;	;; mache ein Überschriften-Suchmuster daraus:
;	;; ->> sucht alles, -> sucht Überschriften mit dem Muster als vollständigem Wort
;	(setq originalpattern pattern)
;	(if (not (string= pattern ""))
;	    (progn
;	      ;; Überschriftenmuster und kein Pfeil (->) im Muster vornedran
;	      (setq pattern (concat "[*]+.*" "[ \t(<{[]" pattern "\\>"))
;	      (setq no-headline-search nil)))))
;    ;; Wenn kein Verweis auf ein anderes Fenster stattfindet ("->"), dann
;    ;; ist u.U. eine Mehrfachsuche sehr nützlich. Dazu:
;    ;; Wenn schon mal gesucht wurde und der Cursor im gesuchten Buffer 
;    ;; nicht bewegt wurde: nicht zum Anfang zurück, sondern weitersuchen
;    (if (= am-i-at-point (point))
;	(forward-char 1)
;      (goto-char (point-min)))
;    ;; suche nach regex, aber verbiete Suche nach ->...regex (damit Knöpfe sich nicht selbst finden)
;    ;;
;    ;; ja ja ich weiß, der nächste Teil könnte sehr viel schöner geschrieben werden (wie so manches hier),
;    ;; ich habe ihn aber so gelassen, da er sonst für meinen Geschmack unlesbar dicht geworden wäre.
;    ;;
;    (let ((tmp 0))
;      (progn
;	(message "searching...")
;	(while
;	    ;; wenn er das Muster "pattern" findet ohne einen Pfeil "->" vornedran
;	    (if (or
;		 (re-search-forward (concat "^" pattern) nil t)
;		 (and
;		  no-headline-search
;		  (re-search-forward (concat "\\([ \t\r]\\|^\\)\\([^ \t\r]*\\)" pattern) nil t)
;		  (not (string-equal "->" (buffer-substring-no-properties (match-beginning 2) (+ 2 (match-beginning 2)))))))
;		(progn
;		  (message " ")
;		  ;; dann dieses hier
;		  (recenter)
;		  (isearch-highlight (match-beginning 0)(match-end 0))
;		  (re-search-backward originalpattern nil t)
;		  ;; mit -> am Ende des Strings: wechsle das Fenster
;		  (setq am-i-at-point (point))
;		  (if (not (string-match "-\>$" regex))
;		      (other-window -1)
;		    (if (not (string-match "-$" regex))
;			(progn
;			  (other-window -1)
;			  (delete-window))))
;		  nil)
;	      ;; wenn er es nicht findet, sehe nach, ob er an diesem Punkt schon mal war.
;	      (if (not (= tmp (point)))
;		  ;; wenn nein, suche weiter
;		  (setq tmp (point))
;		;; sonst: Abbruch
;		(message "not found")
;		(message "no-pattern-stop")
;		(delete-window)
;		(recenter)
;		(isearch-dehighlight t)
;		nil)))))))


(defun FUNC:func (file startpunkt)
  (let ((pointsv (point)) (current-buf (current-buffer)))
;    (setq startpunkt (regexp-quote startpunkt))
    (if file
	(if (get-buffer (file-name-nondirectory file))
	    (set-buffer (file-name-nondirectory file))
	  (if (file-exists-p file)
	      (find-file file)
	    (if (file-exists-p (concat file ".crypt"))
		(find-file (concat file ".crypt")))))
      (if (file-exists-p ".op-navigate")
	  (find-file ".op-navigate")
	(error "no file \".op-navigate\" in current directory found")))
    (goto-char (point-min))
    ;; wenn Startpunkt im Buffer angegeben ist,
    (if startpunkt
	(progn
	  (re-search-forward startpunkt nil t)
	  ;; dann eval ab dort -
	  (setq pointsv (point))
	  ;; bis zum "END"-statement
	  (if (re-search-forward "END" nil t)
	      (backward-char (length "END"))
	    ;; oder bis zum Ende des Files
	    (goto-char (point-max))))
      (goto-char (point-max)))
    ;; ansonsten: sowieso bis zum Ende
    (narrow-to-region pointsv (point))
    (eval-current-buffer)
    (widen)
    (switch-to-buffer current-buf)))
;;
;; Beispiele
;;  
;   ->FUNC:funcfile*startpunkt
;   ->FUNC:funcfile*Fakultät
;   ->FUNC:startpunkt
;



(defun EVAL:func (program file)
  ;; wenn programm: führe dieses aus
  ;; ACHTUNG: Kommandozeilenargumente werden nur genommen, wenn ein Prozentzeichen auftaucht !
  (if program
      (progn
	;; schaue nach, ob da was in Anführungsstrichen herumliegt (wie "Programm")
	(if (string-match "^\".*\"$" program)
	    ;; ersten und letzen Character wegstreichen (sollten die Anführungsstriche sein)
	    (setq program (substring program 1 -1)))
	(shell-command (concat program " " file)))
    ;; wenn nicht: Suche nach Extention
    (if (string-match "^\".*\"$" file)
	;; ersten und letzen Character wegstreichen (sollten die Anführungsstriche sein)
	(setq file (substring file 1 -1)))
    (if (string-match ".tki" file)
	(shell-command (concat "tkined" " " file))
      ;; weitere Suche nach Extention...
      (if (string-match ".pl" file)
	  (shell-command (concat "perl" " " file))
	(if (string-match ".fig" file)
	    (shell-command (concat "xfig" " " file))
	  (if (string-match ".py" file)
	      (shell-command (concat "python" " " file))
	    ;; wenn bisher noch keine Extention gematcht hat, führe den File direkt aus
	    (shell-command file))))))
  ;; im Falle eines Batch-Betriebes: lösche das Fenster, daß die asynchone Ausgabe aufnimmt
  (if (string-match "&" file)
      (progn
	(other-window 1)
	(switch-to-buffer "*Asynch Shell Command*")
	(delete-window)
	(message (concat "Output stream of " file " goes to buffer \"*Asynch Shell Command*\"")))))
;;

;; Beispiele
;;  
;   ->EVAL:xeyes
;   ->EVAL:xeyes&
;   ->EVAL:"date --version"  
;   ->EVAL:xfig*test.fig
;   ->EVAL:"xfig -bg red"*test.fig    ; Argumente in Anführungsstrichen, Ausführungspunkt NUR vor dem 
;                                     ; ersten Blank (beim "->EVAL:xxxx"
;   ->EVAL:"xfig -bg red"*test.fig&   ; wie eben
;   ->FILE:op-hyper.el*defun;EVAL:"date --version"  
;				      ; bei Kettenargumenten: EVAL immer an den Schluß,
;                                     ;  andere Funktionen immer mit *-Zeichen. Nur je ein Buttontyp ist erlaubt.
;   ->EVAL:test.fig                   ; Defaultextention ist einprogrammiert (siehe oben)
;  



(defvar am-i-at-point-tmp 0)
(make-local-variable 'am-i-at-point-tmp)
(defun REF:func (file button-match)
  ;; Verhalten: Beim ersten Betätigen: Suche erstes Button-Pattern. Ist schon ein Fenster offen: 
  ;; siehe nach, ob sich der Cursor bewegt hat. Wenn nicht: Suche nächsen Verweis.
  (let ((pattern button-match) (current-buf (current-buffer)) (was-one-window nil))
;    (setq button-match (regexp-quote button-match))
    (if (one-window-p)
	(progn
	  (split-window)
	  (setq was-one-window t)))
    (other-window 1)
    (if file
	(if (get-buffer (file-name-nondirectory file))
	    (progn
	      (switch-to-buffer (file-name-nondirectory file))
	      (if was-one-window
		  (setq am-i-at-point-tmp 0)))
	  (if (file-exists-p file)
	      (find-file file)
	    (if (file-exists-p (concat file ".crypt"))
		(find-file (concat file ".crypt"))))
	  (setq am-i-at-point-tmp 0))
      (switch-to-buffer current-buf))
    ;; ist ein Pfeil am Ende des Ausdrucks ? "pattern" ist "button-match" ohne "->" am Ende.
    (if (string-match "-\>$" button-match)
	(setq pattern (substring button-match 0 -2)))
    (if (string-match "-$" button-match)
	(setq pattern (substring button-match 0 -1)))
      ;; Wenn kein Verweis auf ein anderes Fenster stattfindet ("->"), dann
      ;; ist u.U. eine Mehrfachsuche sehr nützlich. Dazu:
      ;; Wenn schon mal gesucht wurde und der Cursor im gesuchten Buffer 
      ;; nicht bewegt wurde: nicht zum Anfang zurück, sondern weitersuchen
    (if (= am-i-at-point-tmp (point))
	(forward-char 1)
      (goto-char (point-min)))
    ;; suche nach button-match, aber verbiete Suche nach ->...button-match (damit Knöpfe sich nicht selbst finden)
    (if (re-search-forward (concat "-\>" pattern) nil t)
	(progn
	  (recenter)
	  (isearch-highlight (match-beginning 0)(match-end 0))
	  (forward-char 1)
	  ;; mit -> am Ende des Strings: wechsle das Fenster
	  (setq am-i-at-point-tmp (point))
	  (if (not (string-match "-\>$" button-match))
	      (other-window -1)
	    (if (not (string-match "-$" button-match))
		(progn
		  (other-window -1)
		  (delete-window)))))
      (message "no-pattern-stop")
      (delete-window)
      (isearch-dehighlight t)
      (recenter))))
;;
;; Beispiele
;;     ->defun  defun
;   ->REF:../op-hyper/op-hyper.el*1button      ; Muster ist hier "1button", kann aber beliebig sein
;   ->REF:../op-hyper/op-hyper.el*1button->
;   ->REF:1button
;   ->REF:1button->
;
; die alle sollten hier enden:   ->1button:FILE:defun   ; Doppelpunkt ist günstig, aber nicht Pflicht:
;                                ->1button#FILE:defun   ; geht z. B. auch
;


(defvar am-i-at-point-tmp-R 0)
(make-local-variable 'am-i-at-point-tmp-R)
(defun REF-R:func (file button-match)
  ;; Verhalten: Beim ersten Betätigen: Suche erstes Button-Pattern. Ist schon ein Fenster offen: 
  ;; siehe nach, ob sich der Cursor bewegt hat. Wenn nicht: Suche nächsen Verweis.
  (let ((pattern button-match) (current-buf (current-buffer)) (was-one-window nil))
;    (setq button-match (regexp-quote button-match))
    (if (one-window-p)
	(progn
	  (split-window)
	  (setq was-one-window t)))
    (other-window 1)
    (if file
	(if (get-buffer (file-name-nondirectory file))
	    (progn
	      (switch-to-buffer (file-name-nondirectory file))
	      (if was-one-window
		  (setq am-i-at-point-tmp-R 0)))
	  (if (file-exists-p file)
	      (find-file file)
	    (if (file-exists-p (concat file ".crypt"))
		(find-file (concat file ".crypt"))))
	  (setq am-i-at-point-tmp-R 0))
      (switch-to-buffer current-buf))
    ;; ist ein Pfeil am Ende des Ausdrucks ? "pattern" ist "button-match" ohne "->" am Ende.
    (if (string-match "-\>$" button-match)
	(setq pattern (substring button-match 0 -2)))
      ;; Wenn kein Verweis auf ein anderes Fenster stattfindet ("->"), dann
      ;; ist u.U. eine Mehrfachsuche sehr nützlich. Dazu:
      ;; Wenn schon mal gesucht wurde und der Cursor im gesuchten Buffer 
      ;; nicht bewegt wurde: nicht zum Anfang zurück, sondern weitersuchen
    (if (= am-i-at-point-tmp-R (point))
	(forward-char 1)
      (goto-char (point-min)))
    ;; suche nach button-match, aber verbiete Suche nach ->...button-match (damit Knöpfe sich nicht selbst finden)
    (if (re-search-forward (concat "-\>" pattern) nil t)
	(progn
	  (recenter)
	  (isearch-highlight (match-beginning 0)(match-end 0))
	  (forward-char 1)
	  ;; mit -> am Ende des Strings: wechsle das Fenster
	  (setq am-i-at-point-tmp-R (point))
	  (if (not (string-match "-\>$" button-match))
	      (other-window -1)
	    (select-hyper-func t)))
      (message "no-pattern-stop")
      (delete-window)
      (isearch-dehighlight t)
      (recenter))))
;;
;; Beispiele (siehe auch die bei REF:func)
;;
;   ->REF-R:../op-hyper/op-hyper.el*2button->      ; Muster ist hier "2button", kann aber beliebig sein
;
;   ->REF-R:2button->
;
;   ->2button:REF-R:3button->
;
; die alle sollten hier enden:   ->3button:FILE:das_hier_gibts_nicht   
;
;
;; weiteres Beispiel: Loop vom ersten Button wird nicht unendlich durchlaufen 
;; (Ideal zum Einlesen vieler Files !!!)
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



;;;
;;; Wie der Name schon sagt: Dieses hier führt den Button aus, wenn ein Mausklick kommt.
;;;
(defun Activate-button-with-mouse (click)
  (interactive "e")
  (let* ((start (event-start click))
	 (window (car start))
	 (pos (car (cdr start))))
    (select-window window)
    (goto-char pos))
  (select-hyper-func))





;(global-set-key "\M-\C-m" 'select-hyper-func)
(define-key (current-local-map) "\M-\C-m" 'select-hyper-func)
(if xemacsp
    (define-key (current-local-map) [(shift button2)] 'Activate-button-with-mouse)
  (define-key (current-local-map) [\S-mouse-2] 'Activate-button-with-mouse))







;;;;;;;includeforelc!
;;;;;;(load "op-hyper/op-consistent")

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

(automatic-change-mode t)            ; Alle Labels automatisch aktualisieren 
(automatic-change-mode)     	     ;(nil macht es aber nicht aus -- geht nur durch toggeln)


;; zur Kontrolle
(message "6")


;;;----------------------------------------------------------------------------------------------------



;;; für die #include-files
;;;;;;;includeforelc!
;;;;;;(load "op-includer")


(defun filename ()
  (let ((begin) (end))
    (save-excursion
      (if (re-search-backward "\"" nil t)
	  (forward-char 1)
	(beginning-of-line))
      (setq begin (point))
      (if (re-search-forward "\"" nil t)
	  (backward-char 1))
      (setq end (point)))
    (buffer-substring-no-properties begin end)))

(defun load-files-at-include-statements (&optional cvs)
  "Schleife über load-file-at-include-statement von Anfang an"
  (interactive "P")
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "#include" nil t)
	(progn
	  (backward-word 1)
	  (backward-char 1)
	  (while (looking-at "#include")
	    (load-file-at-include-statement cvs)
	    (forward-word 1)
	    (forward-char 1)
	    ;;
	    (if (re-search-forward "#include" nil t)
		(progn
		  (backward-word 1)
		  (backward-char 1)))))
      (if window-system
	  (hilit-repaint-command t)))))

(defun load-file-at-include-statement (&optional cvs)
  (interactive "P")
  (let (filename)
    (save-excursion
      (if (looking-at "#include")
	  (progn
	    (forward-word 2)
	    (setq filename (filename)))
	(beginning-of-line)
	(search-forward "\"" (save-excursion (end-of-line) (point)) t)
	(setq filename (filename))))
    (load-file-at-include-statement-sub filename cvs)))

(defun load-file-at-include-statement-sub (filename &optional cvs)
  (let ((before-change-functions nil)(after-change-functions nil)
	(insert-buf) (current-buf (current-buffer)) ediff-result-buffer)
    (save-excursion
      (if (file-exists-p filename)
	  (progn
	    (split-window-vertically 4)
	    (switch-to-buffer "*THIS_FILE_WILL_BE_INCLUDED*")
	    (insert (concat "\n\t" filename))
	    ;; wenn der einzuladende Fileauch ein #include-Statment enthält, dann VERWERFE IHN 
	    ;; mit dem Durchgeben einer Nachricht
	    (goto-char (point-min))
	    (other-window 1)
	    (re-search-forward "[ \t\r\n]" nil t)
	    (backward-char 1)
	    (if cvs
		(progn

		  ;;
		  ;;
		  ;; Die nächsten Zeilen ahben es in sich, nur Rainer hat das so gelöst:
		  ;; ediff-buffers läuft normalerweise weiter, wird hier nur durch 
		  ;; recursive-edit ausgebremst
		  ;;
		  (save-excursion
		    (let ((control-buffer (ediff-.op-to-.cvs.op-file filename)))
		      (set-buffer control-buffer)
		      (local-set-key "q" 'exit-recursive-edit)
		      (recursive-edit)
		      (ediff-quit nil)
		      (setq ediff-result-buffer (current-buffer))))
		  (switch-to-buffer ediff-result-buffer))

	      (find-file filename))
	    (if (search-forward "#include" nil t)
		(progn
		  (ding)(ding)
		  (message (concat "Der File " filename " enthält selbst ein #inlcude: Laden verweigert, da rekursiver Zugriff nicht verfügbar"))
		  (sleep-for 2))
	      (setq insert-buf (current-buffer))
	      (switch-to-buffer current-buf)
	      (end-of-line)
	      (insert-char ?\n 1)
	      (save-excursion
		(insert "\r#end_include_statement"))
	      (insert-buffer insert-buf)
	      (re-search-forward "[\r\n]#end_include_statement" nil t)
	      (switch-to-buffer insert-buf)
	      (set-buffer-modified-p nil)
	      (kill-buffer insert-buf)
	      (switch-to-buffer current-buf)
	      (other-window -1)
	      )
	    (if (not cvs)
		(delete-window))
	    (kill-buffer "*THIS_FILE_WILL_BE_INCLUDED*")
	    (switch-to-buffer current-buf)
	    (message (concat "Der File " filename " wurde eingefügt")))
	(ding)(ding)
	(message (concat "Der File " filename " konnte nicht gefunden werden: nicht eingefügt"))
	(sleep-for 2))))
  (if window-system
      (hilit-repaint-command t)))


(defun save-buffer-with-control ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (search-forward "end_include_statement" nil t)
	(if (y-or-n-p "Sie haben registriert, dass es noch nicht ausgelagerte #include-Files in diesem File gibt? ")
	    (save-buffer)
	  (message "Speichervorgang abgebrochen"))
      (save-buffer))))


(defun extract-and-save-includes (&optional cvs)
  (interactive)
  (let ((before-change-functions nil)(after-change-functions nil) (tmpvar))
    (message "This will take a while --- please wait")
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "#include" nil t)
	  (progn
	    (backward-word 1)
	    (backward-char 1)
	    (let ((insert-buf) (filename) (current-buf (current-buffer)) (mark1) (mark2) (was-a-crypted-file nil))
	      (while (looking-at "#include")

		;; Syntaxcheck
		(save-excursion
		  (if (looking-at "#include")
		      (search-forward "#include" nil t))
		  (if (search-forward "#include" nil t)
		      (setq tmpvar (point))
		    (setq tmpvar (point-max))))
		(save-excursion
		  (if (or (not (search-forward "end_include_statement" nil t))
			  (< tmpvar (point)))
		      (progn
			(ding)(ding)(ding)
			(error "FATALES SPEICHERPROBLEM: Nicht alle #include-Statements sind abgeschlossen. Bitte Label \"\nend_include_statement\"  korrekt einsetzen oder entsprechende Zeilen entfernen !!!")
			(sleep-for 3))))
		(forward-word 2)
		(setq filename (filename))

		(if (or (file-exists-p filename)
			(and (not (file-exists-p filename))
			     (if (y-or-n-p "Bitte bestätigen: Der File wird NEU angelegt, er existiert noch nicht"))))
		    (progn
		      (if (string-match "\\.crypt" filename)
			  (progn
			    (setq was-a-crypted-file t)
			    (setq filename (substring filename 0 (string-match "\\.crypt" filename)))))
		      (re-search-forward "[ \t\r\n]" nil t)
		      (setq mark1 (point))
		      (if (re-search-forward "[\r\n]#end_include_statement" nil t)
			  (progn
			    (forward-char 1)
			    (setq mark2 (point))
			    (re-search-backward "[\r\n]#end_include_statement" nil t)
			    (kill-region mark2 (point))
			    (kill-region mark1 (point))
			    (switch-to-buffer "*save-include*")
			    (yank)
			    (goto-char (point-min))
			    (replace-regexp "\r" "\n")
			    (goto-char (point-min))

			    ;; File herausschreiben, sichern, falls nicht möglich
			    (if (file-writable-p filename)
				(if was-a-crypted-file
				    (progn
				      (write-file-crypted filename t)
				      ;; für gecryptete files im Moment eigentlich nicht supported
				      ;;(if cvs
				      ;;(make-.cvs.op-copy-from-current (buffer-file-name))))
				      )
				  (write-file filename)
				  (if cvs
				      (make-.cvs.op-copy-from-current)))
			      (ding)(ding)
			      (message (concat "File " filename " ist nicht gesichert worden: keine Schreibberechtigung!"))
			      (sleep-for 2)
			      (while (string-match "\/" filename)
				(setq filename (replace-match "%" t t filename)))
			      (setq filename (concat "Buffer-Sicherung_von_" filename))
			      (save-excursion 
				(let ((savebuffer (create-file-buffer filename)))
				  (copy-to-buffer savebuffer (point-min)(point-max))
				  (set-buffer savebuffer)
				  (setq buffer-file-name filename)
				  (set-buffer-modified-p t))))

			    (setq was-a-crypted-file nil)
			    (kill-buffer (current-buffer))
			    (switch-to-buffer current-buf)
			    (if (re-search-forward "#include" nil t)
				(progn
				  (backward-word 1)
				  (backward-char 1))))
			(error "FATAL: FEHLENDES END-LABEL für #include !!! Bitte UNDO aufrufen oder Label \"\nend_include_statement\"  korrekt einsetzen!!!")
			(sleep-for 3)))))
	      (if (search-forward "#include" nil t)
		  (progn
		    (ding)(ding)(ding)
		    (message "FATAL: SPEICHERPROBLEM: Nicht alle #include-Statements sind ABGESCHLOSSEN. Bitte Label \"\nend_include_statement\"  korrekt einsetzen oder entsprechende Zeilen entfernen !!!")
		    (sleep-for 3)))
	      )))
;;;    (save-buffer)
      )))



(defun extract-and-save-this-include (&optional cvs)
  (interactive)
  (let ((before-change-functions nil)(after-change-functions nil) (tmpvar)
	(insert-buf) (filename) (current-buf (current-buffer)) (mark1) (mark2) (was-a-crypted-file nil))
    (save-excursion
      (message "This will take a while --- please wait")
      (if (looking-at "#include")
	  (progn
	    (forward-word 2)
	    (setq filename (filename)))
	(beginning-of-line)
	(search-forward "\"" (save-excursion (end-of-line) (point)) t)
	(setq filename (filename))
	(beginning-of-line))
      
      ;; Syntaxcheck
      (save-excursion
	(if (looking-at "#include")
	    (search-forward "#include" nil t))
	(if (search-forward "#include" nil t)
	    (setq tmpvar (point))
	  (setq tmpvar (point-max))))
      (save-excursion
	(if (or (not (search-forward "end_include_statement" nil t))
		(< tmpvar (point)))
	    (progn
	      (ding)(ding)(ding)
	      (error "FATALES SPEICHERPROBLEM: Nicht alle #include-Statements sind abgeschlossen. Bitte Label \"\nend_include_statement\"  korrekt einsetzen oder entsprechende Zeilen entfernen !!!")
	      (sleep-for 3))))

      (forward-word 2)
      (setq filename (filename))
      (if (or (file-exists-p filename)
	      (and (not (file-exists-p filename))
		   (if (y-or-n-p "Bitte bestätigen: Der File wird NEU angelegt, er existiert noch nicht"))))
	  (progn
	    (if (string-match "\\.crypt" filename)
		(progn
		  (setq was-a-crypted-file t)
		  (setq filename (substring filename 0 (string-match "\\.crypt" filename)))))
	    (re-search-forward "[ \t\r\n]" nil t)
	    (setq mark1 (point))
	    (if (re-search-forward "[\r\n]#end_include_statement" nil t)
		(progn
		  (forward-char 1)
		  (setq mark2 (point))
		  (re-search-backward "[\r\n]#end_include_statement" nil t)
		  (kill-region mark2 (point))
		  (kill-region mark1 (point))
		  (switch-to-buffer "*save-include*")
		  (yank)
		  (goto-char (point-min))
		  (replace-regexp "\r" "\n")
		  (goto-char (point-min))
		  ;; File herausschreiben, sichern, falls nicht möglich
		  (if (file-writable-p filename)
		      (if was-a-crypted-file
			  (progn
			    (write-file-crypted filename t)
			    ;; für gecryptete files im Moment eigentlich nicht supported
			    ;;(if cvs
			    ;;(make-.cvs.op-copy-from-current (buffer-file-name))))
			    )
			(write-file filename)
			(if cvs
			    (make-.cvs.op-copy-from-current)))
		    (ding)(ding)
		    (message (concat "File " filename " ist nicht gesichert worden: keine Schreibberechtigung!"))
		    (sleep-for 2)
		    (while (string-match "\/" filename)
		      (setq filename (replace-match "%" t t filename)))
		    (setq filename (concat "Buffer-Sicherung_von_" filename))
		    (save-excursion 
		      (let ((savebuffer (create-file-buffer filename)))
			(copy-to-buffer savebuffer (point-min)(point-max))
			(set-buffer savebuffer)
			(setq buffer-file-name filename)
			(set-buffer-modified-p t))))
		  
		  (setq was-a-crypted-file nil)
		  (kill-buffer (current-buffer))
		  (switch-to-buffer current-buf))
	      (error "FATAL: FEHLENDES END-LABEL für #include !!! Bitte UNDO aufrufen oder Label \"\nend_include_statement\"  korrekt einsetzen!!!")
	      (sleep-for 3)))))))



(defun hide-end-of-include-statments ()
  (let ((already-modified (buffer-modified-p)))
    (save-excursion
      (goto-char (point-min))
      (replace-regexp "\n#end_include_statement" "\r#end_include_statement"))
    (set-buffer-modified-p already-modified)))


(define-project-mode-key "\M-i" 'load-files-at-include-statements)
(define-project-mode-key "\M-z" 'extract-and-save-includes)




;;; --------------------------------------------------------------------------------
;;; --------------------------------------------------------------------------------
;;; --------------------------------------------------------------------------------




(defun kill-region-insert-include (name crypted)
  (interactive
   (list
    (let ((transient-mark-mode transient-mark-mode))
      (setq transient-mark-mode t)
      (exchange-point-and-mark)
      (exchange-point-and-mark)
      (if buffer-file-name
	  (read-file-name "Write region to file (without extention .crypt): "
			  nil nil nil nil)
	(read-file-name "Write region to file (without extention .crypt): "
			(cdr (assq 'default-directory
				   (buffer-local-variables)))
			nil nil (buffer-name))))
    (yes-or-no-p "Do you want to write the file crypted ?	" )))
  (let ((before-change-functions nil)(after-change-functions nil))
    (write-region-crypted name crypted t)
    ;; an den Anfang der Region
    (if xemacsp (setq mark-active (mark)))
    (goto-char (if mark-active 
		   (if (< (region-beginning)(region-end))
		       (region-beginning)
		     (region-end))
		 (point-min)))
    ;; Einsetzen des #include
    (insert (concat "\n#include \"" name (if crypted ".crypt") "\"\n"))
    (backward-char 1)
    (if window-system
	(hilit-repaint-command t))))


(defun kill-region-insert-include-and-mail (name crypted)
  (interactive
   (list
    (let ((transient-mark-mode transient-mark-mode))
      (setq transient-mark-mode t)
      (exchange-point-and-mark)
      (exchange-point-and-mark)
      (if buffer-file-name
	  (read-file-name "Write region to file (without extention .crypt): "
			  nil nil nil nil)
	(read-file-name "Write region to file (without extention .crypt): "
			(cdr (assq 'default-directory
				   (buffer-local-variables)))
			nil nil (buffer-name))))
    (yes-or-no-p "Do you want to write the file crypted ?	" )))
  (kill-region-insert-include name crypted)
  ;; send mail
  (message "Sending Mail")
  (sleep-for 1)
  (let ((tmpposition)(before-change-functions nil)(after-change-functions nil))
    (copy-region-as-kill (save-excursion (beginning-of-line)(setq tmpposition (point))) (point))
    (mh-smail-other-window)
    (insert-char ?\n 1)
    (save-excursion
      (insert-char ?\n 1)
      (yank)
      (insert-char ?\n 2))))





(condition-case var

;;; ediff-Erweiterung, damit fine-diffs besser unterstützt werden

(load "ediff")

;;;;;;;includeforelc!
;;;;;;(load "op-ediff")

;;;
;;;
;;;   Leicht variierter Ediff-Mode zum Überladen für die 2er-Befehle des
;;;   Ediffs (ediff-buffers, ediff-files,...). 
;;;
;;;   Neue Eigenschaft: Möglichkeit, Fine-Diff-Regionen variieren und manipulieren 
;;;   zu können, ohne die großen Difference-Regionen zu beeinflussen (sehr nützlich
;;;   falls zum Beispiel bei kleinen Änderungen auch noch Verschiebungen in der 
;;;   Indentierung vorgenommen wurden)
;;;


;;XEmacs 21.11: BUG: ediff-diff3-programm ist nicht definiert
(if xemacsp
    (defcustom ediff-diff3-program "diff3"
      "*Program to be used for three-way comparison.
Must produce output compatible with Unix's diff3 program."
      :type 'string
      :group 'ediff-diff))


(condition-case var
    (require 'ediff)
  (error "Problem loading ediff package "))

(if gnuemacsp
    (progn
      (ediff-defvar-local ediff-current-fine-difference -1 "")
      (ediff-defvar-local ediff-current-difference-save -1 "")))




(defun ediff-goto-fine-difference-s (from-buffer to-buffer arg)
  (interactive "P")
  (ediff-barf-if-not-control-buffer)

  (let ((ediff-vector-from (symbol-value (intern (format "ediff-difference-vector-%S" from-buffer))))
	(ediff-vector-to   (symbol-value (intern (format "ediff-difference-vector-%S" to-buffer))))
	fine-diff-number fine-diff-max current-diff
	begin-from end-from begin-to end-to
	(ecd ediff-current-difference)
	(ediff-multiframe-sv ediff-multiframe)
	)

    (setq fine-diff-number (max 1 arg)
	  fine-diff-max (if (aref
			     (ediff-get-difference ediff-current-difference from-buffer)
			     1)
			    (progn
			      (setq ediff-current-difference ecd)
			      (length (aref
				       (ediff-get-difference ediff-current-difference from-buffer)
				       1)))
			  0)
	  fine-diff-number (max (min (- fine-diff-max 1) arg) 0))

    (if (>= ediff-current-fine-difference fine-diff-max)
	(setq ediff-current-fine-difference (- fine-diff-max 1)))
    (if (< ediff-current-fine-difference 0)
	(setq ediff-current-fine-difference 0))

    (if (> fine-diff-max 0)
	(progn
	  (setq begin-from (overlay-start
			    (aref
			     (aref
			      (ediff-get-difference ediff-current-difference from-buffer)
			      1)
			     fine-diff-number))
		begin-to   (overlay-start
			    (aref
			     (aref
			      (ediff-get-difference ediff-current-difference to-buffer)
			      1)
			     fine-diff-number)))

	  (let ((current-buf (current-buffer))
		(current-frame (window-frame (selected-window)))
		(b1 (symbol-value (intern (format "ediff-buffer-%S" from-buffer))))
		(b2 (symbol-value (intern (format "ediff-buffer-%S" to-buffer)))))
	    (pop-to-buffer b2)
	    (goto-char begin-to)
	    (pop-to-buffer b1)
	    (goto-char begin-from)
	    (if (and
		 (ediff-window-display-p)
		 ediff-multiframe-sv)  ;;; spät -- SMW
		(raise-frame current-frame)
	      (pop-to-buffer current-buf))
	    )))))


(defun ediff-copy-fine-difference (from-buffer to-buffer &optional kill)
  (interactive "P")

  (ediff-barf-if-not-control-buffer)
  (let ((ediff-vector-from (symbol-value (intern (format "ediff-difference-vector-%S" from-buffer))))
	(ediff-vector-to   (symbol-value (intern (format "ediff-difference-vector-%S" to-buffer))))
	fine-diff-number fine-diff-max current-diff
	begin-from end-from begin-to end-to
	(ecd ediff-current-difference))

    (setq fine-diff-max (if (aref
			     (ediff-get-difference ediff-current-difference from-buffer)
			     1)
			    (progn
			      (setq ediff-current-difference ecd)
			      (length (aref
				       (ediff-get-difference ediff-current-difference from-buffer)
				       1))
			      )
			  nil)
	  fine-diff-number (max (min ediff-current-difference 
				     (if fine-diff-max (- fine-diff-max 1) 1))
				0)
	  )
    (if fine-diff-max
	(progn
	  ;(while (> fine-diff-number 0)
	    (setq ediff-current-difference ecd)

	    (setq begin-from (overlay-start
			      (aref
			       (aref
				(ediff-get-difference ediff-current-difference from-buffer)
				1)
			       fine-diff-number))
		  end-from   (overlay-end
			      (aref
			       (aref
				(ediff-get-difference ediff-current-difference from-buffer)
				1)
			       fine-diff-number))
		  begin-to    (overlay-start
			       (aref
				(aref
				 (ediff-get-difference ediff-current-difference to-buffer)
				 1)
				fine-diff-number))
		  end-to      (overlay-end
			       (aref
				(aref
				 (ediff-get-difference ediff-current-difference to-buffer)
				 1)
				fine-diff-number)))

	    (let ((current-buf (current-buffer))
		  (b1 (symbol-value (intern (format "ediff-buffer-%S" from-buffer))))
		  (b2 (symbol-value (intern (format "ediff-buffer-%S" to-buffer)))))
	      ;; falls kill gesetzt ist, wird die andere fine-region gelöscht
	      (if kill
		  (progn
		    (pop-to-buffer b2)
		    (goto-char begin-to)
		    (kill-region begin-to end-to)))
	      (pop-to-buffer b1)
	      (goto-char begin-from)
	      (copy-region-as-kill begin-from end-from)
	      (pop-to-buffer b2)
	      (goto-char begin-to)
	      (yank)
	      (insert-char ?  1)
	      (pop-to-buffer current-buf))
	    ;)

	  (fset 'dummy-func (symbol-function 'modify-frame-parameters))
	  (unwind-protect
	      (progn
		(fset 'modify-frame-parameters 'ediff-dummy)
		(ediff-recenter t)
		(ediff-update-diffs))
	    (fset 'modify-frame-parameters (symbol-function 'dummy-func)))

	  (message "Recovery of buffer A or B with rfa or rfb"))
      (message "no fine diffs found")
      )))



(defun ediff-dummy (&optional arg1 arg2)
;;;  '(lambda()(message "not including \"#include\" files")))
  nil)




(defun ediff-next-fine-difference (&optional arg)
  (interactive "P")
  (if (not arg)
      (setq arg 1))
  (ediff-barf-if-not-control-buffer)
  (if (= ediff-current-difference-save ediff-current-difference)
      (progn
	(setq ediff-current-fine-difference (+ ediff-current-fine-difference arg))
	(ediff-goto-fine-difference-s 'A 'B ediff-current-fine-difference))
    (setq ediff-current-difference-save ediff-current-difference)
    (setq ediff-current-fine-difference 1)
    (ediff-goto-fine-difference 1)))  ; spät


(defun ediff-previous-fine-difference (&optional arg)
  (interactive "P")
  (if (not arg)
      (setq arg 1))
  (ediff-barf-if-not-control-buffer)
  (if (= ediff-current-difference-save ediff-current-difference)
      (progn
	(setq ediff-current-fine-difference (- ediff-current-fine-difference arg))
	(ediff-goto-fine-difference-s 'A 'B ediff-current-fine-difference))
    (setq ediff-current-difference-save ediff-current-difference)
    (setq ediff-current-fine-difference 10000)
    (ediff-goto-fine-difference 10000)))  ; spät



(defun ediff-copy-fine-differences-only-A-B ()
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (ediff-copy-fine-difference 'A 'B))


(defun ediff-copy-fine-differences-only-B-A ()
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (ediff-copy-fine-difference 'B 'A))

(defun ediff-copy-and-kill-fine-differences-only-A-B ()
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (ediff-copy-fine-difference 'A 'B t))


(defun ediff-copy-and-kill-fine-differences-only-B-A ()
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (ediff-copy-fine-difference 'B 'A t))

(defun ediff-goto-fine-difference (arg)
  (interactive "NJump to which fine difference ? ")
  (ediff-barf-if-not-control-buffer)
   (if (not arg)
      (setq arg 1))
  (setq ediff-current-difference-save ediff-current-difference)
  (setq ediff-current-fine-difference (- arg 1))
  (ediff-goto-fine-difference-s 'A 'B ediff-current-fine-difference))


(defun ediff-recover-A ()
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (let ((current-buf (current-buffer)))
    (pop-to-buffer ediff-buffer-A)
    (undo)
    (pop-to-buffer current-buf)
    (fset 'dummy-func (symbol-function 'modify-frame-parameters))
    (unwind-protect
	(progn
	  (fset 'modify-frame-parameters 'ediff-dummy)
	  (ediff-recenter t)
	  (ediff-update-diffs))
      (fset 'modify-frame-parameters (symbol-function 'dummy-func)))))


(defun ediff-recover-B ()
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (let ((current-buf (current-buffer)))
    (pop-to-buffer ediff-buffer-B)
    (undo)
    (pop-to-buffer current-buf)
    (unwind-protect
	(progn
	  (fset 'modify-frame-parameters 'ediff-dummy)
	  (ediff-recenter t)
	  (ediff-update-diffs))
      (fset 'modify-frame-parameters (symbol-function 'dummy-func)))))


(defun ediff-recover-C ()
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (let ((current-buf (current-buffer)))
    (pop-to-buffer ediff-buffer-C)
    (undo)
    (pop-to-buffer current-buf)
    (unwind-protect
	(progn
	  (fset 'modify-frame-parameters 'ediff-dummy)
	  (ediff-recenter t)
	  (ediff-update-diffs))
      (fset 'modify-frame-parameters (symbol-function 'dummy-func)))))




(defun ediff-install-fine-diff-keys ()
  (interactive)
  (save-excursion
    (let ((control-buffer (current-buffer)))
      (set-buffer control-buffer)

      (define-key ediff-mode-map "f" nil)
      (define-key ediff-mode-map "fn" 'ediff-next-fine-difference)
      (define-key ediff-mode-map "fp" 'ediff-previous-fine-difference)
      (define-key ediff-mode-map "fa" 'ediff-copy-fine-differences-only-A-B)
      (define-key ediff-mode-map "fb" 'ediff-copy-fine-differences-only-B-A)
      (define-key ediff-mode-map "fda" 'ediff-copy-and-kill-fine-differences-only-A-B)
      (define-key ediff-mode-map "fdb" 'ediff-copy-and-kill-fine-differences-only-B-A)
      (define-key ediff-mode-map "fg" 'ediff-goto-fine-difference)
      (define-key ediff-mode-map "rfa" 'ediff-recover-A)
      (define-key ediff-mode-map "rfb" 'ediff-recover-B)
      (define-key ediff-mode-map "rfc" 'ediff-recover-C)
      )))




(add-hook 'ediff-startup-hook 'ediff-install-fine-diff-keys t t)

(defconst ediff-long-help-message-head
  "   Moving around	|     Toggling features	    |	    Manipulations
=====================|===========================|======================================"
  "The head of the full help message.")
(defconst ediff-long-help-message-tail
  "=====================|===========================|======================================
    R -show registry |				 |  M	-show session group
    D -diff output   |	   E -browse Ediff manual|  G	-send bug report
    i -status info   |	   ? -help off		 |  z/q -suspend/quit
----------------------------------------------------------------------------------------
X,Y (x,y)  on the left are meta-symbols for the keys  A,B (a,b).
X,Y on the right are meta-symbols for buffers A,B.
A,B on the right denote the working buffers A,B respectively."
  "The tail of the full-help message.")

(setq ediff-long-help-message-compare2
  "
p,DEL -previous diff |	   | -vert/horiz split	 |a/b -copy A/B's region to B/A
n,SPC -next diff     |	   h -hiliting		 | rx -restore buf X's old diff
    j -jump to diff  |	   @ -auto-refinement	 |  * -refine current region
   gx -goto X's point|				 |  ! -update diff regions
  C-l -recenter	     |	  ## -ignore whitespace	 |
  v/V -scroll up/dn  | #f/#h -focus/hide regions | wx -save buf X
  </> -scroll lt/rt  |	   X -read-only in buf X | wd -save diff output
   fn -next fine diff|	   m -wide display	 |
   fp -previous fine |				 | fa -copy fine region from A to B
   fg -goto fine diff|				 | fb -copy fine region from B to A
		     |				 | fda -replace fine region from A to B
		     |				 | fdb -replace fine region from B to A
		     |				 | rfx - undo in buffer X (repeatable)
")


;;; op-ediff ends here

  (error "Problem loading op-ediff package, probably problem connecting to diff program"))

;;;;;;;includeforelc!
;;;;;;(load "op-ediff-mods")

(defun ediff-info ()
  (interactive)
  (ediff-barf-if-not-control-buffer)
    (ediff-print-diff-vector (intern "ediff-difference-vector-A"))
    (ediff-print-diff-vector (intern "ediff-difference-vector-B"))
    ;(ediff-print-diff-vector (intern "ediff-difference-vector-C"))
    ;(ediff-print-diff-vector (intern "ediff-difference-vector-Ancestor"))
    )



(defun ediff-op-diff-to-diff (arg &optional keys)
  "Copy buffer-X'th difference region to buffer Y \(X,Y are A, B, or C\).
If numerical prefix argument, copy the difference specified in the arg.
Otherwise, copy the difference given by `ediff-current-difference'.
This command assumes it is bound to a 2-character key sequence, `ab', `ba',
`ac', etc., which is used to determine the types of buffers to be used for
copying difference regions. The first character in the sequence specifies
the source buffer and the second specifies the target.

If the second optional argument, a 2-character string, is given, use it to
determine the source and the target buffers instead of the command keys."
  (interactive "P")
  (ediff-barf-if-not-control-buffer)
  (or keys (setq keys (this-command-keys)))
  (if (eq arg '-) (setq arg -1)) ; translate neg arg to -1
  (if (numberp arg) (ediff-jump-to-difference arg))

  (let* ((key1 (aref keys 0))
	 (key2 (aref keys 1))
	 (char1 (if (and ediff-xemacs-p (eventp key1)) (event-key key1) key1))
	 (char2 (if (and ediff-xemacs-p (eventp key1)) (event-key key2) key2))
	 ediff-verbose-p)
    (ediff-op-copy-diff ediff-current-difference
		     (ediff-char-to-buftype char1)
		     (ediff-char-to-buftype char2))
    ;; recenter with rehighlighting, but no messages
    (ediff-recenter)))











;;; AUS ediff-diffs.el

;; Convert diff list to overlays for a given DIFF-REGION
;; in buffer of type FROM-BUFFER
(defun ediff-op-copy-fine-diffs (from-buffer diff-list region-num to-buffer)




  (let* ((current-diff -1)
	 (reg-start-from (ediff-get-diff-posn from-buffer 'beg region-num))
	 (reg-start-to (ediff-get-diff-posn to-buffer 'beg region-num))
	 (buff-from (ediff-get-buffer from-buffer))
	 (buff-to   (ediff-get-buffer to-buffer))
	 combined-merge-diff-list
	 diff-overlay-list list-element
	 begin end overlay)

    (ediff-clear-fine-differences-in-one-buffer region-num from-buffer)
    (setq diff-list (cdr diff-list)) ; discard list type (words or points)
    (ediff-eval-in-buffer buff-from (goto-char reg-start-from))
    (ediff-eval-in-buffer buff-to   (goto-char reg-start-to))
    
    ;; if it is a combined merge then set overlays in buff C specially
    (if (and ediff-merge-job (eq from-buffer 'C)
	     (setq combined-merge-diff-list
		   (ediff-looks-like-combined-merge region-num)))
	(ediff-set-fine-overlays-for-combined-merge
	 combined-merge-diff-list region-num)
      ;; regular fine diff

      (let ((tmp-beginning-of-line)(before-change-funktion nil)
	    (after-change-function nil))
	

	(while diff-list
	  (setq current-diff (1+ current-diff)
		list-element (car diff-list)

		begin-from 	 (aref list-element (cond ((eq from-buffer 'A) 0)
						  ((eq from-buffer 'B) 2)
						  (t 4)))  ; buf C
		end-from 	 (aref list-element (cond ((eq from-buffer 'A) 1)
						  ((eq from-buffer 'B) 3)
						  (t 5)))) ; buf C

		begin-to 	 (aref list-element (cond ((eq to-buffer 'A) 0)
						  ((eq to-buffer 'B) 2)
						  (t 4)))  ; buf C
		end-to 		 (aref list-element (cond ((eq to-buffer 'A) 1)
						  ((eq to-buffer 'B) 3)
						  (t 5)))) ; buf C

	  (if (not (or begin-from end-from))
	      () ; skip this diff
	    ;; Put overlays at appropriate places in buffers
	    ;; convert lines to points, if necessary
	    (setq begin-from (ediff-goto-word (1+ begin-from) buff-from)
		  end-from (ediff-goto-word end-from buff-from 'end))
	    (setq begin-to (ediff-goto-word (1+ begin-to) buff-to)
		  end-to (ediff-goto-word end-to buff-to 'end))
	    ;; Die Variablen begin-from, end-from
	    ;;               begin-to,   end-to
	    ;; geben die jeweilige Point-Position der fine-diff-Region an
	    
	    (ediff-eval-in-buffer buff-to (goto-char begin-to))
	    (ediff-eval-in-buffer buff-from (copy-region-as-kill begin-from end-from))
	    (ediff-eval-in-buffer buff-to (yank))

	    (ediff-make-or-kill-fine-diffs t)

)))))





;;; AUS ediff-utils.el

(defun ediff-op-copy-diff (n from-buf-type to-buf-type
			  &optional batch-invocation reg-to-copy)
  (let* ((to-buf (ediff-get-buffer to-buf-type))
	 ;;(from-buf (if (not reg-to-copy) (ediff-get-buffer from-buf-type)))
	 (ctrl-buf ediff-control-buffer)
	 (saved-p t)
	 (three-way ediff-3way-job)
	 messg
	 ediff-verbose-p
	 reg-to-delete reg-to-delete-beg reg-to-delete-end)
	
    (setq reg-to-delete-beg
	  (ediff-get-diff-posn to-buf-type 'beg n ctrl-buf))
    (setq reg-to-delete-end
	  (ediff-get-diff-posn to-buf-type 'end n ctrl-buf))
	  
    (if reg-to-copy
	(setq from-buf-type nil)
      (setq reg-to-copy (ediff-get-region-contents n from-buf-type ctrl-buf)))
    
    (setq reg-to-delete (ediff-get-region-contents
			 n to-buf-type ctrl-buf
			 reg-to-delete-beg reg-to-delete-end))
    
    (if (string= reg-to-delete reg-to-copy)
	(setq saved-p nil) ; don't copy identical buffers
      ;; seems ok to copy
      (if (or batch-invocation (ediff-test-save-region n to-buf-type))
	  (condition-case conds
	      (progn
		(ediff-eval-in-buffer to-buf
		  ;; to prevent flags from interfering if buffer is writable
		  (let ((inhibit-read-only (null buffer-read-only)))
		    
		    (goto-char reg-to-delete-end)
		    (insert reg-to-copy)
		    




;;; hier Aufruf des Fine-Diff-Teils
		    (message "hier")

		 ;;   (ediff-op-copy-fine-diffs from-buf-type op-diff-list n to-buf-type)





		    (if (> reg-to-delete-end reg-to-delete-beg)
			(kill-region reg-to-delete-beg reg-to-delete-end))
		    ))
		(or batch-invocation
		    (setq 
		     messg
		     (ediff-save-diff-region n to-buf-type reg-to-delete))))
	    (error (message "ediff-copy-diff: %s %s"
			    (car conds)
			    (mapconcat 'prin1-to-string (cdr conds) " "))
		   (beep 1)
		   (sit-for 2) ; let the user see the error msg
		   (setq saved-p nil)
		   )))
      )
    
    ;; adjust state of difference in case 3-way and diff was copied ok
    (if (and saved-p three-way)
	(ediff-set-state-of-diff-in-all-buffers n ctrl-buf))

    (if batch-invocation
	(ediff-clear-fine-differences n)
      ;; If diff3 job, we should recompute fine diffs so we clear them
      ;; before reinserting flags (and thus before ediff-recenter).
      (if (and saved-p three-way)
	  (ediff-clear-fine-differences n))
      
      (ediff-refresh-mode-lines)
      
      ;; For diff2 jobs, don't recompute fine diffs, since we know there
      ;; aren't any. So we clear diffs after ediff-recenter.
      (if (and saved-p (not three-way))
	  (ediff-clear-fine-differences n))
      ;; Make sure that the message about saving and how to restore is seen
      ;; by the user
      (message messg))
    ))



;; zur Kontrolle
(message "7")


;;; für den gemeinsamen cvs-Pool
;;;;;;;includeforelc!
;;;;;;(load "op-cvs")


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




;; zur Kontrolle
(message "7")

;;;--------------------------------------------------------------------------------

;
; Logbuch-Mode (ist auch Text-Mode)
; (steht hier, um den Indentet-Text-Mode zu überschreiben, weil das
; Hilit das abfragt)
;;;;;;;includeforelc!
;;;;;;(load "op-logbuch")

;;;Zwischennotiz
;;;
;;; LOGPRG-Buffer, wenn die variable logprg-projectdoc-buffer allgemein nicht definiert ist,
;;; ansonsten wird der in dieser Variablen beschriebene Buffer an das tageslicht geholt.
;;; Aus diesem Buffer kann man direkt raus mit "q", wenn der Cursor an Position 1 steht
;;;
;; Vorschläge zum Eintrag in den .emacs-File:
;;	(load-library "logbuch-mode")
;;	(logbuch-alarm-on 60) ; einmal pro Stunde
;;
;; When you are tired of LOGBUCHding use "M-x logbuch-alarm-off"
;;
;; Alternately, use "M-x logbuch" whenever you want to log something.
;;
;; Für das persönliche Logbuch: logbuch-mode
;; (defvar use-logbuch-projectdoc-buffer t)   ;; Zu einen neuen Buffer (=nil) oder zum Buffer  logbuch-projectdoc-buffer  springen (=t) ?
;; (defvar logbuch-projectdoc-buffer "~/projects/myprojects.op.crypt")
;; (defvar current-logbuch-file "~/projects/Wochenbericht.txt")	 ;; an diesen File sollen die Logbuch-Kommentare angehängt werden
;;
;; (logbuch-alarm-on 90) ;; alle 90 Minuten Nachfragen
;;
;;



(provide 'logbuch)

(if (not (boundp 'use-logbuch-projectdoc-buffer))
    (setq use-logbuch-projectdoc-buffer nil))

(defun logbuch-mode ()
  "Major mode for editing text to be sent to logbuch.
Like Text Mode but with these additional commands:
C-c C-c	 logbuch-send-and-exit
C-c C-r	 logbuch-send-region"
  (interactive)
  (setq logbuch-map (copy-keymap (current-local-map)))
  (use-local-map logbuch-map)
  (define-key logbuch-map "\C-c\C-c"   'logbuch-send-and-exit)
  (define-key logbuch-map "\C-c\C-r"   'logbuch-send-region)
  (define-key logbuch-map "\C-c\C-s"   'logbuch-set-document-buffer)
  (if (not use-logbuch-projectdoc-buffer)
      (define-key logbuch-map "\M-\C-i"  'tab-to-tab-stop))
;  (define-key (current-local-map) "\C-c\C-l\C-c"   'logbuch-send-and-exit)
;  (define-key (current-local-map) "\C-c\C-l\C-r"   'logbuch-send-region)
;  (define-key (current-local-map) "\C-c\C-l\C-s"   'logbuch-set-document-buffer)
;  (if (not use-logbuch-projectdoc-buffer)
;      (define-key (current-local-map) "\M-\C-i"  'tab-to-tab-stop))
  (setq major-mode 'logbuch-mode)
  (setq mode-name "Logbuch")
  (setq buffer-offer-save t))


(defun logbuch-set-document-buffer ()
  (interactive)
  (setq logbuch-projectdoc-buffer (current-buffer))
  (message "This buffer is now set as the \"logbuch-projectdoc-buffer\" "))


(defun logbuch-send-and-exit (arg)
  "Send message like logbuch-send-buffer, then, if no errors, exit from logbuch buffer.
Prefix arg means don't delete this window."
  (interactive "P")
  (if (not (boundp 'current-logbuch-buffer))
      (setq current-logbuch-buffer (current-buffer))
    (if (eq (current-buffer) current-logbuch-buffer)
	(setq current-logbuch-buffer (current-buffer))))
  (logbuch-send-buffer)
  (if (not use-logbuch-projectdoc-buffer)
      (progn
	(bury-buffer (current-buffer))
	(if (and (not arg)
		 (not (one-window-p)))
	    (delete-window)
	  (switch-to-buffer current-logbuch-buffer)))))


(defun logbuch-send-buffer ()
  "Send the message in the current buffer to logbuch."
  (message "Sending...")
  (if (and (string= (prin1-to-string (current-buffer)) "#<buffer *logbuch*>")
	   (not (buffer-modified-p)))
      (message "(No changes need to be saved)")
    (if (eq (current-buffer) current-logbuch-buffer)
	(setq current-logbuch-buffer (current-buffer)))
    (logbuch-send-buffer-sub)
    (if (not use-logbuch-projectdoc-buffer)
	(progn
	  (set-buffer-modified-p nil)
	  (delete-auto-save-file-if-necessary)))
    (switch-to-buffer current-logbuch-buffer)
    (message "Sending done")))


(defun logbuch-send-buffer-sub ()
  (if (and use-logbuch-projectdoc-buffer
	   (not (eq (current-buffer) current-logbuch-buffer)))
      (if (string-match "\\.crypt" logbuch-projectdoc-buffer)
	  (progn
	    (save-buffer-crypted)
	    (bury-buffer-smartly))
	(save-buffer)
	(bury-buffer-smartly))
    (if (not (eq (current-buffer) current-logbuch-buffer))
	(progn
	  (goto-char (point-min))
	  (insert "\n>>>  ")(insert-date-in-brackets)(insert "	")(insert logbuch-trenner-anfang)(insert-char ?\n 1)
	  (goto-char (point-max))
	  (insert-char ?\n 1)(insert logbuch-trenner-ende)
	  (insert-char ?\n 1)(insert-char ?  50)(insert signature)(insert-char ?\n 4)
	  (write-region (point-min) (point-max) current-logbuch-file t))
      (if (y-or-n-p (concat "Are you sure to save the whole buffer in logbuch-file  " current-logbuch-file " ? "))
	  (progn
	    (write-region (concat "\n>>>  " (calendar-date-string (calendar-current-date)) "	"
				  logbuch-trenner-anfang "\n") t current-logbuch-file t)
	    (write-region (point-min) (point-max) current-logbuch-file t)
	    (write-region (concat "\n" logbuch-trenner-ende
				  "\n														    "
				  signature  "\n\n\n\n") t current-logbuch-file t)
	    (message "Sending...done"))
	(message "Sending aborted")))
    (switch-to-buffer "*logbuch*")
    (kill-buffer "*logbuch*")))


(defun logbuch-send-region-sub (region-begin region-end)
  (if (and use-logbuch-projectdoc-buffer
	   (not (eq (current-buffer) current-logbuch-buffer)))
      (if (string-match "\\.crypt" logbuch-projectdoc-buffer)
	  (progn
	    (save-buffer-crypted)
	    (bury-buffer-smartly))
	(save-buffer)
	(bury-buffer-smartly))
    (if (> region-begin region-end)
	(let ((tmp region-end))
	  (setq region-end region-begin)
	  (setq region-begin tmp)))
    (if (not (eq (current-buffer) current-logbuch-buffer))
	(progn
	  (goto-char region-begin)
	  (insert "\n>>>  ")(insert-date-in-brackets)(insert "	")(insert logbuch-trenner-anfang)(insert-char ?\n 2)
	  (setq region-end (+ region-begin
			      5
			      (length (calendar-date-string (calendar-current-date)))
			      1
			      (length logbuch-trenner-anfang)
			      2
			      ))
	  (goto-char region-end)
	  (insert-char ?\n 1)(insert logbuch-trenner-ende)
	  (insert-char ?\n 1)(insert-char ?  109)(insert signature)(insert-char ?\n 4)
	  (write-region region-begin (point) current-logbuch-file t))
      (write-region (concat "\n>>>  " (calendar-date-string (calendar-current-date)) "	"
			    logbuch-trenner-anfang "\n") t current-logbuch-file t)
      (write-region region-begin region-end current-logbuch-file t)
      (write-region (concat "\n" logbuch-trenner-ende
			    "\n														    "
			    signature  "\n\n\n\n") t current-logbuch-file t))
    (switch-to-buffer "*logbuch*")
    (kill-buffer "*logbuch*")))



(defun logbuch-send-buffer-crypted-sub () ; noch in Arbeit
  (if (and use-logbuch-projectdoc-buffer
	   (not (eq (current-buffer) current-logbuch-buffer)))
      (if (string-match "\\.crypt" logbuch-projectdoc-buffer)
	  (progn
	    (save-buffer-crypted)
	    (bury-buffer-smartly))
	(save-buffer)
	(bury-buffer-smartly))
    (if (string-match "\\.crypt" current-logbuch-file)
	(progn
	  (if (not (eq (current-buffer) current-logbuch-buffer))
	      (progn
		(goto-char (point-min))
		(insert "\n>>>  ")(insert-date-in-brackets)(insert "	")(insert logbuch-trenner-anfang)(insert-char ?\n 1)
		(goto-char (point-max))
		(insert-char ?\n 1)(insert logbuch-trenner-ende)
		(insert-char ?\n 1)(insert-char ?  50)(insert signature)(insert-char ?\n 4)
		(write-region (point-min) (point-max) current-logbuch-file t))
	    (if (y-or-n-p (concat "Are you sure to save the whole buffer in logbuch-file  " current-logbuch-file " ? "))
		(progn
		  (write-region (concat "\n>>>  " (calendar-date-string (calendar-current-date)) "	"
					logbuch-trenner-anfang "\n") t current-logbuch-file t)
		  (write-region (point-min) (point-max) current-logbuch-file t)
		  (write-region (concat "\n" logbuch-trenner-ende
					"\n														    "
					signature  "\n\n\n\n") t current-logbuch-file t)
		  (message "Sending...done"))
	      (message "Sending aborted")))
	  (switch-to-buffer "*logbuch*")
	  (kill-buffer "*logbuch*")))))


(defun logbuch-send-region-crypted-sub (region-begin region-end) ; noch in Arbeit
  (if (and use-logbuch-projectdoc-buffer
	   (not (eq (current-buffer) current-logbuch-buffer)))
      (if (string-match "\\.crypt" logbuch-projectdoc-buffer)
	  (progn
	    (save-buffer-crypted)
	    (bury-buffer-smartly))
	(save-buffer)
	(bury-buffer-smartly))
    (if (> region-begin region-end)
	(let ((tmp region-end))
	  (setq region-end region-begin)
	  (setq region-begin tmp)))
    (if (not (eq (current-buffer) current-logbuch-buffer))
	(progn
	  (goto-char region-begin)
	  (insert "\n>>>  ")(insert-date-in-brackets)(insert "	")(insert logbuch-trenner-anfang)(insert-char ?\n 2)
	  (setq region-end (+ region-begin
			      5
			      (length (calendar-date-string (calendar-current-date)))
			      1
			      (length logbuch-trenner-anfang)
			      2
			      ))
	  (goto-char region-end)
	  (insert-char ?\n 1)(insert logbuch-trenner-ende)
	  (insert-char ?\n 1)(insert-char ?  109)(insert signature)(insert-char ?\n 4)
	  (write-region region-begin (point) current-logbuch-file t))
      (write-region (concat "\n>>>  " (calendar-date-string (calendar-current-date)) "	"
			    logbuch-trenner-anfang "\n") t current-logbuch-file t)
      (write-region region-begin region-end current-logbuch-file t)
      (write-region (concat "\n" logbuch-trenner-ende
			    "\n														    "
			    signature  "\n\n\n\n") t current-logbuch-file t))
    (switch-to-buffer "*logbuch*")
    (kill-buffer "*logbuch*")))


(defvar logbuch-buffer nil
  "Der aktuelle Logbuch-Buffer, in den die Logs eingetragen werden")
(defun logbuch (&optional noerase)
  "Edit a message to be sent.  Argument means resume editing (don't erase).
Returns with message buffer selected; value t if message freshly initialized.
While editing message, type C-c C-c to send the message and exit.

If logbuch-setup-hook is bound, its value is called with no arguments
after the message is initialized. "
  (interactive "P")
;;; sehe nach, ob der Pointer schon im logbuch-projectdoc-buffer steht, wenn nein, dann definiere den current-buffer
;;; als den current-logbuch-buffer:
  ;; wenn logbuch-projectdoc-buffer benutzt wird
  (if use-logbuch-projectdoc-buffer
      ;; wenn gecryptet:
      (if (string-match "\\.crypt" logbuch-projectdoc-buffer)
	  ;; dann hänge an den Namen des Files .crypt an und vergleiche die beiden Namen
	  ;; (sieht umständlich aus, aber der emacs expandiert im afs die Filenamen nicht immer auf die gleiche Weise)
	  (if (not (string= (expand-file-name (file-name-nondirectory (concat (buffer-file-name (current-buffer)) ".crypt")))
			    (expand-file-name (file-name-nondirectory logbuch-projectdoc-buffer))))
	      ;; wenn nicht gleich, dann
	      (setq current-logbuch-buffer (current-buffer)))
	;; wenn nicht gecryptet und nicht gleich:
	(if (not (string= (expand-file-name (file-name-nondirectory (buffer-file-name (current-buffer))))
			  (expand-file-name (file-name-nondirectory logbuch-projectdoc-buffer))))
	    ;; dann
	    (setq current-logbuch-buffer (current-buffer))))
    ;; wenn logbuch-projectdoc-buffer nicht benutzt wird
    (if (not (string= (prin1-to-string (current-buffer)) "#<buffer *logbuch*>"))
	(setq current-logbuch-buffer (current-buffer))))
;;; das nächste verhält sich nur so, wenn die Variable logbuch-projectdoc-buffer allgemein definiert ist, sonst wie ein LOGBUCH-buffer
  (if use-logbuch-projectdoc-buffer
      (progn
	(if (string-match "\\.crypt" logbuch-projectdoc-buffer)
	    (find-file-crypted logbuch-projectdoc-buffer)
	  (find-file logbuch-projectdoc-buffer))
	(setq logbuch-buffer (current-buffer))
	(message "Dieser Buffer ist nun ein Logbuch-Buffer: Sichern und in den Hintergrund mit	\C-c \C-c"))
    (switch-to-buffer "*logbuch*")
    (setq logbuch-buffer (current-buffer))
    (setq default-directory (expand-file-name "~/"))
    (auto-save-mode auto-save-default)
    (logbuch-mode)
    ;; naechste Zeile loescht alle lokalen Variablen
    (indented-text-mode)
    (and (not noerase)
	 (or (not (buffer-modified-p))
	     (y-or-n-p "Unsent message being composed; erase it? "))
	 (progn (erase-buffer)
		(set-buffer-modified-p nil)
		(run-hooks 'logbuch-setup-hook)
		t))))


(defun logbuch-other-window (&optional noerase)
  "Like `logbuch' command, but display logbuch buffer in another window."
  (interactive "P")
  (let ((pop-up-windows t))
    (pop-to-buffer "*logbuch*"))
  (logbuch noerase))


;; run every interval & at initial call to logbuch-alarm-on
(defun logbuch-alarm-filter ()
  (if (or (and use-logbuch-projectdoc-buffer
	       (not (eq (current-buffer) (file-name-nondirectory logbuch-projectdoc-buffer))))
	  (and (not use-logbuch-projectdoc-buffer)
	       (not (eq (current-buffer) (file-name-nondirectory current-logbuch-file))))
	  (not (eq (current-buffer) logbuch-buffer)))
      (progn
	(message "3")
	(ding)
	(sleep-for 1)
	(message "2")
	(ding)
	(sleep-for 1)
	(message "1")
	(ding)
	(sleep-for 1)
	;; abarbeiten der Events, die während des sleep-for's gekommen sind	
	(while (input-pending-p)
	  (insert (quotet-insert (read-char))))
	(logbuch)
	(message "Report-buffer: send and store with C-c C-c after you have typed something")
	)))


(defvar logbuch-zeitobjekt nil)

;; Starte LOGBUCH in eimen regelmäßigem Intervall
;;
(defvar logbuch-abfrageintervall 90
  "Der Abfrageabstand des Logbuch-Zeitbuffers im Minuten, globale Variable")
(defun logbuch-alarm-on (interval)
  "Turn the Emacs logbuch alarm on.  The alarm goes off every INTERVAL minutes
and you will be switched to the logbuch buffer automatically.
Use logbuch-alarm-off to stop this behaviour."
  (interactive "nEnter logbuch alarm interval (in minutes): ")
  (setq logbuch-abfrageintervall interval)
  (logbuch-alarm-off)
  (setq logbuch-zeitobjekt (run-with-timer
			    ;; convert minutes -> seconds for wakeup
			    (* 60.0 interval) (* 60.0 interval)
			    'logbuch-alarm-filter)))


;; Turn LOGBUCH alarm off
;;
(defun logbuch-alarm-off ()
  "Turn the Emacs logbuch alarm off."
  (interactive)
  (if logbuch-zeitobjekt
      (progn
	(cancel-timer logbuch-zeitobjekt)
	(setq logbuch-zeitobjekt nil)))
  (cancel-function-timers 'logbuch-alarm-filter))



(defun logbuch-send-region (region-begin region-end)
  "Send the message in the current region to logbuch."
  (interactive "r")
  (if (not (boundp 'current-logbuch-buffer))
      (setq current-logbuch-buffer (current-buffer))
    (if (eq (current-buffer) current-logbuch-buffer)
	(setq current-logbuch-buffer (current-buffer))))
  (message "Sending...")
  (logbuch-send-region-sub region-begin region-end)
  (switch-to-buffer current-logbuch-buffer)
  (message "Sending...done"))




(logbuch-mode)


; --------------------------------------------------------------------------------





;;SMW isearch

;;;;;;;includeforelc!
;;;;;;(load "op-search")  ;;; alle Search-Funktionen



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
  





;;;;;;;includeforelc!
;;;;;;(load "op-columns")



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
      (if (not (looking-at "^*")) (re-search-backward "^*" nil t))
      ;(beginning-of-line)
      (let ((already-modified (buffer-modified-p))(beg (point)))
	(outline-end-of-subtree)
	(if (not (re-search-forward "^*" nil t))
	    (point-max)
	  (backward-char 1))
	(narrow-to-region beg (point))
	(goto-char beg)
	(show-all-hide)
	(toggle-read-only t)(toggle-read-only) ; gerade mal unklar: könnte auch eine Zeile 
	                                       ; obendran müssen, wahrscheinlich jedoch nicht
	(widen)
	(save-excursion
	  (hide-all-ifdefs)
	  (end-of-show called-noninteractive)
	  (setq selective-display t))
	(set-buffer-modified-p already-modified)))))



(define-project-mode-key  "\M-d" 'hide-subtree-hide)
(defun hide-subtree-hide ()
  (interactive)
  (let ((before-change-functions nil)(after-change-functions nil))
  (save-excursion
    (outline-back-to-heading)
    ;(beginning-of-line)
    (let ((already-modified (buffer-modified-p))(beg (point)))
      (outline-end-of-subtree)
      (if (not (re-search-forward "^*" nil t))
	  (point-max)
	(backward-char 1))
      (narrow-to-region beg (point))
      (goto-char beg)
      (hide-other-hide t)
      (goto-char beg)
      (hide-subtree)
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
	    (/= (point) end)))
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
(defun show-entry-hide (&optional called-noninteractive)
  (interactive "P")
  (let ((p (point)))
    (if (not (looking-at "*"))
	(if (not (re-search-backward "^*" nil t))
	    (error "no headline found")))
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
	(end-of-show called-noninteractive)
	(setq selective-display t)
	(set-buffer-modified-p already-modified)))
    (goto-char p)))


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


(define-project-mode-key "\M-p" 'hide-other-hide)
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




(define-project-mode-key "\M-o" 'hide-other-more)
(defun hide-other-more ()
  "A variation to hide-other, but shows more of the environment"
  (interactive)
  (let ((count 0)(position nil)(wasthere 0)(countmax 0))
    ;; Einschränken der Sichtbarkeit (narrow)
    ;; wenn nur bei Einstern-Überschriften dann
    (if (save-excursion (re-search-backward "^\\*" nil t) (looking-at "^\\*[^*]"))
	(progn
	  (hide-all)
	  (show-entry-hide))
      ;; sonst
      (setq position (point))
      (if (outline-previous-heading)
	  (setq countmax (1+ (count-level))))
      ;; suche die Überschrift des Einstern-Levels
      (top-heading)
      ; verstecke alle
      (hide-all)
      ;; gehe zum Ursprung
      (goto-char position)
      ;; suche die Überschrift ein Level höher = ein Stern weniger
      (while (and (< wasthere (point))
		  (<= count countmax)) ; maximal 13 mal
	(setq count (1+ count))
	(condition-case nil
	    (progn
	      (show-children-hide)
	      (previous-line 1)
	      (end-of-line)
	      (forward-char 1)
	      (setq wasthere (point))
	      (goto-char position)
	      )
	  (error "Problen showing children in \"hide-other-mode\"")))
      (goto-char position)
      (show-entry-hide))))





(defun count-level ()
  (let ((count 0))
    (save-excursion 
      (beginning-of-line)
      (while (looking-at "\\*")
	(forward-char 1)
	(setq count (1+ count))))
    count))





;; und hier noch die Tasten für den Präsentationsmodus
(define-key (current-local-map) [(control +)] 'presentation-walk-forward)
(if (equal system-type 'windows-nt)
    (define-key (current-local-map) [(control ü)] 'presentation-walk-backward)
  (define-key (current-local-map) [(control udiaeresis)] 'presentation-walk-backward))


(defun presentation-walk-forward ()
  (interactive)
  (isearch-dehighlight)
  (outline-next-heading)
  (hide-other)
  (save-excursion
    (if (outline-previous-heading)
      (show-entry-hide)))
  (save-excursion
    (if (outline-next-heading)
      (show-entry-hide)))
  (if window-system
      (hilit-recenter nil))
  (isearch-highlight (save-excursion (beginning-of-line)(point)) (save-excursion (end-of-line)(point)))
  (backward-char 1)
  (column-ruler)
  (add-one-shot-hook 'pre-command-hook '(lambda () (isearch-dehighlight)(forward-char 1))))
(defun presentation-walk-backward ()
  (interactive)
  (isearch-dehighlight)
  (re-search-backward "^\\*" nil t)
  ;(outline-previous-heading)
  (hide-other)
  (save-excursion
    (re-search-backward "^\\*" nil t)
    ;(outline-previous-heading)
    (show-entry-hide))
  (save-excursion
    (if (outline-next-heading)
      (show-entry-hide)))
  (if window-system
      (hilit-recenter nil))
  (isearch-highlight (save-excursion (beginning-of-line)(point)) (save-excursion (end-of-line)(point)))
  (backward-char 1)
  (column-ruler)
  (add-one-shot-hook 'pre-command-hook 'isearch-dehighlight))






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
    (save-excursion
      (toggle-read-only t)(toggle-read-only)
      (outline-up-heading 2)
      (show-subtree)
      (save-excursion
	(end-of-show called-noninteractive)
	(setq selective-display t))
      (set-buffer-modified-p already-modified))))





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
	    (and (/= (outline-level) thislevel)
		 (/= (point) end))))
      (if (= (outline-level) thislevel)
	  (setq tmpplace2 (point))
	(setq tmpplace2 tmpplace1)))
    (if (/= tmpplace1 tmpplace2)
	(goto-char tmpplace2)
      (message "no visible heading forward one level less")))))


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
	    (and (/= (outline-level) thislevel)
		 (/= (point) begin))))
      (if (= (outline-level) thislevel)
	  (setq tmpplace2 (point))
	(setq tmpplace2 tmpplace1)))
    (if (/= tmpplace1 tmpplace2)
	(goto-char tmpplace2)
      (message "no visible heading backwards one level less")))))



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
    (setq ausblend-toggle-status-0 t)
    (setq ausblend-toggle-status-1 t)
    (setq ausblend-toggle-status-2 t)
    (setq ausblend-toggle-status-3 t)
    (setq ausblend-toggle-status-4 t)
    (setq ausblend-toggle-status-5 t)
    (setq ausblend-toggle-status-6 t)
    (outline-flag-region (point-min) (point-max) nil)
    (if window-system
	(hilit-repaint-command t))
    (goto-char p)
    )
  )


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
  (setq ausblend-toggle-status-0 t)
  (setq ausblend-toggle-status-1 t)
  (setq ausblend-toggle-status-2 t)
  (setq ausblend-toggle-status-3 t)
  (setq ausblend-toggle-status-4 t)
  (setq ausblend-toggle-status-5 t)
  (setq ausblend-toggle-status-6 t)
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
  (outline-flag-region (point-min) (point-max) nil)
  (widen)
  (trim-buffer)
  (hide-other-hide t)
  (if window-system
      (hilit-repaint-command t))))





(defun end-of-show (&optional called-noninteractive re-adjust)
  (let ((before-change-functions nil)(after-change-functions nil)
	(already-modified (buffer-modified-p)))
  (op-hide-at-end-of-show)
  (hide-end-of-include-statments)
  ;; Mache den File schreibbar. Wichtig, da im outline ein Bug ist, der nicht mit der Version-Control zusammenarbeitet
  (toggle-read-only t)(toggle-read-only)
  (if (and (not called-noninteractive)
	   window-system)
      (hilit-repaint-command t))
  (message (concat "Position " (prin1-to-string (point))))

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
      (if (/= position1 position2)
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
      (if (/= position1 position2)
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
  (let ((before-change-functions nil)(after-change-functions nil) (pnt (region-beginning)))        
  (if (not arg) (setq arg 1))        
  (save-excursion
    (narrow-to-region (region-beginning) (region-end))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "\r" nil t)
	(replace-match "\n" nil t))
      (outline-flag-region (point-min) (point-max) nil))
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
    (end-of-show))
    (goto-char pnt)))
(define-project-mode-key ">" 'op-shift-right)


(defun op-shift-left (arg)
  (interactive "P")
  (let ((before-change-functions nil)(after-change-functions nil)(pnt (region-beginning)))
  (if (not arg) (setq arg 1))
    (save-excursion
      (narrow-to-region (region-beginning) (region-end))
      (save-excursion
	(goto-char (point-min))
	(while (search-forward "\r" nil t)
	  (replace-match "\n" nil t))
	(outline-flag-region (point-min) (point-max) nil))
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
    (end-of-show)
    (goto-char pnt)))
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
;;;;;;(load "op-hilit.gnuemacs")

;;
;; LEICHT VARIIERTER CODE TAUCHT BEI DER FUNKTION mpx-msp-text-extractor NOCHMAL AUF !!!
;;
(if window-system
    (progn
      (if (x-display-color-p)
	  (progn
	    (hilit-set-mode-patterns
	     'logbuch-mode
	     '(
	       ;("/\\*" "\\*/" msg-from)   ; wie C-Kommentare
	       ;; Doppelzeilenkombination für golden unterlegt
	       ("/\\*\\|\\*/" nil msg-separator)
	       ("/\\*\\|\\*/" nil msg-quote)

	       ;;	   ("->" "[ \t\r]\\|$" jargon-keyword)
	       ;; ("->" nil label)
	       ;;	   ("->" "[ \t\r]\\|$" string)
	       
	       ;; Dreizeilenkombination für Dunkelgrün
	       ("#[^TR]" ".#\\|[\r\n]" msg-quote)
	       ("#[^TR]" ".#\\|[\r\n]" struct)
	       ("#[^TR]" ".#\\|[\r\n]" msg-quote)
	       
	       ;; Doppelzeilenkombination für golden unterlegt
	       ;("#T" ".#\\|[\r\n]" msg-separator)
	       ;("#T" ".#\\|[\r\n]" msg-quote)
	       ("#T" ".#\\|$" jargon-keyword)
	       
	       ("#R" ".#\\|[\r\n]" comment)
	       
	       ("\\<\\(Info\\|INFO\\|info\\)\\>.*" nil struct)
	       ;;glob-struct)
	       ("\\[Meilenstein\\]" nil define)
	       ("|| \\[Unterprojekt\\]" " ||" error)
	       ("#include[ \t]+\"" "\"" define)
	       ("[\n\r]#end_include_statement" nil define)
	       
	       ;; speziell für MPX-MSP
	       ("{Nr\\." "}" goldenrod)
	       ("{Verantwortlich" "}" goldenrod)
	       
	       ("^\\(\\*E\\|\\*FW\\).*" "[\r\n]" summary-deleted) ; Erledigtes in Hauptüberschrift
	       ("^\\(\\*\\*+E\\|\\*+FW\\).*" "[\r\n]" keyword)     ; Erledigtes in Unterüberschrift
					                           ;(generelle Doku: in op-menus.el)
	       ("^\\(\\*+V\\|\\*+S\\).*" "[\r\n]" include)         ; Verschobenes und ständig 
					                           ;              wiederkehrendes
	       ("^\\*+\\-+" "[\r\n]" string)			   ; *---- - Zeilen
	       ("^\\*+\\++" "[\r\n]" string)			   ; *++++ - Zeilen
	       ("^\\*[^*EFVDSR].*" "[\r\n]" msg-header)	           ; Hauptüberschrift ; F != FW !!!
	       ("^\\*\\*+[^*EFVDSR].*" "[\r\n]" defun)		   ; Unterüberschriften
	       ("^\\*+D" "[\r\n]" error)			   ; Dringendes
	       )))
	(hilit-set-mode-patterns
	 'logbuch-mode
	 '(
	   ("==>" nil string)
	   ("^\\*+E.*\\|^\\*+FW.*\\|^\\*+V.*" "[\r\n]" keyword)
	   ("#[^#]" ".#\\|[\r\n]" msg-from)
	   ("\\<\\(Info\\|INFO\\|info\\)\\>.*" "[\r\n]" glob-struct)
	   ("^\\*+\\-+" "[\r\n]" msg-from)
	   ("^\\*" "[\r\n]" defun)
	   )))

      ;;(setq hilit-face-check nil) ; Fonds werden ja meistens nicht extra geändert, sonst hilft auch noch C-u S-C-l    
      (setq hilit-inhibit-rebinding t) ; wenn der Server nur wenige Farben hat, geht sonst manchmal Yank nicht mehr
      (setq hilit-auto-rehighlight 10000) ; Anzahl der automatisch upgedateten Zeilen bei Benutzung des X-Servers

      ;; auch der hilit-mode kommt nicht mit dem (setq selective-display t) zurecht
      (setq hilit-auto-rehighlight-fallback '(20000 . 10000))
      (make-variable-buffer-local 'hilit-auto-rehighlight-fallback)

      (hilit-repaint-command t)
      ))
;;(";.*" nil comment)
;;("#|" "|#" comment)
;;("%.*$" nil comment)
;;("Vorgänger" nil crosref)
;;("==>" nil summary-seen)
;; ("==>" nil mgs-separator)


;
; alle Definitionen oben aus lisp_dir: hilit19.el
;
; error rot
; jargon-keyword  ;golden
; string ; grau
; comment ist im Prinzip nocht fast nicht genutzt
; decl schwaches blau
; dired-marked: schwaches lila
; jargon-keyword rot unterstrichen
; struct ganz schwarz
; summary-Xed blaß sehr hellgrün
; goldenrod golden
; summary-deleted laues rot
; glob-struct dunkelrosa
; summary-current  Balken in blau
; summary-unread schwaches blau
; define bold grün
; msg-from violett
; glob-struct rosa
;

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
;;;;;;(load "op-appt-changes")

;;; Aenderungen zu dem File
;;; appt.el --- appointment notification functions.
;;; aus dem Emacs-Lisp-Directory



(defun appt-disp-window-softly (min-to-app new-time appt-msg)
  "Veränderte Funktion aus appt.el (ursprünglich appt-disp-window).
Diese hier poppt aber nicht ständig den Emacs nach vorne."
  (require 'electric)

  ;; Make sure we're not in the minibuffer
  ;; before splitting the window.

  (if (equal (selected-window) (minibuffer-window))
      (if (other-window 1) 
	  (select-window (other-window 1))
	(if window-system
	    (select-frame (other-frame 1)))))
      
  (let* ((this-buffer (current-buffer))
	 (this-window (selected-window))
	 (appt-disp-buf (set-buffer (get-buffer-create appt-buffer-name))))

    (appt-select-lowest-window)
    (if (cdr (assq 'unsplittable (frame-parameters)))
	;; In an unsplittable frame, use something somewhere else.
	(display-buffer appt-disp-buf)
      ;; Otherwise, split the bottom window and use the lower part.
      (split-window)
      (pop-to-buffer appt-disp-buf))
    (setq mode-line-format 
	  (concat "-------------------- ! Termin in "
		  min-to-app " Minuten ! " new-time " %-"))
    (insert-string appt-msg)
    (shrink-window-if-larger-than-buffer (get-buffer-window appt-disp-buf t))
    (set-buffer-modified-p nil)
    ;;; SMW (raise-frame (selected-frame))
    (select-window this-window)
    (if appt-audible
	(beep 1))))

(setq appt-disp-window-function 'appt-disp-window-softly)


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
;;;;;;(load "op-schedule")





(defun project-schedule (&optional numberofdays startdate includingdiaryfile)
  "Macht einen Projektkalender aus allen sichbaren Zeilen, die 
mit einem #T anfangen. Auch mehrere Zeilen werden akzeptiert und 
hintereinander abgearbeitet.

Beispiel:

* Projekt 1

**	Unterprojekt 1
	(ein von-bis)
     #T 1.1.1997 - 3.1.1997

**	Unterprojekt 2
	(ein konkretes Datum)
		    #T 2.1.1997

**	Unterprojekt 3
	(nochmal ein konkretes Datum)
#T 3.1.1997

* Projekt 2

**	Unterprojekt 1
	(zweimal ein von-bis)
     #T vom 1.1.1997 bis zum 3.1.1997
     #T 8.1.1997 - 11.1.1997   "
  (interactive "P")
  ;; mache momentanen Projekt-Buffer als temporären zum project-to-diary-file
  (if (not numberofdays)
      (setq numberofdays (read-string "Anzahl der Tage [7]: " numberofdays)))
  (if (string= "" numberofdays)
      (setq numberofdays "7"))
  (if (not startdate)
      (setq startdate (read-string "Startdatum (deutsches Datumsformat bitte, z.B. 30.4.1997) [Default: heute]: ")))
  (if (string= "" startdate)
      (setq startdate (concat (number-to-string (nth 1 (calendar-current-date))) "."
			      (number-to-string (nth 0 (calendar-current-date))) "."
			      (number-to-string (nth 2 (calendar-current-date))))))
  (if (or (string= "" includingdiaryfile)
	  (string= "n" includingdiaryfile)
	  (string= "N" includingdiaryfile)
	  (not includingdiaryfile))
      (setq includingdiaryfile nil)
    (setq includingdiaryfile t))
  (goto-char (point-min))
  (let ((project-to-diary-file (concat "*" (buffer-name) "*"))
	(current-buf (current-buffer))(point)(pointsv (point-min))
	(diary-file diary-file)
	(calendar-date-display-form calendar-date-display-form))
    (if (get-buffer project-to-diary-file) 
	(kill-buffer project-to-diary-file))
    (setq calendar-date-display-form '(day " " monthname " " year))
    (get-buffer-create project-to-diary-file)
    ;; Buffer aufräumen
    (while (re-search-forward "\n[ \t]*#T" nil t)
      ;; alle Terminzeilen rüberkopieren
      (backward-char 2)
      (append-to-buffer project-to-diary-file (point) (save-excursion (end-of-line) (point)))
      ;; alle dazugehörigen Überschriften rüberkopieren
      ;; - 
      ;; setze die Variable op-current-headline-string
      (schedule-headlines nil t)
      ;; kopiere diese, d.h. jeweils die aktuellen Überschriften, in den File
      (if calendar-setup
	  (switch-to-buffer-other-frame project-to-diary-file)
	(switch-to-buffer project-to-diary-file))
      
      ;test(switch-to-buffer project-to-diary-file)
      (insert-char ?\n 1)
      (beginning-of-line)
      (setq point (point))
      (insert op-current-headline-string)
      (indent-rigidly point (point) tab-width)
      (switch-to-buffer current-buf))
    (switch-to-buffer project-to-diary-file)	      
    (goto-char (point-min))
    (insert-char ?\n 1)
    (save-excursion
      (replace-regexp "\r.*$" ""))
    (setq truncate-lines t)
    (let ((day) (month) (year) (daybis) (monthbis) (yearbis))
      (while (re-search-forward "[ \t]*\\([0-9]?[0-9]\\)\\.\\([0-9]?[0-9]\\)\\.\\([0-9]?[0-9]?[0-9][0-9]\\)" 
				nil t)
	
	(if (save-excursion
	      (save-match-data
		(re-search-backward "\n[ \t]*#T" (save-excursion (beginning-of-line) (backward-char 1) (point)) t)))
	    (progn
	      (setq day (match-string 1))
	      (setq month (match-string 2))
	      (setq year (match-string 3))
	      (if (re-search-forward "\\([0-9]?[0-9]\\)\\.\\([0-9][0-9]?\\)\\.\\([0-9]?[0-9]?[0-9][0-9]\\)" 
				     (save-excursion (end-of-line) (point))
				     t)
		  (progn
		    (setq daybis (match-string 1))
		    (setq monthbis (match-string 2))
		    (setq yearbis (match-string 3))
		    (beginning-of-line)
		    (insert (concat "\n%%(diary-block  " day " " month " " year " " daybis " " monthbis " " yearbis ")  \n"))
		    (kill-line 2)
		    (backward-char 1))  
		(beginning-of-line)
		(insert-char ?\n 1)
		(insert (concat (calendar-date-string 
				 (list (string-to-int month) 
				       (string-to-int day) 
				       (string-to-int year)) t t) 
				"\n"))
		(kill-line 2)
		(backward-char 1)))))
      ;;
      ;; nächster Abschnitt: mache die Diary-Eintraege
      ;;
      (if includingdiaryfile
	  (progn
	    (end-of-buffer)
	    (insert-char ?\n 1)
	    (if (file-readable-p diary-file)
		(progn
		  (find-file diary-file)
		  (append-to-buffer project-to-diary-file (point-min) (point-max))
		  (bury-buffer)))
	    (beginning-of-buffer)
	    ))

      (schedule-include-other-diary-files)
      (setq diary-file project-to-diary-file)
      ;; Trickreiches Überladen der Funktion (find-buffer-visiting), die in der Funktion diary gerufen wird
      ;; (flet git es nicht in elisp)
      ;;
      (fset 'dummy-func (symbol-function 'find-buffer-visiting))
      (condition-case nil
	  (unwind-protect
	      (progn
		(fset 'find-buffer-visiting (symbol-function 'substitute-in-file-name))
		(goto-char (point-min))
		(string-match "\\([0-9]?[0-9]\\)\\.\\([0-9]?[0-9]\\)\\.\\([0-9]?[0-9]?[0-9][0-9]\\)" startdate)
		(setq day (substring startdate (match-beginning 1) (match-end 1)))
		(setq month (substring startdate (match-beginning 2) (match-end 2)))
		(setq year (substring startdate (match-beginning 3) (match-end 3)))
		(list-diary-entries
		 (list (string-to-int month) 
		       (string-to-int day) 
		       (string-to-int year))
		 (string-to-number numberofdays)))
	    (fset 'find-buffer-visiting (symbol-function 'dummy-func)))
	(error "Fehler bei der Erstellung des Projektkalenders")))
    (switch-to-buffer project-to-diary-file)
    (kill-buffer nil)
    (split-window-vertically)
    (other-window -1)
    (setq truncate-lines t)
    (other-window 1)
    (switch-to-buffer current-buf)
    (delete-window)))





(defun teschtprojectdiaryfile ()
  (let ((project-to-diary-file (concat "*" (buffer-name) "*"))
	(current-buf (current-buffer))(point)(pointsv (point-min))
	(diary-file diary-file))
    (setq diary-file project-to-diary-file)
    (fset 'dummy-func (symbol-function 'find-buffer-visiting))
    (unwind-protect
	(progn
	  (fset 'find-buffer-visiting (symbol-function 'substitute-in-file-name))
	  (goto-char (point-min))
	  (list-diary-entries (list 1 1 1997) 3))
      (fset 'find-buffer-visiting (symbol-function 'dummy-func)))
    (other-window -1)
    (setq truncate-lines t)
    (other-window 1)
    (switch-to-buffer current-buf)))




(defun schedule-headlines (&optional leiste setheadlinestring)
  "Inserts either the current headlines (without argument) or a
column ruler momentarily above current line, till next keystroke.
The ruler is defined by the value of column-ruler.
The key typed is executed unless it is SPC."
  (interactive "P")
  (let ((selective-display-sv selective-display))
    (if leiste
	(progn
	  (let ((truncate-lines-sv truncate-lines))
	    (setq truncate-lines t)
	    (momentary-string-display
	     column-ruler (save-excursion (beginning-of-line) (point))
	     nil "Type SPC or any command to erase ruler.")
	    (setq truncate-lines truncate-lines-sv)))
      (let ((headlines-string "------------------------------------------------------\n")
	    (truncate-lines-sv truncate-lines) (mark1) (mark2) (mark3))
	(save-excursion
	  (if (not (looking-at "^[*]"))
	      (progn
		(beginning-of-line)
		(setq mark1 (point))
		(outline-previous-visible-heading 1)
		(setq mark2 (point))
		(goto-char mark1)
		(end-of-line)
		(setq mark3 (point))
		(goto-char mark1)
		;; kann man mit reinnehmen, muss man aber nicht...
		(if (not (re-search-forward "^[ \t]+==>" mark3 t))
		    (progn
		      (goto-char mark1)
		      (if (re-search-backward "^[ \t]+==>" mark2 t)
			  (setq headlines-string (concat (current-line-x) "\n" headlines-string)))))
		(outline-previous-visible-heading 1)
		(setq headlines-string (concat (current-line-x) "\n" headlines-string))))
	  (while (not (looking-at "^[*][^*]"))
	    (setq selective-display nil)
	    (outline-up-heading 1)
	    (setq headlines-string (concat (current-line-x) "\n" headlines-string))))
	(setq headlines-string (concat "\nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
				       headlines-string))
	(setq truncate-lines t)
	(setq selective-display selective-display-sv)
	(if setheadlinestring
	    (setq op-current-headline-string headlines-string)
	  (momentary-string-display headlines-string (save-excursion (beginning-of-line) (point))
				    nil "Type SPC or any command to erase ruler."))
	(setq truncate-lines truncate-lines-sv)))))



(define-key calendar-mode-map "z" '(lambda (&optional nixda) 
				     (interactive "s [Return] fuer einen Projektterminplan aus dem Buffer obenan: " )
				     (let ((tmp (calendar-cursor-to-date)))
				       (other-window -1)
				       (project-schedule
					nil
					(concat (number-to-string (nth 1 tmp)) "."
						(number-to-string (nth 0 tmp)) "."
						(number-to-string (nth 2 tmp)))))
				     (other-window 1)))

(defun schedule-include-other-diary-files ()
  (goto-char (point-min))
  (while (re-search-forward
          (concat
           "\\(\\`\\|\^M\\|\n\\)"
           (regexp-quote diary-include-string)
           " \"\\([^\"]*\\)\"")
          nil t)
    (let ((diary-file (substitute-in-file-name
                       (buffer-substring-no-properties
                        (match-beginning 2) (match-end 2)))))
      (if (file-exists-p diary-file)
	  (if (file-readable-p diary-file)
	      (progn
		(beginning-of-line)
		(kill-line)
		(insert-file-contents diary-file))
            (beep)
            (message "Can't read included diary file %s" diary-file)
            (sleep-for 2))
        (beep)
        (message "Can't find included diary file %s" diary-file)
        (sleep-for 2))))
    (goto-char (point-min)))

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
;;;;;;(load "op-menus.xemacs")

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
;;;;;;(load "op-hilit.xemacs")

;;;
;;;
;;;
;;;   Achtung: zum Debuggen:
;;;   Variablen setzen langt nicht - ab besten debuggen in op-project.el (nach Löschen des .elc-Files),
;;;   dann übernehmen. Und nach jeder Änderung muss dann der File komplett durch Hochrufen eines
;;;   .op-Files neu geladen werden.
;;;
;;;   Grundsätzlich werden die Muster in der Reihenfolge des Erscheinens verarbeitet und beendet, wenn
;;;   das erste Muster trifft [so scheint es wohl zu sein...]
;;;
;;;







;;
;; Stichworte aus font-lock.el
;;


;;; laden der Faces
(require 'custom)
(require 'cus-edit)
(require 'font-lock)

;;
;; mit der Einstellung "lazy shot" im Options->Highlighting->Lazy Shot funktioniert der op-mode 
;; im Highlighting nicht
;;
(progn (lazy-shot-mode -1) 
       (customize-set-variable (quote lazy-shot-mode) lazy-shot-mode) 
       (redraw-modeline))


(defgroup gnus-visual nil
  "Options controling the visual fluff."
  :group 'gnus
  :group 'faces)


(defface gnus-group-news-1-face
  '((((class color)
      (background dark))
     (:foreground "PaleTurquoise" :bold t))
    (((class color)
      (background light))
     (:foreground "DarkGreen" :bold t))
    (t
     ()))
  "Level 1 newsgroup face.")

(defface gnus-group-news-1-empty-face
  '((((class color)
      (background dark))
     (:foreground "PaleTurquoise"))
    (((class color)
      (background light))
     (:foreground "DarkGreen"))
    (t
     ()))
  "Level 1 empty newsgroup face.")

(defface gnus-group-news-2-face
  '((((class color)
      (background dark))
     (:foreground "turquoise" :bold t))
    (((class color)
      (background light))
     (:foreground "CadetBlue4" :bold t))
    (t
     ()))
  "Level 2 newsgroup face.")

(defface gnus-group-news-2-empty-face
  '((((class color)
      (background dark))
     (:foreground "turquoise"))
    (((class color)
      (background light))
     (:foreground "CadetBlue4"))
    (t
     ()))
  "Level 2 empty newsgroup face.")

(defface gnus-group-news-3-face
  '((((class color)
      (background dark))
     (:bold t))
    (((class color)
      (background light))
     (:bold t))
    (t
     ()))
  "Level 3 newsgroup face.")

(defface gnus-group-news-3-empty-face
  '((((class color)
      (background dark))
     ())
    (((class color)
      (background light))
     ())
    (t
     ()))
  "Level 3 empty newsgroup face.")

(defface gnus-group-news-4-face
  '((((class color)
      (background dark))
     (:bold t))
    (((class color)
      (background light))
     (:bold t))
    (t
     ()))
  "Level 4 newsgroup face.")

(defface gnus-group-news-4-empty-face
  '((((class color)
      (background dark))
     ())
    (((class color)
      (background light))
     ())
    (t
     ()))
  "Level 4 empty newsgroup face.")

(defface gnus-group-news-5-face
  '((((class color)
      (background dark))
     (:bold t))
    (((class color)
      (background light))
     (:bold t))
    (t
     ()))
  "Level 5 newsgroup face.")

(defface gnus-group-news-5-empty-face
  '((((class color)
      (background dark))
     ())
    (((class color)
      (background light))
     ())
    (t
     ()))
  "Level 5 empty newsgroup face.")

(defface gnus-group-news-6-face
  '((((class color)
      (background dark))
     (:bold t))
    (((class color)
      (background light))
     (:bold t))
    (t
     ()))
  "Level 6 newsgroup face.")

(defface gnus-group-news-6-empty-face
  '((((class color)
      (background dark))
     ())
    (((class color)
      (background light))
     ())
    (t
     ()))
  "Level 6 empty newsgroup face.")

(defface gnus-group-news-low-face
  '((((class color)
      (background dark))
     (:foreground "DarkTurquoise" :bold t))
    (((class color)
      (background light))
     (:foreground "DarkGreen" :bold t))
    (t
     ()))
  "Low level newsgroup face.")

(defface gnus-group-news-low-empty-face
  '((((class color)
      (background dark))
     (:foreground "DarkTurquoise"))
    (((class color)
      (background light))
     (:foreground "DarkGreen"))
    (t
     ()))
  "Low level empty newsgroup face.")

(defface gnus-group-mail-1-face
  '((((class color)
      (background dark))
     (:foreground "aquamarine1" :bold t))
    (((class color)
      (background light))
     (:foreground "DeepPink3" :bold t))
    (t
     (:bold t)))
  "Level 1 mailgroup face.")

(defface gnus-group-mail-1-empty-face
  '((((class color)
      (background dark))
     (:foreground "aquamarine1"))
    (((class color)
      (background light))
     (:foreground "DeepPink3"))
    (t
     (:italic t :bold t)))
  "Level 1 empty mailgroup face.")

(defface gnus-group-mail-2-face
  '((((class color)
      (background dark))
     (:foreground "aquamarine2" :bold t))
    (((class color)
      (background light))
     (:foreground "HotPink3" :bold t))
    (t
     (:bold t)))
  "Level 2 mailgroup face.")

(defface gnus-group-mail-2-empty-face
  '((((class color)
      (background dark))
     (:foreground "aquamarine2"))
    (((class color)
      (background light))
     (:foreground "HotPink3"))
    (t
     (:bold t)))
  "Level 2 empty mailgroup face.")

(defface gnus-group-mail-3-face
  '((((class color)
      (background dark))
     (:foreground "aquamarine3" :bold t))
    (((class color)
      (background light))
     (:foreground "magenta4" :bold t))
    (t
     (:bold t)))
  "Level 3 mailgroup face.")

(defface gnus-group-mail-3-empty-face
  '((((class color)
      (background dark))
     (:foreground "aquamarine3"))
    (((class color)
      (background light))
     (:foreground "magenta4"))
    (t
     ()))
  "Level 3 empty mailgroup face.")

(defface gnus-group-mail-low-face
  '((((class color)
      (background dark))
     (:foreground "aquamarine4" :bold t))
    (((class color)
      (background light))
     (:foreground "DeepPink4" :bold t))
    (t
     (:bold t)))
  "Low level mailgroup face.")

(defface gnus-group-mail-low-empty-face
  '((((class color)
      (background dark))
     (:foreground "aquamarine4"))
    (((class color)
      (background light))
     (:foreground "DeepPink4"))
    (t
     (:bold t)))
  "Low level empty mailgroup face.")

;; Summary mode faces.

(defface gnus-summary-selected-face '((t
				       (:underline t)))
  "Face used for selected articles.")

(defface gnus-summary-cancelled-face
  '((((class color))
     (:foreground "yellow" :background "black")))
  "Face used for cancelled articles.")

(defface gnus-summary-high-ticked-face
  '((((class color)
      (background dark))
     (:foreground "pink" :bold t))
    (((class color)
      (background light))
     (:foreground "firebrick" :bold t))
    (t
     (:bold t)))
  "Face used for high interest ticked articles.")

(defface gnus-summary-low-ticked-face
  '((((class color)
      (background dark))
     (:foreground "pink" :italic t))
    (((class color)
      (background light))
     (:foreground "firebrick" :italic t))
    (t
     (:italic t)))
  "Face used for low interest ticked articles.")

(defface gnus-summary-normal-ticked-face
  '((((class color)
      (background dark))
     (:foreground "pink"))
    (((class color)
      (background light))
     (:foreground "firebrick"))
    (t
     ()))
  "Face used for normal interest ticked articles.")

(defface gnus-summary-high-ancient-face
  '((((class color)
      (background dark))
     (:foreground "SkyBlue" :bold t))
    (((class color)
      (background light))
     (:foreground "RoyalBlue" :bold t))
    (t
     (:bold t)))
  "Face used for high interest ancient articles.")

(defface gnus-summary-low-ancient-face
  '((((class color)
      (background dark))
     (:foreground "SkyBlue" :italic t))
    (((class color)
      (background light))
     (:foreground "RoyalBlue" :italic t))
    (t
     (:italic t)))
  "Face used for low interest ancient articles.")

(defface gnus-summary-normal-ancient-face
  '((((class color)
      (background dark))
     (:foreground "SkyBlue"))
    (((class color)
      (background light))
     (:foreground "RoyalBlue"))
    (t
     ()))
  "Face used for normal interest ancient articles.")

(defface gnus-summary-high-unread-face
  '((t
     (:bold t)))
  "Face used for high interest unread articles.")

(defface gnus-summary-low-unread-face
  '((t
     (:italic t)))
  "Face used for low interest unread articles.")

(defface gnus-summary-normal-unread-face
  '((t
     ()))
  "Face used for normal interest unread articles.")

(defface gnus-summary-high-read-face
  '((((class color)
      (background dark))
     (:foreground "PaleGreen"
		  :bold t))
    (((class color)
      (background light))
     (:foreground "DarkGreen"
		  :bold t))
    (t
     (:bold t)))
  "Face used for high interest read articles.")

(defface gnus-summary-low-read-face
  '((((class color)
      (background dark))
     (:foreground "PaleGreen"
		  :italic t))
    (((class color)
      (background light))
     (:foreground "DarkGreen"
		  :italic t))
    (t
     (:italic t)))
  "Face used for low interest read articles.")

(defface gnus-summary-normal-read-face
  '((((class color)
      (background dark))
     (:foreground "PaleGreen"))
    (((class color)
      (background light))
     (:foreground "DarkGreen"))
    (t
     ()))
  "Face used for normal interest read articles.")

(defface gnus-splash-face
  '((((class color)
      (background dark))
     (:foreground "Brown"))
    (((class color)
      (background light))
     (:foreground "Brown"))
    (t
     ()))
  "Face of the splash screen.")

(defface message-header-to-face
  '((((class color)
      (background dark))
     (:foreground "green2" :bold t))
    (((class color)
      (background light))
     (:foreground "MidnightBlue" :bold t))
    (t
     (:bold t :italic t)))
  "Face used for displaying From headers."
  :group 'message-faces)

(defface message-header-cc-face
  '((((class color)
      (background dark))
     (:foreground "green4" :bold t))
    (((class color)
      (background light))
     (:foreground "MidnightBlue"))
    (t
     (:bold t)))
  "Face used for displaying Cc headers."
  :group 'message-faces)

(defface message-header-subject-face
  '((((class color)
      (background dark))
     (:foreground "green3"))
    (((class color)
      (background light))
     (:foreground "navy blue" :bold t))
    (t
     (:bold t)))
  "Face used for displaying subject headers."
  :group 'message-faces)

(defface message-header-newsgroups-face
  '((((class color)
      (background dark))
     (:foreground "yellow" :bold t :italic t))
    (((class color)
      (background light))
     (:foreground "blue4" :bold t :italic t))
    (t
     (:bold t :italic t)))
  "Face used for displaying newsgroups headers."
  :group 'message-faces)

(defface message-header-other-face
  '((((class color)
      (background dark))
     (:foreground "#b00000"))
    (((class color)
      (background light))
     (:foreground "steel blue"))
    (t
     (:bold t :italic t)))
  "Face used for displaying newsgroups headers."
  :group 'message-faces)

(defface message-header-name-face
  '((((class color)
      (background dark))
     (:foreground "DarkGreen"))
    (((class color)
      (background light))
     (:foreground "cornflower blue"))
    (t
     (:bold t)))
  "Face used for displaying header names."
  :group 'message-faces)

(defface message-header-xheader-face
  '((((class color)
      (background dark))
     (:foreground "blue"))
    (((class color)
      (background light))
     (:foreground "blue"))
    (t
     (:bold t)))
  "Face used for displaying X-Header headers."
  :group 'message-faces)

(defface message-separator-face
  '((((class color)
      (background dark))
     (:foreground "blue3"))
    (((class color)
      (background light))
     (:foreground "brown"))
    (t
     (:bold t)))
  "Face used for displaying the separator."
  :group 'message-faces)

(defface message-cited-text-face
  '((((class color)
      (background dark))
     (:foreground "red"))
    (((class color)
      (background light))
     (:foreground "red"))
    (t
     (:bold t)))
  "Face used for displaying cited text names."
  :group 'message-faces)

(defface message-mml-face
  '((((class color)
      (background dark))
     (:foreground "DarkGreen"))
    (((class color)
      (background light))
     (:foreground "DarkGreen"))
    (t
     (:bold t)))
  "Face used for displaying MML."
  :group 'message-faces)

(if window-system
    (progn
      (if (x-display-color-p)
	  (progn
;;	    (hilit-set-mode-patterns
;;	     'logbuch-mode
	    (setq op-font-lock-keywords
	      (list


	       '("^\\(\\*E\\|\\*FW\\).*[\r\n]" . widget-button-pressed-face)       ; Erledigtes in Hauptüberschrift
	       '("^\\(\\*\\*+E\\|\\*+FW\\).*[\r\n]" . gnus-group-news-2-empty-face); Erledigtes in Unterüberschrift
					                                           ; (generelle Doku: in op-menus.el)
	       '("^\\(\\*+V\\|\\*+S\\).*[\r\n]" . gnus-group-mail-3-face)          ; Verschobenes und ständig 
					                                           ;              wiederkehrendes
	       '("^\\*+[ \t]+#.*[\r\n]" . cvs-need-action-face)	    		   ; Schablonen-Färbung

	       '("^\\*+\\-.*[\r\n]" . gnus-group-news-2-face)			   ; *---- - Zeilen
	       '("^\\*+\\+.*[\r\n]" . message-header-to-face)			   ; *++++ - Zeilen
	       '("^\\*+|.*[\r\n]" . custom-invalid-face)			   ; *|||| - Zeilen
	       '("^\\*+=.*[\r\n]" . display-time-mail-balloon-enhance-face)	   ; *==== - Zeilen
	       '("^\\*[^*+=-EFVDSR].*[\r\n]" . gnus-summary-high-ticked-face)      ; Hauptüberschrift ; F != FW !!!
	       '("^\\*\\*[^*+=-EFVDSR].*[\r\n]" . gnus-summary-high-unread-face)             ; **-Unterüberschrift ; F != FW !!!
	       '("^\\*\\*\\*+[^*+=-EFVDSR].*[\r\n]" . message-header-to-face)  ; Unterüberschriften
	;;     '("^\\*\\*\\*+[^*+=-EFVDSR].*[\r\n]" . font-latex-bold-face)        ; Unterüberschriften
	       '("^\\*+D.*[\r\n]" . font-lock-warning-face)			   ; Dringendes
	       ;'("^\\*+IB.*[\r\n]"  . dired-face-socket)                          ; lila schwach: in Bearbeitung (noch BUG)
	       '("^\\*+IB.*[\r\n]" . custom-set-face)                              ; blau schwach: in Bearbeitung

	       ;; speziell für MPX-MSP
	       '("{Nr\\..*}" . gnus-group-news-low-empty-face)                     ; goldenrot wäre besser
	       '("{Verantwortlich.*}" . gnus-group-news-low-empty-face)
	       '("{Dauer.*}" . message-header-newsgroups-face)


	       ;; Doppelzeilenkombination für golden unterlegt -> xemacs: rot grau unterlegt
	       '("#T.*\\(#\\|[\r\n]\\)" . modeline-mousable)
	       '("#R.*\\(#\\|[\r\n]\\)" . custom-comment-tag-face)

	       ;; eigentlich gewünscht: dunkelgrün
	       '("#[^TR].*\\(#\\|[\r\n]\\)" . dired-face-socket)


	       '("\\<\\(Info\\|INFO\\|info\\)\\>.*\\(\\|[\r\n]\\)" . custom-button-face)    ; bold schwarz
	          ;;glob-struct)
	       '("\\[Meilenstein\\]" . gnus-group-news-1-face) ; grün bold
	       '("|| \\[Unterprojekt\\].*||" . font-lock-warning-face)             ; rot bold
	       '("#include[ \t]+\".*\"" . gnus-group-news-1-face)
	       '("[\r\n]#end_include_statement" . gnus-group-news-1-face)

	       ;;	   ("->" "[ \t\r]\\|$" jargon-keyword)
	       ;; ("->" nil label)
	       ;;("/\\*" "\\*/" msg-from)   ; wie C-Kommentare
	       ;; Doppelzeilenkombination für golden unterlegt
	       '("/\\*\\.*\\*/" . yellow)

	       '("\\<\\(Info\\|INFO\\|info\\)\\>.*\\(\\|[\r\n]\\)" . custom-button-face)    ; bold schwarz
	          ;;glob-struct)
	       '("\\[Meilenstein\\]" . gnus-group-news-1-face) ; grün bold
	       '("|| \\[Unterprojekt\\].*||" . font-lock-warning-face)             ; rot bold
	       '("#include[ \t]+\".*\"" . gnus-group-news-1-face)
	       '("[\r\n]#end_include_statement" . gnus-group-news-1-face)

	       ;;	   ("->" "[ \t\r]\\|$" jargon-keyword)
	       ;; ("->" nil label)
	       ;;	   ("->" "[ \t\r]\\|$" string)

	       
	       ))))))



;;
;; mache ein anderes hilit-repaint-command auf control-shift-l = control L
;;
(if xemacsp
    (progn
      (defun hilit-repaint-command-interactive (&optional var)
	(interactive)
	(save-excursion
	  (font-lock-fontify-buffer)))

      (defun hilit-repaint-command (&optional var)
	(interactive)
	(if (fboundp 'font-lock-fontify-buffer)
	    (save-excursion
	      (font-lock-fontify-buffer)))
	t)

      (defun hilit-repaint-command-region ()
	(interactive)
	(if (fboundp 'font-lock-fontify-region)
	    (save-excursion
	      (font-lock-fontify-region (region-beginning) (region-end)))))))
;;
;; nur zum merken
;; die frühere Version, sehr viel langsamer und massiver
;;; verändertes hilit-repaint-command für den XEmacs
;      (progn (require (quote font-lock)) 
;	     (if (and (integerp font-lock-maximum-decoration) 
;		      (= 2 font-lock-maximum-decoration)) 
;		 nil 
;	       (customize-set-variable (quote font-lock-maximum-decoration) 2) 
;      (font-lock-recompute-variables)
;	       ))
;      (progn (require (quote font-lock)) 
;	     (if (or (eq font-lock-maximum-decoration t) 
;		     (and (integerp font-lock-maximum-decoration) 
;			  (>= font-lock-maximum-decoration 3))) 
;		 nil 
;	       (customize-set-variable (quote font-lock-maximum-decoration) t) 
;	       (font-lock-recompute-variables)))
;      )
;  )
(define-key outline-minor-mode-map [(control L)] 'hilit-repaint-command-interactive)
(define-key outline-minor-mode-map [(control meta L)] 'hilit-repaint-command-region)



(setq font-lock-keywords op-font-lock-keywords )
(put 'logbuch-mode 'font-lock-defaults '(op-font-lock-keywords))


;	(hilit-set-mode-patterns
;	 'logbuch-mode
;	 '(
;	   ("==>" nil string)
;	   ("^\\*+E.*\\|^\\*+FW.*\\|^\\*+V.*" "[\r\n]" keyword)
;	   ("#[^#]" ".#\\|[\r\n]" msg-from)
;	   ("\\<\\(Info\\|INFO\\|info\\)\\>.*" "[\r\n]" glob-struct)
;	   ("^\\*+\\-+" "[\r\n]" msg-from)
;	   ("^\\*" "[\r\n]" defun)
;	   )))

;      ;;(setq hilit-face-check nil) ; Fonds werden ja meistens nicht extra geändert, sonst hilft auch noch C-u S-C-l    
;      (setq hilit-inhibit-rebinding t) ; wenn der Server nur wenige Farben hat, geht sonst manchmal Yank nicht mehr
;      (setq hilit-auto-rehighlight 10000) ; Anzahl der automatisch upgedateten Zeilen bei Benutzung des X-Servers

;      ;; auch der hilit-mode kommt nicht mit dem (setq selective-display t) zurecht
;      (setq hilit-auto-rehighlight-fallback '(20000 . 10000))
;      (make-variable-buffer-local 'hilit-auto-rehighlight-fallback)

;      (hilit-repaint-command t)
;      ))
;;(";.*" nil comment)
;;("#|" "|#" comment)
;;("%.*$" nil comment)
;;("Vorgänger" nil crosref)
;;("==>" nil summary-seen)
;; ("==>" nil mgs-separator)

;
; alle Definitionen oben aus lisp_dir: hilit19.el
;
; error rot
; jargon-keyword  ;golden
; string ; grau
; comment ist im Prinzip nocht fast nicht genutzt
; decl schwaches blau
; dired-marked: schwaches lila
; jargon-keyword rot unterstrichen
; struct ganz schwarz
; summary-Xed blaß sehr hellgrün
; goldenrod golden
; summary-deleted laues rot
; glob-struct dunkelrosa
; summary-current  Balken in blau
; summary-unread schwaches blau
; define bold grün
; msg-from violett
; glob-struct rosa
;
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
      (if window-system
	  (if xemacsp
	      (hilit-repaint-command-interactive)
	    (hilit-repaint-command)))
      (hide-other-hide)
      (goto-char (point-min))
      (if (re-search-forward "^\\*" nil t)
	  (progn
	    (beginning-of-line)))))



(message "... und fertig mit dem Project-mode")





