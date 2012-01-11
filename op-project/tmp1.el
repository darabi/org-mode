;;;
;;; Project-Mode-File,
;;; geschrieben 12.11.1994 - 27.8.1997 von Stefan Walther (SMW), Mainz, Rudolf-Diesel-Str. 3
;;;
;;; Spezialmode zum Verwalten von Projektplanungsfiles
;;;


(defun byte-compiled-function_743154472 ()
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
;; (byte-compiled-function_743154472)
;; mit C-x C-e  in diesem Buffer


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
;; (byte-compiled-function_542999964) incorporated patch which improves its behavior
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
;;   (defun byte-compiled-function_463759259 (message)
;; --- 152,159 ----
;;                   (insert "->")
;;                   (delete-char 2)
;;                   (forward-char -2)
;; !                 (and w vm-auto-center-summary (vm-auto-center-summary))
;; !                 (run-hooks 'vm-summary-pointer-hook)))
;;             (and old-window (select-window old-window)))))))
;;   
;;   (defun byte-compiled-function_463759259 (message)
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

(defun byte-compiled-function_1006431373 ()
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
	    cmdl (cons (list 'byte-compiled-function_252480007 ''hilit-face-translation-table
			     (list 'quote from) to)
		       cmdl)))
    (cons 'progn cmdl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; This function actually translates and then creates the faces...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defun byte-compiled-function_542999964 (face &optional force)
  "Get a FACE, or create it if it doesn't exist.  In order for it to
properly create the face, the following naming convention must be used:
    [reverse-](fgcolor[/bgcolor])[-bold][-italic][-underline]
Example: (byte-compiled-function_542999964 'comment-face) might create and return 'red

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

(defun byte-compiled-function_378758324 (start end &optional quietly)
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
;;(defun byte-compiled-function_378758324 (start end &optional quietly)
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

(defun byte-compiled-function_549304070 (start end &optional patterns quietly)
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
		face (byte-compiled-function_542999964 (nth 2 p)))
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

(defun byte-compiled-function_404291319 (start end &optional quietly)
  "Re-highlights the region, optionally in a QUIET way"
  (interactive "r")
  (save-restriction
    (widen)
    (setq start (apply 'min start (mapcar 'overlay-start (overlays-at start)))
	  end (apply 'max end (mapcar 'overlay-end (overlays-at end))))
    (byte-compiled-function_378758324 start end quietly)
    (byte-compiled-function_549304070   start end nil quietly)))

(defun byte-compiled-function_643971630 (&optional quietly)
  "Re-highlights the buffer, optionally in a QUIET way"
  (interactive "")
  (let ((parse-fn (cdr (assq major-mode hilit-parser-alist))))
    (if parse-fn
	(funcall parse-fn quietly)
      (byte-compiled-function_404291319 (point-min) (point-max) quietly)))
  nil)

(defun byte-compiled-function_251075944 ()
  (byte-compiled-function_643971630 t))

(defun byte-compiled-function_693812173 (quietly)
  "Highlight a buffer containing a news article or mail message."
  (save-excursion
    (goto-char (point-min))
    ;; find separation between headers and body (either a blank line or
    ;; the message separator line in mail-mode)
    (re-search-forward "^\\(\\|--text follows this line--\\)$" nil 'noerr)
    (byte-compiled-function_378758324 (point-min) (point-max) quietly)
    (byte-compiled-function_549304070 (point-min) (point) 'msg-header quietly)
    (byte-compiled-function_549304070 (point) (point-max) 'msg-body quietly)))

(defalias 'hilit-highlight-buffer 'byte-compiled-function_643971630)

;; Well, I want to remove this function...there's one sure way to find out if
;; anyone uses it or not...and that's to comment it out.
;; 
;; (defun byte-compiled-function_477978052 (arg)
;;   "Locally toggle highlighting.  With arg, forces highlighting off."
;;   (interactive "P")
;;   ;; FIXME -- this loses numeric information in hilit-auto-rehighlight
;;   (setq hilit-auto-rehighlight
;;         (and (not arg) (not hilit-auto-rehighlight)))
;;   (if hilit-auto-rehighlight
;;       (byte-compiled-function_643971630)
;;     (byte-compiled-function_378758324 (point-min) (point-max)))
;;   (message "Rehighlighting is set to %s" hilit-auto-rehighlight))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HOOKS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun byte-compiled-function_78982046 ()
  "Find-file hook for hilit package.  See the variable hilit-auto-highlight."
  (cond ((and hilit-auto-highlight
	      (assq major-mode hilit-patterns-alist))
	 (if (> buffer-saved-size (car hilit-auto-rehighlight-fallback))
	     (setq hilit-auto-rehighlight
		   (cdr hilit-auto-rehighlight-fallback)))
	 (if (> buffer-saved-size hilit-auto-highlight-maxout)
	     nil
	   (let ((bm (buffer-modified-p)))
	     (byte-compiled-function_643971630)
	     (set-buffer-modified-p bm))))))

(defun byte-compiled-function_503165602 (arg)
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
	   (byte-compiled-function_643971630)))
    (if st
	  (byte-compiled-function_404291319 st en quietly))))

(defun byte-compiled-function_951238441 (arg)
  "Recenter, then rehighlight according to hilit-auto-rehighlight.  If called
with an unspecified prefix argument (^U but no number), then a rehighlight of
the entire buffer is forced."
  (interactive "P")
  (recenter arg)
  ;; force display update
  (sit-for 0)
  (byte-compiled-function_503165602 (consp arg)))

(defun byte-compiled-function_278174960 (arg)
  "Yank with rehighlighting"
  (interactive "*P")
  (let ((transient-mark-mode nil))
    (yank arg)
    (and hilit-auto-rehighlight
	 (byte-compiled-function_404291319 (region-beginning) (region-end) t))
    (setq this-command 'yank)))

(defun byte-compiled-function_1016665817 (arg)
  "Yank-pop with rehighlighting"
  (interactive "*p")
  (let ((transient-mark-mode nil))
    (yank-pop arg)
    (and hilit-auto-rehighlight
	 (byte-compiled-function_404291319 (region-beginning) (region-end) t))
    (setq this-command 'yank)))

;;; this line highlighting stuff is untested.  play with it only if you feel
;;; adventurous...don't ask me to fix it...though you're welcome to.  -- Stig
;; 
;; (defun byte-compiled-function_25631136 (&rest args)
;;   "Quietly rehighlight just this line.
;; Useful as an after change hook in VM/gnus summary buffers and dired buffers.
;; If only there were an after-change-function, that is..."
;;   (save-excursion
;;     (push-mark nil t)
;;     (hilit-rehighlight-yank-region)
;;     (and orig-achange-function (apply orig-achange-function args))))
;; 
;; (defun byte-compiled-function_35993941 ()
;;   (make-variable-buffer-local 'after-change-function)
;;   (make-local-variable 'orig-achange-function)
;;   (setq orig-achange-function after-change-function)
;;   (setq after-change-function 'byte-compiled-function_25631136))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wysiwyg Stuff...  take it away and build a whole package around it!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; ; For the Jargon-impaired, WYSIWYG === What You See Is What You Get
;; ; Sure, it sucks to type.  Oh, well.
;; (defun byte-compiled-function_1027905439 ()
;;   "Replace overstruck text with normal text that's been overlaid with the 
;; appropriate text attribute.  Suitable for a find-file hook."
;;   (save-excursion
;;     (goto-char (point-min))
;;     (let ((wysb (byte-compiled-function_542999964 'wysiwyg-bold))
;; 	  (wysu (byte-compiled-function_542999964 'wysiwyg-underline))
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
;; (defun byte-compiled-function_748476685 ()
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
    (global-set-key [(control L)] 'byte-compiled-function_503165602) ;;Xemacs korrekt??
  (global-set-key [?\C-\S-l] 'byte-compiled-function_503165602))

(and window-system
     (add-hook 'find-file-hooks 'byte-compiled-function_78982046 t))

(eval-when-compile (require 'gnus))	; no compilation gripes

(and (not hilit-inhibit-hooks)
     window-system
     (condition-case c
	 (progn

	   ;; BUFFER highlights...
	   (mapcar (function
		    (lambda (hook)
		      (add-hook hook 'byte-compiled-function_251075944)))
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

(defun byte-compiled-function_252480007 (alist key val)
  "creates, or destructively replaces, the pair (key . val) in alist"
  (let ((oldentry (assq key (eval alist))))
    (if oldentry
	(setcdr oldentry val)
      (set alist (cons (cons key val) (eval alist))))))
  
(defun byte-compiled-function_494802963 (modelist patterns
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
			   (byte-compiled-function_252480007 'hilit-parser-alist m parse-fn))
		      (byte-compiled-function_252480007 'hilit-patterns-alist m patterns)))))
	    modelist)))

(defun byte-compiled-function_53242517 (pstart pend face &optional mode first)
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
	   (byte-compiled-function_494802963 mode (list new-pat)))
	  (first
	   (setcdr old-patterns (cons new-pat (cdr old-patterns))))
	  (t 
	   (nconc old-patterns (list new-pat)))))
  (and (interactive-p) (byte-compiled-function_643971630)))

(defun byte-compiled-function_416726573 (qchar)
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
      (strings      '((byte-compiled-function_416726573 ?' string)))
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
   (byte-compiled-function_416726573 ?\\ string)
   ("^[ \t]*PROCEDURE[ \t]+\\w+[^ \t(;]*" nil defun)
   ("\\<\\(RECORD\\|ARRAY\\|OF\\|POINTER\\|TO\\|BEGIN\\|END\\|FOR\\|IF\\|THEN\\|ELSE\\|ELSIF\\|CASE\\|WHILE\\|DO\\|MODULE\\|FROM\\|RETURN\\|IMPORT\\|EXPORT\\|VAR\\|LOOP\\|UNTIL\\|\\DEFINITION\\|IMPLEMENTATION\\|AND\\|OR\\|NOT\\|CONST\\|TYPE\\|QUALIFIED\\)\\>" nil keyword)
   )
 nil 'case-insensitive)

(byte-compiled-function_494802963 'prolog-mode
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
  (byte-compiled-function_494802963 'msg-header header-patterns)
  (byte-compiled-function_494802963 'msg-body body-patterns)
  (byte-compiled-function_494802963 '(vm-mode text-mode mail-mode rmail-mode
			     gnus-article-mode news-reply-mode mh-show-mode)
			   message-patterns
			   'byte-compiled-function_693812173))

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
   (byte-compiled-function_416726573 ?\\ string)

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
   (byte-compiled-function_416726573 ?\\ string)

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
 (when (or (not (fboundp 'byte-compiled-function_1038303325))
           (not (fboundp 'byte-compiled-function_590419240)))
   ;; `buffer-invisibility-spec' mutators snarfed from Emacs 20.3 lisp/subr.el
   (defun byte-compiled-function_1038303325 (arg)
     (cond
      ((or (null buffer-invisibility-spec) (eq buffer-invisibility-spec t))
       (setq buffer-invisibility-spec (list arg)))
      (t
       (setq buffer-invisibility-spec
             (cons arg buffer-invisibility-spec)))))
   (defun byte-compiled-function_590419240 (arg)
     (if buffer-invisibility-spec
         (setq buffer-invisibility-spec
               (delete arg buffer-invisibility-spec)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; postscript stuff
      

(load "ps-print")


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
;;   (byte-compiled-function_455696239 t)
;;
;;   USE
;;
;;   ;;Switch to landscape mode:
;;   (byte-compiled-function_417627836 t)  ;; no arg: toggeling
;;   ;;Switch to portrait mode:
;;   (byte-compiled-function_455696239 t)   ;; no arg: toggeling
;;
;;   Afterwards all ps-print-commands should use ps in landscape or portrait format
;;


(setq ps-landscape-mode nil)

(defun byte-compiled-function_417627836 (&optional on)  ;;; SMW
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


(defun byte-compiled-function_455696239 (&optional on)   ;;;SMW
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
                                 (set-variable 'byte-compiled-function_442969003 nil)
                               (set-variable 'byte-compiled-function_442969003 t))
                             (redraw-display)))




(defun byte-compiled-function_427348111 ()
  (setq ps-page-dimensions (assq ps-paper-type ps-pages-alist))
  (let ((ps-page-height (nth ps-page-width-i ps-page-dimensions))    ;;; AE + SMW
	(ps-page-width (nth ps-page-height-i ps-page-dimensions)))   ;;; AE + SMW
    (setq ps-print-height (- ps-page-height ps-top-margin ps-bottom-margin))
    (setq ps-print-width (- ps-page-width ps-left-margin ps-right-margin))))

(defun byte-compiled-function_815570140 ()
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

  (byte-compiled-function_427348111)
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




(byte-compiled-function_417627836 t)

(defun byte-compiled-function_826254242 ()
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


;;
;;
;; Die nächsten defadvice sind dafür, daß ps-print auch mit dem 
;; Outline-mode läuft
;;

(defun byte-compiled-function_998183373 ()
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
(defun byte-compiled-function_462719084 ()
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
(defun byte-compiled-function_340378747 ()
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
(defun byte-compiled-function_160184825 ()
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
    (defun byte-compiled-function_503165602 (&optional var)
      t))

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
;;;   - Schnellfunktion (c) im gleichen Buffer oder in einem extra Fenster (byte-compiled-function_316408522)
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



(defun byte-compiled-function_806315143 ()
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
(byte-compiled-function_806315143)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun byte-compiled-function_781836839 (&optional args)
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
     (format "%s(byte-compiled-function_573264086   %s \"%s\")"
             sexp-diary-entry-symbol
	     weeknumber
	     dayofweek) args)))



(defun byte-compiled-function_573264086 (weeknumber dayofweek)
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
(defun byte-compiled-function_533720597 (date)
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
(defun byte-compiled-function_93826975 (phase)
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
	    (byte-compiled-function_614374748)
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
    (fset 'mark-all-calendar-holidays-original (symbol-function 'byte-compiled-function_398657241)))

(defun byte-compiled-function_398657241 ()
  "Wie calendar-holidays, aber ohne die \"merktage\"-, sondern 
nur mit der \"gcal\"-feiertage\"-Liste."
  (interactive)
  (let ((calendar-holidays nil))
    (setq calendar-holidays gcal-feiertage)
    (mark-all-calendar-holidays-original)
    ))


(defun byte-compiled-function_802999490 ()
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


(define-key calendar-mode-map "?" 'byte-compiled-function_285999729)
(defun byte-compiled-function_285999729 ()
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


(defun byte-compiled-function_799191644 (date)
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


(defun byte-compiled-function_157835516 ()
  (interactive)
  (let* ((day-number (calendar-day-number (calendar-cursor-to-date t)))
	 (datestring (calendar-date-string (calendar-cursor-to-date t)))
	 (calendarweek (byte-compiled-function_799191644 (calendar-cursor-to-date t)))
	 (mark-diary-entries-in-calendar mark-diary-entries-in-calendar)
	 (mark-diary-entries-in-calendar nil))
    (if window-system
	(progn
	  (byte-compiled-function_503165602 t)
	  (if mark-sundays (mark-calendar-days-named 7)))) ;; alle Sonntage rot einfärben
    (if (< day-number 20)
	(message ">> %s, der %dte Tag des Jahres in Kalenderwoche %d <<"
		 datestring day-number calendarweek)
      (message ">> %s, der %dste Tag des Jahres in Kalenderwoche %d <<"
	       datestring day-number calendarweek)
    )))

(message "Kalender geladen bis Marke 3b")


(defun byte-compiled-function_954576792 ()
  (byte-compiled-function_157835516))



(defun byte-compiled-function_528263723 (kallist)
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
	    (byte-compiled-function_398657241))

	(byte-compiled-function_157835516)

;	(modify-syntax-entry ?ä "w")
;	(modify-syntax-entry ?Ä "w")
;	(modify-syntax-entry ?ö "w")
;	(modify-syntax-entry ?Ö "w")
;	(modify-syntax-entry ?ü "w")
;	(modify-syntax-entry ?Ü "w")
;	(modify-syntax-entry ?ß "w")

	(defun byte-compiled-function_12861896 (date)
	  "Gives you the number of the week of DATE in the current year"
	  (calendar-goto-date (reverse (cons  (extract-calendar-year date) '(1 1))))
	  (calendar-end-of-week 1)
	  ;; in calendar-end-of-week ist ein Bug: daher
	  (if (> (calendar-day-number (calendar-cursor-to-date t)) 7)
	      (calendar-backward-week 1))
	  (let ((offset (- 7 (calendar-day-number (calendar-cursor-to-date t)))))
	    (calendar-goto-date date)
	    (ceiling (/ (+ (calendar-day-number date) offset) 7.0))))
	
	(setq actual-week-number (byte-compiled-function_12861896 (calendar-current-date)))

	(if window-system
	    (progn
	      (byte-compiled-function_503165602 t)
	      (if mark-sundays (mark-calendar-days-named 7)))) ;; alle Sonntage rot einfärben
	(delete-window)
	(setq diary-list-include-blanks t)

	(add-hook 'diary-display-hook 'fancy-diary-display)
	(add-hook 'diary-display-hook 'byte-compiled-function_582998651)

	;; the next two statements are enabeling the #include "filename" statement in the diary file
	(add-hook 'list-diary-entries-hook 'byte-compiled-function_957235300)
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
  (byte-compiled-function_490225394 ask-for-date)
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
    ;(byte-compiled-function_914163068)
    (if mark-calendar-holidays-initially
	(byte-compiled-function_398657241))
    (unless mark-diary-entries-in-calendar
      (byte-compiled-function_157835516))
    (define-key calendar-mode-map "d" 'byte-compiled-function_709523722)
    (define-key calendar-mode-map "D" 'byte-compiled-function_711801601)
    ;; Umdefinition der Funktion make-diary-entry
))


  

(defun byte-compiled-function_490225394 (&optional arg)
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


(defun byte-compiled-function_720737204 ()
  (interactive)
  (if mark-diary-entries-in-calendar
      (progn
	(setq mark-diary-entries-in-calendar nil)
	(calendar-forward-month 1)
	(setq mark-diary-entries-in-calendar t))
    (calendar-forward-month 1))
  (byte-compiled-function_914163068))

(defun byte-compiled-function_550892419 ()
  (interactive)
  (if mark-diary-entries-in-calendar
      (progn
	(setq mark-diary-entries-in-calendar nil)
	(calendar-backward-month 1)
	(setq mark-diary-entries-in-calendar t))
    (calendar-backward-month 1))
  (byte-compiled-function_914163068))

(defun byte-compiled-function_181995313 ()
  (interactive)
  (calendar-goto-today)
  (if window-system
      (progn
	(byte-compiled-function_503165602 t)
	(if mark-sundays (mark-calendar-days-named 7)))) ;; alle Sonntage rot einfärben
  (byte-compiled-function_914163068))

(defun byte-compiled-function_914163068 ()
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
	  (byte-compiled-function_503165602 t)
	  (if mark-sundays (mark-calendar-days-named 7)))))) ;; alle Sonntage rot einfärben

(defun byte-compiled-function_438893814 () 
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

(define-key calendar-mode-map [down-mouse-2] 'byte-compiled-function_344375903)
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



(defun byte-compiled-function_344375903 (event)
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


(defun byte-compiled-function_659148460 ()
  "Insert diary entry for mouse-selected date."
  (interactive)
  (let ((tmp diary-file))
    (setq diary-file personal-diary-file)
    (save-excursion
      (calendar-mouse-goto-date (calendar-event-to-date))
      (insert-diary-entry nil))
    (setq diary-file tmp)))

(defun byte-compiled-function_207352388 ()
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





(define-key calendar-mode-map " " 'byte-compiled-function_490843972) ; Blank-Taste
(define-key calendar-mode-map [menu-bar goto calendar-scroll-down-diary-1]
  '("Scolle Terminkalenderfenster noch unten" . calendar-scroll-diary-down))
(define-key calendar-mode-map "\C-?" 'byte-compiled-function_569957066) ; delete-Taste
(define-key calendar-mode-map [menu-bar goto calendar-scroll-up-diary-1]
  '("Scolle Terminkalenderfenster noch oben" . calendar-scroll-diary-up))



(define-key calendar-mode-map "\M-d" 'byte-compiled-function_490843972)
(define-key calendar-mode-map [menu-bar goto scroll-down-diary]
  '("Blättere Terminkalenderfenster noch unten" . calendar-scroll-diary-down))
(defun byte-compiled-function_490843972 () 
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

(define-key calendar-mode-map "\M-u" 'byte-compiled-function_569957066)
(define-key calendar-mode-map [menu-bar goto scroll-up-diary]
  '("Blättere Terminkalenderfenster nach oben" . calendar-scroll-diary-up))
(defun byte-compiled-function_569957066 () 
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


(define-key calendar-mode-map "\C-o" 'byte-compiled-function_658957709)
(define-key calendar-mode-map [menu-bar goto change-focus]
  '("Ins andere Fenster" . change-focus))
(defun byte-compiled-function_658957709 ()
  (interactive)
  (if calendar-setup
      (raise-frame (next-frame nil 'visible))
    (other-window 1)))









(message "Kalender geladen bis Marke 5")


(define-key calendar-mode-map "." 'byte-compiled-function_181995313)
(define-key calendar-mode-map [menu-bar goto today]
  '("Zum heutigen Datum" . calendar-goto-today-rehilit))
(define-key calendar-mode-map "b" 'byte-compiled-function_550892419)
(define-key calendar-mode-map [menu-bar goto bk-1]
  '("1 Monat zurück" . calendar-backward-month-center))
(define-key calendar-mode-map "f" 'byte-compiled-function_720737204)
(define-key calendar-mode-map [menu-bar goto fwd-1]
  '("1 Monat vorwärts" . calendar-forward-month-center))
(define-key calendar-mode-map "\C-l" 'byte-compiled-function_914163068)
(define-key calendar-mode-map [menu-bar goto recenter]
  '("Kalender neu zeichnen" . calendar-recenter))
(define-key calendar-mode-map "n" 'byte-compiled-function_157835516)
(define-key calendar-mode-map [menu-bar goto daymess]
  '("Info über den Tag" . calendar-day-message))
(define-key calendar-mode-map "d" 'byte-compiled-function_709523722)
(define-key calendar-mode-map "D" 'byte-compiled-function_711801601)
(define-key calendar-mode-map "q" 'byte-compiled-function_438893814)
(if (not xemacsp)
    (define-key menu-bar-file-menu [quit-cal]
      '("Kalender iconifizieren" . calendar-quit)))
(define-key calendar-mode-map "c" 'byte-compiled-function_802999490)
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
(defun byte-compiled-function_470904403 ()
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
  (setq auto-fill-function 'byte-compiled-function_470904403)
  (set-fill-column diary-fill-column) ;; an den PS-Mode angeglichen
  (setq calendar-date-display-form calendar-date-display-form-tmp)
  )

(defadvice insert-block-diary-entry (after activate-auto-fill-function activate)
  (make-local-variable 'auto-fill-function)
  (setq auto-fill-function 'byte-compiled-function_470904403)
  (set-fill-column diary-fill-column) ;; an den PS-Mode angeglichen
  )
(defadvice insert-cyclic-diary-entry (after activate-auto-fill-function activate)
  (make-local-variable 'auto-fill-function)
  (setq auto-fill-function 'byte-compiled-function_470904403)
  (set-fill-column diary-fill-column) ;; an den PS-Mode angeglichen
  )
(defadvice insert-anniversary-diary-entry (after activate-auto-fill-function activate)
  (make-local-variable 'auto-fill-function)
  (setq auto-fill-function 'byte-compiled-function_470904403)
  (set-fill-column diary-fill-column) ;; an den PS-Mode angeglichen
  )
(defadvice insert-yearly-diary-entry (after activate-auto-fill-function activate)
  (make-local-variable 'auto-fill-function)
  (setq auto-fill-function 'byte-compiled-function_470904403)
  (set-fill-column diary-fill-column) ;; an den PS-Mode angeglichen
  )
(defadvice insert-monthly-diary-entry (after activate-auto-fill-function activate)
  (make-local-variable 'auto-fill-function)
  (setq auto-fill-function 'byte-compiled-function_470904403)
  (set-fill-column diary-fill-column) ;; an den PS-Mode angeglichen
  )
(defadvice insert-weekly-diary-entry (after activate-auto-fill-function activate)
  (make-local-variable 'auto-fill-function)
  (setq auto-fill-function 'byte-compiled-function_470904403)
  (set-fill-column diary-fill-column) ;; an den PS-Mode angeglichen
  )
(defadvice insert-weekday-diary-entry (after activate-auto-fill-function activate)
  (make-local-variable 'auto-fill-function)
  (setq auto-fill-function 'byte-compiled-function_470904403)
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

(define-key calendar-mode-map "ipd" 'byte-compiled-function_300979462)
(defun byte-compiled-function_300979462 (&optional args) 
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
(define-key calendar-mode-map "ipb" 'byte-compiled-function_1053784945)
(defun byte-compiled-function_1053784945 (&optional args) 
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
(define-key calendar-mode-map "ipc" 'byte-compiled-function_738986225)
(defun byte-compiled-function_738986225 (&optional args) 
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
(define-key calendar-mode-map "ipa" 'byte-compiled-function_53899419)
(defun byte-compiled-function_53899419 (&optional args)
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
(define-key calendar-mode-map "ipy" 'byte-compiled-function_151860648)
(defun byte-compiled-function_151860648 (&optional args)
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
(define-key calendar-mode-map "ipm" 'byte-compiled-function_1025902629)
(defun byte-compiled-function_1025902629 (&optional args)
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
(define-key calendar-mode-map "ipw" 'byte-compiled-function_656401460)
(defun byte-compiled-function_656401460 (&optional args)
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
(define-key calendar-mode-map "ipr" 'byte-compiled-function_33321071)
(defun byte-compiled-function_33321071 (&optional args)
  (interactive "P")
  (let ((tmp diary-file))
    (setq diary-file personal-diary-file)
    (byte-compiled-function_781836839 args)
    (if (not (or (string-match user-login-name (file-name-nondirectory buffer-file-name))
		 (string-match user-real-login-name (file-name-nondirectory buffer-file-name))))
	(save-excursion
	  (insert "  <" signature ">")))
;    (save-excursion
;      (insert (concat "\n\t\t<File: " (file-name-nondirectory buffer-file-name) ">")))
    (setq diary-file tmp)))
;;; den Gruppenkalenderfile ansehen (wie s normal)
(define-key calendar-mode-map "p" 'byte-compiled-function_450509852)
(defun byte-compiled-function_450509852 ()
  (interactive)
  (let ((tmp diary-file))
    (setq diary-file personal-diary-file)
    (show-all-diary-entries)
    (setq diary-file tmp)))

(defadvice show-all-diary-entries (after show-really-everything-c activate)
  (byte-compiled-function_12901847))


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


(define-key calendar-mode-map "igd" 'byte-compiled-function_30786652)
(defun byte-compiled-function_30786652 (&optional args) 
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
(define-key calendar-mode-map "igb" 'byte-compiled-function_43032443)
(defun byte-compiled-function_43032443 (&optional args) 
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
(define-key calendar-mode-map "igc" 'byte-compiled-function_628123653)
(defun byte-compiled-function_628123653 (&optional args) 
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
(define-key calendar-mode-map "iga" 'byte-compiled-function_591662421)
(defun byte-compiled-function_591662421 (&optional args)
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
(define-key calendar-mode-map "igy" 'byte-compiled-function_581836207)
(defun byte-compiled-function_581836207 (&optional args)
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
(define-key calendar-mode-map "igm" 'byte-compiled-function_825818667)
(defun byte-compiled-function_825818667 (&optional args)
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
(define-key calendar-mode-map "igw" 'byte-compiled-function_431449851)
(defun byte-compiled-function_431449851 (&optional args)
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
(define-key calendar-mode-map "igr" 'byte-compiled-function_639790797)
(defun byte-compiled-function_639790797 (&optional args)
  (interactive "P")
  (let ((tmp diary-file))
    (setq diary-file group-diary-file)
    (byte-compiled-function_781836839 args)
    (if (not (or (string-match user-login-name (file-name-nondirectory buffer-file-name))
		 (string-match user-real-login-name (file-name-nondirectory buffer-file-name))))
	(save-excursion
	  (insert "  <" signature ">")))
;    (save-excursion
;      (insert "\n\t\t<Gruppenterminkalenderfile>"))
    (setq diary-file tmp)))
;;; den Gruppenkalenderfile ansehen (wie s normal)
(define-key calendar-mode-map "g" 'byte-compiled-function_750454365)
(defun byte-compiled-function_750454365 ()
  (interactive)
  (let ((tmp diary-file))
    (setq diary-file group-diary-file)
    (show-all-diary-entries)
    (setq diary-file tmp)))
;;; nur den Gruppenkalender, ohne den eigenen
(define-key calendar-mode-map "e" 'byte-compiled-function_653987184)
(defun byte-compiled-function_653987184 (&optional args) 
  (interactive "P")
  (let ((tmp diary-file))
    (setq diary-file group-diary-file)
    (view-diary-entries (if (not args) 1 args))
    (byte-compiled-function_157835516)
    (setq diary-file tmp)))
;;; nur den eigenen, nicht den Gruppenkalender
;;; (programmiertes Function-Overloading in LISP)
(define-key calendar-mode-map "r" 'byte-compiled-function_609367247)
(defun byte-compiled-function_609367247 (&optional args)
  (interactive "P")
  (fset 'dummy-func (symbol-function 'byte-compiled-function_957235300))
  (unwind-protect
      (progn
	(fset 'byte-compiled-function_957235300 'dummy)
	(view-diary-entries (if (not args) 1 args))
	(byte-compiled-function_157835516))
    (fset 'byte-compiled-function_957235300 (symbol-function 'dummy-func))))
(defun dummy (&optional args)
  ;;  '(lambda()(message "not including \"#include\" files")))
  (message "not including \"#include\" files"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Für andere Terminkalender


(define-key calendar-mode-map "iod" 'byte-compiled-function_460569856)
(defun byte-compiled-function_460569856 (other-diary-file &optional args) 
  (interactive "fTerminkalenderfile: \nPnonmarking")
  (let ((tmp diary-file))
    (setq diary-file other-diary-file)
    (insert-diary-entry args)
    (save-excursion
      (insert "  <" signature ">"))
    (save-excursion
      (insert (concat "\n\t\t<File: " (file-name-nondirectory buffer-file-name) ">")))
    (setq diary-file tmp)))
(define-key calendar-mode-map "iob" 'byte-compiled-function_310468551)
(defun byte-compiled-function_310468551 (other-diary-file &optional args) 
  (interactive "fTerminkalenderfile: \nPnonmarking")
  (let ((tmp diary-file))
    (setq diary-file other-diary-file)
    (insert-block-diary-entry args)
    (save-excursion
      (insert "  <" signature ">"))
    (save-excursion
      (insert (concat "\n\t\t<File: " (file-name-nondirectory buffer-file-name) ">")))
    (setq diary-file tmp)))
(define-key calendar-mode-map "ioc" 'byte-compiled-function_956687032)
(defun byte-compiled-function_956687032 (other-diary-file &optional args) 
  (interactive "fTerminkalenderfile: \nPnonmarking")
  (let ((tmp diary-file))
    (setq diary-file other-diary-file)
    (insert-cyclic-diary-entry args)
    (save-excursion
      (insert "  <" signature ">"))
    (save-excursion
      (insert (concat "\n\t\t<File: " (file-name-nondirectory buffer-file-name) ">")))
    (setq diary-file tmp)))
(define-key calendar-mode-map "ioa" 'byte-compiled-function_106595174)
(defun byte-compiled-function_106595174 (other-diary-file &optional args)
  (interactive "fTerminkalenderfile: \nPnonmarking")
  (let ((tmp diary-file))
    (setq diary-file other-diary-file)
    (insert-anniversary-diary-entry args)
    (save-excursion
      (insert "  <" signature ">"))
    (save-excursion
      (insert (concat "\n\t\t<File: " (file-name-nondirectory buffer-file-name) ">")))
    (setq diary-file tmp)))
(define-key calendar-mode-map "ioy" 'byte-compiled-function_597996015)
(defun byte-compiled-function_597996015 (other-diary-file &optional args)
  (interactive "fTerminkalenderfile: \nPnonmarking")
  (let ((tmp diary-file))
    (setq diary-file other-diary-file)
    (insert-yearly-diary-entry args)
    (save-excursion
      (insert "  <" signature ">"))
    (save-excursion
      (insert (concat "\n\t\t<File: " (file-name-nondirectory buffer-file-name) ">")))
    (setq diary-file tmp)))
(define-key calendar-mode-map "iom" 'byte-compiled-function_37778674)
(defun byte-compiled-function_37778674 (other-diary-file &optional args)
  (interactive "fTerminkalenderfile: \nPnonmarking")
  (let ((tmp diary-file))
    (setq diary-file other-diary-file)
    (insert-monthly-diary-entry args)
    (save-excursion
      (insert "  <" signature ">"))
    (save-excursion
      (insert (concat "\n\t\t<File: " (file-name-nondirectory buffer-file-name) ">")))
    (setq diary-file tmp)))
(define-key calendar-mode-map "iow" 'byte-compiled-function_638609973)
(defun byte-compiled-function_638609973 (other-diary-file &optional args)
  (interactive "fTerminkalenderfile: \nPnonmarking")
  (let ((tmp diary-file))
    (setq diary-file other-diary-file)
    (insert-weekly-diary-entry args)
    (save-excursion
      (insert "  <" signature ">"))
    (save-excursion
      (insert (concat "\n\t\t<File: " (file-name-nondirectory buffer-file-name) ">")))
    (setq diary-file tmp)))
(define-key calendar-mode-map "ior" 'byte-compiled-function_113553798)
(defun byte-compiled-function_113553798 (other-diary-file &optional args)
  (interactive "fTerminkalenderfile: \nPnonmarking")
  (let ((tmp diary-file))
    (setq diary-file other-diary-file)
    (byte-compiled-function_781836839 args)
    (save-excursion
      (insert "  <" signature ">"))
    (save-excursion
      (insert (concat "\n\t\t<File: " (file-name-nondirectory buffer-file-name) ">")))
    (setq diary-file tmp)))






;; neue Funktion im privaten Kalender

(define-key calendar-mode-map "ir" 'byte-compiled-function_781836839)



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
(define-key calendar-mode-map "u"   'byte-compiled-function_607126792)
(define-key calendar-mode-map [menu-bar Holidays unmark]
  '("Markierung löschen" . calendar-unmark-rehilit))
(define-key calendar-mode-map [menu-bar diary daymessage]
  '("Tagesorientierung" . calendar-day-message))
(define-key calendar-mode-map "P" 'byte-compiled-function_600325022)
(define-key calendar-mode-map [menu-bar diary ps-print-diary-entries]
  '("PS-Druck des anderen Fensters" . ps-print-diary-entries))
(defun byte-compiled-function_600325022 ()
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


(defun byte-compiled-function_607126792 ()
  (interactive)
  (calendar-unmark)
  (if window-system
      (progn
	(byte-compiled-function_503165602 t)
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



(defun byte-compiled-function_110904670 ()
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
;;;(defun byte-compiled-function_614374748 (&optional arg)
;;;  "Generate the diary window for ARG days starting with the current date.
;;; If no argument is provided, the number of days of diary entries is governed
;;; by the variable `number-of-diary-entries'.	This function is suitable for
;;;execution in a `.emacs' file."
;;;  (interactive "P")
    (if (or (not arg) (>= arg 0))
	(byte-compiled-function_614374748 arg)
      (let ((d-file (substitute-in-file-name diary-file))
	    (date (byte-compiled-function_110904670)) (arg (- arg)))
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


(defun byte-compiled-function_165569174 ()
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
	      (byte-compiled-function_12901847)
	      (error "Eintrag gefunden"))))
	(if (boundp 'personal-diary-file)
	    (progn
	      (find-file personal-diary-file)
	      (goto-char (point-max))
	      (if (not (search-backward tmp-substring nil t))
		  (progn
		    (message "Keinen IDENTISCHEN Eintrag in den durchsuchten Files gefunden: %s %s" diary-file personal-diary-file)
		    (switch-to-buffer current-buf))
		(byte-compiled-function_12901847)
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
		(byte-compiled-function_12901847)
		(error "Eintrag gefunden")))))))

(defun byte-compiled-function_12901847 ()
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



(defun byte-compiled-function_624212334 ()
  "Ruft change-diary-entry"
  (interactive)
  (byte-compiled-function_165569174))





;;;
;;; Funktion herauskopiert aus
;;; diary-lib.el    66330  Emacs-Lisp	/usr/share/emacs/20.3/lisp/calendar/diary-lib.el
;;;
(if (>= emacs-major-version 20)
(defun byte-compiled-function_827328247 (&optional ndays)
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
    (byte-compiled-function_709523722 ndays)
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
    (defun byte-compiled-function_957235300 ()
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
	      (list-diary-entries-hook 'byte-compiled-function_957235300)
	      (diary-display-hook 'ignore)
	      (diary-hook nil))
	  (if (file-exists-p diary-file)
	      (if (file-readable-p diary-file)
		  (unwind-protect
		      (progn
			(setq diary-entries-list
			      (append diary-entries-list
				      (byte-compiled-function_1051700708 original-date number))))
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
  (defun byte-compiled-function_957235300 ()
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
	    (list-diary-entries-hook 'byte-compiled-function_957235300)
	    (diary-display-hook 'ignore)
	    (diary-hook nil))
	(if (file-exists-p diary-file)
	    (if (file-readable-p diary-file)
		(unwind-protect
		    (setq diary-entries-list
			  (append diary-entries-list
				  (byte-compiled-function_1051700708 original-date number)))
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


(defun byte-compiled-function_250283601 (a)
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
    (defun byte-compiled-function_1051700708 (date number)
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
  (defun byte-compiled-function_1051700708 (date number)
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

(defun byte-compiled-function_316408522 (&optional arg)
  "Start calendar and diary in separate, dedicated frames."
  (interactive "p")
  (if arg (if (= arg 1) (setq arg nil)))
  (if (not window-system)
      (calendar-basic-setup arg)
    (if (< emacs-major-version 20)
	(standard-display-european t))
    (setq display-time-24hr-format t)
    (setq display-time-day-and-date t)
    (setq calendar-setup 'byte-compiled-function_316408522)
    (if (frame-live-p calendar-frame) (delete-frame calendar-frame))
    ;;(if (frame-live-p diary-frame) (delete-frame diary-frame))
    (add-hook 'diary-hook 'byte-compiled-function_501941784)
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
	(byte-compiled-function_914163068)
	(setq calendar-setup 'byte-compiled-function_316408522)
	(if mark-calendar-holidays-initially
	    (byte-compiled-function_398657241))
	(byte-compiled-function_157835516)
	(if mark-sundays (mark-calendar-days-named 7)) ;; alle Sonntage rot einfärben
	(define-key calendar-mode-map "d" 'byte-compiled-function_709523722)
	(define-key calendar-mode-map "D" 'byte-compiled-function_711801601)
        (run-hooks 'calendar-after-frame-setup-hooks)))))
(defun byte-compiled-function_501941784 ()
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


(defun byte-compiled-function_709523722 (&optional args)
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
	  (setq diary-hook (delq 'byte-compiled-function_501941784 diary-hook))
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
	    (byte-compiled-function_53242517 
	     "^Montag\\|^Dienstag\\|^Mittwoch\\|^Donnerstag\\|^Freitag\\|^Samstag\\|^Sonntag"
	     "^=+$" 'struct)
	    (byte-compiled-function_503165602 t)))
		  
      (if calendar-setup-from-calendar-buffer
	  (progn
	    (switch-to-buffer-other-frame fancy-diary-buffer)
	    (setq calendar-setup calendar-setup-from-calendar-buffer)
	    (calendar-set-mode-line "Eingetragene Termine")
	    (byte-compiled-function_773717038)
	    (local-set-key "\C-o" 'byte-compiled-function_658957709)
	    (if window-system
		(byte-compiled-function_503165602 t))
	    (switch-to-buffer-other-frame calendar-buffer))
	(switch-to-buffer calendar-buffer)
	(other-window 1)
	(switch-to-buffer fancy-diary-buffer)
	(calendar-set-mode-line "Eingetragene Termine")
	(byte-compiled-function_773717038)
	(setq calendar-setup calendar-setup-from-calendar-buffer)
	(local-set-key "\C-o" 'byte-compiled-function_658957709)
	(if window-system
	    (byte-compiled-function_503165602 t))
	(other-window -1)))
    (byte-compiled-function_157835516)))


(defun byte-compiled-function_711801601 (byte-compiled-function_790394554 &optional args)
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
	  (setq diary-hook (delq 'byte-compiled-function_501941784 diary-hook))
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
	    (byte-compiled-function_773717038)
	    (local-set-key "\C-o" 'byte-compiled-function_658957709)
	    (if window-system
		(byte-compiled-function_503165602 t))
	    (switch-to-buffer-other-frame calendar-buffer))
	(switch-to-buffer calendar-buffer)
	(other-window 1)
	(switch-to-buffer fancy-diary-buffer)
	(calendar-set-mode-line "Eingetragene Termine")
	(byte-compiled-function_773717038)
	(local-set-key "\C-o" 'byte-compiled-function_658957709)
	(if window-system
	    (byte-compiled-function_503165602 t))
	(other-window -1)))
    (byte-compiled-function_157835516)))

(defun byte-compiled-function_773717038 ()
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

(defun byte-compiled-function_716280519 ()
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
;)(byte-compiled-function_528263723 '((11 20 1960) "Totensonntag (DE) [Merktag]")
;)(byte-compiled-function_528263723 '((12 24 1960) "Heiligabend (Std) [Merktag]")
;)(byte-compiled-function_528263723 '((12 31 1960) "Silvester (Std) [Merktag]")
;)
;(holiday-fixed 7 27 "Siebenschläfer [Merktag]")
;(holiday-fixed 6 21 "Johannisnacht [+\"Fete de la musique\"] [Merktag]")
;))

(defun byte-compiled-function_830664381 (infile)
  "Macht aus einem gcal-Output-File einen emacs-Holiday-File,
  der dann durch ein eval bzw. load ausgeführt werden kann.
  der Ausgabefile ist unter \"op-project/op-calendar-holidaylist.el\"
  festŽim Code eingetragen."
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
	  (setq tag (string-to-number (byte-compiled-function_714459213)))
	  (forward-word 2)
	  (backward-char 1)
	  (setq monat (string-to-number (byte-compiled-function_714459213)))
	  (forward-word 2)
	  (backward-char 1)
	  (setq jahr (string-to-number (byte-compiled-function_714459213)))
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
	  (if (not (or (string= (byte-compiled-function_714459213) "(DE)")
		       (string= (byte-compiled-function_714459213) "(Std)")))
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


(defun byte-compiled-function_714459213 ()
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
(defun byte-compiled-function_957040606 (string &optional nonmarking file)
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
(defun byte-compiled-function_998775305 ()
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
			(byte-compiled-function_614374748)
		      (let ((diary-display-hook 'byte-compiled-function_582998651))
			(byte-compiled-function_614374748))))
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
(defun byte-compiled-function_208681160 (min-to-app new-time appt-msg)
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
;         (add-hook 'activate-menubar-hook 'byte-compiled-function_610089747 nil t)))
(defun byte-compiled-function_610089747 ()
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
(defun byte-compiled-function_888456825 ()
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
(defun byte-compiled-function_582998651 ()
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
(defun byte-compiled-function_442969003 ()
  "Toggles between truncated and not truncated lines"
  (interactive) 
  (if truncate-lines 
      (set-variable (quote truncate-lines) nil) 
    (set-variable (quote truncate-lines) t)) 
  (redraw-display))

(define-key ctl-x-map "t" 'truncate-lines-toggle)
(setq truncate-lines t)

(setq scroll-step 1)


;;;=============================================================================

;; braucht die Variable truncate-lines, sonst unabhängig
;;;;;;;includeforelc!
;;;;;;(load "op-isearch")

              ;
(if gnuemacsp ;
    nil       ;
;=============;



(defun byte-compiled-function_4306144 ()
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

(defun byte-compiled-function_281319172 ()
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

(defun byte-compiled-function_487744574 ()
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
      (byte-compiled-function_946158545)
      )
    (set-buffer-modified-p already-modified)))
;;
;; !!
;; 
(define-key isearch-mode-map "\C-j" 'byte-compiled-function_487744574)


;;
;; wenn Blöcke versteckt waren: diese alle wieder anzeigen
(defun byte-compiled-function_142055821 ()
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


(defun byte-compiled-function_843768328 (start end)
  (add-text-properties start end '(invisible t))
  (map-extents (lambda
		 (extent ignored)
		 (set-extent-property extent 'isearch-open-invisible t))
	       nil
	       start end
	       nil
	       'all-extents-closed 'invisible))

;;;;;;;;;


(defun byte-compiled-function_859724133 ()
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
(defun byte-compiled-function_769945463 ()
  (let (tmp)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\r" nil t)
	(forward-char 1)
;;	(byte-compiled-function_843768328 (point) (save-excursion
;;					    (re-search-forward "\n" nil t)
;;					    (point)))
	(byte-compiled-function_859724133)
;;	(goto-char (save-excursion
;;		     (re-search-forward "\n" nil t)
;;		     (point)))
	))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  Anfangsfunktionen
;;;
;;;
;;; wenn auch nur einmal: alles zum Anfang mit der Option "isearch-open-invisible" markieren
(defun byte-compiled-function_262790327 ()
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
(byte-compiled-function_262790327)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;(setq isearch-mode-hook nil)
(setq isearch-mode-hook 'byte-compiled-function_709449761)
(defun byte-compiled-function_709449761 ()
  (save-excursion
    (setq selective-display nil)
    (setq truncate-lines-tmp-save truncate-lines)
    (setq truncate-lines nil)))


;;;(setq isearch-mode-end-hook nil)
(setq isearch-mode-end-hook 'byte-compiled-function_759202025)
(defun byte-compiled-function_759202025 ()
  (save-excursion
    (setq truncate-lines truncate-lines-tmp-save)
    (setq selective-display t)
    ))



;=============;
       )      ; Ende des ifs am Anfang: op-isearch gilt in der aktuellen Version nur für den XEmacs
              ;



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
;;     (defun byte-compiled-function_595935949 (gl)
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
;;     (byte-compiled-function_595935949 (make-glyph [jpeg :file "/tmp/file1.jpg"]))
;;     ;; This will insert the glyph at point.
     
;;     ;; Here's an example of how to insert two glyphs side-by-side, at point
;;     ;; (using the above code):
;;     (progn
;;       (byte-compiled-function_595935949 (make-glyph [jpeg :file "/tmp/file1.jpg"]))
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


;;(defun byte-compiled-function_457273878 (string face)
;;  (let ((ext (make-extent 0 (length string) string)))
;;    (set-extent-property ext 'duplicable t)
;;    (set-extent-property ext 'unique t)
;;    (set-extent-property ext 'start-open t)
;;    (set-extent-property ext 'end-open t)
;;    (set-extent-face ext face))
;;  string)





(defun byte-compiled-function_595935949 (gl)
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
;; (byte-compiled-function_595935949 tmptmp)



(defun byte-compiled-function_739837062 (picture)
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
    (byte-compiled-function_595935949 tmpglyph)
    ;; (byte-compiled-function_595935949 (make-glyph [jpeg :file picture]))
    ;; (byte-compiled-function_595935949 (make-glyph [jpeg :file "c:\\tmp\\nettesWerbeMailBild.jpg"])
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
;;; War: (defun byte-compiled-function_335754943 () ...)
;;;
(defun byte-compiled-function_57453038 ()
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
  (setq major-mode 'byte-compiled-function_335754943)
  (run-hooks 'text-mode-hook 'indented-text-mode-hook)
  ) 
(load "text-mode")
(byte-compiled-function_57453038)


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

(defun byte-compiled-function_385524669 ()
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

  (if (not (fboundp 'byte-compiled-function_266316501))
      (defun byte-compiled-function_266316501 (dum) nil))  ; gefährlich, Effekt noch nicht ganz abgeschätzt
	   
  (if (not (byte-compiled-function_266316501 'dum))
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
			    (if (looking-at " ")
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
(defun byte-compiled-function_56118394 ()
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
	    (byte-compiled-function_385524669)
	    (end-of-line)
	    ;; wenn nein, dann wieder an den Zeilenanfang, sonst so lassen
	    (if (= p (point))
		(byte-compiled-function_385524669)))
	(byte-compiled-function_385524669))
    (byte-compiled-function_385524669)))

(define-key (current-local-map) "\C-i" 'byte-compiled-function_56118394)


(defun byte-compiled-function_511573002 ()
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
	  (byte-compiled-function_385524669)
	  (byte-compiled-function_385524669)))))

(make-local-variable 'auto-fill-function)
(setq auto-fill-function 'byte-compiled-function_511573002)
;;
;; Vor emacs 19.29:
;;(define-key (current-local-map) "\C-m" 'byte-compiled-function_511573002)



(defun byte-compiled-function_625820513 (&optional rb re)
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
	      (byte-compiled-function_385524669)
	      (next-line 1))
	  (end-of-line)
	  (byte-compiled-function_132890259)
	  (byte-compiled-function_56118394)
	  (next-line 1))))))

(defun byte-compiled-function_709718897 ()
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
	(byte-compiled-function_385524669)
	(next-line 1)))))


(defun byte-compiled-function_132890259 ()
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
	    (byte-compiled-function_56118394)
	    (re-search-forward "[\n]\\([ \t]*\\)" (save-excursion (next-line 1)(end-of-line)(- (point) 1)) t)
	    (replace-match "" nil nil)
	    (byte-compiled-function_56118394))
	(beginning-of-line)
	(insert-char ?  3)
	(insert-char ?\n 1)
	(previous-line 1)
	(byte-compiled-function_56118394)
	(re-search-forward "[\n]\\([ \t]*\\)" (save-excursion (next-line 1)(end-of-line)(- (point) 1)) t)
	(replace-match "" nil nil)
	(end-of-line)
	(byte-compiled-function_56118394)
	(end-of-line)))))
(define-key (current-local-map) "\M-;" 'byte-compiled-function_132890259)


(defun byte-compiled-function_222605910 ()
  (interactive)
  (let ((headline)(before-change-functions nil)(after-change-functions nil))
    (save-excursion
      (byte-compiled-function_469010529)
      (setq heading (buffer-substring (point) (re-search-forward "\\*+" nil t))))
    (beginning-of-line)
    (insert-string heading)
    (byte-compiled-function_56118394)))
(define-key (current-local-map) "\C-c\C-i" 'byte-compiled-function_222605910)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;
;;; schalte die auto-fill-function aus für das newline, da die beiden Funktionen sich überschneiden
(define-key (current-local-map) "\C-m" '(lambda()(interactive)(let((auto-fill-function nil))(newline))))

(if xemacsp
    (define-key (current-local-map) [(control return)] 'fixup-whitespace)
  (define-key (current-local-map) [C-return] 'fixup-whitespace))

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
;;	 (byte-compiled-function_110641169 'list2) ; use list2 by default
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
  (define-key hide-ifdef-mode-submap "d" 'byte-compiled-function_172854557)
  (define-key hide-ifdef-mode-submap "u" 'byte-compiled-function_395587460)
  (define-key hide-ifdef-mode-submap "D" 'byte-compiled-function_595506696)
  (define-key hide-ifdef-mode-submap "U" 'byte-compiled-function_110641169)

  (define-key hide-ifdef-mode-submap "h" 'byte-compiled-function_263376788)
  (define-key hide-ifdef-mode-submap "s" 'byte-compiled-function_63073901)
  (define-key hide-ifdef-mode-submap "\C-d" 'byte-compiled-function_597599103)
  (define-key hide-ifdef-mode-submap "\C-s" 'byte-compiled-function_732304087)

  (define-key hide-ifdef-mode-submap "\C-q" 'byte-compiled-function_121811600)
  ;;
  ;; SMW
  ;;
  (if gnuemacsp
      (let ((where (where-is-internal 'toggle-read-only '(keymap) t)))
	(if where
	    (define-key hide-ifdef-mode-submap
	      where
	      'byte-compiled-function_142635053))))
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
(or (assq 'byte-compiled-function_1034548444 minor-mode-map-alist)
    (setq minor-mode-map-alist
          (cons (cons 'byte-compiled-function_1034548444 hide-ifdef-mode-map)
                minor-mode-map-alist)))

(or (assq 'hide-ifdef-hiding minor-mode-alist)
    (setq minor-mode-alist
          (cons '(hide-ifdef-hiding " Hiding")
                minor-mode-alist)))

(or (assq 'byte-compiled-function_1034548444 minor-mode-alist)
    (setq minor-mode-alist
          (cons '(byte-compiled-function_1034548444 " Ifdef")
                minor-mode-alist)))

;; fix c-mode syntax table so we can recognize whole symbols.
(defvar hide-ifdef-syntax-table
  (copy-syntax-table c-mode-syntax-table)
  "Syntax table used for tokenizing #if expressions.")

(modify-syntax-entry ?_ "w" hide-ifdef-syntax-table)
(modify-syntax-entry ?& "." hide-ifdef-syntax-table)
(modify-syntax-entry ?\| "." hide-ifdef-syntax-table)

;;;###autoload
(defun byte-compiled-function_1034548444 (arg)
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
  (make-local-variable 'byte-compiled-function_1034548444)
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
	    (byte-compiled-function_263376788)
	  (byte-compiled-function_63073901))
	(message "Enter Hide-Ifdef mode")
	)
     ; else end hide-ifdef-mode
    (if hide-ifdef-hiding
	(byte-compiled-function_63073901))
    (message "Exit Hide-Ifdef mode")
    ))
  

;; from outline.el with docstring fixed.
(defun byte-compiled-function_484084225 (from to flag)
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

(defun byte-compiled-function_830686949 ()
  "Show all of the text in the current buffer."
  (interactive)
  (byte-compiled-function_484084225 (point-min) (point-max) ?\n))

;; By putting this on after-revert-hook, we arrange that it only
;; does anything when revert-buffer avoids turning off the mode.
;; (That can happen in VC.)
(defun byte-compiled-function_339218059 ()
  (and hide-ifdef-mode hide-ifdef-hiding
       (byte-compiled-function_263376788 t)))
(add-hook 'after-revert-hook 'byte-compiled-function_339218059)

(defun byte-compiled-function_851151019 (start end)
  "START is the start of a #if or #else form.  END is the ending part.
Everything including these lines is made invisible."
  (byte-compiled-function_484084225 start end ?\^M)
  )

(defun byte-compiled-function_235654128 (start end)
  "Everything between START and END is made visible."
  (byte-compiled-function_484084225 start end ?\n)
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


(defun byte-compiled-function_222297903 (var value)
  "Prepend (var value) pair to hide-ifdef-env."
  (setq hide-ifdef-env (cons (cons var value) hide-ifdef-env)))


(defun byte-compiled-function_804835684 (var)
;  (message "hif-lookup %s" var)
  (let ((val (assoc var hide-ifdef-env)))
    (if val
	(cdr val)
      hif-undefined-symbol)))

(defun byte-compiled-function_547907184 (var)
  (byte-compiled-function_804835684 var)
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


(defun byte-compiled-function_162367211 (token-list)
  "Convert list of tokens in infix into prefix list"
;  (message "hif-infix-to-prefix: %s" token-list)
  (if (= 1 (length token-list))
      (` (byte-compiled-function_804835684 (quote (, (car token-list)))))
    (byte-compiled-function_160497956 token-list))
  )

; pattern to match initial identifier, !, &&, ||, (, or ).
; Added ==, + and -: garyo@avs.com 8/9/94
(defconst hif-token-regexp "^\\(&&\\|||\\|[!=]=\\|!\\|[()+-]\\|[<>]=?\\|\\w+\\)")
(defconst hif-end-of-comment "\\*/")


(defun byte-compiled-function_131765142 (expr-string)
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
			((string-equal token "!=") 'byte-compiled-function_634948472)
			((string-equal token "!")  'not)
			((string-equal token "defined") 'byte-compiled-function_547907184)
			((string-equal token "(") 'lparen)
			((string-equal token ")") 'rparen)
			((string-equal token ">") 'byte-compiled-function_963554099)
			((string-equal token "<") 'byte-compiled-function_659093382)
			((string-equal token ">=") 'byte-compiled-function_225742762)
			((string-equal token "<=") 'byte-compiled-function_332334153)
			((string-equal token "+") 'byte-compiled-function_168145642)
			((string-equal token "-") 'byte-compiled-function_308551533)
			(t (intern token)))
		       token-list))))
	     (t (error "Bad #if expression: %s" expr-string)))))
      (set-syntax-table current-syntax-table))
    (nreverse token-list)))

;;;-----------------------------------------------------------------
;;; Translate C preprocessor #if expressions using recursive descent.
;;; This parser is limited to the operators &&, ||, !, and "defined".
;;; Added ==, !=, +, and -.  Gary Oberbrunner, garyo@avs.com, 8/9/94

(defun byte-compiled-function_160497956 (token-list)
  "Parse the TOKEN-LIST.  Return translated list in prefix form."
  (byte-compiled-function_55445360)
  (prog1
      (byte-compiled-function_152618853)
    (if token ; is there still a token?
	(error "Error: unexpected token: %s" token))))

(defun byte-compiled-function_55445360 ()
  "Pop the next token from token-list into the let variable \"token\"."
  (setq token (car token-list))
  (setq token-list (cdr token-list))
  token)

(defun byte-compiled-function_152618853 ()
  "Parse an expression as found in #if.
       expr : term | expr '||' term."
  (let ((result (byte-compiled-function_937985312)))
    (while (eq  token 'or)
      (byte-compiled-function_55445360)
      (setq result (list 'or result (byte-compiled-function_937985312))))
  result))

(defun byte-compiled-function_937985312 ()
  "Parse a term : eq-expr | term '&&' eq-expr."
  (let ((result (byte-compiled-function_313933837)))
    (while (eq token 'and)
      (byte-compiled-function_55445360)
      (setq result (list 'and result (byte-compiled-function_313933837))))
    result))

(defun byte-compiled-function_313933837 ()
  "Parse an eq-expr : math | eq-expr `=='|`!='|`<'|`>'|`>='|`<=' math."
  (let ((result (byte-compiled-function_881197333))
	(eq-token nil))
    (while (memq token '(equal hif-notequal hif-greater hif-less
			       hif-greater-equal hif-less-equal))
      (setq eq-token token)
      (byte-compiled-function_55445360)
      (setq result (list eq-token result (byte-compiled-function_881197333))))
    result))

(defun byte-compiled-function_881197333 ()
  "Parse an expression with + or - and simpler things.
       math : factor | math '+|-' factor."
  (let ((result (byte-compiled-function_594392854))
	(math-op nil))
    (while (or (eq  token 'byte-compiled-function_168145642) (eq token 'byte-compiled-function_308551533))
      (setq math-op token)
      (byte-compiled-function_55445360)
      (setq result (list math-op result (byte-compiled-function_594392854))))
  result))
  
(defun byte-compiled-function_594392854 ()
  "Parse a factor: '!' factor | '(' expr ')' | 'defined(' id ')' | id."
  (cond
    ((eq token 'not)
     (byte-compiled-function_55445360)
     (list 'not (byte-compiled-function_594392854)))

    ((eq token 'lparen)
     (byte-compiled-function_55445360)
     (let ((result (byte-compiled-function_152618853)))
       (if (not (eq token 'rparen))
	   (error "Bad token in parenthesized expression: %s" token)
	 (byte-compiled-function_55445360)
	 result)))

    ((eq token 'byte-compiled-function_547907184)
     (byte-compiled-function_55445360)
     (if (not (eq token 'lparen))
	 (error "Error: expected \"(\" after \"defined\""))
     (byte-compiled-function_55445360)
     (let ((ident token))
       (if (memq token '(or and not hif-defined lparen rparen))
	   (error "Error: unexpected token: %s" token))
       (byte-compiled-function_55445360)
       (if (not (eq token 'rparen))
	   (error "Error: expected \")\" after identifier"))
       (byte-compiled-function_55445360)
       (` (byte-compiled-function_547907184 (quote (, ident))))
       ))

    (t ; identifier
      (let ((ident token))
	(if (memq ident '(or and))
	    (error "Error: missing identifier"))
	(byte-compiled-function_55445360)
	(` (byte-compiled-function_804835684 (quote (, ident))))
	))
    ))

(defun byte-compiled-function_269974586 (val)
  "Treat VAL as a number: if it's t or nil, use 1 or 0."
  (cond ((eq val t)
	 1)
	((null val)
	 0)
	(t val)))

(defun byte-compiled-function_168145642 (a b)
  "Like ordinary plus but treat t and nil as 1 and 0."
  (+ (byte-compiled-function_269974586 a) (byte-compiled-function_269974586 b)))
(defun byte-compiled-function_308551533 (a b)
  "Like ordinary minus but treat t and nil as 1 and 0."
  (- (byte-compiled-function_269974586 a) (byte-compiled-function_269974586 b)))
(defun byte-compiled-function_634948472 (a b)
  "Like (not (equal A B)) but as one symbol."
  (not (equal a b)))
(defun byte-compiled-function_963554099 (a b)
  "Simple comparison."
  (> (byte-compiled-function_269974586 a) (byte-compiled-function_269974586 b)))
(defun byte-compiled-function_659093382 (a b)
  "Simple comparison."
  (< (byte-compiled-function_269974586 a) (byte-compiled-function_269974586 b)))
(defun byte-compiled-function_225742762 (a b)
  "Simple comparison."
  (>= (byte-compiled-function_269974586 a) (byte-compiled-function_269974586 b)))
(defun byte-compiled-function_332334153 (a b)
  "Simple comparison."
  (<= (byte-compiled-function_269974586 a) (byte-compiled-function_269974586 b)))
;;;----------- end of parser -----------------------


(defun byte-compiled-function_361808063 ()
  "When at beginning of #ifX, returns a Lisp expression for its condition."
  (save-excursion
    (let ((negate (looking-at hif-ifndef-regexp)))
      (re-search-forward hif-ifx-regexp)
      (let* ((expr-string
	      (buffer-substring (point)
				(progn (skip-chars-forward "^\n\r") (point))))
	     (expr (byte-compiled-function_162367211 (byte-compiled-function_131765142 expr-string))))
;	(message "hif-canonicalized: %s" expr)
	(if negate
	    (list 'not expr)
	  expr)))))


(defun byte-compiled-function_800172851 ()
  "Move to next #if..., or #ifndef, at point or after."
;  (message "find ifX at %d" (point))
  (prog1
      (re-search-forward hif-ifx-regexp (point-max) t)
    (beginning-of-line)))


(defun byte-compiled-function_520002208 ()
  "Move to next #if..., #else, or #endif, after the current line."
;  (message "hif-find-next-relevant at %d" (point))
  (end-of-line)
  ; avoid infinite recursion by only going to beginning of line if match found
  (if (re-search-forward hif-ifx-else-endif-regexp (point-max) t)
      (beginning-of-line)))

(defun byte-compiled-function_256965132 ()
  "Move to previous #if..., #else, or #endif, before the current line."
;  (message "hif-find-previous-relevant at %d" (point))
  (beginning-of-line)
  ; avoid infinite recursion by only going to beginning of line if match found
  (if (re-search-backward hif-ifx-else-endif-regexp (point-min) t)
     (beginning-of-line)))


(defun byte-compiled-function_687499893 ()		;; Should eventually see #if
  (looking-at hif-ifx-regexp))
(defun byte-compiled-function_840761668 ()
  (looking-at hif-endif-regexp))
(defun byte-compiled-function_658730827 ()
  (looking-at hif-else-regexp))



(defun byte-compiled-function_690420872 ()
  "If positioned at #ifX or #else form, skip to corresponding #endif."
;  (message "hif-ifdef-to-endif at %d" (point)) (sit-for 1)
  (byte-compiled-function_520002208)
  (cond ((byte-compiled-function_687499893)
	 (byte-compiled-function_690420872) ; find endif of nested if
	 (byte-compiled-function_690420872)) ; find outer endif or else
	((byte-compiled-function_658730827)
	 (byte-compiled-function_690420872)) ; find endif following else
	((byte-compiled-function_840761668)
	 'done)
	(t
	 (error "Mismatched #ifdef #endif pair"))))


(defun byte-compiled-function_759195472 ()
  "If positioned at #endif form, skip backward to corresponding #ifX."
;  (message "hif-endif-to-ifdef at %d" (point))
  (let ((start (point)))
    (byte-compiled-function_256965132)
    (if (= start (point))
	(error "Mismatched #ifdef #endif pair")))
  (cond ((byte-compiled-function_840761668)
	 (byte-compiled-function_759195472) ; find beginning of nested if
	 (byte-compiled-function_759195472)) ; find beginning of outer if or else
	((byte-compiled-function_658730827)
	 (byte-compiled-function_759195472))
	((byte-compiled-function_687499893)
	 'done)
	(t)))			; never gets here


(defun byte-compiled-function_726788499 (&optional arg)
  "Move point to beginning of line of the next ifdef-endif.
With argument, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (if (< arg 0)
      (backward-ifdef (- arg)))
  (while (< 0 arg)
    (setq arg (- arg))
    (let ((start (point)))
      (if (not (byte-compiled-function_687499893))
	  (byte-compiled-function_520002208))
      (if (byte-compiled-function_687499893)
	  (byte-compiled-function_690420872)
	(goto-char start)
	(error "No following #ifdef")
	))))


(defun backward-ifdef (&optional arg)
  "Move point to beginning of the previous ifdef-endif.
With argument, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (if (< arg 0)
      (byte-compiled-function_726788499 (- arg)))
  (while (< 0 arg)
    (setq arg (1- arg))
    (beginning-of-line)
    (let ((start (point)))
      (if (not (byte-compiled-function_840761668))
	  (byte-compiled-function_256965132))
      (if (byte-compiled-function_840761668)
	  (byte-compiled-function_759195472)
	(goto-char start)
	(error "No previous #ifdef")))))


(defun byte-compiled-function_868121926 ()
  "Move point to beginning of nested ifdef or else-part."
    (interactive)
    (let ((start (point)))
      (byte-compiled-function_520002208)
      (if (or (byte-compiled-function_687499893) (byte-compiled-function_658730827))
	  ()
	(goto-char start)
	(error "No following #ifdef"))))


(defun byte-compiled-function_421855559 ()
  "Move point to beginning of enclosing ifdef or else-part."
  (interactive)
  (beginning-of-line)
  (let ((start (point)))
    (if (not (byte-compiled-function_840761668))
	(byte-compiled-function_256965132))
    (if (byte-compiled-function_840761668)
	(byte-compiled-function_759195472))
      (if (= start (point))
	  (error "No previous #ifdef"))))

(defun byte-compiled-function_190032434 (&optional arg)
  "Move to the beginning of the next #ifX, #else, or #endif.
With argument, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (if (< arg 0)
      (byte-compiled-function_306169596 (- arg)))
  (while (< 0 arg)
    (setq arg (1- arg))
    (byte-compiled-function_520002208)
    (if (eolp)
	(progn
	  (beginning-of-line)
	  (error "No following #ifdefs, #elses, or #endifs")))))

(defun byte-compiled-function_306169596 (&optional arg)
  "Move to the beginning of the previous #ifX, #else, or #endif.
With argument, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (if (< arg 0)
      (byte-compiled-function_190032434 (- arg)))
  (while (< 0 arg)
    (setq arg (1- arg))
    (let ((start (point)))
      (byte-compiled-function_256965132)
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

(defun byte-compiled-function_761519956 (else-p start end &optional else)
  (list else-p start else end))

(defun byte-compiled-function_510206488 (range)  (elt range 0))
(defun byte-compiled-function_1035865180 (range) (elt range 1))
(defun byte-compiled-function_205665759 (range) (elt range 2))
(defun byte-compiled-function_786617819 (range) (elt range 3))



;;; Find-Range
;;; The workhorse, it delimits the #if region.  Reasonably simple:
;;; Skip until an #else or #endif is found, remembering positions.  If
;;; an #else was found, skip some more, looking for the true #endif.

(defun byte-compiled-function_127560589 ()
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
      (byte-compiled-function_520002208)
      (while (byte-compiled-function_687499893)		; Skip nested ifdef
	(byte-compiled-function_690420872)
	(byte-compiled-function_520002208))
      ;; Found either a #else or an #endif.
      (cond ((byte-compiled-function_658730827)
	     (setq else-p t)
	     (setq else (point)))
	    (t
	     (setq end (point)) ; (save-excursion (end-of-line) (point))
	     ))
      ;; If found #else, look for #endif.
      (if else-p
	  (progn
	    (byte-compiled-function_520002208)
	    (while (byte-compiled-function_687499893)	; Skip nested ifdef
	      (byte-compiled-function_690420872)
	      (byte-compiled-function_520002208))
	    (if (byte-compiled-function_658730827)
		(error "Found two elses in a row?  Broken!"))
	    (setq end (point))  ; (save-excursion (end-of-line) (point))
	    ))
      (byte-compiled-function_761519956 else-p start end else))))

	  
;;; A bit slimy.
;;; NOTE:  If there's an #ifdef at the beginning of the file, we can't
;;; hide it.  There's no previous newline to replace.  If we added
;;; one, we'd throw off all the counts.  Feh.

(defun byte-compiled-function_451121144 (point)
  "Hide the line containing point.  Does nothing if `hide-ifdef-lines' is nil."
  (if hide-ifdef-lines
      (save-excursion
	(goto-char point)
	(let ((modp (buffer-modified-p)))
	  (unwind-protect
	      (progn
		(beginning-of-line)
		(if (not (= (point) 1))
		    (byte-compiled-function_851151019 (1- (point)) (point))))
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

(defun byte-compiled-function_157115042 (start end)
  "Call `hide-ifdef-guts' after narrowing to end of START line and END line."
  (save-excursion
    (save-restriction
      (goto-char start)
      (end-of-line)
      (narrow-to-region (point) end)
      (byte-compiled-function_589970302))))

(defun byte-compiled-function_48583012 ()
  "Called at #ifX expression, this hides those parts that should be hidden.
It uses the judgement of `hide-ifdef-evaluator'."
;  (message "hif-possibly-hide") (sit-for 1)
    (let ((test (byte-compiled-function_361808063))
	  (range (byte-compiled-function_127560589)))
;      (message "test = %s" test) (sit-for 1)
      
      (byte-compiled-function_451121144 (byte-compiled-function_786617819 range))
      (if (funcall hide-ifdef-evaluator test)
	  (cond ((byte-compiled-function_510206488 range) ; case 1
		 (byte-compiled-function_451121144 (byte-compiled-function_205665759 range))
		 (byte-compiled-function_851151019 (byte-compiled-function_205665759 range) 
				    (1- (byte-compiled-function_786617819 range)))
		 (byte-compiled-function_157115042 (byte-compiled-function_1035865180 range)
				 (byte-compiled-function_205665759 range)))
		(t ; case 2
		 (byte-compiled-function_157115042 (byte-compiled-function_1035865180 range)
				 (byte-compiled-function_786617819 range))))
	(cond ((byte-compiled-function_510206488 range) ; case 3
	       (byte-compiled-function_451121144 (byte-compiled-function_205665759 range))
	       (byte-compiled-function_851151019 (byte-compiled-function_1035865180 range)
				  (1- (byte-compiled-function_205665759 range)))
	       (byte-compiled-function_157115042 (byte-compiled-function_205665759 range)
			       (byte-compiled-function_786617819 range)))
	      (t ; case 4
	       (byte-compiled-function_851151019 (point)
				  (1- (byte-compiled-function_786617819 range))))
	      ))
      (byte-compiled-function_451121144 (byte-compiled-function_1035865180 range))	; Always hide start.
      (goto-char (byte-compiled-function_786617819 range))
      (end-of-line)
      ))



(defun byte-compiled-function_589970302 ()
  "Does most of the work of `hide-ifdefs'.
It does not do the work that's pointless to redo on a recursive entry."
;  (message "hide-ifdef-guts")
  (save-excursion
    (goto-char (point-min))
    (while (byte-compiled-function_800172851)
      (byte-compiled-function_48583012))))

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

(defun byte-compiled-function_121811600 ()
  "Toggle hide-ifdef-read-only."
  (interactive)
  (setq hide-ifdef-read-only (not hide-ifdef-read-only))
  (message "Hide-Read-Only %s"
	   (if hide-ifdef-read-only "ON" "OFF"))
  (if hide-ifdef-hiding
      (setq buffer-read-only (or hide-ifdef-read-only hif-outside-read-only)))
  (force-mode-line-update))

(defun byte-compiled-function_142635053 ()
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

      
(defun byte-compiled-function_172854557 (var)
  "Define a VAR so that #ifdef VAR would be included."
  (interactive "SDefine what? ")
  (byte-compiled-function_222297903 var 1)
  (if hide-ifdef-hiding (byte-compiled-function_263376788)))

(defun byte-compiled-function_395587460 (var)
  "Undefine a VAR so that #ifdef VAR would not be included."
  (interactive "SUndefine what? ")
  (byte-compiled-function_222297903 var nil)
  (if hide-ifdef-hiding (byte-compiled-function_263376788)))


(defun byte-compiled-function_263376788 (&optional nomsg)
  "Hide the contents of some #ifdefs.  
Assume that defined symbols have been added to `hide-ifdef-env'.  
The text hidden is the text that would not be included by the C
preprocessor if it were given the file with those symbols defined.

Turn off hiding by calling `show-ifdefs'."

  (interactive)
  (message "Hiding...")
  (setq hif-outside-read-only buffer-read-only)
  (if (not hide-ifdef-mode)
      (byte-compiled-function_1034548444 1)) ; turn on hide-ifdef-mode
  (if hide-ifdef-hiding
      (byte-compiled-function_63073901))			; Otherwise, deep confusion.
  (let ((inhibit-read-only t))
    (setq selective-display t)
    (setq hide-ifdef-hiding t)
    (byte-compiled-function_589970302))
  (setq buffer-read-only (or hide-ifdef-read-only hif-outside-read-only))
  (or nomsg
      (message "Hiding done")))


(defun byte-compiled-function_63073901 ()
  "Cancel the effects of `hide-ifdef': show the contents of all #ifdefs."
  (interactive)
  (setq buffer-read-only hif-outside-read-only)
  (setq selective-display nil)	; defaults
  (let ((inhibit-read-only t))
    (byte-compiled-function_830686949))
  (setq hide-ifdef-hiding nil))


(defun byte-compiled-function_175716695 ()
  "Utility for hide and show `ifdef-block'.
Set top and bottom of ifdef block."
  (let (max-bottom)
  (save-excursion
    (beginning-of-line)
    (if (not (or (byte-compiled-function_658730827) (byte-compiled-function_687499893)))
	(byte-compiled-function_421855559))
    (setq top (point))
    (byte-compiled-function_690420872)
    (setq max-bottom (1- (point))))
  (save-excursion
    (beginning-of-line)
    (if (not (byte-compiled-function_840761668))
	(byte-compiled-function_520002208))
    (while (byte-compiled-function_687499893)
      (byte-compiled-function_690420872)
      (byte-compiled-function_520002208))
    (setq bottom (min max-bottom (1- (point))))))
  )


(defun byte-compiled-function_597599103 ()
  "Hide the ifdef block (true or false part) enclosing or before the cursor."
  (interactive)
  (if (not hide-ifdef-mode)
      (byte-compiled-function_1034548444 1))
  (setq selective-display t)
  (let (top bottom (inhibit-read-only t))
    (byte-compiled-function_175716695) ; set top and bottom - dynamic scoping
    (byte-compiled-function_851151019 top bottom)
    (if hide-ifdef-lines
	(progn
	  (byte-compiled-function_451121144 top)
	  (byte-compiled-function_451121144 (1+ bottom))))
    (setq hide-ifdef-hiding t))
  (setq buffer-read-only (or hide-ifdef-read-only hif-outside-read-only)))


(defun byte-compiled-function_732304087 ()
  "Show the ifdef block (true or false part) enclosing or before the cursor."
  (interactive)
  (let ((inhibit-read-only t))
    (if hide-ifdef-lines
	(save-excursion
	  (beginning-of-line)
	  (byte-compiled-function_235654128 (1- (point)) (progn (end-of-line) (point))))

      (let (top bottom)
	(byte-compiled-function_175716695)
	(byte-compiled-function_235654128 (1- top) bottom)))))


;;;  definition alist support

(defvar hide-ifdef-define-alist nil
  "A global assoc list of pre-defined symbol lists")

(defun byte-compiled-function_78078162 (env)
  "Compress the define list ENV into a list of defined symbols only."
  (let ((defs (mapcar '(lambda (arg)
			 (if (byte-compiled-function_804835684 (car arg)) (car arg)))
		      env))
	(new-defs nil))
    (while defs
      (if (car defs)
	  (setq new-defs (cons (car defs) new-defs)))
      (setq defs (cdr defs)))
    new-defs))

(defun byte-compiled-function_595506696 (name)
  "Set the association for NAME to `hide-ifdef-env'."
  (interactive "SSet define list: ")
  (setq hide-ifdef-define-alist
	(cons (cons name (byte-compiled-function_78078162 hide-ifdef-env))
	      hide-ifdef-define-alist)))

(defun byte-compiled-function_110641169 (name)
  "Set `hide-ifdef-env' to the define list specified by NAME."
  (interactive "SUse define list: ")
  (let ((define-list (assoc name hide-ifdef-define-alist)))
    (if define-list
	(setq hide-ifdef-env
	      (mapcar '(lambda (arg) (cons arg t))
		      (cdr define-list)))
      (error "No define list for %s" name))
    (if hide-ifdef-hiding (byte-compiled-function_263376788))))

(provide 'hideif)

;;; hideif.el ends here



(byte-compiled-function_1034548444 t)
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
  (define-key outline-mode-prefix-map "\C-n" 'byte-compiled-function_760238784)
  (define-key outline-mode-prefix-map "\C-p" 'byte-compiled-function_951328898)
  (define-key outline-mode-prefix-map "\C-i" 'byte-compiled-function_619663136)
  (define-key outline-mode-prefix-map "\C-s" 'byte-compiled-function_182906083)
  (define-key outline-mode-prefix-map "\C-d" 'byte-compiled-function_38737195)
  (define-key outline-mode-prefix-map "\C-u" 'byte-compiled-function_402232290)
  (define-key outline-mode-prefix-map "\C-f" 'byte-compiled-function_103337988)
  (define-key outline-mode-prefix-map "\C-b" 'byte-compiled-function_129512964)
  (define-key outline-mode-prefix-map "\C-t" 'byte-compiled-function_675606449)
  (define-key outline-mode-prefix-map "\C-a" 'byte-compiled-function_720330687)
  (define-key outline-mode-prefix-map "\C-c" 'byte-compiled-function_663981991)
  (define-key outline-mode-prefix-map "\C-e" 'byte-compiled-function_613274775)
  (define-key outline-mode-prefix-map "\C-l" 'byte-compiled-function_687492214)
  (define-key outline-mode-prefix-map "\C-k" 'byte-compiled-function_644551147)
  (define-key outline-mode-prefix-map "\C-q" 'byte-compiled-function_983432855)
  (define-key outline-mode-prefix-map "\C-o" 'byte-compiled-function_753010473))

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
(defun byte-compiled-function_661224210 ()
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
  (setq major-mode 'byte-compiled-function_661224210)
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
  (add-hook 'change-major-mode-hook 'byte-compiled-function_720330687)
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
      (byte-compiled-function_683255193 (point-min) (point-max) ?\n))
  (force-mode-line-update))

(defvar outline-level 'outline-level
  "Function of no args to compute a header's nesting level in an outline.
It can assume point is at the beginning of a header line.")

;; This used to count columns rather than characters, but that made ^L
;; appear to be at level 2 instead of 1.  Columns would be better for
;; tab handling, but the default regexp doesn't use tabs, and anyone
;; who changes the regexp can also redefine the outline-level variable
;; as appropriate.
(defun byte-compiled-function_779083082 ()
  "Return the depth to which a statement is nested in the outline.
Point must be at the beginning of a header line.  This is actually
the number of characters that `outline-regexp' matches."
  (save-excursion
    (looking-at outline-regexp)
    (- (match-end 0) (match-beginning 0))))

(defun byte-compiled-function_278599968 ()
  "Skip forward to just before the next heading line.
If there's no following heading line, stop before the newline
at the end of the buffer."
  (if (re-search-forward (concat "[\n\^M]\\(" outline-regexp "\\)")
			 nil 'move)
      (goto-char (match-beginning 0)))
  (if (memq (preceding-char) '(?\n ?\^M))
      (forward-char -1)))

(defun byte-compiled-function_912387168 ()
  "Move to the next (possibly invisible) heading line."
  (interactive)
  (if (re-search-forward (concat "[\n\^M]\\(" outline-regexp "\\)")
			 nil 'move)
      (goto-char (1+ (match-beginning 0)))))

(defun byte-compiled-function_469010529 ()
  "Move to previous heading line, or beg of this line if it's a heading.
Only visible heading lines are considered."
  (beginning-of-line)
  (or (byte-compiled-function_60251609)
      (re-search-backward (concat "^\\(" outline-regexp "\\)") nil t)
      (error "before first heading")))

(defun byte-compiled-function_60251609 ()
  "Return t if point is on a (visible) heading line."
  (save-excursion
    (beginning-of-line)
    (and (bolp)
	 (looking-at outline-regexp))))

(defun byte-compiled-function_449469528 ()
  (if (re-search-forward outline-heading-end-regexp nil 'move)
      (forward-char -1)))

(defun byte-compiled-function_760238784 (arg)
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

(defun byte-compiled-function_951328898 (arg)
  "Move to the previous heading line.
With argument, repeats or can move forward if negative.
A heading line is one that starts with a `*' (or that
`outline-regexp' matches)."
  (interactive "p")
  (byte-compiled-function_760238784 (- arg)))

(defun byte-compiled-function_683255193 (from to flag)
  "Hides or shows lines from FROM to TO, according to FLAG.
If FLAG is `\\n' (newline character) then text is shown,
while if FLAG is `\\^M' (control-M) the text is hidden."
  (let (buffer-read-only)
    (subst-char-in-region from to
			  (if (= flag ?\n) ?\^M ?\n)
			  flag t)))

(defun byte-compiled-function_663981991 ()
  "Hide the body directly following this heading."
  (interactive)
  (byte-compiled-function_469010529)
  (byte-compiled-function_449469528)
  (save-excursion
   (byte-compiled-function_683255193 (point) (progn (byte-compiled-function_278599968) (point)) ?\^M)))

(defun byte-compiled-function_613274775 ()
  "Show the body directly following this heading."
  (interactive)
  (save-excursion
   (byte-compiled-function_683255193 (point) (progn (byte-compiled-function_278599968) (point)) ?\n)))

(defun byte-compiled-function_675606449 ()
  "Hide all of buffer except headings."
  (interactive)
  (byte-compiled-function_660003855 (point-min) (point-max)))

(defun byte-compiled-function_660003855 (start end)
  "Hide all body lines in the region, but not headings."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (if (byte-compiled-function_60251609)
	  (byte-compiled-function_449469528))
      (while (not (eobp))
	(byte-compiled-function_683255193 (point)
			     (progn (byte-compiled-function_278599968) (point)) ?\^M)
	(if (not (eobp))
	    (progn
	      (forward-char
	       (if (looking-at "[\n\^M][\n\^M]")
		   2 1))
	      (byte-compiled-function_449469528)))))))

(defun byte-compiled-function_720330687 ()
  "Show all of the text in the buffer."
  (interactive)
  (byte-compiled-function_683255193 (point-min) (point-max) ?\n))

(defun byte-compiled-function_38737195 ()
  "Hide everything after this heading at deeper levels."
  (interactive)
  (byte-compiled-function_169080791 ?\^M))

(defun byte-compiled-function_687492214 ()
  "Hide all body after this heading at deeper levels."
  (interactive)
  (byte-compiled-function_469010529)
  (byte-compiled-function_449469528)
  (byte-compiled-function_660003855 (point) (progn (byte-compiled-function_119382628) (point))))

(defun byte-compiled-function_182906083 ()
  "Show everything after this heading at deeper levels."
  (interactive)
  (byte-compiled-function_169080791 ?\n))

(defun byte-compiled-function_983432855 (levels)
  "Hide everything but the top LEVELS levels of headers, in whole buffer."
  (interactive "p")
  (if (< levels 1)
      (error "Must keep at least one level of headers"))
  (setq levels (1- levels))
  (save-excursion
    (goto-char (point-min))
    ;; Keep advancing to the next top-level heading.
    (while (or (and (bobp) (byte-compiled-function_60251609))
	       (byte-compiled-function_912387168))
      (let ((end (save-excursion (byte-compiled-function_119382628) (point))))
	;; Hide everything under that.
	(byte-compiled-function_683255193 (point) end ?\^M)
	;; Show the first LEVELS levels under that.
	(if (> levels 0)
	    (byte-compiled-function_619663136 levels))
	;; Move to the next, since we already found it.
	(goto-char end)))))

(defun byte-compiled-function_753010473 ()
  "Hide everything except for the current body and the parent headings."
  (interactive)
  (byte-compiled-function_983432855 1)
  (let ((last (point))
	(pos (point)))
    (while (save-excursion
	     (and (re-search-backward "[\n\r]" nil t)
		  (eq (following-char) ?\r)))
      (save-excursion
	(beginning-of-line)
	(if (eq last (point))
	    (progn
	      (byte-compiled-function_912387168)
	      (byte-compiled-function_683255193 last (point) ?\n))
	  (byte-compiled-function_619663136)
	  (setq last (point)))))))

(defun byte-compiled-function_169080791 (flag)
  (save-excursion
    (byte-compiled-function_469010529)
    (byte-compiled-function_449469528)
    (byte-compiled-function_683255193 (point)
			  (progn (byte-compiled-function_119382628) (point))
			  flag)))

(defun byte-compiled-function_119382628 ()
  (byte-compiled-function_469010529)
  (let ((opoint (point))
	(first t)
	(level (funcall outline-level)))
    (while (and (not (eobp))
		(or first (> (funcall outline-level) level)))
      (setq first nil)
      (byte-compiled-function_912387168))
    (if (memq (preceding-char) '(?\n ?\^M))
	(progn
	  ;; Go to end of line before heading
	  (forward-char -1)
	  (if (memq (preceding-char) '(?\n ?\^M))
	      ;; leave blank line before heading
	      (forward-char -1))))))

(defun byte-compiled-function_644551147 ()
  "Show all subheadings of this heading, but not their bodies."
  (interactive)
  (byte-compiled-function_619663136 1000))

(defun byte-compiled-function_619663136 (&optional level)
  "Show all direct subheadings of this heading.
Prefix arg LEVEL is how many levels below the current level should be shown.
Default is enough to cause the following heading to appear."
  (interactive "P")
  (setq level
	(if level (prefix-numeric-value level)
	  (save-excursion
	    (byte-compiled-function_469010529)
	    (let ((start-level (funcall outline-level)))
	      (byte-compiled-function_912387168)
	      (if (eobp)
		  1
		(max 1 (- (funcall outline-level) start-level)))))))
  (save-excursion
    (save-restriction
      (byte-compiled-function_469010529)
      (setq level (+ level (funcall outline-level)))
      (narrow-to-region (point)
			(progn (byte-compiled-function_119382628)
			       (if (eobp) (point-max) (1+ (point)))))
      (goto-char (point-min))
      (while (and (not (eobp))
		  (progn
		    (byte-compiled-function_912387168)
		    (not (eobp))))
	(if (<= (funcall outline-level) level)
	    (save-excursion
	      (byte-compiled-function_683255193 (save-excursion
				     (forward-char -1)
				     (if (memq (preceding-char) '(?\n ?\^M))
					 (forward-char -1))
				     (point))
				   (progn (byte-compiled-function_449469528) (point))
				   ?\n)))))))

(defun byte-compiled-function_402232290 (arg)
  "Move to the heading line of which the present line is a subheading.
With argument, move up ARG levels."
  (interactive "p")
  (byte-compiled-function_469010529)
  (if (eq (funcall outline-level) 1)
      (error ""))
  (while (and (> (funcall outline-level) 1)
	      (> arg 0)
	      (not (bobp)))
    (let ((present-level (funcall outline-level)))
      (while (not (< (funcall outline-level) present-level))
	(byte-compiled-function_951328898 1))
      (setq arg (- arg 1)))))

(defun byte-compiled-function_103337988 (arg)
  "Move forward to the ARG'th subheading at same level as this one.
Stop at the first and last subheadings of a superior heading."
  (interactive "p")
  (byte-compiled-function_469010529)
  (while (> arg 0)
    (let ((point-to-move-to (save-excursion
			      (byte-compiled-function_946680247))))  
      (if point-to-move-to
	  (progn
	    (goto-char point-to-move-to)
	    (setq arg (1- arg)))
	(progn
	  (setq arg 0)
	  (error ""))))))

(defun byte-compiled-function_946680247 ()
  "Move to next heading of the same level, and return point or nil if none."
  (let ((level (funcall outline-level)))
    (byte-compiled-function_760238784 1)
    (while (and (> (funcall outline-level) level)
		(not (eobp)))
      (byte-compiled-function_760238784 1))
    (if (< (funcall outline-level) level)
	nil
      (point))))
	
(defun byte-compiled-function_129512964 (arg)
  "Move backward to the ARG'th subheading at same level as this one.
Stop at the first and last subheadings of a superior heading."
  (interactive "p")
  (byte-compiled-function_469010529)
  (while (> arg 0)
    (let ((point-to-move-to (save-excursion
			      (byte-compiled-function_813607463))))
      (if point-to-move-to
	  (progn
	    (goto-char point-to-move-to)
	    (setq arg (1- arg)))
	(progn
	  (setq arg 0)
	  (error ""))))))

(defun byte-compiled-function_813607463 ()
  "Move to next heading of the same level, and return point or nil if none."
  (let ((level (funcall outline-level)))
    (byte-compiled-function_951328898 1)
    (while (and (> (funcall outline-level) level)
		(not (bobp)))
      (byte-compiled-function_951328898 1))
    (if (< (funcall outline-level) level)
	nil
        (point))))

(provide 'outline)

;;; outline.el ends here
	;; unnötige Menübalkenbelegung
	;; else
)
;;;;;;;includeforelc!
;;;;;;(load "op-outline-mode-emacs20")  ;; nix anderes als der outline-mode OHNE die 

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package is a major mode for editing outline-format documents.
;; An outline can be `abstracted' to show headers at any given level,
;; with all stuff below hidden.  See the Emacs manual for details.

;;; Code:

(defgroup outlines nil
  "Support for hierarchical outlining"
  :prefix "outline-"
  :group 'editing)

(defcustom outline-regexp nil
  "*Regular expression to match the beginning of a heading.
Any line whose beginning matches this regexp is considered to start a heading.
The recommended way to set this is with a Local Variables: list
in the file it applies to.  See also outline-heading-end-regexp."
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

(defvar outline-mode-prefix-map nil)

(if outline-mode-prefix-map
    nil
  (if xemacsp
      (setq outline-mode-prefix-map (make-keymap))
    (setq outline-mode-prefix-map (make-sparse-keymap)))
  (define-key outline-mode-prefix-map "@" 'byte-compiled-function_490960576)
  (define-key outline-mode-prefix-map "\C-n" 'byte-compiled-function_760238784)
  (define-key outline-mode-prefix-map "\C-p" 'byte-compiled-function_951328898)
  (define-key outline-mode-prefix-map "\C-i" 'byte-compiled-function_619663136)
  (define-key outline-mode-prefix-map "\C-s" 'byte-compiled-function_182906083)
  (define-key outline-mode-prefix-map "\C-d" 'byte-compiled-function_38737195)
  (define-key outline-mode-prefix-map "\C-u" 'byte-compiled-function_402232290)
  (define-key outline-mode-prefix-map "\C-f" 'byte-compiled-function_103337988)
  (define-key outline-mode-prefix-map "\C-b" 'byte-compiled-function_129512964)
  (define-key outline-mode-prefix-map "\C-t" 'byte-compiled-function_675606449)
  (define-key outline-mode-prefix-map "\C-a" 'byte-compiled-function_720330687)
  (define-key outline-mode-prefix-map "\C-c" 'byte-compiled-function_663981991)
  (define-key outline-mode-prefix-map "\C-e" 'byte-compiled-function_613274775)
  (define-key outline-mode-prefix-map "\C-l" 'byte-compiled-function_687492214)
  (define-key outline-mode-prefix-map "\C-k" 'byte-compiled-function_644551147)
  (define-key outline-mode-prefix-map "\C-q" 'byte-compiled-function_983432855)
  (define-key outline-mode-prefix-map "\C-o" 'byte-compiled-function_753010473))

(defvar outline-mode-menu-bar-map nil)
(if outline-mode-menu-bar-map
    nil
  (if xemacsp
      (setq outline-mode-menu-bar-map (make-keymap))
  (setq outline-mode-menu-bar-map (make-sparse-keymap))))

(defvar outline-mode-map nil "")

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
		  0 '(or (cdr (assq (byte-compiled-function_518884568)
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

(defun byte-compiled-function_518884568 ()
  (let ((count 1))
    (save-excursion
      (byte-compiled-function_469010529)
      (condition-case nil
	  (while (not (bobp))
	    (byte-compiled-function_402232290 1)
	    (setq count (1+ count)))
	(error)))
    count))

(defvar outline-view-change-hook nil
  "Normal hook to be run after outline visibility changes.")

;;;###autoload
(defun byte-compiled-function_661224210 ()
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
  (use-local-map outline-mode-map)
  (setq mode-name "Outline")
  (setq major-mode 'byte-compiled-function_661224210)
  (define-abbrev-table 'text-mode-abbrev-table ())
  (setq local-abbrev-table text-mode-abbrev-table)
  (set-syntax-table text-mode-syntax-table)
  (make-local-variable 'line-move-ignore-invisible)
  (setq line-move-ignore-invisible t)
  ;; Cause use of ellipses for invisible text.
  (byte-compiled-function_1038303325 '(outline . t))
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
  (add-hook 'change-major-mode-hook 'byte-compiled-function_720330687)
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
	(byte-compiled-function_1038303325 '(outline . t))
	(run-hooks 'outline-minor-mode-hook))
    (setq line-move-ignore-invisible nil)
    ;; Cause use of ellipses for invisible text.
    (byte-compiled-function_590419240 '(outline . t)))
  ;; When turning off outline mode, get rid of any outline hiding.
  (or outline-minor-mode
      (byte-compiled-function_720330687))
  (force-mode-line-update))

(defcustom outline-level 'outline-level
  "*Function of no args to compute a header's nesting level in an outline.
It can assume point is at the beginning of a header line."
  :type 'function
  :group 'outlines)

;; This used to count columns rather than characters, but that made ^L
;; appear to be at level 2 instead of 1.  Columns would be better for
;; tab handling, but the default regexp doesn't use tabs, and anyone
;; who changes the regexp can also redefine the outline-level variable
;; as appropriate.
(defun byte-compiled-function_779083082 ()
  "Return the depth to which a statement is nested in the outline.
Point must be at the beginning of a header line.  This is actually
the number of characters that `outline-regexp' matches."
  (save-excursion
    (looking-at outline-regexp)
    (- (match-end 0) (match-beginning 0))))

(defun byte-compiled-function_278599968 ()
  "Skip forward to just before the next heading line.
If there's no following heading line, stop before the newline
at the end of the buffer."
  (if (re-search-forward (concat "\n\\(" outline-regexp "\\)")
			 nil 'move)
      (goto-char (match-beginning 0)))
  (if (and (bolp) (not (bobp)))
      (forward-char -1)))

(defun byte-compiled-function_912387168 ()
  "Move to the next (possibly invisible) heading line."
  (interactive)
  (if (re-search-forward (concat "\n\\(" outline-regexp "\\)")
			 nil 'move)
      (goto-char (1+ (match-beginning 0)))))

(defsubst outline-visible ()
  "Non-nil if the character after point is visible."
  (not (get-char-property (point) 'invisible)))

(defun byte-compiled-function_469010529 (&optional invisible-ok)
  "Move to previous heading line, or beg of this line if it's a heading.
Only visible heading lines are considered, unless INVISIBLE-OK is non-nil."
  (beginning-of-line)
  (or (byte-compiled-function_60251609 t)
      (let (found)
	(save-excursion
	  (while (not found)
	    (or (re-search-backward (concat "^\\(" outline-regexp "\\)")
				    nil t)
		(error "before first heading"))
	    (setq found (and (or invisible-ok (outline-visible)) (point)))))
	(goto-char found)
	found)))

(defun byte-compiled-function_60251609 (&optional invisible-ok)
  "Return t if point is on a (visible) heading line.
If INVISIBLE-OK is non-nil, an invisible heading line is ok too."
  (save-excursion
    (beginning-of-line)
    (and (bolp) (or invisible-ok (outline-visible))
	 (looking-at outline-regexp))))

(defun byte-compiled-function_449469528 ()
  (if (re-search-forward outline-heading-end-regexp nil 'move)
      (forward-char -1)))

(defun byte-compiled-function_760238784 (arg)
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
		(re-search-backward (concat "^\\(" outline-regexp "\\)")
				    nil 'move)
		(not (outline-visible))))
    (setq arg (1+ arg)))
  (while (and (not (eobp)) (> arg 0))
    (while (and (not (eobp))
		(re-search-forward (concat "^\\(" outline-regexp "\\)")
				   nil 'move)
		(not (outline-visible))))
    (setq arg (1- arg)))
  (beginning-of-line))

(defun byte-compiled-function_951328898 (arg)
  "Move to the previous heading line.
With argument, repeats or can move forward if negative.
A heading line is one that starts with a `*' (or that
`outline-regexp' matches)."
  (interactive "p")
  (byte-compiled-function_760238784 (- arg)))

(defun byte-compiled-function_490960576 ()
  "Mark the current subtree in an outlined document.
This puts point at the start of the current subtree, and mark at the end."
  (interactive)
  (let ((beg))
    (if (byte-compiled-function_60251609)
	;; we are already looking at a heading
	(beginning-of-line)
      ;; else go back to previous heading
      (byte-compiled-function_951328898 1))
    (setq beg (point))
    (byte-compiled-function_119382628)
    (push-mark (point))
    (goto-char beg)))

(defun byte-compiled-function_683255193 (from to flag)
  "Hides or shows lines from FROM to TO, according to FLAG.
If FLAG is nil then text is shown, while if FLAG is t the text is hidden."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char from)
      (end-of-line)
      (byte-compiled-function_1059013179 (point) to 'outline)
      (if flag
          (let ((o (make-overlay (point) to)))
            (overlay-put o 'invisible 'outline)
	    (overlay-put o 'isearch-open-invisible
			 'byte-compiled-function_962145196)))))
  (run-hooks 'outline-view-change-hook))


;; Function to be set as an outline-isearch-open-invisible' property
;; to the overlay that makes the outline invisible (see
;; `outline-flag-region').
(defun byte-compiled-function_962145196 (overlay)
  (save-excursion
    (goto-char (overlay-start overlay))
    (byte-compiled-function_613274775)))


;; Exclude from the region BEG ... END all overlays
;; which have PROP as the value of the `invisible' property.
;; Exclude them by shrinking them to exclude BEG ... END,
;; or even by splitting them if necessary.
;; Overlays without such an `invisible' property are not touched.
(defun byte-compiled-function_1059013179 (beg end prop)
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
		      (setq o1 (byte-compiled-function_860814873 o))
		      (move-overlay o1 (overlay-start o1) beg)
		      (move-overlay o end (overlay-end o)))
		  (move-overlay o (overlay-start o) beg))
	      (if (> (overlay-end o) end)
		  (move-overlay o end (overlay-end o))
		(delete-overlay o))))
	(setq overlays (cdr overlays))))))

;; Make a copy of overlay O, with the same beginning, end and properties.
(defun byte-compiled-function_860814873 (o)
  (let ((o1 (make-overlay (overlay-start o) (overlay-end o)
			  (overlay-buffer o)))
	(props (overlay-properties o)))
    (while props
      (overlay-put o1 (car props) (nth 1 props))
      (setq props (cdr (cdr props))))
    o1))

(defun byte-compiled-function_663981991 ()
  "Hide the body directly following this heading."
  (interactive)
  (byte-compiled-function_469010529)
  (byte-compiled-function_449469528)
  (save-excursion
   (byte-compiled-function_683255193 (point) (progn (byte-compiled-function_278599968) (point)) t)))

(defun byte-compiled-function_613274775 ()
  "Show the body directly following this heading.
Show the heading too, if it is currently invisible."
  (interactive)
  (save-excursion
    (byte-compiled-function_469010529 t)
    (byte-compiled-function_683255193 (1- (point))
			 (progn (byte-compiled-function_278599968) (point)) nil)))

(defun byte-compiled-function_675606449 ()
  "Hide all of buffer except headings."
  (interactive)
  (byte-compiled-function_660003855 (point-min) (point-max)))

(defun byte-compiled-function_660003855 (start end)
  "Hide all body lines in the region, but not headings."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (if (byte-compiled-function_60251609)
	  (byte-compiled-function_449469528))
      (while (not (eobp))
	(byte-compiled-function_683255193 (point)
			     (progn (byte-compiled-function_278599968) (point)) t)
	(if (not (eobp))
	    (progn
	      (forward-char
	       (if (looking-at "\n\n")
		   2 1))
	      (byte-compiled-function_449469528)))))))

(defun byte-compiled-function_720330687 ()
  "Show all of the text in the buffer."
  (interactive)
  (byte-compiled-function_683255193 (point-min) (point-max) nil))

(defun byte-compiled-function_38737195 ()
  "Hide everything after this heading at deeper levels."
  (interactive)
  (byte-compiled-function_169080791 t))

(defun byte-compiled-function_687492214 ()
  "Hide all body after this heading at deeper levels."
  (interactive)
  (byte-compiled-function_469010529)
  (byte-compiled-function_449469528)
  (byte-compiled-function_660003855 (point) (progn (byte-compiled-function_119382628) (point))))

(defun byte-compiled-function_182906083 ()
  "Show everything after this heading at deeper levels."
  (interactive)
  (byte-compiled-function_169080791 nil))

(defun byte-compiled-function_983432855 (levels)
  "Hide everything but the top LEVELS levels of headers, in whole buffer."
  (interactive "p")
  (if (< levels 1)
      (error "Must keep at least one level of headers"))
  (setq levels (1- levels))
  (save-excursion
    (goto-char (point-min))
    ;; Keep advancing to the next top-level heading.
    (while (or (and (bobp) (byte-compiled-function_60251609))
	       (byte-compiled-function_912387168))
      (let ((end (save-excursion (byte-compiled-function_119382628) (point))))
	;; Hide everything under that.
	(byte-compiled-function_683255193 (point) end t)
	;; Show the first LEVELS levels under that.
	(if (> levels 0)
	    (byte-compiled-function_619663136 levels))
	;; Move to the next, since we already found it.
	(goto-char end)))))


;;;SMW ff
;;; Angleich der Verhaltensweise an den emacs19 - war für meinen Geschmack auch besser
;;; - diese hier ist deshalb auch aus dem emacs19 
; (defun byte-compiled-function_753010473 ()
;   "Hide everything except for the current body and the parent headings."
;   (interactive)
;   (byte-compiled-function_983432855 1)
;   (let ((last (point))
; 	(pos (point)))
;     (while (save-excursion
; 	     (and (re-search-backward "[\n\r]" nil t)
; 		  (eq (following-char) ?\r)))
;       (save-excursion
; 	(beginning-of-line)
; 	(if (eq last (point))
; 	    (progn
; 	      (byte-compiled-function_912387168)
; 	      (byte-compiled-function_683255193 last (point) ?\n))
; 	  (byte-compiled-function_619663136)
; 	  (setq last (point)))))))
;;;SMW
;;; und hier das Orginal vom emacs20
(defun byte-compiled-function_753010473 ()
  "Hide everything except current body and parent and top-level headings."
  (interactive)
  (byte-compiled-function_983432855 1)
  (save-excursion
    (byte-compiled-function_469010529 t)
    (byte-compiled-function_613274775)
    (while (condition-case nil (progn (byte-compiled-function_402232290 1) t) (error nil))
      (byte-compiled-function_683255193 (1- (point))
			   (save-excursion (forward-line 1) (point))
			   nil))))


(defun byte-compiled-function_169080791 (flag)
  (save-excursion
    (byte-compiled-function_469010529)
    (byte-compiled-function_449469528)
    (byte-compiled-function_683255193 (point)
			  (progn (byte-compiled-function_119382628) (point))
			  flag)))

(defun byte-compiled-function_119382628 ()
  (byte-compiled-function_469010529)
  (let ((opoint (point))
	(first t)
	(level (funcall outline-level)))
    (while (and (not (eobp))
		(or first (> (funcall outline-level) level)))
      (setq first nil)
      (byte-compiled-function_912387168))
    (if (bolp)
	(progn
	  ;; Go to end of line before heading
	  (forward-char -1)
	  (if (bolp)
	      ;; leave blank line before heading
	      (forward-char -1))))))

(defun byte-compiled-function_644551147 ()
  "Show all subheadings of this heading, but not their bodies."
  (interactive)
  (byte-compiled-function_619663136 1000))

(defun byte-compiled-function_619663136 (&optional level)
  "Show all direct subheadings of this heading.
Prefix arg LEVEL is how many levels below the current level should be shown.
Default is enough to cause the following heading to appear."
  (interactive "P")
  (setq level
	(if level (prefix-numeric-value level)
	  (save-excursion
	    (byte-compiled-function_469010529)
	    (let ((start-level (funcall outline-level)))
	      (byte-compiled-function_912387168)
	      (if (eobp)
		  1
		(max 1 (- (funcall outline-level) start-level)))))))
  (save-excursion
    (save-restriction
      (byte-compiled-function_469010529)
      (setq level (+ level (funcall outline-level)))
      (narrow-to-region (point)
			(progn (byte-compiled-function_119382628)
			       (if (eobp) (point-max) (1+ (point)))))
      (goto-char (point-min))
      (while (and (not (eobp))
		  (progn
		    (byte-compiled-function_912387168)
		    (not (eobp))))
	(if (<= (funcall outline-level) level)
	    (save-excursion
	      (byte-compiled-function_683255193 (save-excursion
				     (forward-char -1)
				     (if (bolp)
					 (forward-char -1))
				     (point))
				   (progn (byte-compiled-function_449469528) (point))
				   nil)))))))

(defun byte-compiled-function_402232290 (arg)
  "Move to the heading line of which the present line is a subheading.
With argument, move up ARG levels."
  (interactive "p")
  (byte-compiled-function_469010529)
  (if (eq (funcall outline-level) 1)
      (error "Already at top level of the outline"))
  (while (and (> (funcall outline-level) 1)
	      (> arg 0)
	      (not (bobp)))
    (let ((present-level (funcall outline-level)))
      (while (and (not (< (funcall outline-level) present-level))
		  (not (bobp)))
	(byte-compiled-function_951328898 1))
      (setq arg (- arg 1)))))

(defun byte-compiled-function_103337988 (arg)
  "Move forward to the ARG'th subheading at same level as this one.
Stop at the first and last subheadings of a superior heading."
  (interactive "p")
  (byte-compiled-function_469010529)
  (while (> arg 0)
    (let ((point-to-move-to (save-excursion
			      (byte-compiled-function_946680247))))
      (if point-to-move-to
	  (progn
	    (goto-char point-to-move-to)
	    (setq arg (1- arg)))
	(progn
	  (setq arg 0)
	  (error "No following same-level heading"))))))

(defun byte-compiled-function_946680247 ()
  "Move to next heading of the same level, and return point or nil if none."
  (let ((level (funcall outline-level)))
    (byte-compiled-function_760238784 1)
    (while (and (> (funcall outline-level) level)
		(not (eobp)))
      (byte-compiled-function_760238784 1))
    (if (< (funcall outline-level) level)
	nil
      (point))))

(defun byte-compiled-function_129512964 (arg)
  "Move backward to the ARG'th subheading at same level as this one.
Stop at the first and last subheadings of a superior heading."
  (interactive "p")
  (byte-compiled-function_469010529)
  (while (> arg 0)
    (let ((point-to-move-to (save-excursion
			      (byte-compiled-function_813607463))))
      (if point-to-move-to
	  (progn
	    (goto-char point-to-move-to)
	    (setq arg (1- arg)))
	(progn
	  (setq arg 0)
	  (error "No previous same-level heading"))))))

(defun byte-compiled-function_813607463 ()
  "Move to next heading of the same level, and return point or nil if none."
  (let ((level (funcall outline-level)))
    (byte-compiled-function_951328898 1)
    (while (and (> (funcall outline-level) level)
		(not (bobp)))
      (byte-compiled-function_951328898 1))
    (if (< (funcall outline-level) level)
	nil
        (point))))

(provide 'outline)
(provide 'noutline)

;;; outline.el ends here
	;; unnötige Menübalkenbelegung
	)))

(if xemacsp
    (progn
      (load "outline")
      ))
(outline-minor-mode)



(defun byte-compiled-function_878090507 ()
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
	      (if (not (byte-compiled-function_547907184 (intern string-store))) 
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


(defun byte-compiled-function_923294036 ()
;
; change the next lines for new functions
;
    (if ausblend-toggle-status-1
	(byte-compiled-function_241860486 "\\*+E\\|[\n\r]\\*+FW" (point-min) (point-max)))
    (if ausblend-toggle-status-2
	(byte-compiled-function_241860486 "\\*+V\\|[\n\r]\\*+S" (point-min) (point-max)))
    (if ausblend-toggle-status-3
	(byte-compiled-function_241860486 "\\*+D" (point-min) (point-max)))
    (if ausblend-toggle-status-4
	(byte-compiled-function_684241226 "[ \t]*#+R" (point-min) (point-max)))
    (if ausblend-toggle-status-5
	(byte-compiled-function_684241226 "[ \t]*#+T" (point-min) (point-max))))


(defvar ausblend-toggle-status-1 nil)
(make-local-variable 'ausblend-toggle-status-1)
(defun byte-compiled-function_557942838 ()
  (interactive)
  (if xemacsp (setq mark-active (mark)))
  (byte-compiled-function_776902872 "[\n\r]\\*+E\\|[\n\r]\\*+FW" 'ausblend-toggle-status-1
					     (if mark-active (region-beginning)(point-min))
					     (if mark-active (region-end)(point-max))))
(define-key outline-mode-prefix-map "1" 'byte-compiled-function_557942838)


(defvar ausblend-toggle-status-2 nil)
(make-local-variable 'ausblend-toggle-status-2)
(defun byte-compiled-function_912699813 ()
  (interactive)
  (if xemacsp (setq mark-active (mark)))
  (byte-compiled-function_776902872 "[\n\r]\\*+V\\|[\n\r]\\*+S" 'ausblend-toggle-status-2
			       (if mark-active (region-beginning)(point-min))
			       (if mark-active (region-end)(point-max))))
(define-key outline-mode-prefix-map "2" 'byte-compiled-function_912699813)


(defvar ausblend-toggle-status-3 nil)
(make-local-variable 'ausblend-toggle-status-3)
(defun byte-compiled-function_135994980 ()
  (interactive)
  (if xemacsp (setq mark-active (mark)))
  (byte-compiled-function_776902872 "[\n\r]\\*+D" 'ausblend-toggle-status-3
			       (if mark-active (region-beginning)(point-min))
			       (if mark-active (region-end)(point-max))))
(define-key outline-mode-prefix-map "3" 'byte-compiled-function_135994980)


(defvar ausblend-toggle-status-4 nil)
(make-local-variable 'ausblend-toggle-status-3)
(defun byte-compiled-function_290800667 ()
  (interactive)
  (if xemacsp (setq mark-active (mark)))
  (byte-compiled-function_12114906 "[ \t]*#+R" 'ausblend-toggle-status-3
			       (if mark-active (region-beginning)(point-min))
			       (if mark-active (region-end)(point-max))))
(define-key outline-mode-prefix-map "4" 'byte-compiled-function_290800667)


(defvar ausblend-toggle-status-5 nil)
(make-local-variable 'ausblend-toggle-status-5)
(defun byte-compiled-function_846627637 ()
  (interactive)
  (if xemacsp (setq mark-active (mark)))
  (byte-compiled-function_12114906 "[ \t]*#+T" 'ausblend-toggle-status-5
			       (if mark-active (region-beginning)(point-min))
			       (if mark-active (region-end)(point-max))))
(define-key outline-mode-prefix-map "5" 'byte-compiled-function_846627637)


(defvar ausblend-toggle-status-0 nil)
(make-local-variable 'ausblend-toggle-status-0)
(defun byte-compiled-function_449448063 ()
  (interactive)
  (if xemacsp (setq mark-active (mark)))
  (byte-compiled-function_12114906 "." 'ausblend-toggle-status-0
			       (if mark-active (region-beginning)(point-min))
			       (if mark-active (region-end)(point-max))))
(define-key outline-mode-prefix-map "0" 'byte-compiled-function_449448063)



;;;
;;; ----------------------------------------------------------------------------------------------------
;;; ----------------------------------------------------------------------------------------------------
;;; ----------------------------------------------------------------------------------------------------
;;;
;;; Mode-Funktionen



(defun byte-compiled-function_776902872 (regexpr hidden-toggle-pat1-status startpkt ende)
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
		  (let ((tmp))
		    (while (re-search-forward (concat "\r\\(" regexpr "\\)") nil t)
		      (replace-match "\n\\1" nil nil)
		      ;; gehe zum Anfang des Musters
		      (if (< (point-min) (match-beginning 0))
			  (setq tmp (- (match-beginning 0) 1))
			(setq tmp (match-beginning 0)))
		      ;; bis zur naechsten Ueberschrift
		      (if (re-search-forward "[\n\r]\\*" nil t)
			  nil
			(goto-char (point-max)))
		      (backward-char 3)
		      ;; und jetzt alles ersetzen
		      (while (re-search-backward "\r" tmp t)
			(replace-match "\n" nil nil))))
		;; einklappen
		(let ((tmp))
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
		      (replace-match "\r" nil nil)))))
	      (set-buffer-modified-p already-modified))))
      (if xemacsp (setq mark-active (mark)))
      (if mark-active (widen))
      (if window-system
	  (byte-compiled-function_503165602 t))
      (setq selective-display t)
      (set hidden-toggle-pat1-status (not (symbol-value hidden-toggle-pat1-status))))))



(defun byte-compiled-function_241860486 (regexpr startpkt ende)
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




(defun byte-compiled-function_12114906 (regexpr hidden-toggle-pat1-status startpkt ende)
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
	  (byte-compiled-function_503165602 t))
      (setq selective-display t)
      (set hidden-toggle-pat1-status (not (symbol-value hidden-toggle-pat1-status))))))



(defun byte-compiled-function_684241226 (regexpr startpkt ende)
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
(defun byte-compiled-function_272135483 ()
  (interactive)
  (save-excursion
    (if xemacsp (setq mark-active (mark)))
    (byte-compiled-function_682439367 "\r.*"
			   (if mark-active (region-beginning)(point-min))
			   (if mark-active (region-end)(point-max)))
    (other-window 1)
    (mark-whole-buffer)
    (byte-compiled-function_358171230 1 "\n[ \t]*(find-file-other-window" ")[ \t]*\n" )
    (mark-whole-buffer)
    (byte-compiled-function_358171230 1 "(find-file-other-window" ")" )
    (mark-whole-buffer)
    (byte-compiled-function_358171230 1 "\n[ \t]*(insert-file" ")[ \t]*\n" )
    (mark-whole-buffer)
    (byte-compiled-function_358171230 1 "(insert-file" ")" )
    (mark-whole-buffer)
    (byte-compiled-function_353471660 1)
    (other-window -1)))



(defun byte-compiled-function_877359099 ()
  (interactive)
  (save-excursion
    (if xemacsp (setq mark-active (mark)))
    (byte-compiled-function_682439367 "\r.*"
			   (if mark-active (region-beginning)(point-min))
			   (if mark-active (region-end)(point-max)))))


(defun byte-compiled-function_682439367 (regexpr startpkt ende)
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





(defun byte-compiled-function_1017852626 ()
  (interactive)
  (if xemacsp (setq mark-active (mark)))
  (byte-compiled-function_451500451 (if mark-active (region-beginning)(point-min))
			 (if mark-active (region-end)(point-max)))
  (byte-compiled-function_10309609))
(define-key (current-local-map) "\M-c\M-s" 'byte-compiled-function_1017852626)

(defun byte-compiled-function_451500451 (startpkt ende)
  (save-excursion
    (setq selective-display nil)
    (let ((already-modified (buffer-modified-p))(before-change-functions nil)(after-change-functions nil))
      (narrow-to-region startpkt ende)
      (goto-char (point-min))
      (byte-compiled-function_63073901)
      (widen)
      (setq selective-display t)
      (set-buffer-modified-p already-modified))))




(defun byte-compiled-function_991337799 ()
  (interactive)
  (if xemacsp (setq mark-active (mark)))
  (byte-compiled-function_434177831 (if mark-active (region-beginning)(point-min))
			 (if mark-active (region-end)(point-max))))
(define-key (current-local-map) "\M-c\M-h" 'byte-compiled-function_991337799)

(defun byte-compiled-function_434177831 (startpkt ende)
  (save-excursion
    (setq selective-display t)
    (let ((already-modified (buffer-modified-p))(before-change-functions nil)(after-change-functions nil))
      (narrow-to-region startpkt ende)
      (goto-char (point-min))
      (byte-compiled-function_63073901)
      (byte-compiled-function_263376788)
      (widen)
      (setq selective-display t)
      (set-buffer-modified-p already-modified))))





;; from outline.el with docstring fixed.
(defun byte-compiled-function_484084225 (from to flag)
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


(defun byte-compiled-function_754222724 (byte-compiled-function_790394554 &optional force)
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


(defun byte-compiled-function_394138603 ()
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



(defun byte-compiled-function_658806041 (name &optional noninteractive)
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
		(byte-compiled-function_503165602 t)))))))



(defun byte-compiled-function_51293477 (name crypted &optional kill)
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
	  (byte-compiled-function_658806041 name)
	  (byte-compiled-function_413319921))
      (crypt-encrypted-mode 0)
      (write-file name)
      (kill-buffer (current-buffer)))))

      
(defun byte-compiled-function_413319921 (&optional dummy)
  (interactive "sTo kill current buffer please hit [Return]")
  (let ((current-buf (current-buffer)))
    (if (boundp 'logbuch-buffer)
	(if (eq current-buf logbuch-buffer)
	    (setq logbuch-buffer nil)))
    (if (get-buffer (concat (prin1-to-string current-buf t) ".crypt"))
	(kill-buffer (concat (prin1-to-string current-buf t) ".crypt")))
    (kill-buffer current-buf)))


(defun byte-compiled-function_727423026 (&optional yesorno)
  (interactive "sDo you want to save the file crypted ? (yes or no) [no]: ")
  (if (string= yesorno "yes")
      (progn
	(byte-compiled-function_394138603)
	(message (concat "Maybe there is still an uncrypted file --- be careful !" )))
    (crypt-encrypted-mode 0)
    (byte-compiled-function_252209645)))


(defun byte-compiled-function_199712133 (name crypted)
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
	(byte-compiled-function_658806041 name)
	(setq use-keymap-for-crypted-files t))
    (crypt-encrypted-mode 0)
    (write-file name)
    (setq use-keymap-for-crypted-files nil)))


(defun byte-compiled-function_643290226 (file)
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
		 (byte-compiled-function_503165602 t)))
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

(local-set-key "\C-x\C-w" 'byte-compiled-function_199712133)
(local-set-key "\C-x\C-s" 'byte-compiled-function_252209645)

(defvar project-mode-crypted-keymap (make-sparse-keymap))
(define-key project-mode-crypted-keymap "\C-x\C-s" 'byte-compiled-function_394138603)
(define-key project-mode-crypted-keymap "\C-xk" 'byte-compiled-function_413319921)
(define-key project-mode-crypted-keymap "\C-x\C-f" 'byte-compiled-function_754222724)

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



;
; Definition des Prefix-Arguments für den Project-Mode
;
(defvar project-mode-prefix nil)
;(defvar project-mode-keymap (make-sparse-keymap))

(setq project-mode-prefix "\M-c\M-o")

(defvar outline-minor-mode-map nil)
(defun byte-compiled-function_493573711 (key command)
  (define-key outline-minor-mode-map (concat project-mode-prefix key) command))



;;;;;;;includeforelc!
;;;;;;(load "op-xfig-analyser")




(defun byte-compiled-function_7442265 ()
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

(defun byte-compiled-function_887748482 ()
  (let ((begin) (end))
    (save-excursion
      (beginning-of-line)
      (setq begin (point))
      (end-of-line)
      (setq end (point)))
    (regexp-quote (buffer-substring-no-properties begin end))))

(defun byte-compiled-function_361242307 ()
  (let ((begin) (end))
    (save-excursion
      (beginning-of-line)
      (setq begin (point))
      (end-of-line)
      (setq end (point)))
    (buffer-substring-no-properties begin end)))



(defun byte-compiled-function_905990392 ()
  "removes all lines in a buffer that are two following times the same"
  (interactive)
  (beginning-of-buffer)
  (while
      (progn
	(setq dumvar (byte-compiled-function_887748482))
	(next-line 1)
	(if (string= (byte-compiled-function_887748482) dumvar)
	    (kill-line 1))
	(not (looking-at "^$")))))



(defvar op-jump-to-line-position 0)
(defun byte-compiled-function_360347016 ()
  "Searches for the occurence of the line in the buffer
of the following window"
  (interactive)
  (beginning-of-line)
  (kill-line)
  (yank)
  (beginning-of-line)
  (fixup-whitespace)
  (let ((current-string (byte-compiled-function_361242307)))
    ;;
    (if (= op-jump-to-line-position 0)
	(progn
	  (other-window 1)
	  (beginning-of-buffer)
	  (other-window -1)))
    (setq op-jump-to-line-position (point))
    (kill-line)
    (yank 2)
    (beginning-of-line)
    ;; falls an der Zeile hintendran oder vornedran ewas steht: reduziere auf das Wesentliche
    (if (posix-string-match "^\\*+[^*][ \t]*" current-string)
	(setq current-string (substring current-string (match-end 0) nil)))
;;    (if (posix-string-match "\\[Meilenstein\\][ \t]+[ \t]*" current-string)
;;	(setq current-string (substring current-string (match-end 0) nil)))
;;    (if (posix-string-match "[ \t]+{Nr\\..*$" current-string)
;;	(setq current-string (substring current-string 0 (match-beginning 0))))
    (other-window 1)
    ;;
    (end-of-line)
    (if (search-forward current-string nil t)
	(progn
	  (beginning-of-line)
	  t)
      (other-window -1)
      (setq op-jump-to-line-position 0)
      nil)))

(byte-compiled-function_493573711  "\M-i" 'byte-compiled-function_360347016)


(defun byte-compiled-function_961737068 (event)
  (interactive "e")
  (mouse-set-point event)
  (byte-compiled-function_360347016))


(defun byte-compiled-function_374851153 ()
  (interactive)
  (sort-lines nil (point-min) (point-max)))



(defun byte-compiled-function_5029865 ()
"Extracts from the xfig-file at the current cursor position the
text entries"
  (interactive)
  (let (find-file-not-found-hooks find-file-not-found-hooks)
    (add-hook 'find-file-not-found-hooks
	      (function (lambda ()
			  (error "Keinen solchen File gefunden: %s" buffer-file-name))))
    (let ((current-buff (current-buffer)))
      (find-file (byte-compiled-function_7442265))
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
      (toggle-read-only t)
      (toggle-read-only)
      (rename-buffer "*xfig-text-in-figure*")
      (local-set-key "\C-c\C-c" 'byte-compiled-function_360347016)
      (local-set-key [mouse-2] 'byte-compiled-function_961737068)
      (local-set-key "\C-c\C-s" 'byte-compiled-function_374851153)
      (beginning-of-buffer))))

(byte-compiled-function_493573711  "\M-x" 'byte-compiled-function_5029865)













(defun byte-compiled-function_909625514 ()
"Extracts from the corell-draw eps-file at the current cursor position the
text entries"
  (interactive)
  (let (find-file-not-found-hooks find-file-not-found-hooks)
    (add-hook 'find-file-not-found-hooks
	      (function (lambda ()
			  (error "Keinen solchen File gefunden: %s" buffer-file-name))))
    (let ((current-buff (current-buffer))(dumvar)(dumbuf))
      (find-file (byte-compiled-function_7442265))
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
		  (byte-compiled-function_278174960 1)
		  (newline)
		  (switch-to-buffer dumbuf)
		  (throw 'loop t)))

	    (switch-to-buffer "*Occur*")
	    (toggle-read-only t)
	    (toggle-read-only)
	    (byte-compiled-function_278174960 1)
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


      (byte-compiled-function_905990392)

      (beginning-of-buffer)
      (let ((save-case case-fold-search))
	(setq case-fold-search nil)
	(replace-regexp "\\([a-zäöü.,!;)]\\)\\([A-ZÄÖÜ]\\)" "\\1  \\2")
	(setq case-fold-search save-case))

      (rename-buffer "*corell-text-in-figure*")
      (local-set-key "\C-c\C-c" 'byte-compiled-function_360347016)
      (local-set-key [mouse-2] 'byte-compiled-function_961737068)
      (local-set-key "\C-c\C-s" 'byte-compiled-function_374851153)
      (beginning-of-buffer))))

(byte-compiled-function_493573711  "\M-y" 'byte-compiled-function_909625514)





(defun byte-compiled-function_897747330 (&optional withcomments)
  "Extracts from the Microsoft Project 4.0 mpx-file the text
entries; with positive Argument: inserts comment also,
with - as Argument: Outline-Outfit without comments,
negative number as argument: Outlin-outfit with comments"
  (interactive "P")
  (if (eq withcomments '-)
      (setq withcomments '(0)))
  (if (listp withcomments)
      (setq withcomments (car withcomments)))
  (if withcomments
      (if (> (abs withcomments) 0)
	  (occur "^70;\\|^71;")
	(occur "^70;"))
    (occur "^70;"))
  (switch-to-buffer "*Occur*")
  (toggle-read-only t)
  (toggle-read-only)
  (load "op-indent")
  (kill-line)
  (kill-line)
  (let ((save-start) (save-end) (meilenstein "") (prozentfertig "")
	(anfang)(ende)(tmppoint)(tmppoint2)
	(vorgangsnummer 0)(kontaktperson "")
	(unterprojekt ""))
    (while (not (eobp))
      (beginning-of-line)
      (forward-word 1)
      (forward-char 1)
      ;; Projektzeilen mit "70" bearbeiten
      (if (looking-at "70;")
	  (progn

	    (beginning-of-line)
	    (forward-word 2)
	    (forward-char 1)
	    (setq vorgangsnummer (buffer-substring-no-properties (point) (1- (search-forward ";"))))

	    (beginning-of-line)
	    (forward-word 4)
	    (forward-char 1)
	    (kill-line 0)
	    (re-search-forward "\\(\".*\";\\)\\|;" (save-excursion
						     (end-of-line)
						     (point))
			       t)
	    (setq save-start (point))
	    ;;
	    ;; Wenn Meilenstein, dann setze String
	    (if (looking-at "0d\\|0t")
		(setq meilenstein "[Meilenstein]  ")
	      (setq meilenstein ""))
	    ;; für die Prozentzahlen, wieviel erledigt ist
	    (save-excursion
	      (end-of-line)
	      (re-search-backward "\\(;\\)\\([0-9]+%\\)" nil t)
	      (setq tmppoint (point))
	      (setq prozentfertig (buffer-substring-no-properties
				  (match-beginning 2)
				  (match-end 2))))
	    ;; zum Ermitteln der Kontaktperson
	    (save-excursion
	      (end-of-line)
	      (re-search-backward ";" nil t 10)
	      (backward-char 1)
	      (if (looking-at "\"")
		  (progn
		    (forward-char 1)
		    ;; suche nach " "
		    (re-search-backward ";\"\\(.*;.*\\)\"" nil t))
		;; oder nach ;
		(forward-char 1)
		(re-search-backward ";\\(.*\\)" nil t))
	      (setq tmppoint2 (match-beginning 1))
	      (setq kontaktperson (buffer-substring-no-properties
				   (match-beginning 1)
				   (match-end 1))))
	    ;;
	    ;; zum Ermitteln, ob hier ein Unterprojektfile eingebaut wurde
	    (save-excursion
	      (goto-char tmppoint2)
	      (backward-char 2)
	      (if (save-excursion (backward-char 1) 
				  (looking-at "\""))
		  (progn
		    ;; suche nach " "
		    (re-search-backward ";\"\\(.*;.*\\)\"" nil t))
		;; oder nach ;
		(forward-char 1)
		(re-search-backward ";\\(.*\\)" nil t))
	      (setq unterprojekt (buffer-substring-no-properties
				  (match-beginning 1)
				  (match-end 1)))
	      (if (not (string= unterprojekt ""))
		  (setq unterprojekt (concat "|| [Unterprojekt]  " 
					     unterprojekt
					     " || "))))
	    ;;
	    ;; ermitteln des Anfangs- und des Endtermines
	    (save-excursion
	      (goto-char tmppoint)
	      ;;    zum Feld
	      (re-search-backward ";" nil t 9)
	      (re-search-forward "\\([^;]+\\);" nil t)
	      (setq anfang (buffer-substring-no-properties (match-beginning 1)(match-end 1)))
	      (re-search-forward "\\([^;]+\\);" nil t)
	      (setq ende (buffer-substring-no-properties (match-beginning 1)(match-end 1))))
	    ;;
	    ;;
	    (re-search-forward ";" nil t)
	    (re-search-forward ";" nil t)
	    (kill-region save-start (point))
	    (re-search-forward ";" nil t)
	    (kill-line)
	    ;;
	    ;; setze Meilenstein ein
	    (beginning-of-line)
	    (insert meilenstein)
	    ;;
	    ;; setze Unterprojekt ein
	    (beginning-of-line)
	    (insert unterprojekt)
	    ;;
	    ;; setze Prozentzahl ein (nach dem Semikolon)
	    (end-of-line)
	    (re-search-backward ";.*;" nil t)
	    (forward-char 1)
	    ;; setze Kontaktperson ein
	    (insert (concat "\r  {Nr." vorgangsnummer "; " prozentfertig
			    " erledigt"
			    (if (string= anfang ende)
				(concat "; Termin " anfang "}")
			      (concat
			       "; Anfangstermin: "
			       anfang
			       ", fertigzustellen bis: "
			       ende
			       "}"))
			    (if (not (string= kontaktperson ""))
				(concat "\r {Verantwortlich: " kontaktperson "}"))))
	    ;;
	    (next-line 1))
	(next-line 1)))
    ;; Indentierung der Projektzeilen lösen
    (goto-char (point-min))
    (while (not (eobp))
      (if (not (save-excursion
		 (beginning-of-line)
		 (forward-word 1)
		 (forward-char 1)
		 (looking-at "71;")))
	  (progn
	    (end-of-line)
	    (re-search-backward "[0-9]+;" nil t)
	    (kill-line)
	    (beginning-of-line)
	    (newline)
	    (previous-line 1)
	    (yank)
	    (end-of-line)
	    (backward-delete-char 1)
	    (let ((thisstring (current-word)) (indent-anzahl))
	      (kill-line 0)
	      (kill-line)
	      (setq indent-anzahl (string-to-int thisstring))

	      (if withcomments
		  (if (> withcomments 0)
		      (insert-char ?\t indent-anzahl)
		    (insert-char ?* (1+ indent-anzahl))
		    ;;
		    ;; wenn schon zu 100% erledigt: setze noch ein "E" hintendran
		    (if (save-excursion
			  (end-of-line)
			  (search-backward "erledigt" nil t)
			  (re-search-backward " [0-9]+%" nil t)
			  (looking-at " 100%"))
			(if (looking-at "[^ \t]")
			    (insert-char ?E 1)))
		    ;;
		    ;;
		    ;; setze Tabs ein
		    (if (= indent-anzahl 1)
			(insert-char ?  1)
		      (insert-char ?\t
				   (- indent-anzahl
				      ;; tab-Weite modulo Anzahl
				      (floor (/ indent-anzahl tab-width))
				      1))))
		(insert-char ?\t indent-anzahl)))
	    (next-line 1)
	    (beginning-of-line)
	    ;; Wenn die nächste Zeile kein Kommentar ist: noch ein Newline
	    (if (and (not (eobp))
		     (not (save-excursion
			    (forward-word 1)
			    (forward-char 1)
			    (looking-at "71;"))))
		(insert-char ?\n 1)))
	(next-line 1)))
    ;; alles um eines Zurückindentieren
    (goto-char (point-min))
    (while (not (eobp))
      (next-line 1)
      (if (not (looking-at "^$"))
	  (delete-char 1))
      (beginning-of-line))
    ;; jetzt steht er in einer Kommentarzeile (71)
    (if withcomments
	(progn
	  (goto-char (point-min))
	  (while (not (eobp))
	    (beginning-of-line)
	    (forward-word 1)
	    (forward-char 1)
	    ;; Projektzeilen mit "71" bearbeiten
	    (if (looking-at "71;")
		(progn
		  (beginning-of-line)
		  (forward-word 2)
		  (forward-char 1)
		  (kill-line 0)
		  (setq save-start (point))
		  (end-of-line)
		  (setq save-end (point))
		  (goto-char save-start)
		  ;; Zeilen des Kommentars wiederherstellen
		  (while (re-search-forward "" save-end t)
		    (replace-match "\n" nil nil))
		  (goto-char save-start)
		  (if (not (looking-at "-\\|[0-9]+\\."))
		      (progn
			(insert " ==> ")
			(setq arrowinserted 5))
		    (setq arrowinserted 0))
		  (set-mark (point))
		  (goto-char (+ save-end arrowinserted))
		  ;;
		  (insert-char ?\n 1)
		  (backward-char 2)
		  (beginning-of-line)
		  (if (= (point) save-start)
		      (byte-compiled-function_132890259)
		    (byte-compiled-function_625820513))
		  (end-of-line)
		  (setq save-end (point))
		  ;;
		  ;; anstatt (fill-paragraph): Umbrechen der zu langen Textzeilen im OP-Modus
		  (while (and (>= (point) save-start)
			      (<= (point) save-end))
		    (if (< (save-excursion
			     (beginning-of-line)
			     (save-excursion
			       (goto-char save-end)
			       (untabify save-start save-end)
			       (end-of-line)
			       (setq save-end (point)))
			     (forward-char (min fill-column
						(save-excursion (end-of-line) (current-column))))
			     (if (not (looking-at "$"))
				 (progn
				   (forward-word 1)
				   (if (not (looking-at "$"))
				       (forward-char 1))))
			     (current-column))
			   (save-excursion
			     (end-of-line)
			     (current-column)))
			(progn
			  (beginning-of-line)
			  (save-excursion
			    (goto-char save-end)
			    (untabify save-start save-end)
			    (setq save-end (point)))
			  (forward-char (min fill-column
					     (save-excursion (end-of-line) (current-column))))
			  (if (not (looking-at "$"))
			      (progn
				(forward-word 1)
				(if (not (looking-at "$"))
				    (forward-char 1))
				(byte-compiled-function_511573002)
				(setq save-end (point)))))
		      (previous-line 1)))
		  ;; end (fill-paragraph)
		  ;; Leerzeichen abräumen
		  (next-line 1))
	      (next-line 1)))))
    (setq truncate-lines t)
    ;; Lösche doppelte Newlines heraus
    (goto-char (point-min))
    (while (re-search-forward "\n\n+" nil t)
      (replace-match "\n\n" nil nil))
    ;;
    (tabify (point-min)(point-max))
    (byte-compiled-function_344550657)
    ;;
    ;; mache Zeilenumbruch
    (goto-char (point-min))
    (while (search-forward "\r" nil t)
      (backward-char 1)
      (if (save-excursion
	    (search-backward "\\[Meilenstein\\]" (save-excursion
						      (beginning-of-line)
						      (point)) t))
	  (kill-line)
	(if (search-forward "\r" (save-excursion (end-of-line)(point)) t)
	    (progn
	      (replace-match "\n" nil nil)
	      (byte-compiled-function_385524669)
	      (if (search-forward "\r" (save-excursion (end-of-line)(point)) t)
		  (progn
		    (replace-match "\n" nil nil)
		    (byte-compiled-function_385524669)))))))
    ;;
    (byte-compiled-function_782652504)
    (byte-compiled-function_882163010)
    ;;
    (delete-window)
    (switch-to-buffer current-buff)
    (split-window-vertically 8)
    (switch-to-buffer "*Occur*")
    (toggle-read-only t)
    (toggle-read-only)
    (rename-buffer "*mpx*" t)
    (local-set-key "\C-c\C-c" 'byte-compiled-function_360347016)
    (local-set-key [mouse-2] 'byte-compiled-function_961737068)
    (local-set-key "\C-c\C-s" 'byte-compiled-function_374851153)
    (beginning-of-buffer)))


(defun byte-compiled-function_782652504 ()
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

	(byte-compiled-function_503165602 t)
	)))

(defun byte-compiled-function_733288260 ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\n[ \t]*{.*}" nil t)
      (replace-match "" nil nil))))

(defun byte-compiled-function_465094380 ()
  "Im erzeugten *mpx*-Buffer werden alle Überschriftenzeilen
mit denen im Projektfile nach Abgleich durchsucht. Eindeutigkeit
der Zeilen ist Voraussetzung."
  (interactive)
  (let ((ptrpossv))
    (if window-system
	(require 'hilit19.variation))
    (while (re-search-forward "*+" nil t)
      (if (byte-compiled-function_360347016)
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
			       (byte-compiled-function_378758324 (save-excursion (beginning-of-line)
									 (point))
							 (save-excursion (end-of-line)
									 (point))
							 'highlight))
			   (other-window -1)
			   (if window-system
			       (byte-compiled-function_378758324 (1- (match-beginning 0))(match-end 0)))))
		     ;; wenn mehr als einmal die gleiche Zeile auftaucht
		     (other-window -1)
		     (byte-compiled-function_360347016))))
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
;         (if window-system
;	      (byte-compiled-function_378758324 (save-excursion
;					  (beginning-of-line)
;					  (point))
;					(save-excursion
;					  (end-of-line)
;					  (point))
;					'highlight))
	  (next-line 1))
	)
      (end-of-line))))



(defun byte-compiled-function_762536041 ()
  "Gleicht *mpx*-Buffer mit Projektkontrollbuffer ab. Beide Buffer müssen in 
zwei verschiedenen Rahmen stehen, Aufruf im *mpx*-Buffer"
  (interactive)
  (goto-char (point-min))
  (other-window 1)
  (byte-compiled-function_828488248)
  (goto-char (point-min))
  (other-window -1)
  (byte-compiled-function_465094380)
  (goto-char (point-min))
  (other-window 1)
  (goto-char (point-min))
  (other-window -1)
  (byte-compiled-function_733288260)
  (goto-char (point-min))
  (other-window 1)
  (goto-char (point-min))
  (let ((tmpbuf (current-buffer)))
    (other-window -1)
    (ediff-buffers (current-buffer) tmpbuf)))
  


;;
;; NOCH NACHZUHOLEN VOM MS-Extractor:
;;
;; Meilensteine, Sterneinsätze, Einfärben... mpx von CA ist unbrauchbar
;;
(defun byte-compiled-function_415164312 (&optional withcomments)
  "Extracts from CA SuperProject 4.0 mpx-file the text
entries; with Argument: inserts comments also"
  (interactive "P")
  (if withcomments
      (occur "^70,\\|^71,")
    (occur "^70,"))
  (switch-to-buffer "*Occur*")
  (toggle-read-only t)
  (toggle-read-only)
  (kill-line)
  (kill-line)
  (let ((save-start) (save-end))
    (while (not (eobp))
      (beginning-of-line)
      (forward-word 1)
      (forward-char 1)
      ;; Projektzeilen mit "70" bearbeiten
      (if (looking-at "70,")
	  (progn
	    (beginning-of-line)
	    (forward-word 2)
	    (forward-char 1)
	    (kill-line 0)
	    (re-search-forward "\\(\".*\",\\)\\|," (save-excursion
						     (end-of-line)
						     (point))
			       t)
	    (if (re-search-backward "\"[0-9]+,[0-9]+.*\"" (save-excursion
							    (beginning-of-line)
							    (point))
				    t)
		(progn
		  (search-backward "\"")
		  (search-forward ",")
		  ))
	    (setq save-start (point))
	    (re-search-forward "," nil t)
	    (kill-region save-start (point))
	    (re-search-forward "," nil t)
	    (kill-line)
	    (next-line 1))
	(next-line 1)))
    ;; Indentierung der Projektzeilen lösen
    (goto-char (point-min))
    (while (not (eobp))
      (if (not (save-excursion
		 (beginning-of-line)
		 (forward-word 1)
		 (forward-char 1)
		 (looking-at "71,")))
	  (progn
	    (end-of-line)
	    (re-search-backward "[0-9]+," nil t)
	    (kill-line)
	    (beginning-of-line)
	    (newline)
	    (previous-line 1)
	    (yank)
	    (end-of-line)
	    (backward-delete-char 1)
	    (let ((thisstring (current-word)))
	      (kill-line 0)
	      (kill-line)
	      (insert-char ?\t (string-to-int thisstring)))
	    (next-line 1)
	    (beginning-of-line)
	    ;; Wenn die nächste zeile kein Kommentar ist: noch ein Newline
	    ;; (if withcomments
	    (if (and (not (eobp))
		     (not (save-excursion
			    (forward-word 1)
			    (forward-char 1)
			    (looking-at "71,"))))
		(insert-char ?\n 1)))
	(next-line 1)))
    ;; alles um eines Zurückindentieren
    (goto-char (point-min))
    (while (not (eobp))
      (next-line 1)
      (if (not (looking-at "^$"))
	  (delete-char 1))
      (beginning-of-line))
    ;; jetzt steht er in einer Kommentarzeile (71)
    (if withcomments
	(progn
	  (goto-char (point-min))
	  (while (not (eobp))
	    (beginning-of-line)
	    (forward-word 1)
	    (forward-char 1)
	    ;; Projektzeilen mit "71" bearbeiten
	    (if (looking-at "71,")
		(progn
		  (beginning-of-line)
		  (forward-word 2)
		  (forward-char 1)
		  (kill-line 0)
		  (setq save-start (point))
		  (end-of-line)
		  (setq save-end (point))
		  (goto-char save-start)
		  ;; Zeilen des Kommentars wiederherstellen
		  (while (re-search-forward "" save-end t)
		    (replace-match "\n" nil nil))
		  (goto-char save-start)
		  (if (not (looking-at "-\\|[0-9]+\\."))
		      (progn
			(insert " ==> ")
			(setq arrowinserted 5))
		    (setq arrowinserted 0))
		  (set-mark (point))
		  (goto-char (+ save-end arrowinserted))
		  ;;
		  (insert-char ?\n 1)
		  (backward-char 2)
		  (beginning-of-line)
		  (if (= (point) save-start)
		      (byte-compiled-function_132890259)
		    (load "op-indent")
		    (byte-compiled-function_625820513))
		  (end-of-line)
		  (setq save-end (point))
		  ;;
		  ;; anstatt (fill-paragraph)
		  (while (and (>= (point) save-start)
			      (<= (point) save-end))
		    (if (< (save-excursion
			     (beginning-of-line)
			     (save-excursion
			       (goto-char save-end)
			       (untabify save-start save-end)
			       (setq save-end (point)))
			     (forward-char (min fill-column
						(save-excursion (end-of-line) (current-column))))
			     (if (not (looking-at "$"))
				 (progn
				   (forward-word 1)
				   (if (not (looking-at "$"))
				       (forward-char 1))))
			     (current-column))
			   (save-excursion
			     (end-of-line)
			     (current-column)))
			(progn
			  (beginning-of-line)
			  (save-excursion
			    (goto-char save-end)
			    (untabify save-start save-end)
			    (setq save-end (point)))
			  (forward-char (min fill-column
					     (save-excursion (end-of-line) (current-column))))
			  (if (not (looking-at "$"))
			      (progn
				(forward-word 1)
				(if (not (looking-at "$"))
				    (forward-char 1))
				(byte-compiled-function_511573002))))
		      (previous-line 1)))
		  ;; end (fill-paragraph)
		  ;; Leerzeichen abräumen
		  (next-line 1))
	      (next-line 1)))))
    (setq truncate-lines t)
    ;; Lösche doppelte Newlines heraus
    (goto-char (point-min))
    (while (re-search-forward "\n\n+" nil t)
      (replace-match "\n\n" nil nil))
    ;;
    ;;
    ;; CA-SuperProject hat leider nur marodes mpx-Format zur Verfügung:
    ;; die Reihenfolge stimmt nicht und die Projektzeilen sind nur begrenzt lang.
    ;; Daher vermeide Nichtfinden der jeweiligen Zeilen durch abschneiden des
    ;; Kommas am Zeilenende
    (goto-char (point-min))
    (while (re-search-forward ",$" nil t)
      (replace-match "" nil nil))
    ;;
    (tabify (point-min)(point-max))
    ;;
    (delete-window)
    (switch-to-buffer current-buff)
    (split-window-vertically 8)
    (switch-to-buffer "*Occur*")
    (toggle-read-only t)
    (toggle-read-only)
    (rename-buffer "*mpx*" t)
    (local-set-key "\C-c\C-c" 'byte-compiled-function_360347016)
    (local-set-key [mouse-2] 'byte-compiled-function_961737068)
    (local-set-key "\C-c\C-s" 'byte-compiled-function_374851153)
    (beginning-of-buffer)))



(defun byte-compiled-function_257535182 (&optional withcomments)
  "Extracts from Microsoft Project or CA SuperProject the mpx-file at the current
cursor position the text entries; with Argument: inserts comments also"
  (interactive "P")
  (let (find-file-not-found-hooks find-file-not-found-hooks)
    (add-hook 'find-file-not-found-hooks
	      (function (lambda ()
			  (error "Keinen solchen File gefunden: %s" buffer-file-name))))
    (let ((current-buff (current-buffer)) (save-start)(save-end) (arrowinserted))
      (find-file (byte-compiled-function_7442265))
      (if (looking-at "MPX,Microsoft Project for Windows,4.0,ANSI") ; CA SuperProject
	  (progn
	    (message "CA-SuperProject 4.0")
	    (byte-compiled-function_415164312 withcomments))
	(message "MS Project 4.0/95/98")
	(byte-compiled-function_897747330 withcomments)) ;; anderenfalls Microsoft-Konform
      )))

(byte-compiled-function_493573711  "\M-m" 'byte-compiled-function_257535182)

(setq selective-display t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun byte-compiled-function_882163010 ()
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
  ;;    '("Test" . tescht))
  ;;
  ;;
  (define-key (current-local-map) [menu-bar] mpx-mode-menu-bar-map))



;;(defun byte-compiled-function_385366952 ()
;;  (interactive)
;;  (setq window-system nil)
;;    (y-or-n-p "test "))




;;;;;;;includeforelc!
;;;;;;(load "op-BF")





(defun byte-compiled-function_529861335 ()
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
	(while (looking-at "
\n[^*]")
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
			(byte-compiled-function_56118394)
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


(defun byte-compiled-function_676479602 ()
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
	  (byte-compiled-function_529861335)
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
	  (while (re-search-forward "
\n" nil t) ; neu kreierte Punkte in BrainForest
	    (replace-match ""))
 	  (goto-char 1)
	  (while (re-search-forward "
" nil t)
	    (replace-match "\n"))
 	  (goto-char 1)
	  (while (re-search-forward "
" nil t)
	    (replace-match "\n"))
	  (goto-char 1)
	  (while (re-search-forward "
$" nil t)
	    (replace-match "\n"))
	  (goto-char 1)
	  (while (re-search-forward "
+" nil t)
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
			      (byte-compiled-function_56118394)
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
		    (byte-compiled-function_56118394)
		    (next-line 1)
		    (beginning-of-line)))
					;)
;;; 	  ;; jetzt noch indents nachholen, die durch die Baumstruktur vielleicht verloren gegangen sind
;;; 	  (goto-char 1)
;;; 	  (while (re-search-forward "^\t" nil t)
;;; 	    (byte-compiled-function_56118394))
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


(defun byte-compiled-function_355263970 ()
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
		  (insert-char ?
 1)
		  (if (not (eobp))
		      (forward-char 1)))))
	  ;; ersetze alle reinen Freizeilen durch das Zeichen , damit diese bei der 
	  ;; Rekonstruktion wieder herstellbar sind
	  (byte-compiled-function_344550657)
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
;;    (autoload 'byte-compiled-function_607524941 "sql-mode"
;;      "Start the interactive SQL interpreter in a new buffer." t)
;;
;;    (autoload 'byte-compiled-function_201349162 "sql-mode"
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
      (define-key sql-mode-map "O"        'byte-compiled-function_1027881757)
      (define-key sql-mode-map "o"        'byte-compiled-function_1027881757)
      (define-key sql-mode-map ";"        'byte-compiled-function_272275924)
      (define-key sql-mode-map "\C-cg"    'byte-compiled-function_431872784)
      (define-key sql-mode-map "\C-c\C-e" 'byte-compiled-function_388589019)
      (define-key sql-mode-map "\C-cb"    'byte-compiled-function_331909761)
      (define-key sql-mode-map "\C-ct"    'byte-compiled-function_597369130)
      (define-key sql-mode-map "\C-cp"    'byte-compiled-function_80939493)
      (define-key sql-mode-map "\C-cn"    'byte-compiled-function_891586834)
      (define-key sql-mode-map "\C-c\C-w" 'sql-buffer-display-window)
      (define-key sql-mode-map "\C-c\C-l" 'byte-compiled-function_939060790)
      (define-key sql-mode-map "\C-c\C-b" 'byte-compiled-function_911728626)
      (define-key sql-mode-map "\C-c\C-f" 'byte-compiled-function_660748264)
      (define-key sql-mode-map "\C-c\C-p" 'byte-compiled-function_354740184)
      (define-key sql-mode-map "\C-c\C-n" 'byte-compiled-function_379884840)
      (define-key sql-mode-map "\C-c\C-i" 'byte-compiled-function_445086866)
      (define-key sql-mode-map "\C-c\C-r" 'byte-compiled-function_961703156)
      (define-key sql-mode-map "\C-c\C-c" 'byte-compiled-function_186059184)
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

(defun byte-compiled-function_201349162 nil
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
(byte-compiled-function_186059184).  Just position the cursor on or near the SQL
statement you wish to send and press '\\[sql-send-current]' to run it and
display the results.

SQL-mode starts configured to interact with isql from Sybase.  Oracle
users can switch to the sqlplus configuration using 'M-x sql-oracle',
or by adding a call to sql-oracle to their sql-mode-hook function.

Mode Specific Bindings:

\\{sql-mode-map} "

  (interactive)
  (setq major-mode 'byte-compiled-function_201349162)
  (setq mode-name "SQL")
  (use-local-map sql-mode-map)
  (setq local-abbrev-table sql-mode-abbrev-table)
  (abbrev-mode 1)
  (setq abbrev-all-caps 1)
  (run-hooks 'sql-mode-hook))


;;;  SQL-mode Settings for Specific Vendors

(defun byte-compiled-function_587766621 nil
  "Configure SQL-mode for Sybase ``isql'' interaction."
  (interactive)
  (setq sql-terminator "\ngo")
  (setq sql-command "isql")
  (fset 'sql-start-interpreter 'byte-compiled-function_267146950))

(defun byte-compiled-function_1072832700 nil
  "Configure SQL-mode for Oracle ``sqlplus'' interaction."
  (interactive)
  (setq sql-terminator ";")
  (setq sql-command "sqlplus")
  (fset 'sql-start-interpreter 'byte-compiled-function_617794751)
  (cond (sql-emacs18-p
	 (and (featurep 'gmhist)
	      (put 'sql-get-server-hist 'initial-hist (list ""))))
	(t
	 (setq sql-get-server-hist (make-list 2 "")))))

(defun byte-compiled-function_267146950 nil
  "Start up the Sybase isql program."
  (start-process sql-process-name sql-buffer-name sql-command "-w"
                 "2048" "-n" "-S" sql-server "-U" sql-username "-P"
                 (byte-compiled-function_806834926 "Password: ")))

(defun byte-compiled-function_617794751 nil
  "Start up the Oracle sqlplus program."
  (start-process
   sql-process-name sql-buffer-name sql-command
   (concat sql-username "/" (byte-compiled-function_806834926 "Password: ")
	   (if (string= sql-server "") "" (concat "@" sql-server)))))


;;;  Utilitities

(defun byte-compiled-function_69287798 (buffer-name string &optional force-display)
  "Displays string in the named buffer, creating the buffer if needed.
If force-display is true, the buffer will appear if not already shown."
  (let ((buffer (get-buffer-create buffer-name)))
    (if force-display (display-buffer buffer))
    (set-buffer buffer)
    (goto-char (point-max))
    (insert string)
    (if force-display
	(set-window-point (get-buffer-window buffer-name) (point-max)))))

(defun byte-compiled-function_973554015 nil
  "Generates reasonable error messages abouut the SQL connection."
  (if (not (get-buffer sql-buffer-name))
      (error "No SQL output buffer!  Use 'M-x sql' to initialize the SQL interpreter."))
  (if (not (get-buffer-process sql-buffer-name))
      (error "Buffer '%s' is not talking to anybody!" sql-buffer-name)))

(defun byte-compiled-function_898203695 (strings)
  "Sends strings to the SQL process.
Also shows the string at the top of the SQL output buffer."
  (byte-compiled-function_973554015)
  (byte-compiled-function_69287798 sql-buffer-name
		      (concat "\n" sql-output-separator "\n\n"))
  (byte-compiled-function_331909761)
  (byte-compiled-function_896110565)
  (byte-compiled-function_69287798 sql-buffer-name (apply 'concat strings))
  (byte-compiled-function_69287798 sql-buffer-name "\n\n")
  (let ((string (apply 'concat strings))
	(process  (get-buffer-process sql-buffer-name)))
    (send-string process (concat string "\n"))
    (if (eq (current-buffer) (process-buffer process))
	(set-marker (process-mark process) (point))))
  (byte-compiled-function_939060790))

(defun byte-compiled-function_978484378 nil
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

(defun byte-compiled-function_961703156 (start end)
  "Send a region to the SQL process."
  (interactive "r")
  (save-excursion
    (byte-compiled-function_898203695 (list (buffer-substring start end) sql-terminator))))

(defun byte-compiled-function_186059184 nil
  "Send the current SQL command(s) to the SQL process."
  (interactive)
  (byte-compiled-function_978484378)
  (byte-compiled-function_961703156 sql-region-beginning-mark sql-region-end-mark))


(defun byte-compiled-function_1027881757 (arg)
  "Insert ``o'' and submit the current SQL to the interpreter."
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  (and sql-magic-go (save-excursion
                      (beginning-of-line)
                      (and (looking-at "go\\b") (byte-compiled-function_186059184)))))

(defun byte-compiled-function_272275924 (arg)
  "Insert semicolon and submit the current SQL to the interpreter."
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  (and sql-magic-semicolon (byte-compiled-function_186059184)))


;;;  SQL-Output Buffer Operations -

(defun byte-compiled-function_179797930 (&optional fcn &rest args)
  "Makes the SQL output buffer visible in the other window."
  (interactive)
  (byte-compiled-function_973554015)
  (let ((window  (selected-window)))
    (if (not (eq (window-buffer window) (get-buffer sql-buffer-name)))
	(switch-to-buffer-other-window sql-buffer-name))
    (if fcn (condition-case nil (apply fcn args) (error nil)))
    (select-window window)))

(fset 'sql-buffer-display-window 'byte-compiled-function_179797930)

(defun byte-compiled-function_379884840 nil
  "Scroll-up in the SQL output buffer window."
  (interactive)
  (byte-compiled-function_179797930 'scroll-up))

(defun byte-compiled-function_354740184 nil
  "Scroll-down in the SQL output buffer window."
  (interactive)
  (byte-compiled-function_179797930 'scroll-down))

(defun byte-compiled-function_660748264 (num)
  "Scroll-left in the SQL output buffer window."
  (interactive "p")
  (byte-compiled-function_179797930 'scroll-left (* num (/ (window-width) 2))))

(defun byte-compiled-function_911728626 (num)
  "Scroll-right in the SQL output buffer window."
  (interactive "p")
  (byte-compiled-function_179797930 'scroll-right (* num (/ (window-width) 2))))

(defun byte-compiled-function_896110565 nil
  "Mark the current position in the SQL output window."
  (byte-compiled-function_179797930 'byte-compiled-function_440376426))

(defun byte-compiled-function_440376426 nil
  "Set the sql-buffer-marker."
  (setq sql-buffer-mark (copy-marker (point))))

(defun byte-compiled-function_939060790 nil
  "Go to the current sql-buffer-mark."
  (interactive)
  (byte-compiled-function_179797930 'byte-compiled-function_1013168424))

(defun byte-compiled-function_1013168424 nil
  (goto-char sql-buffer-mark)
  (recenter 0))

(defun byte-compiled-function_597369130 nil
  "Goto the top of the SQL output buffer."
  (interactive)
  (byte-compiled-function_179797930 'byte-compiled-function_955441954))

(defun byte-compiled-function_955441954 nil (goto-char (point-min)))

(defun byte-compiled-function_331909761 nil
  "Goto the bottom of the SQL output buffer."
  (interactive)
  (byte-compiled-function_179797930 'byte-compiled-function_326516781))

(defun byte-compiled-function_326516781 nil (goto-char (point-max)) (recenter -1))

(defun byte-compiled-function_388589019 nil
  "Clear the SQL output buffer."
  (interactive)
  (byte-compiled-function_179797930 'erase-buffer))

(defun byte-compiled-function_891586834 nil
  "Search for the next command in the SQL output buffer."
  (interactive)
  (byte-compiled-function_179797930 'byte-compiled-function_813028208))

(defun byte-compiled-function_813028208 nil
  "Search for the next command in the SQL output buffer."
  (cond ((re-search-forward  sql-output-separator nil t)
	 (forward-line 2)
	 (recenter 0))
	(t (beep) (message "No more commands."))))

(defun byte-compiled-function_80939493 nil
  "Search for the previous command in the SQL output buffer."
  (interactive)
  (byte-compiled-function_179797930 'byte-compiled-function_1039923970))

(defun byte-compiled-function_1039923970 nil
  "Search for the previous command in the SQL output buffer."
  (let ((start (point)))
    (re-search-backward  sql-output-separator nil t)
    (cond ((re-search-backward  sql-output-separator nil t)
	   (forward-line 2)
	   (recenter 0))
	  (t
	   (message "No more commands.") (beep)
	   (goto-char start)))))

(defun byte-compiled-function_445086866 nil
  "Send an interrupt the the SQL interpreter process."
  (interactive)
  (interrupt-process sql-process-name))


;;;  Miscellaneous

(defun byte-compiled-function_431872784 (n)
  "Moves to the n'th line in the most recently executed SQL."
  (interactive "NLine number of error: ")
  (goto-char sql-region-beginning-mark)
  (forward-line (1- n)))

(defun byte-compiled-function_816411065 nil
  "Inserts 'go' statements between each apparent block of SQL code."
  (interactive)
  (while (not (eobp))
    (forward-line 1)
    (if (and (looking-at "[a-z]") (not (looking-at "go")))
	(progn (insert "go\n")))))


;;;  SQL Interpreter

(defun byte-compiled-function_607524941 nil
  "Start the interactive SQL interpreter in a new buffer."
  (interactive)
  (byte-compiled-function_349584316)
  (get-buffer-create sql-buffer-name)            ; move to the buffer.
  (or (get-buffer-process sql-buffer-name)       ; already got a process?
   (progn			                 ; no, start it up!
     (set-buffer sql-buffer-name)
     (setq truncate-lines t)
     (sql-start-interpreter)))
  (set-buffer sql-buffer-name)
  (byte-compiled-function_179797930))

;; Start with Sybase isql
(fset 'sql-start-interpreter 'byte-compiled-function_267146950)

(defun byte-compiled-function_349584316 nil
  "Set server, username, process, and buffer for the SQL interpreter."
  (interactive)
  (call-interactively 'byte-compiled-function_196698448)
  ;; skip username query if ``server/user'' was input.
  (let ((stroke (string-match "/" sql-server)))
    (cond (stroke
	   (setq sql-username (substring sql-server (1+ stroke) nil))
	   (setq sql-server (substring sql-server 0 stroke)))
	  (t
	   (call-interactively 'byte-compiled-function_978423830))))
  (setq sql-process-name (if (string= "" sql-server) sql-username
                           (concat sql-server "/" sql-username)))
  (setq sql-buffer-name
        (concat sql-buffer-prefix sql-process-name sql-buffer-postfix)))

(defun byte-compiled-function_196698448 (server)
  "Set the SQL server name."
  (interactive (list (byte-compiled-function_41302669 "Server: " 'sql-get-server-hist 2)))
  (setq sql-server server))

(defun byte-compiled-function_978423830 (username)
  "Set the SQL server username."
  (interactive (list (byte-compiled-function_41302669 "Username: " 'sql-get-username-hist 2)))
  (setq sql-username username))

(defun byte-compiled-function_41302669 (prompt &optional symbol offset)
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

(defun byte-compiled-function_806834926 (prompt &optional default)
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
    (byte-compiled-function_890914436)
    (substring pass 0 -1)))

(setq sql-ange-ftp-tmp-keymap (make-sparse-keymap))
(define-key sql-ange-ftp-tmp-keymap "\C-m" 'exit-minibuffer)

(defun byte-compiled-function_890914436 nil
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

(fset 'sql-set-user 'byte-compiled-function_607524941)


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


(provide 'byte-compiled-function_201349162)

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



(defun byte-compiled-function_479084779 ()
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
(defun byte-compiled-function_873619000 (&optional stop-recursive)
  (interactive)
  (let ((command (byte-compiled-function_479084779)))
    (if (string-match "->[>]?[^ \t]*\\(SQL:\\|FILE:\\|A:\\|FUNC:\\|EVAL:\\|REF:\\|REF-R\\)" command)
	(progn
	  (setq called-via-OP-button t)
	  
	  ;; ---------- SQL-Button
	  ;; suche nach ->...SQL:func und führe func im SQL-Modus aus
	  ;; erst: Suche nach "->SQL:file*evalstartpunkt"
	  (if (string-match "\\(-\>[^ \t]*SQL:\\)\\(.*\\*\\)\\([^ \t\r;]*\\)\\(;?\\)" command)
	      (progn
		(byte-compiled-function_14281547 (substring command (match-beginning 2) (- (match-end 2) 1))
			  (substring command (match-end 2) (match-beginning 4))))
	    ;; oder auch: suche nach ->SQL:file
	    (if (string-match "-\>[^ \t]*SQL:" command)
		(byte-compiled-function_14281547 (substring command (match-end 0)) nil)))
	  
	  ;; ---------- FILE-Button
	  ;; suche nach ->>...FILE:file*RegexStelle
	  (if (string-match "\\(-\>\>[^ \t]*FILE:\\)\\(.*\\*\\)\\([^ \t\r;]*\\)\\(;?\\)" command)
	      (byte-compiled-function_560762339 (substring command (match-beginning 2) (- (match-end 2) 1))
			 (concat ">" (substring command (match-end 2) (match-beginning 4))))
	    ;; oder auch: suche nach ->>FILE:RegexStelle (dann im aktuellen File)
	    (if (string-match "-\>\>[^ \t]*FILE:" command)
		(byte-compiled-function_560762339 nil (concat ">"(substring command (match-end 0))))
	      ;; suche nach ->...FILE:file*RegexStelle
	      (if (string-match "\\(-\>[^ \t]*FILE:\\)\\(.*\\*\\)\\([^ \t\r;]*\\)\\(;?\\)" command)
		  (byte-compiled-function_560762339 (substring command (match-beginning 2) (- (match-end 2) 1))
			     (substring command (match-end 2) (match-beginning 4)))
		;; oder auch: suche nach ->FILE:RegexStelle (dann im aktuellen File)
		(if (string-match "-\>[^ \t]*FILE:" command)
		    (byte-compiled-function_560762339 nil (substring command (match-end 0)))))))
	  
	  ;; ---------- A-Button (für den eigenen Adressenfile) (Kurzform für ->FILE:adressen.op*"Name"
	  ;; suche nach ->...A:RegexStelle
	  (if (string-match "-\>[^ \t]*A:" command)
	      (byte-compiled-function_560762339 adressenfile (substring command (match-end 0))))

	  ;; ---------- FUNC-Button
	  ;; suche nach ->...FUNC:funcfile*func, sehe im funcfile nach, was func machen soll
	  ;;    und führe dieses dann aus
	  (if (string-match "\\(-\>[^ \t]*FUNC:\\)\\(.*\\*\\)\\([^ \t\r;]*\\)\\(;?\\)" command)
	      (progn
		(byte-compiled-function_358304232 (substring command (match-beginning 2) (- (match-end 2) 1))
			   (substring command (match-end 2) (match-beginning 4))))
	    ;; oder nehme dafür den Default-Funcfile: .op-navigate
	    (if (string-match "-\>[^ \t]*FUNC:" command)
		(byte-compiled-function_358304232 nil (substring command (match-end 0)))))
	  
	  ;; ---------- EVAL-Button
	  ;; suche nach ->...EVAL:programm*file, ruft Programm auf, um damit File auszuführen
	  (if (string-match "\\(-\>[^ \t]*EVAL:\\)\\(.*\\*\\)\\([^ \t\r;]*\\)\\(;?\\)" command)
	      (progn
		(byte-compiled-function_880273721 (substring command (match-beginning 2) (- (match-end 2) 1))
			   (substring command (match-end 2) (match-beginning 4))))
	    ;; oder nach EVAL:file, dieses sucht sich 
	    (if (string-match "-\>[^ \t]*EVAL:" command)
		(byte-compiled-function_880273721 nil (substring command (match-end 0)))))
	  
	  ;; ---------- REF-Button
	  ;; suche nach ->...REF:file*RegexStelle
	  (if (string-match "\\(-\>[^ \t]*REF:\\)\\(.*\\*\\)\\([^ \t\r;]*\\)\\(;?\\)" command)
	      (progn
		(byte-compiled-function_190214153 (substring command (match-beginning 2) (- (match-end 2) 1))
			  (substring command (match-end 2) (match-beginning 4))))
	    ;; oder auch: suche nach ->REF:RegexStelle (dann im aktuellen File)
	    (if (string-match "-\>[^ \t]*REF:" command)
		(byte-compiled-function_190214153 nil (substring command (match-end 0)))))
	  
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
		    (byte-compiled-function_476131929 (substring command (match-beginning 2) (- (match-end 2) 1))
				(substring command (match-end 2) (match-beginning 4))))
		;; oder auch: suche nach ->REF-R:RegexStelle (dann im aktuellen File)
		(if (string-match "-\>[^ \t]*REF-R:" command)
		    (byte-compiled-function_476131929 nil (substring command (match-end 0)))))
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
		    (byte-compiled-function_880273721 (substring command (match-beginning 2) (- (match-end 2) 1))
			       (substring command (match-end 2))))
		;; oder auch: suche nach ->!programm
		(if (string-match "-\>!" command)
		    ;;(nur ->file*RegexStelle oder nur ->RegexStelle
		    (byte-compiled-function_880273721 nil (substring command (match-end 0)))))
	    ;; oder nach ->>file*RegexStelle
	    (if (string-match "\\(-\>\>\\)\\(.*\\*\\)" command)
		(byte-compiled-function_560762339 (substring command (match-beginning 2) (- (match-end 2) 1))
			   (concat ">" (substring command (match-end 2) (match-beginning 4))))
	      ;; oder auch: suche nach ->>RegexStelle (dann im aktuellen File)
	      (if (string-match "-\>\>" command)
		  (byte-compiled-function_560762339 nil (concat ">" (substring command (match-end 0))))
		;; suche nach ->file*RegexStelle
		(if (string-match "\\(-\>\\)\\(.*\\*\\)" command)
		    (byte-compiled-function_560762339 (substring command (match-beginning 2) (- (match-end 2) 1))
			       (substring command (match-end 2) (match-beginning 4)))
		  ;; mit / im Link: daraus wird ein FILE: extrapoliert
		  (if (or (string-match "\\(-\>\\)\\(.*/.*\\)" command)
			  ;; mit . mit Nachbuchstabe im Link: daraus wird ein FILE: extrapoliert
			  (string-match "\\(-\>\\)\\(.*\\.+.*\\)" command))
		      (byte-compiled-function_560762339 (substring command (match-beginning 2) (match-end 2))
				 (substring command (match-end 2) (match-beginning 4)))
		    ;; oder auch: suche nach ->RegexStelle (dann im aktuellen File)
		    (if (string-match "-\>" command)
			(byte-compiled-function_560762339 nil (substring command (match-end 0)))))))))

;	    (if (string-match "\\(-\>\\)\\(.*\\*\\)" command)
;		(byte-compiled-function_560762339 (substring command (match-beginning 2) (- (match-end 2) 1))
;			   (substring command (match-end 2)))
;	      ;; oder auch: suche nach ->RegexStelle (dann im aktuellen File)
;	      (if (string-match "-\>" command)
;		  (byte-compiled-function_560762339 nil (substring command (match-end 0))))))
	(message "not recognised as a button")))
    (setq called-via-OP-button nil)
    ))



(defun byte-compiled-function_14281547 (file startpunkt)
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
    (byte-compiled-function_961703156 pointsv (point))
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
(defun byte-compiled-function_560762339 (file regex)
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
		  (recenter)
		  (re-search-backward originalpattern nil t)
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
		      (recenter)
		      (re-search-backward originalpattern nil t)
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
		    (message "no-pattern-stop")
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
;(defun byte-compiled-function_634141231 (file regex)
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


(defun byte-compiled-function_358304232 (file startpunkt)
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



(defun byte-compiled-function_880273721 (program file)
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
	  ;; wenn bisher noch keine Extention gematcht hat, führe den File direkt aus
	  (shell-command file)))))
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
(defun byte-compiled-function_190214153 (file button-match)
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
(defun byte-compiled-function_476131929 (file button-match)
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
	    (byte-compiled-function_873619000 t)))
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
(defun byte-compiled-function_165472474 (click)
  (interactive "e")
  (let* ((start (event-start click))
	 (window (car start))
	 (pos (car (cdr start))))
    (select-window window)
    (goto-char pos))
  (byte-compiled-function_873619000))





;(global-set-key "\M-\C-m" 'byte-compiled-function_873619000)
(define-key (current-local-map) "\M-\C-m" 'byte-compiled-function_873619000)
(if xemacsp
    (define-key (current-local-map) [(shift button2)] 'byte-compiled-function_165472474)
  (define-key (current-local-map) [\S-mouse-2] 'byte-compiled-function_165472474))







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
(setq before-change-functions '(byte-compiled-function_383253877))
(setq after-change-functions '(byte-compiled-function_181311207))


(defun byte-compiled-function_156474084 (&optional on)
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
    (setq before-change-functions '(byte-compiled-function_383253877))
    (setq after-change-functions '(byte-compiled-function_181311207))
    (if on
	(message "automatic-change-mode switched to on")
    (message "automatic-change-mode toggled to on"))))


(defun byte-compiled-function_383253877 (a b)
  ;; suche, ob vorne an dem Wort ein <- oder ein -> steht (siehe op-hyper.el)
  (let ((tmp (byte-compiled-function_714847600)))
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



(defun byte-compiled-function_181311207 (a b deleted)
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


(defun byte-compiled-function_714847600 ()
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

(byte-compiled-function_156474084 t)            ; Alle Labels automatisch aktualisieren 
(byte-compiled-function_156474084)     	     ;(nil macht es aber nicht aus -- geht nur durch toggeln)


;; zur Kontrolle
(message "6")


;;;----------------------------------------------------------------------------------------------------



;;; für die #include-files
;;;;;;;includeforelc!
;;;;;;(load "op-includer")


(defun byte-compiled-function_790394554 ()
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

(defun byte-compiled-function_864735500 (&optional cvs)
  "Schleife über load-file-at-include-statement von Anfang an"
  (interactive "P")
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "#include" nil t)
	(progn
	  (backward-word 1)
	  (backward-char 1)
	  (while (looking-at "#include")
	    (byte-compiled-function_872370904 cvs)
	    (forward-word 1)
	    (forward-char 1)
	    ;;
	    (if (re-search-forward "#include" nil t)
		(progn
		  (backward-word 1)
		  (backward-char 1)))))
      (if window-system
	  (byte-compiled-function_503165602 t)))))

(defun byte-compiled-function_872370904 (&optional cvs)
  (interactive "P")
  (let (byte-compiled-function_790394554)
    (save-excursion
      (if (looking-at "#include")
	  (progn
	    (forward-word 2)
	    (setq filename (byte-compiled-function_790394554)))
	(beginning-of-line)
	(search-forward "\"" (save-excursion (end-of-line) (point)) t)
	(setq filename (byte-compiled-function_790394554))))
    (byte-compiled-function_475476911 filename cvs)))

(defun byte-compiled-function_475476911 (byte-compiled-function_790394554 &optional cvs)
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
		    (let ((control-buffer (byte-compiled-function_809435777 filename)))
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
      (byte-compiled-function_503165602 t)))


(defun byte-compiled-function_252209645 ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (search-forward "end_include_statement" nil t)
	(if (y-or-n-p "Sie haben registriert, dass es noch nicht ausgelagerte #include-Files in diesem File gibt? ")
	    (save-buffer)
	  (message "Speichervorgang abgebrochen"))
      (save-buffer))))


(defun byte-compiled-function_652464789 (&optional cvs)
  (interactive)
  (let ((before-change-functions nil)(after-change-functions nil) (tmpvar))
    (message "This will take a while --- please wait")
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "#include" nil t)
	  (progn
	    (backward-word 1)
	    (backward-char 1)
	    (let ((insert-buf) (byte-compiled-function_790394554) (current-buf (current-buffer)) (mark1) (mark2) (was-a-crypted-file nil))
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
		(setq filename (byte-compiled-function_790394554))

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
				      (byte-compiled-function_658806041 filename t)
				      ;; für gecryptete files im Moment eigentlich nicht supported
				      ;;(if cvs
				      ;;(byte-compiled-function_949159257 (buffer-file-name))))
				      )
				  (write-file filename)
				  (if cvs
				      (byte-compiled-function_949159257)))
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



(defun byte-compiled-function_797353265 (&optional cvs)
  (interactive)
  (let ((before-change-functions nil)(after-change-functions nil) (tmpvar)
	(insert-buf) (byte-compiled-function_790394554) (current-buf (current-buffer)) (mark1) (mark2) (was-a-crypted-file nil))
    (save-excursion
      (message "This will take a while --- please wait")
      (if (looking-at "#include")
	  (progn
	    (forward-word 2)
	    (setq filename (byte-compiled-function_790394554)))
	(beginning-of-line)
	(search-forward "\"" (save-excursion (end-of-line) (point)) t)
	(setq filename (byte-compiled-function_790394554))
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
      (setq filename (byte-compiled-function_790394554))
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
			    (byte-compiled-function_658806041 filename t)
			    ;; für gecryptete files im Moment eigentlich nicht supported
			    ;;(if cvs
			    ;;(byte-compiled-function_949159257 (buffer-file-name))))
			    )
			(write-file filename)
			(if cvs
			    (byte-compiled-function_949159257)))
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



(defun byte-compiled-function_231209922 ()
  (let ((already-modified (buffer-modified-p)))
    (save-excursion
      (goto-char (point-min))
      (replace-regexp "\n#end_include_statement" "\r#end_include_statement"))
    (set-buffer-modified-p already-modified)))


(byte-compiled-function_493573711 "\M-i" 'byte-compiled-function_864735500)
(byte-compiled-function_493573711 "\M-z" 'byte-compiled-function_652464789)




;;; --------------------------------------------------------------------------------
;;; --------------------------------------------------------------------------------
;;; --------------------------------------------------------------------------------




(defun byte-compiled-function_1008318197 (name crypted)
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
    (byte-compiled-function_51293477 name crypted t)
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
	(byte-compiled-function_503165602 t))))


(defun byte-compiled-function_1047529787 (name crypted)
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
  (byte-compiled-function_1008318197 name crypted)
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






;;; ediff-Erweiterung, damit fine-diffs besser unterstützt werden
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

(load "ediff")
(if gnuemacsp
    (progn
      (ediff-defvar-local ediff-current-fine-difference -1 "")
      (ediff-defvar-local ediff-current-difference-save -1 "")))




(defun byte-compiled-function_769886075 (from-buffer to-buffer arg)
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


(defun byte-compiled-function_501306485 (from-buffer to-buffer &optional kill)
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
		(fset 'modify-frame-parameters 'byte-compiled-function_190571531)
		(ediff-recenter t)
		(ediff-update-diffs))
	    (fset 'modify-frame-parameters (symbol-function 'dummy-func)))

	  (message "Recovery of buffer A or B with rfa or rfb"))
      (message "no fine diffs found")
      )))



(defun byte-compiled-function_190571531 (&optional arg1 arg2)
;;;  '(lambda()(message "not including \"#include\" files")))
  nil)




(defun byte-compiled-function_706280974 (&optional arg)
  (interactive "P")
  (if (not arg)
      (setq arg 1))
  (ediff-barf-if-not-control-buffer)
  (if (= ediff-current-difference-save ediff-current-difference)
      (progn
	(setq ediff-current-fine-difference (+ ediff-current-fine-difference arg))
	(byte-compiled-function_769886075 'A 'B ediff-current-fine-difference))
    (setq ediff-current-difference-save ediff-current-difference)
    (setq ediff-current-fine-difference 1)
    (byte-compiled-function_75221801 1)))  ; spät


(defun byte-compiled-function_616539203 (&optional arg)
  (interactive "P")
  (if (not arg)
      (setq arg 1))
  (ediff-barf-if-not-control-buffer)
  (if (= ediff-current-difference-save ediff-current-difference)
      (progn
	(setq ediff-current-fine-difference (- ediff-current-fine-difference arg))
	(byte-compiled-function_769886075 'A 'B ediff-current-fine-difference))
    (setq ediff-current-difference-save ediff-current-difference)
    (setq ediff-current-fine-difference 10000)
    (byte-compiled-function_75221801 10000)))  ; spät



(defun byte-compiled-function_705715643 ()
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (byte-compiled-function_501306485 'A 'B))


(defun byte-compiled-function_337951151 ()
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (byte-compiled-function_501306485 'B 'A))

(defun byte-compiled-function_1046392450 ()
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (byte-compiled-function_501306485 'A 'B t))


(defun byte-compiled-function_590110106 ()
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (byte-compiled-function_501306485 'B 'A t))

(defun byte-compiled-function_75221801 (arg)
  (interactive "NJump to which fine difference ? ")
  (ediff-barf-if-not-control-buffer)
   (if (not arg)
      (setq arg 1))
  (setq ediff-current-difference-save ediff-current-difference)
  (setq ediff-current-fine-difference (- arg 1))
  (byte-compiled-function_769886075 'A 'B ediff-current-fine-difference))


(defun byte-compiled-function_408113443 ()
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (let ((current-buf (current-buffer)))
    (pop-to-buffer ediff-buffer-A)
    (undo)
    (pop-to-buffer current-buf)
    (fset 'dummy-func (symbol-function 'modify-frame-parameters))
    (unwind-protect
	(progn
	  (fset 'modify-frame-parameters 'byte-compiled-function_190571531)
	  (ediff-recenter t)
	  (ediff-update-diffs))
      (fset 'modify-frame-parameters (symbol-function 'dummy-func)))))


(defun byte-compiled-function_513707758 ()
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (let ((current-buf (current-buffer)))
    (pop-to-buffer ediff-buffer-B)
    (undo)
    (pop-to-buffer current-buf)
    (unwind-protect
	(progn
	  (fset 'modify-frame-parameters 'byte-compiled-function_190571531)
	  (ediff-recenter t)
	  (ediff-update-diffs))
      (fset 'modify-frame-parameters (symbol-function 'dummy-func)))))


(defun byte-compiled-function_233202579 ()
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (let ((current-buf (current-buffer)))
    (pop-to-buffer ediff-buffer-C)
    (undo)
    (pop-to-buffer current-buf)
    (unwind-protect
	(progn
	  (fset 'modify-frame-parameters 'byte-compiled-function_190571531)
	  (ediff-recenter t)
	  (ediff-update-diffs))
      (fset 'modify-frame-parameters (symbol-function 'dummy-func)))))




(defun byte-compiled-function_306491000 ()
  (interactive)
  (save-excursion
    (let ((control-buffer (current-buffer)))
      (set-buffer control-buffer)

      (define-key ediff-mode-map "f" nil)
      (define-key ediff-mode-map "fn" 'byte-compiled-function_706280974)
      (define-key ediff-mode-map "fp" 'byte-compiled-function_616539203)
      (define-key ediff-mode-map "fa" 'byte-compiled-function_705715643)
      (define-key ediff-mode-map "fb" 'byte-compiled-function_337951151)
      (define-key ediff-mode-map "fda" 'byte-compiled-function_1046392450)
      (define-key ediff-mode-map "fdb" 'byte-compiled-function_590110106)
      (define-key ediff-mode-map "fg" 'byte-compiled-function_75221801)
      (define-key ediff-mode-map "rfa" 'byte-compiled-function_408113443)
      (define-key ediff-mode-map "rfb" 'byte-compiled-function_513707758)
      (define-key ediff-mode-map "rfc" 'byte-compiled-function_233202579)
      )))




(add-hook 'ediff-startup-hook 'byte-compiled-function_306491000 t t)

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


;;;;;;;includeforelc!
;;;;;;(load "op-ediff-mods")

(defun byte-compiled-function_345092845 ()
  (interactive)
  (ediff-barf-if-not-control-buffer)
    (ediff-print-diff-vector (intern "ediff-difference-vector-A"))
    (ediff-print-diff-vector (intern "ediff-difference-vector-B"))
    ;(ediff-print-diff-vector (intern "ediff-difference-vector-C"))
    ;(ediff-print-diff-vector (intern "ediff-difference-vector-Ancestor"))
    )



(defun byte-compiled-function_256843022 (arg &optional keys)
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
    (byte-compiled-function_298275335 ediff-current-difference
		     (ediff-char-to-buftype char1)
		     (ediff-char-to-buftype char2))
    ;; recenter with rehighlighting, but no messages
    (ediff-recenter)))











;;; AUS ediff-diffs.el

;; Convert diff list to overlays for a given DIFF-REGION
;; in buffer of type FROM-BUFFER
(defun byte-compiled-function_60086983 (from-buffer diff-list region-num to-buffer)




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

(defun byte-compiled-function_298275335 (n from-buf-type to-buf-type
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

		 ;;   (byte-compiled-function_60086983 from-buf-type op-diff-list n to-buf-type)





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

(defun byte-compiled-function_949159257 ()
  (interactive)
  "Creates from current file .op a file with extention .cvs.op, which has been
stripped from C++ like comments"
  (let ((current-buf (current-buffer)) filebuffer
	(before-change-functions nil)(after-change-functions nil)
	(find-file-hooks nil)(find-file-not-found-hooks nil)(auto-mode-alist nil))
    (find-file (byte-compiled-function_494521561 buffer-file-name))
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
    (byte-compiled-function_353471660 1)
    ;; entferne alle //-Kommentare
    (goto-char (point-min))
    (replace-regexp "\n[ \t]*//.*" "")
    (goto-char (point-min))
    (replace-regexp "//.*" "")
    (save-buffer)
    (kill-buffer (current-buffer))))


(defun byte-compiled-function_809435777 (byte-compiled-function_790394554)
  (interactive "f")
  (let ((filename-cvs) (filename-nocvs) current-frame)
    ;; Finde den zugehörigen Filenamen zum Paar .cvs.op --- .op
    (if (string-match ".cvs" filename)
	(progn
	  (setq filename-nocvs (byte-compiled-function_453225868 filename))
	  (setq filename-cvs filename))
      (setq filename-nocvs filename)
      (setq filename-cvs (byte-compiled-function_494521561 filename)))

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
      (byte-compiled-function_828488248)
      (if (get-buffer (file-name-nondirectory filename-nocryptextention))
	  (setq nocvs-buffer (get-buffer (file-name-nondirectory filename-nocryptextention)))
	(find-file filename-nocvs)
	(setq nocvs-buffer (current-buffer)))
      (byte-compiled-function_828488248)

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
		      (if (get-buffer (file-name-nondirectory (byte-compiled-function_494521561 buffer-file-name)))
			  (progn
			    (select-window (get-buffer-window
					    (file-name-nondirectory (byte-compiled-function_494521561 buffer-file-name))))
			    (kill-buffer (current-buffer))
			    (delete-window))))) t)


(defun byte-compiled-function_688362196 ()
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
  (byte-compiled-function_797353265 t))


(defun byte-compiled-function_148284246 ()
  "Loop über write-this-include-for-cvs über alle #includes"
  (interactive)
  (byte-compiled-function_652464789 t))

(defun byte-compiled-function_123030261 ()
  " - .Nachdem der cvs.op-File cvs-mässig aktualisiert wurde, so dass eine neue Version
     im cvs steht steht, macht load-op-cvs
  - automatisches Ediff zwischen
	   dem .op-File, alte Version, mit geheimen Kommentaren
	   und dem .cvs.op-File, der diese nicht enthält, aber upgedated wurde
    wobei der File .op im Buffer steht, damit er gekrypted gewesen sein kann
  - zurückschreiben der erneuerten Version MIT den eigenen Kommentaren"
  (interactive)
  (byte-compiled-function_872370904 t))

(defun byte-compiled-function_517588812 ()
  "Loop über load-this-include-for-cvs über alle #includes"
  (interactive)
  (byte-compiled-function_864735500 t))



(defun byte-compiled-function_494521561 (oldfilename)
  "Gibt z.B. den Namen name.op.crypt als name.cvs.op.crypt zurück"
  (let ((stringstart (string-match "\\(\.op\\)" (prin1-to-string oldfilename t))))
    (concat (substring oldfilename 0 stringstart) ".cvs" (substring oldfilename stringstart))))

(defun byte-compiled-function_453225868 (oldfilename)
  "Gibt z.B. den Namen name.cvs.op.crypt als name.op.crypt zurück"
  (let ((stringstart (string-match "\\(\.cvs.op\\)" (prin1-to-string oldfilename t))))

  (concat (substring oldfilename 0 stringstart) ".op" (substring oldfilename (match-end 0)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Anschluß an pcl-cvs


(defun byte-compiled-function_526364947 (wohin directory)
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


(defun byte-compiled-function_6427734 (directory)
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
;;	(byte-compiled-function_943712212 60) ; einmal pro Stunde
;;
;; When you are tired of LOGBUCHding use "M-x logbuch-alarm-off"
;;
;; Alternately, use "M-x logbuch" whenever you want to log something.
;;
;; Für das persönliche Logbuch: Logbuch-Mode
;; (defvar use-logbuch-projectdoc-buffer t)   ;; Zu einen neuen Buffer (=nil) oder zum Buffer  logbuch-projectdoc-buffer  springen (=t) ?
;; (defvar logbuch-projectdoc-buffer "~/projects/myprojects.op.crypt")
;; (defvar current-logbuch-file "~/projects/Wochenbericht.txt")	 ;; an diesen File sollen die Logbuch-Kommentare angehängt werden
;;
;; (byte-compiled-function_943712212 90) ;; alle 90 Minuten Nachfragen
;;
;;



(provide 'byte-compiled-function_579551551)

(if (not (boundp 'use-logbuch-projectdoc-buffer))
    (setq use-logbuch-projectdoc-buffer nil))

(defun byte-compiled-function_382702076 ()
  "Major mode for editing text to be sent to logbuch.
Like Text Mode but with these additional commands:
C-c C-c	 logbuch-send-and-exit
C-c C-r	 logbuch-send-region"
  (interactive)
  (setq logbuch-map (copy-keymap (current-local-map)))
  (use-local-map logbuch-map)
  (define-key logbuch-map "\C-c\C-c"   'byte-compiled-function_721677898)
  (define-key logbuch-map "\C-c\C-r"   'byte-compiled-function_141789122)
  (define-key logbuch-map "\C-c\C-s"   'byte-compiled-function_131507186)
  (if (not use-logbuch-projectdoc-buffer)
      (define-key logbuch-map "\M-\C-i"  'tab-to-tab-stop))
;  (define-key (current-local-map) "\C-c\C-l\C-c"   'byte-compiled-function_721677898)
;  (define-key (current-local-map) "\C-c\C-l\C-r"   'byte-compiled-function_141789122)
;  (define-key (current-local-map) "\C-c\C-l\C-s"   'byte-compiled-function_131507186)
;  (if (not use-logbuch-projectdoc-buffer)
;      (define-key (current-local-map) "\M-\C-i"  'tab-to-tab-stop))
  (setq major-mode 'byte-compiled-function_382702076)
  (setq mode-name "Logbuch")
  (setq buffer-offer-save t))


(defun byte-compiled-function_131507186 ()
  (interactive)
  (setq logbuch-projectdoc-buffer (current-buffer))
  (message "This buffer is now set as the \"logbuch-projectdoc-buffer\" "))


(defun byte-compiled-function_721677898 (arg)
  "Send message like logbuch-send-buffer, then, if no errors, exit from logbuch buffer.
Prefix arg means don't delete this window."
  (interactive "P")
  (if (not (boundp 'current-logbuch-buffer))
      (setq current-logbuch-buffer (current-buffer))
    (if (eq (current-buffer) current-logbuch-buffer)
	(setq current-logbuch-buffer (current-buffer))))
  (byte-compiled-function_876021226)
  (if (not use-logbuch-projectdoc-buffer)
      (progn
	(bury-buffer (current-buffer))
	(if (and (not arg)
		 (not (one-window-p)))
	    (delete-window)
	  (switch-to-buffer current-logbuch-buffer)))))


(defun byte-compiled-function_876021226 ()
  "Send the message in the current buffer to logbuch."
  (message "Sending...")
  (if (and (string= (prin1-to-string (current-buffer)) "#<buffer *logbuch*>")
	   (not (buffer-modified-p)))
      (message "(No changes need to be saved)")
    (if (eq (current-buffer) current-logbuch-buffer)
	(setq current-logbuch-buffer (current-buffer)))
    (byte-compiled-function_634886628)
    (if (not use-logbuch-projectdoc-buffer)
	(progn
	  (set-buffer-modified-p nil)
	  (delete-auto-save-file-if-necessary)))
    (switch-to-buffer current-logbuch-buffer)
    (message "Sending done")))


(defun byte-compiled-function_634886628 ()
  (if (and use-logbuch-projectdoc-buffer
	   (not (eq (current-buffer) current-logbuch-buffer)))
      (if (string-match "\\.crypt" logbuch-projectdoc-buffer)
	  (progn
	    (byte-compiled-function_394138603)
	    (bury-buffer-smartly))
	(save-buffer)
	(bury-buffer-smartly))
    (if (not (eq (current-buffer) current-logbuch-buffer))
	(progn
	  (goto-char (point-min))
	  (insert "\n>>>  ")(byte-compiled-function_999722354)(insert "	")(insert logbuch-trenner-anfang)(insert-char ?\n 1)
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


(defun byte-compiled-function_286456950 (region-begin region-end)
  (if (and use-logbuch-projectdoc-buffer
	   (not (eq (current-buffer) current-logbuch-buffer)))
      (if (string-match "\\.crypt" logbuch-projectdoc-buffer)
	  (progn
	    (byte-compiled-function_394138603)
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
	  (insert "\n>>>  ")(byte-compiled-function_999722354)(insert "	")(insert logbuch-trenner-anfang)(insert-char ?\n 2)
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



(defun byte-compiled-function_949825781 () ; noch in Arbeit
  (if (and use-logbuch-projectdoc-buffer
	   (not (eq (current-buffer) current-logbuch-buffer)))
      (if (string-match "\\.crypt" logbuch-projectdoc-buffer)
	  (progn
	    (byte-compiled-function_394138603)
	    (bury-buffer-smartly))
	(save-buffer)
	(bury-buffer-smartly))
    (if (string-match "\\.crypt" current-logbuch-file)
	(progn
	  (if (not (eq (current-buffer) current-logbuch-buffer))
	      (progn
		(goto-char (point-min))
		(insert "\n>>>  ")(byte-compiled-function_999722354)(insert "	")(insert logbuch-trenner-anfang)(insert-char ?\n 1)
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


(defun byte-compiled-function_772804152 (region-begin region-end) ; noch in Arbeit
  (if (and use-logbuch-projectdoc-buffer
	   (not (eq (current-buffer) current-logbuch-buffer)))
      (if (string-match "\\.crypt" logbuch-projectdoc-buffer)
	  (progn
	    (byte-compiled-function_394138603)
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
	  (insert "\n>>>  ")(byte-compiled-function_999722354)(insert "	")(insert logbuch-trenner-anfang)(insert-char ?\n 2)
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
(defun byte-compiled-function_579551551 (&optional noerase)
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
	    (byte-compiled-function_754222724 logbuch-projectdoc-buffer)
	  (find-file logbuch-projectdoc-buffer))
	(setq logbuch-buffer (current-buffer))
	(message "Dieser Buffer ist nun ein Logbuch-Buffer: Sichern und in den Hintergrund mit	\C-c \C-c"))
    (switch-to-buffer "*logbuch*")
    (setq logbuch-buffer (current-buffer))
    (setq default-directory (expand-file-name "~/"))
    (auto-save-mode auto-save-default)
    (byte-compiled-function_382702076)
    ;; naechste Zeile loescht alle lokalen Variablen
    (byte-compiled-function_335754943)
    (and (not noerase)
	 (or (not (buffer-modified-p))
	     (y-or-n-p "Unsent message being composed; erase it? "))
	 (progn (erase-buffer)
		(set-buffer-modified-p nil)
		(run-hooks 'logbuch-setup-hook)
		t))))


(defun byte-compiled-function_150215209 (&optional noerase)
  "Like `logbuch' command, but display logbuch buffer in another window."
  (interactive "P")
  (let ((pop-up-windows t))
    (pop-to-buffer "*logbuch*"))
  (byte-compiled-function_579551551 noerase))


;; run every interval & at initial call to logbuch-alarm-on
(defun byte-compiled-function_986815923 ()
  (if (not (eq (current-buffer) logbuch-buffer))
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
	(byte-compiled-function_579551551)
	(message "Report-buffer: send and store with C-c C-c after you have typed something")
	)))


(defvar logbuch-zeitobjekt nil)

;; Starte LOGBUCH in eimen regelmäßigem Intervall
;;
(defvar logbuch-abfrageintervall 90
  "Der Abfrageabstand des Logbuch-Zeitbuffers im Minuten, globale Variable")
(defun byte-compiled-function_943712212 (interval)
  "Turn the Emacs logbuch alarm on.  The alarm goes off every INTERVAL minutes
and you will be switched to the logbuch buffer automatically.
Use logbuch-alarm-off to stop this behaviour."
  (interactive "nEnter logbuch alarm interval (in minutes): ")
  (setq logbuch-abfrageintervall interval)
  (byte-compiled-function_4952126)
  (setq logbuch-zeitobjekt (run-with-timer
			    ;; convert minutes -> seconds for wakeup
			    (* 60.0 interval) (* 60.0 interval)
			    'byte-compiled-function_986815923)))


;; Turn LOGBUCH alarm off
;;
(defun byte-compiled-function_4952126 ()
  "Turn the Emacs logbuch alarm off."
  (interactive)
  (if logbuch-zeitobjekt
      (progn
	(cancel-timer logbuch-zeitobjekt)
	(setq logbuch-zeitobjekt nil)))
  (cancel-function-timers 'byte-compiled-function_986815923))



(defun byte-compiled-function_141789122 (region-begin region-end)
  "Send the message in the current region to logbuch."
  (interactive "r")
  (if (not (boundp 'current-logbuch-buffer))
      (setq current-logbuch-buffer (current-buffer))
    (if (eq (current-buffer) current-logbuch-buffer)
	(setq current-logbuch-buffer (current-buffer))))
  (message "Sending...")
  (byte-compiled-function_286456950 region-begin region-end)
  (switch-to-buffer current-logbuch-buffer)
  (message "Sending...done"))




(byte-compiled-function_382702076)


; --------------------------------------------------------------------------------





;;SMW isearch

;;;;;;;includeforelc!
;;;;;;(load "op-search")  ;;; alle Search-Funktionen



;;; search for current word ============================================================


(defun byte-compiled-function_818994661 (&optional mark noruler noerror)
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
	(byte-compiled-function_946158545))))

(defun byte-compiled-function_502372405 (&optional mark noruler noerror)
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
	(byte-compiled-function_946158545))))

(define-key (current-global-map) "\M-s" 'byte-compiled-function_818994661)
(define-key (current-global-map) "\M-r" 'byte-compiled-function_502372405)



(defun byte-compiled-function_170283284 (&optional mark)
  (interactive "P")
  (let ((dum))
    (if (byte-compiled-function_266316501 'dum)
	(let ((current-window (get-buffer-window (current-buffer))) (current-word (byte-compiled-function_32143247)))
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
	  (byte-compiled-function_946158545)
	  (other-window 1))
      (byte-compiled-function_818994661 mark t))))


(defun byte-compiled-function_226574873 (&optional mark)
  (interactive "P")
  (let ((dum))
    (if (byte-compiled-function_266316501 'dum)
	(let ((current-window (get-buffer-window (current-buffer))) (current-word (byte-compiled-function_32143247)))
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
	  (byte-compiled-function_946158545)
	  (other-window 1))
      (byte-compiled-function_502372405 mark t))))


(local-set-key "\M-s" 'byte-compiled-function_170283284)
(local-set-key "\M-r" 'byte-compiled-function_226574873)



;;; search in headlines only ============================================================

(defun byte-compiled-function_915652147 (&optional not-regexp no-recursive-edit)
  (interactive "P\np")
  (let ((before-change-functions nil)(after-change-functions nil))
  (isearch-mode t (null not-regexp) 'byte-compiled-function_118601339 (not no-recursive-edit))))

(defun byte-compiled-function_1030488631 (&optional not-regexp no-recursive-edit)
  (interactive "P\np")
  (let ((before-change-functions nil)(after-change-functions nil))
  (isearch-mode nil (null not-regexp) 'byte-compiled-function_21467820 (not no-recursive-edit))))

(defun byte-compiled-function_118601339 ()
  (if (< (length isearch-string) 4)
      (progn
	(setq isearch-string (concat
			      "[\n\r]\\*+"	 ; 2
			      "[^\n\r]+"	 ; 1
			      isearch-string))	 ; 2+1 < 4
	(beginning-of-line)
	(setq isearch-other-end (point))
	(isearch-update))))
(defun byte-compiled-function_21467820 ()
  (if (< (length isearch-string) 4)
      (progn
	(setq isearch-string (concat
			      "[\n\r]\\*+"	 ; 2
			      "[^\n\r]+"	 ; 1
			      isearch-string))	 ; 2+1 < 4
	(end-of-line)
	(setq isearch-other-end (point))
	(isearch-update))))

(define-key (current-local-map) "\C-\M-n" 'byte-compiled-function_915652147)
(define-key (current-local-map) "\C-\M-p" 'byte-compiled-function_1030488631)


;;; ;;; search with visible regions ============================================================

;;; 1.) search current word and make it visible ============================================================

(defun byte-compiled-function_1025400072 ()
  "searches for the next occurence of the actual word and expands tree locally"
  (interactive)
  (let ((before-change-functions nil)(after-change-functions nil))
    (if (not (= am-i-at-the-same-point (point)))
	(progn
	  (while (progn
		   (byte-compiled-function_818994661 nil t)
		   (byte-compiled-function_1000129612 t)
		   (save-excursion
		     (re-search-backward "\n\\|\r" nil t)
		     (looking-at "\r"))))
					;	(save-excursion
					;	  (byte-compiled-function_818994661 nil t)
					;	  (setq am-i-at-the-same-point (point)))
	  (if window-system
	      (byte-compiled-function_503165602 ?-))
	  (byte-compiled-function_946158545)
	  )
      (message "last match")
      (ding)
      (setq am-i-at-the-same-point 999999))))


(defun byte-compiled-function_38766745 ()
  "searches for the last occurence of the actual word and expands tree locally"
  (interactive)
  (let ((before-change-functions nil)(after-change-functions nil))
  (if (not (= am-i-at-the-same-point (point)))
      (progn
	(while (progn
		 (byte-compiled-function_502372405 nil t)
		 (byte-compiled-function_1000129612 t)
		 (save-excursion
		   (re-search-backward "\n\\|\r" nil t)
		   (looking-at "\r"))))
;	(save-excursion
;	  (byte-compiled-function_502372405 nil t)
;	  (setq am-i-at-the-same-point (point)))
	(if window-system
	    (byte-compiled-function_503165602 ?-))
	(byte-compiled-function_946158545))
    (message "last match")
    (ding)
    (setq am-i-at-the-same-point 999999))))

(define-key (current-local-map) "\M-n" 'byte-compiled-function_1025400072)
(define-key (current-local-map) "\M-p" 'byte-compiled-function_38766745)

;;; 2.) search forward in visible regions only ============================================================

;; ein nachprogrammiertes re-search-forward, einfacher und dafür transparenter
;; steht hier nur für Testzwecke, isearch-visible ist in den Details vom Verhalten vollständiger
(defun byte-compiled-function_103649839 ()
  (let ((t1 "") (t2 0) (t3 0) (point (point)) (re 0))
    (while (not (= t2 27)) ; falls nicht ESC
      (setq t2 (if (< emacs-major-version 20)
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



;; ein nachprogrammiertes re-search-forward, einfacher und dafür transparenter
;; dieses sucht allerdings nur in den sichtbaren Bereichen, wenn der selective-display-
;; Mechanismus eingeschaltet ist (funktioniert nicht mit dem emacs20-System der 
;; overlay-properties via buffer-invisible-spec
(defun byte-compiled-function_644676125 ()
  "A search function for Isearch only in visible text.
With selective-display properties (^M), there is no overwrap, but C-s or C-r 
for continuing, character is not accepted if search is failing, end with ESC.
If invisible properties mechanism, just an Isearch in visible regions."
  (interactive)
  (if (or (< emacs-major-version 20)
	  (not (listp buffer-invisibility-spec)))
      (let ((t1 "") (t2 0) (t3 0) (point (point)) (re 0) (first nil) (search-backward-found) (message "I-search visible:"))
	(while (not (= t2 27))		; falls nicht ESC
	  (setq t2 (if (< emacs-major-version 20)
		       (progn
			 (message (concat message t1))
			 (read-char))
		     (read-char (concat message t1))))
					;(if (>= (length t3) 3) (debug))
	  (if (= t2 127)		; ist ein DEL
	      ;; dann letzten Buchstaben wegstreichen
	      (progn
		(setq t1 (substring t1 (- (length t1)) -1))
		(re-search-backward t1 nil t)
		(setq re 0))
	    (if (= t2 19)		; weiteres search forward (cntl s)
		(progn
		  (if (<= re 0)         ; das erste cntl-s
		      (setq first t))   ; dann suche das schon erwischte nicht mit
		  (setq re 1))
	      (if (= t2 18)		; weiteres search backward (cntl r)
		  (setq re -1)
		;; re indiziert ein weiteres Suchen (<> 0)
		(setq re 0)
		;; wenn nicht, dann letzten Buchstaben dranhängen
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
		(progn
		  ;; while-Schleife zum Rückwärts-Finden (kein schöner Code, aber schnell)
		  (while (progn (if (re-search-backward t1 nil t)
				    (setq search-backward-found t)
				  (setq search-backward-found nil))
				(not (beginning-of-line))
				(re-search-forward (concat t1 "\\|\r") (save-excursion (end-of-line) (point)) t)
				(not (backward-char 1))
				(looking-at "\r"))) ; t wenn nicht gefunden
		  (if search-backward-found 
		      (progn 
			(backward-char (1- (length t1)))
			(setq message "I-search visible:")
			(setq point (point)))
		    (setq message "Failing I-search visible: ")))
	      ;; vorwärts:
	      ;;(if (= (length t1) 3)(debug))
	      ;; wenn noch eines gefunden in dieser Zeile: nett
	      (if first 
		  (progn
		    (forward-char 2)
		    (setq first nil))) ; wenn das erste cntl-s, dann nicht das schon gefundene nochmals suchen
	      (if (re-search-forward t1
				     (save-excursion (beginning-of-line)
						     (re-search-forward "\r" 
									nil 
									t))
				     t)
		  (progn
		    (setq message "I-search visible:")
		    (setq point (point)))
		;; sonst: ab der nächsten Zeile
		(forward-line 1)
		(if (re-search-forward t1
				       (save-excursion (beginning-of-line)
						       (re-search-forward "\r" 
									  nil 
									  t))
				       t)
		    (progn
		      (setq message "I-search visible:")
		      (setq point (point)))
		  (ding)
		  (if (= re 0)
		      (progn
			(message (concat "Failing I-search visible: " t1))
			(setq t1 (substring t1 (- (length t1)) -1)))
		    (setq message "Failing I-search visible: ")
		    )))))
	  (goto-char point)
	  ))
    ;; end of selective display stuff
    ;; this is short for invisible
    (let ((search-invisible search-invisible))
      (setq search-invisible nil)
      (isearch-forward))))

(defun byte-compiled-function_717089041 ()
  "A search function for Isearch only in visible text.
With selective-display properties (^M), there is no overwrap, but C-s or C-r 
for continuing, character is not accepted if search is failing, end with ESC.
If invisible properties mechanism, just an Isearch in visible regions."
  (interactive)
  (if (or (< emacs-major-version 20)
	  (not (listp buffer-invisibility-spec)))
      (let ((t1 "") (t2 0) (t3 0) (point (point)) (re 0) (first nil) (search-backward-found) (message "I-search visible:"))
	(while (not (= t2 27))		; falls nicht ESC
	  (setq t2 (if (< emacs-major-version 20)
		       (progn
			 (message (concat message t1))
			 (read-char))
		     (read-char (concat message t1))))
					;(if (>= (length t3) 3) (debug))
	  (if (= t2 127)		; ist ein DEL
	      ;; dann letzten Buchstaben wegstreichen
	      (progn
		(setq t1 (substring t1 (- (length t1)) -1))
		(re-search-backward t1 nil t)
		(setq re 0))
	    (if (= t2 19)		; weiteres search forward (cntl s)
		(progn
		  (if (<= re 0)         ; das erste cntl-s
		      (setq first t))   ; dann suche das schon erwischte nicht mit
		  (setq re 1))
	      (if (= t2 18)		; weiteres search backward (cntl r)
		  (setq re -1)
		;; re indiziert ein weiteres Suchen (<> 0)
		(setq re 0)
		;; wenn nicht, dann letzten Buchstaben dranhängen
		(setq t1 (concat t1 (char-to-string t2))))))
	  ;; und suchen
	  (save-excursion
	    (if (<= re 0)
		(progn
		  ;; while-Schleife zum Rückwärts-Finden (kein schöner Code, aber schnell)
		  (while (progn (if (= re 0)(not (forward-char 1)) t)
				(if (re-search-backward t1 nil t)
				    (progn
				      (setq search-backward-found t)
				      (if (= re 0)(setq point (point))))
				  (setq search-backward-found nil))
				(if (= re -1) (not (beginning-of-line)) t) 
				(re-search-forward (concat t1 "\\|\r") (save-excursion (end-of-line) (point)) t)
				(not (backward-char 1))
				(looking-at "\r"))) ; t wenn nicht gefunden
		  (if search-backward-found 
		      (progn 
			(if (= re -1)
			    (progn
			      (backward-char (1- (length t1)))
			      (setq message "I-search visible:")
			      (setq point (point)))
			  (goto-char point)
			  (forward-char (length t1))
			  (setq message "I-search visible:")
			  (setq point (point))
			  ))
		    (setq message "Failing I-search visible: ")))
	      ;; vorwärts:
	      ;;(if (= (length t1) 3)(debug))
	      ;; wenn noch eines gefunden in dieser Zeile: nett
	      (if first 
		  (progn
		    (forward-char 2)
		    (setq first nil))) ; wenn das erste cntl-s, dann nicht das schon gefundene nochmals suchen
	      (if (re-search-forward t1
				     (save-excursion (beginning-of-line)
						     (re-search-forward "\r" 
									nil 
									t))
				     t)
		  (progn
		    (setq message "I-search visible:")
		    (setq point (point)))
		;; sonst: ab der nächsten Zeile
		(forward-line 1)
		(if (re-search-forward t1
				       (save-excursion (beginning-of-line)
						       (re-search-forward "\r" 
									  nil 
									  t))
				       t)
		    (progn
		      (setq message "I-search visible:")
		      (setq point (point)))
		  (ding)
		  (if (= re 0)
		      (progn
			(message (concat "Failing I-search visible: " t1))
			(setq t1 (substring t1 (- (length t1)) -1)))
		    (setq message "Failing I-search visible: ")
		    )))))
	  (goto-char point)
	  ))
    ;; end of selective display stuff
    ;; this is short for invisible
    (let ((search-invisible search-invisible))
      (setq search-invisible nil)
      (isearch-backward))))


(if gnuemacsp
    (progn
      (define-key (current-local-map) [?\C-\S-S] 'byte-compiled-function_644676125)
      (define-key (current-local-map) [?\C-\S-R] 'byte-compiled-function_717089041))
  (define-key (current-local-map) [(control c) S] 'byte-compiled-function_644676125)
  (define-key (current-local-map) [(control c) R] 'byte-compiled-function_717089041))
  





;;;;;;;includeforelc!
;;;;;;(load "op-columns")



(load "picture")

(message "loading column-mode")


(defun byte-compiled-function_266316501 (endmarker)
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



(defun byte-compiled-function_131416192 ()
"Verschiebt in einem %%%C Zahl -Block spaltenweise nach rechts durch Einsetzen von Blanks"
  (interactive)
;;;
;;; insert space if not column-mode
;;;
  (let ((endmark) (save-column (current-column)) (coldum) (save-position (+ (point) 1)) (leng (length "%%%C"))
	(before-change-functions nil)(after-change-functions nil))
    (if (byte-compiled-function_266316501 'endmark)
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


(defun byte-compiled-function_319134576 ()
"Verschiebt in einem %%%C Zahl -Block spaltenweise nach rechts durch Loeschen von Zeichen"
  (interactive)
;;;
;;; insert space if not column-mode
;;;
  (let ((endmark) (save-column (current-column)) (coldum) (save-position (- (point) 1)) (leng (length "%%%C"))
	(before-change-functions nil)(after-change-functions nil))
    (if (byte-compiled-function_266316501 'endmark)
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


(defun byte-compiled-function_793717425 ()
  (interactive)
;;;
;;; insert space if not column-mode
;;;
  (let ((endmark) (save-column (current-column)) (coldum) (save-position (+ (point) tab-width)) (leng (length "%%%C"))
	(before-change-functions nil)(after-change-functions nil))
    (if (byte-compiled-function_266316501 'endmark)
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


(defun byte-compiled-function_592080834 (start end)
  "Delete rectangle, stay at point."
  (interactive "r")
  (setq killed-rectangle (delete-extract-rectangle start end))
  (exchange-point-and-mark))



(defun byte-compiled-function_588362739 ()
  (interactive)
  (let ((endmark) (save-position (+ (point) 1)))
    (if (byte-compiled-function_266316501 'endmark)
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


(defun byte-compiled-function_298730884 ()
  (interactive)
  (let ((endmark) (save-position (point)) (save-column (current-column)) (save-position (point)))
    (if (byte-compiled-function_266316501 'endmark)
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
      (local-set-key [(control down)] 'byte-compiled-function_588362739)
      (local-set-key [(control up)] 'byte-compiled-function_298730884)
      (local-set-key [(control right)] 'byte-compiled-function_131416192)
      (local-set-key [(control left)] 'byte-compiled-function_319134576)
      (local-set-key [(control tab)] 'byte-compiled-function_793717425))
  (local-set-key [C-down] 'byte-compiled-function_588362739)
  (local-set-key [C-up] 'byte-compiled-function_298730884)
  (local-set-key [C-right] 'byte-compiled-function_131416192)
  (local-set-key [C-left] 'byte-compiled-function_319134576)
  (local-set-key [C-tab] 'byte-compiled-function_793717425))

(local-set-key [?\C->] '(lambda () (interactive) (scroll-right 2) (backward-char 2)))
(local-set-key [?\C-<]	'(lambda () (interactive) (scroll-left 2) (forward-char 2)))
(local-set-key [(meta down)]	 '(lambda () (interactive) (save-excursion (scroll-next-line))))
(local-set-key [(meta-up)]	'(lambda () (interactive) (save-excursion (scroll-previous-line))))




(defvar column-ruler
  (concat "1   5    10	      20	30	  40	    5060	70	  80	    90	     100       110	 120	   130	     140       150	 160	   170	     180       190	 200	   210	     220\n"
	  "[   |    |	 |    |	   |	|    |	  |    |    |	 |    |	   |	|    |	  |    |    |	 |    |	   |	|    |	  |    |    |	 |    |	   |	|    |	  |    |    |	 |    |	   |	|    |	  |    |    |	 |    |\n")
  "*String displayed above current line by column-rouler.")



(defun byte-compiled-function_1011356371 ()
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
(defun byte-compiled-function_946158545 (&optional leiste setheadlinestring)
  "Inserts either the current headlines (without argument) or a
column ruler momentarily above current line, till next keystroke.
The ruler is defined by the value of column-ruler.
The key typed is executed unless it is SPC."
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
		  (byte-compiled-function_951328898 1)
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
			(setq headlines-string (concat (byte-compiled-function_1011356371) "\n" headlines-string)))))
	      (condition-case nil
		  (byte-compiled-function_951328898 1)
		(error "no previous headline"))
	      (setq headlines-string (concat (byte-compiled-function_1011356371) "\n" headlines-string))))
	(if (>= emacs-major-version 20) ;; because of fucking changes of bastards
	    (if (bobp)
		(progn
		  (byte-compiled-function_760238784 1)
		  (setq headlines-string (concat (byte-compiled-function_1011356371) headlines-string))
		  )))
	(while (not (looking-at "^[*][^*]"))
	  (byte-compiled-function_402232290 1)
	  (setq headlines-string (concat (byte-compiled-function_1011356371) "\n" headlines-string))))
      (setq headlines-string (concat "\nvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv\n"
				     headlines-string))
      (setq truncate-lines t)
      (if setheadlinestring
	  (setq op-current-headline-string headlines-string)
	(momentary-string-display headlines-string (save-excursion (beginning-of-line) (point))
				  nil "Type SPC or any command to erase ruler."))
      (setq truncate-lines truncate-lines-sv))))


(if gnuemacsp
    (local-set-key "\C-c\C-g" 'byte-compiled-function_946158545)
  (define-key (current-local-map) [(control c) g] 'byte-compiled-function_946158545)  ; Control-g im XEmacs funktioniert erst nach 
                                                                  ; timeout von C-c
  (define-key (current-local-map) [(control c) (control g)] 'byte-compiled-function_946158545))  ; trotzdem der Gewohnheit halber noch dazu




(defun byte-compiled-function_355644509 ()
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

  
(defun byte-compiled-function_61187635 (beg end)
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
	  (if (byte-compiled-function_355644509)
	      (setq sum (+ sum (byte-compiled-function_355644509))))
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
(defun byte-compiled-function_32143247 ()
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
      (local-set-key "\C-c\C-g" 'byte-compiled-function_946158545)
    (define-key (current-local-map) [(control c) g] 'byte-compiled-function_946158545))
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
    (local-set-key "\C-c\C-g" 'byte-compiled-function_946158545)
  (define-key outline-minor-mode-map [(control c) g] 'byte-compiled-function_946158545))



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
    


(defun byte-compiled-function_28951481 ()
  (interactive)
  (backward-char 1)
  (re-search-backward "^[*][^*]" nil t)
  (beginning-of-line))
(byte-compiled-function_493573711 "^" 'byte-compiled-function_28951481)


(defun byte-compiled-function_633388302 ()
  (interactive)
  (forward-char 1)
  (re-search-forward "^[*][^*]" nil t)
  (beginning-of-line))
(byte-compiled-function_493573711 "v" 'byte-compiled-function_633388302)


(defun byte-compiled-function_852782154 ()
  (interactive)
  (goto-char (point-min))
  (re-search-forward "^[*][^*]" nil t)
  (beginning-of-line))
(byte-compiled-function_493573711 "\M-^" 'byte-compiled-function_852782154)





(byte-compiled-function_493573711  "\M-s" 'byte-compiled-function_390307608)
;;## dies ist die alte Version vor dem Umstieg auf XEmacs, könnte noch einmal zu aktivieren sein,
;;## jedoch funktioniere bei Tests leider die Funktion "show-subtree" nicht, warum auch immer
;;(defun byte-compiled-function_390307608 (&optional called-noninteractive)
;;  (interactive "P")
;;  (let ((before-change-functions nil)(after-change-functions nil)
;;	(already-modified (buffer-modified-p)))
;;    (byte-compiled-function_182906083)
;;    (toggle-read-only t)(toggle-read-only)
;;    (save-excursion
;;      (byte-compiled-function_878090507)
;;      (byte-compiled-function_10309609 called-noninteractive)
;;      (setq selective-display t))
;;    (set-buffer-modified-p already-modified)))
(defun byte-compiled-function_390307608 (&optional called-noninteractive)
  (interactive)
  (let ((before-change-functions nil)(after-change-functions nil)
	(already-modified (buffer-modified-p)))
    (save-excursion	
      (beginning-of-line)
      (let ((already-modified (buffer-modified-p))(beg (point)))
	(byte-compiled-function_119382628)
	(if (not (re-search-forward "^*" nil t))
	    (point-max))
	(narrow-to-region beg (point))
	(goto-char beg)
	(byte-compiled-function_28022772)
	(toggle-read-only t)(toggle-read-only) ; gerade mal unklar: könnte auch eine Zeile 
	                                       ; obendran müssen, wahrscheinlich jedoch nicht
	(save-excursion
	  (byte-compiled-function_878090507)
	  (byte-compiled-function_10309609 called-noninteractive)
	  (setq selective-display t))
	(widen)
	(set-buffer-modified-p already-modified)))))



(byte-compiled-function_493573711  "\M-d" 'byte-compiled-function_490612792)
(defun byte-compiled-function_490612792 ()
  (interactive)
  (let ((before-change-functions nil)(after-change-functions nil))
  (save-excursion
    (beginning-of-line)
    (let ((already-modified (buffer-modified-p))(beg (point)))
      (byte-compiled-function_119382628)
      (if (not (re-search-forward "^*" nil t))
	  (point-max))
      (narrow-to-region beg (point))
      (goto-char beg)
      (byte-compiled-function_1000129612 t)
      (widen)
      (setq selective-display t)
      (set-buffer-modified-p already-modified)))))


(byte-compiled-function_493573711 "\M-k" 'byte-compiled-function_618522042)
(defun byte-compiled-function_618522042 ()
  (interactive)
  (let ((before-change-functions nil)(after-change-functions nil))
  (save-excursion
    (let ((already-modified (buffer-modified-p))(beg (point)))
      (byte-compiled-function_119382628)
      (narrow-to-region beg (point))
      (goto-char (point-min))
      (byte-compiled-function_63073901)
      (byte-compiled-function_263376788)
      (byte-compiled-function_675606449)
      (widen)
      (byte-compiled-function_10309609)
      (setq selective-display t)
      (set-buffer-modified-p already-modified)))))


(byte-compiled-function_493573711 "\M-j" 'byte-compiled-function_118285848)
(defun byte-compiled-function_118285848 ()
  (interactive)
  (let ((before-change-functions nil)(after-change-functions nil))
  (save-excursion
    (let ((already-modified (buffer-modified-p))(beg)(end))
      ;; stelle die letzte Hauptüberschrift fest
      (save-excursion
	(goto-char (point-max))
	(byte-compiled-function_951328898 1)
	(if (= (byte-compiled-function_779083082) 1)
	    (setq end (point))
	  (byte-compiled-function_402232290 100))
	(setq end (point)))
      (setq beg (point))
      (while 
	  (progn
	    (save-excursion
	      (byte-compiled-function_119382628)
	      (narrow-to-region beg (point))
	      (goto-char (point-min))
	      (byte-compiled-function_63073901)
	      (byte-compiled-function_263376788)
	      (byte-compiled-function_675606449)
	      (toggle-read-only t)(toggle-read-only)
	      (widen))
	    (byte-compiled-function_946680247)
	    (setq beg (point))
	    (not (= (point) end))))
      (save-excursion
	(byte-compiled-function_119382628)
	(narrow-to-region beg (point))
	(goto-char (point-min))
	(byte-compiled-function_63073901)
	(byte-compiled-function_263376788)
	(byte-compiled-function_675606449)
	(widen)
	(byte-compiled-function_10309609)
	(setq selective-display t)
	(set-buffer-modified-p already-modified))))))





(byte-compiled-function_493573711 "\M-l" 'byte-compiled-function_739904323)
(defun byte-compiled-function_739904323 ()
  (interactive)
  (let ((before-change-functions nil)(after-change-functions nil))
  (save-excursion
    (beginning-of-line)
    (byte-compiled-function_390307608 t)
    (byte-compiled-function_687492214))
    (byte-compiled-function_10309609)
    (setq selective-display t)))


(byte-compiled-function_493573711 "\M-e" 'byte-compiled-function_393298642)
(defun byte-compiled-function_393298642 ()
  (interactive)
  (let ((before-change-functions nil)(after-change-functions nil)
	(already-modified (buffer-modified-p))(beg (point)))
    (beginning-of-line)
    (if (< emacs-major-version 20)
	(while (re-search-forward "\r\\*" beg t)
	  (replace-match "\n\*" nil nil)))
    (byte-compiled-function_613274775)
    (setq beg (save-excursion
		(beginning-of-line)
		(point)))
    (toggle-read-only t)(toggle-read-only)
    (save-excursion
      (if (not (byte-compiled-function_912387168))
	  (byte-compiled-function_119382628)
;	(beginning-of-line)
;	(next-line 1))
	)  ;; testweise Änderung 8.9.1996
      (narrow-to-region beg (point))
      (goto-char (point-min))
      (byte-compiled-function_390307608 t)
      (widen)
      (byte-compiled-function_10309609)
      (setq selective-display t)
      (set-buffer-modified-p already-modified))))


(byte-compiled-function_493573711 "\M-c" 'byte-compiled-function_361096440)
(defun byte-compiled-function_361096440 ()
  (interactive)
  (let ((before-change-functions nil)(after-change-functions nil)
	(already-modified (buffer-modified-p)))
    (beginning-of-line)
    (byte-compiled-function_663981991)
    (toggle-read-only t)(toggle-read-only)
    (setq selective-display t)
    (beginning-of-line)
    (set-buffer-modified-p already-modified)))

(byte-compiled-function_493573711 "\M-a" 'byte-compiled-function_28022772)
(defun byte-compiled-function_28022772 ()
  (interactive)
  (let ((before-change-functions nil)(after-change-functions nil)
	(already-modified (buffer-modified-p)))
    (save-excursion
      (byte-compiled-function_720330687)
      (toggle-read-only t)(toggle-read-only)
      (setq selective-display t)
      (byte-compiled-function_263376788)
      (byte-compiled-function_10309609))
    (set-buffer-modified-p already-modified)))

; hide-body ungeaendert
(byte-compiled-function_493573711 "\M-t" 'byte-compiled-function_675606449)


;; zur Kontrolle
(message "9")



(byte-compiled-function_493573711 "\C-i" 'byte-compiled-function_657441801)
(defun byte-compiled-function_657441801 (&optional levels)
  (interactive "P")
  (let ((before-change-functions nil)(after-change-functions nil))
  (byte-compiled-function_619663136 levels)
  (save-excursion
    (let ((already-modified (buffer-modified-p))(beg (point))(last-found (point))
	  (anfang_offen)(still-to-search))
      (toggle-read-only t)(toggle-read-only)
      (byte-compiled-function_119382628)
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
      (byte-compiled-function_10309609)
      (setq selective-display t)
      (set-buffer-modified-p already-modified)))))


(byte-compiled-function_493573711 "\M-o" 'byte-compiled-function_1000129612)
(defun byte-compiled-function_1000129612 (&optional called-noninteractive)
  (interactive "P")
  ;;
  ;; hide-other im emacs19 ist buggy, daher der erhebliche Umstand in den nächsten Zeilen
  ;; hide-other im emacs20 habe ich nicht mehr kontrolliert
  ;;
  (let ((before-change-functions nil)(after-change-functions nil)
	(already-modified (buffer-modified-p))(beg (point))(last-found (point))
	(anfang-offen)(still-to-search))
    (byte-compiled-function_63073901)
    (byte-compiled-function_753010473)
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

      (byte-compiled-function_10309609 called-noninteractive)

      (setq selective-display t)
      (set-buffer-modified-p already-modified))))


(byte-compiled-function_493573711 "\M-n" 'byte-compiled-function_1070473924)
(defun byte-compiled-function_1070473924 ()
;; speziell wichtig geworden nach dem verkorksten outline-Mode des emacs20
  (interactive)
  (save-excursion 
    (beginning-of-buffer)
    (if (re-search-forward "^\\*")
        (progn
          (byte-compiled-function_1000129612)
          (byte-compiled-function_687492214)
          (next-line 1)
          (previous-line 1))
      (error "no headline found"))))

(byte-compiled-function_493573711 "\M-q" 'byte-compiled-function_569893557)
(defun byte-compiled-function_569893557 (levels)
  (interactive "P")
  (let ((before-change-functions nil)(after-change-functions nil)
	(already-modified (buffer-modified-p))(last-found (point))(anfang_offen (point))
	(still-to-search (point)))
; hide-other ist buggy, daher der erhebliche Umstand in den nächsten Zeilen
    (byte-compiled-function_63073901)
    (byte-compiled-function_983432855 levels)
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
      (set-buffer-modified-p already-modified))))






(byte-compiled-function_493573711 "\M-y" 'byte-compiled-function_746500617)
(defun byte-compiled-function_746500617 (&optional called-noninteractive)
  (interactive "P")
  (let ((before-change-functions nil)(after-change-functions nil)
	(already-modified (buffer-modified-p)))
  (byte-compiled-function_182906083)
;SMWt  (toggle-read-only t)(toggle-read-only)
  (save-excursion
    (byte-compiled-function_10309609 called-noninteractive)
    (setq selective-display t))
  (set-buffer-modified-p already-modified)))





(byte-compiled-function_493573711 "\M-f" 'byte-compiled-function_202695473)
(defun byte-compiled-function_202695473 ()
  (interactive)
  (let ((before-change-functions nil)(after-change-functions nil))
  (let ((thislevel (byte-compiled-function_779083082))(end)(tmpplace1)(tmpplace2))
    (save-excursion
      (goto-char (point-max))
      (byte-compiled-function_469010529)
      (setq end (point)))
    (setq tmpplace1 (point))
    (save-excursion
      (while
	  (progn
	    (byte-compiled-function_912387168)
	    (and (not (= (byte-compiled-function_779083082) thislevel))
		 (not (= (point) end)))))
      (if (= (byte-compiled-function_779083082) thislevel)
	  (setq tmpplace2 (point))
	(setq tmpplace2 tmpplace1)))
    (if (not (= tmpplace1 tmpplace2))
	(goto-char tmpplace2)
      (message "no next visible heading of the same level")))))


(byte-compiled-function_493573711 "\M-b" 'byte-compiled-function_724065022)
(defun byte-compiled-function_724065022 ()
  (interactive)
  (let ((before-change-functions nil)(after-change-functions nil))
  (let ((thislevel (byte-compiled-function_779083082))(begin)(tmpplace1)(tmpplace2))
    (save-excursion
      (goto-char (point-min))
      (byte-compiled-function_912387168)
      (setq begin (point)))
    (setq tmpplace1 (point))
    (save-excursion
      (while
	  (progn
	    (backward-char 1)
	    (byte-compiled-function_469010529)
	    (and (not (= (byte-compiled-function_779083082) thislevel))
		 (not (= (point) begin)))))
      (if (= (byte-compiled-function_779083082) thislevel)
	  (setq tmpplace2 (point))
	(setq tmpplace2 tmpplace1)))
    (if (not (= tmpplace1 tmpplace2))
	(goto-char tmpplace2)
      (message "no previous visible heading of the same level")))))




(defun byte-compiled-function_828488248 ()
  (interactive)
  (let ((before-change-functions nil)(after-change-functions nil))
  (save-excursion
    (let ((already-modified (buffer-modified-p)))
      (widen)
      (goto-char (point-min))
      (while (search-forward "\r" nil t)
	(replace-match "\n" nil t))
      (setq selective-display t)
      (set-buffer-modified-p already-modified)
      (byte-compiled-function_142055821)))
  (if window-system
      (byte-compiled-function_503165602 t))))

(defun byte-compiled-function_192743683 ()
  (interactive)
  (let ((before-change-functions nil)(after-change-functions nil))
    (if xemacsp (setq mark-active (mark)))
    (narrow-to-region (if mark-active (region-beginning)(point-min))
		      (if mark-active (region-end)(point-max)))
    (save-excursion
      (let ((already-modified (buffer-modified-p)))
	(goto-char (point-min))
	(while (search-forward "\r" nil t)
	  (replace-match "\n" nil t))
	(setq selective-display t)
	(set-buffer-modified-p already-modified)))
    (byte-compiled-function_142055821)
    (widen)
    (if window-system
	(byte-compiled-function_503165602 t))))

(defun byte-compiled-function_578165328 ()
"like show-really-everything, but trims buffer and contracts it again"
  (interactive)
  (let ((before-change-functions nil)(after-change-functions nil))
  (save-excursion
    (let ((already-modified (buffer-modified-p)))
      (widen)
      (goto-char (point-min))
      (while (search-forward "\r" nil t)
	(replace-match "\n" nil t))
      (setq selective-display t)
      (set-buffer-modified-p already-modified)))
  (byte-compiled-function_344550657)
  (byte-compiled-function_1000129612 t)
  (if window-system
      (byte-compiled-function_503165602 t))))





(defun byte-compiled-function_10309609 (&optional called-noninteractive)
  (let ((before-change-functions nil)(after-change-functions nil)
	(already-modified (buffer-modified-p)))
  (byte-compiled-function_923294036)
  (byte-compiled-function_231209922)
  ;; Mache den File schreibbar. Wichtig, da im outline ein Bug ist, der nicht mit der Version-Control zusammenarbeitet
  (toggle-read-only t)(toggle-read-only)
  (if (and (not called-noninteractive)
	   window-system)
      (byte-compiled-function_503165602 t))
  (set-buffer-modified-p already-modified)))




(defun byte-compiled-function_935586782 ()
"Sortiert die aktuelle Region nach den Überschriften der obersten verfügbaren Hierarchie.
 Die sortierte Region wird zur Kennzeichnung nicht farblich hervorgehoben"
  (interactive)
  (let ((before-change-functions nil)(after-change-functions nil))
  (save-excursion
    (narrow-to-region (region-beginning) (region-end))
    (goto-char (point-min))
    (re-search-forward "^\\*" nil t)
    (byte-compiled-function_1000129612)
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
;	(cons 'byte-compiled-function_661224210 outline-mode-map)
;	    minor-mode-map-alist))




;;;--------------------------------------------------------------------------------




(defun byte-compiled-function_358171230 (dummy comment-start-string comment-end-string)
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




(defun byte-compiled-function_938014524 (dummy comment-start-string comment-end-string)
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



(defun byte-compiled-function_353471660 (dummy)
; needs perl to be installed
  (interactive "sHit return to REMOVE C-like comments in the current buffer")
  (let ((before-change-functions nil)(after-change-functions nil))
    ;(save-excursion
    ;  (goto-char (if mark-active (region-beginning)(point-min)))
    ;  (insert-char ?\n 1))
    (save-excursion
      (if xemacsp (setq mark-active (mark)))
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
       "perl -e \"\\\$/ = undef;\\\$_ = <>;s#/\\\\*[^*]*\\\\*+([^/*][^*]*\\\\*+)*/|(\\\"(\\\\\\\\.|[^\\\"\\\\\\\\])*\\\"|'(\\\\\\\\.|[^'\\\\\\\\])*'|\\\\n+|.[^/\\\"'\\\\\\\\]*)#\\$2#g;print\" " t))))



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




(defun byte-compiled-function_396555531 (n)
  "Capitalizes the first letter of the word hat point is on or after"
  (interactive "p")
  (save-excursion
    (begin-of-word 1)
    (if (< n 0)
	(progn
	  (forward-word n)
	  (capitalize-word 1))
      (capitalize-word n))))
(defun byte-compiled-function_456927140 (n)
  "Capitalizes the first letter of the word hat point is on or after"
  (interactive "p")
  (save-excursion
    (begin-of-word 1)
    (if (< n 0)
	(progn
	  (forward-word n)
	  (downcase-word 1))
      (downcase-word n))))
(defun byte-compiled-function_358545611 (n)
  "Capitalizes the first letter of the word hat point is on or after"
  (interactive "p")
  (save-excursion
    (begin-of-word 1)
    (if (< n 0)
	(progn
	  (forward-word n)
	  (upcase-word 1))
      (upcase-word n))))
(defun byte-compiled-function_456927140 (n)
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
(defun byte-compiled-function_358545611 (n)
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
(define-key global-map "\M-l" 'byte-compiled-function_456927140)
(define-key global-map "\M-u" 'byte-compiled-function_358545611)
(define-key global-map "\M-L" 'byte-compiled-function_456927140)
(define-key global-map "\M-U" 'byte-compiled-function_358545611)
(define-key global-map "\M-C" 'byte-compiled-function_396555531)







;;; ********************************************************************************
;;;  Umlautiges	 ---  siehe auch noch op-main.el
;;; ********************************************************************************



(defun byte-compiled-function_461584340 (dummy)
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


(defun byte-compiled-function_309127593 (dummy)
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



(defun byte-compiled-function_344550657 ()
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
	  (while (byte-compiled-function_266316501 'endmark)
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


(defun byte-compiled-function_735214999 ()
  (interactive)
  (widen)
  (setq selective-display t)
  (if window-system
      (byte-compiled-function_503165602 t)))


;; zur Kontrolle
(message "12")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun byte-compiled-function_1061811665 (arg)
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
      (while (byte-compiled-function_912387168)
	(insert-char ?* arg)
	(re-search-forward "[ \t]+" nil t)
	(insert-char ?\t arg))
      (goto-char (point-min))
      (delete-char 1 nil))
    (byte-compiled-function_263376788)
    (widen)
    (byte-compiled-function_10309609))))
(byte-compiled-function_493573711 ">" 'byte-compiled-function_1061811665)


(defun byte-compiled-function_884664994 (arg)
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
      (while (byte-compiled-function_912387168)
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
    (byte-compiled-function_263376788)
    (widen)
    (byte-compiled-function_10309609))))
(byte-compiled-function_493573711 "<" 'byte-compiled-function_884664994)


; --------------------------------------------------------------------------------
; --------------------------------------------------------------------------------
;; zur Kontrolle
(message "13")



;;;----------------------------------------------------------------------------------------------------
;;;----------------------------------------------------------------------------------------------------
;;;----------------------------------------------------------------------------------------------------



(define-key (current-local-map) "\C-?" 'backward-delete-char-untabify) ; Backspace
(define-key outline-minor-mode-map "\C-?" 'backward-delete-char-untabify) ; Backspace


;; zur Kontrolle
(message "14")


;;;----------------------------------------------------------------------------------------------------
;;;----------------------------------------------------------------------------------------------------
;;;----------------------------------------------------------------------------------------------------



;
; Define Help-Key
;
(defun byte-compiled-function_756898759 ()
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
(byte-compiled-function_493573711 "\C-hm" 'byte-compiled-function_756898759)



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

      (byte-compiled-function_503165602 t)
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
               (byte-compiled-function_1070473924))))))




(message "16a")


;;;--------------------------------------------------------------------------------
;;;--------------------------------------------------------------------------------
;;;--------------------------------------------------------------------------------

;;
;;
;; kommt noch Mal bei op-menus.xemacs.el für die Menüs!!
;;
(local-set-key "\C-c\C-h" 'byte-compiled-function_460976248)
(local-set-key "\C-c\C-j" 'byte-compiled-function_999722354)
(local-set-key "\C-c\C-k" 'byte-compiled-function_527512458)

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



(defun byte-compiled-function_344605312 (min-to-app new-time appt-msg)
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

(setq appt-disp-window-function 'byte-compiled-function_344605312)


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





(defun byte-compiled-function_643324788 (&optional numberofdays startdate includingdiaryfile)
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
	(current-buf (current-buffer))(point)(pointsv)
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
      (byte-compiled-function_999263468 nil t)
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

      (byte-compiled-function_961933609)
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





(defun byte-compiled-function_714026089 ()
  (let ((project-to-diary-file (concat "*" (buffer-name) "*"))
	(current-buf (current-buffer))(point)(pointsv)
	(diary-file diary-file))
    (setq diary-file project-to-diary-file)
    (fset 'dummy-func (symbol-function 'find-buffer-visiting))
    (unwind-protect
	(progn
	  (fset 'find-buffer-visiting (symbol-function 'substitute-in-file-name))
	  (goto-char (point-min))
	  (byte-compiled-function_1051700708 (list 1 1 1997) 3))
      (fset 'find-buffer-visiting (symbol-function 'dummy-func)))
    (other-window -1)
    (setq truncate-lines t)
    (other-window 1)
    (switch-to-buffer current-buf)))




(defun byte-compiled-function_999263468 (&optional leiste setheadlinestring)
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
		(byte-compiled-function_951328898 1)
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
			  (setq headlines-string (concat (byte-compiled-function_1011356371) "\n" headlines-string)))))
		(byte-compiled-function_951328898 1)
		(setq headlines-string (concat (byte-compiled-function_1011356371) "\n" headlines-string))))
	  (while (not (looking-at "^[*][^*]"))
	    (setq selective-display nil)
	    (byte-compiled-function_402232290 1)
	    (setq headlines-string (concat (byte-compiled-function_1011356371) "\n" headlines-string))))
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

(defun byte-compiled-function_961933609 ()
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


(defun byte-compiled-function_460976248 ()
  (interactive)
  (let ((before-change-functions nil)(after-change-functions nil))
  (let ((calendar-date-display-form  '(day "." month "." year)))
    (insert (concat "[" signature "," (calendar-date-string (calendar-current-date)) "," (substring (current-time-string) 11 16) "]")))))

(defun byte-compiled-function_999722354 ()
  (interactive)
  (let ((before-change-functions nil)(after-change-functions nil))
  (let ((calendar-date-display-form  '(day "." month "." year)))
    (insert (concat "[" signature "," (calendar-date-string (calendar-current-date)) "]")))))

(defun byte-compiled-function_527512458 ()
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
(when (featurep 'menubar)
  (if gnuemacsp
      (progn
;;;;;;;includeforelc!
;;;;;;(load "op-menus.gnuemacs")



(defvar op-mode-menu-bar-map (make-sparse-keymap))


;; Eintragen in vorhandene Menüs

;; gefährlich !!

(define-key (current-global-map) "\M-s" 'byte-compiled-function_170283284)
(define-key (current-global-map) "\M-r" 'byte-compiled-function_226574873)

;; bei gecrypteten Files: nehme auch die .crypt-Funktionen in die richtigen Menus auf
(define-key menu-bar-files-menu [write-region-crypted]
  '("Save Region As..." . write-region-crypted))
(define-key menu-bar-files-menu [write-file]
  '("Save Buffer As..." . (lambda()(interactive)
			    (if use-keymap-for-crypted-files 
				(byte-compiled-function_199712133)
			      (write-file buffer-file-name)))))
(define-key menu-bar-files-menu [save-buffer]
  '("Save Current Buffer" . (lambda()(interactive)
		      (if use-keymap-for-crypted-files 
			  (byte-compiled-function_394138603)
			(byte-compiled-function_252209645)))))


(define-key menu-bar-help-menu [op-help]
  '("[OP] Outline-Project-Mode: Hilfe" . op-help))

;; bis hierher




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
  '("Set Mark [=Beginning Of Region, Cursor=End]" . set-mark-command))







;;; Muß nicht sein, hilft aber manchmal
;--------------------
  (define-key op-mode-menu-bar-map [OUTLINE]
    (cons "OP-Outline" (make-sparse-keymap "OUTLINE")))

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
  (cons "OP-CONV" (make-sparse-keymap "OP-CONV")))

(define-key op-mode-menu-bar-map [OP-CONV op-text-extractor]
  '("xfig-Text von Cursorposition extrahieren" . xfig-text-extractor))

(define-key op-mode-menu-bar-map [OP-CONV corell-text-extractor]
  '("Corell EPS-File von Cursorposition extrahieren" . corell-text-extractor))

(define-key op-mode-menu-bar-map [OP-CONV separator-others]
  '("--")) ; ------------------------------------------------------------

(define-key op-mode-menu-bar-map [OP-CONV mpx-text-extractor-4]
  '("MPX-File von Cursorposition Outline-Form mit Kommentaren	[neg. Argument]" . (lambda ()(interactive)(byte-compiled-function_257535182 -1))))
(define-key op-mode-menu-bar-map [OP-CONV mpx-text-extractor-3]
  '("MPX-File von Cursorposition Outline-Form ohne Kommentare	[\"-\"-Argument]" . (lambda ()(interactive)(byte-compiled-function_257535182 '-))))
(define-key op-mode-menu-bar-map [OP-CONV mpx-text-extractor-2]
  '("MPX-File von Cursorposition mit Kommentaren	[pos. Argument]" . (lambda ()(interactive)(byte-compiled-function_257535182 1))))
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
  (cons "OP-SEARCH" (make-sparse-keymap "OP-SEARCH")))

(define-key op-mode-menu-bar-map [OP-SEARCH isearch-backward-in-headings]
  '("Isearch Backward Only In Headings" . isearch-backward-in-headings))
(define-key op-mode-menu-bar-map [OP-SEARCH isearch-forward-in-headings]
  '("Isearch Forward Only In Headings" . isearch-forward-in-headings))
(define-key op-mode-menu-bar-map [OP-SEARCH search-backward-for-current-word-1]
  '("Backward To Current .Word." . (lambda()(interactive)(byte-compiled-function_502372405 t))))
(define-key op-mode-menu-bar-map [OP-SEARCH search-forward-for-current-word-1]
  '("Forward To Current .Word." . (lambda()(interactive)(byte-compiled-function_818994661 t))))

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
  (cons "OP-INCLUDE" (make-sparse-keymap "OP-INCLUDE")))

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
  (cons "OP-CVS" (make-sparse-keymap "OP-CVS")))


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
  (cons "OP-IFDEF" (make-sparse-keymap "OP-IFDEF")))

(define-key op-mode-menu-bar-map [OP-IFDEF hide-ifdef-block]
  '("Hide Enclosing Ifdef Blocks This Line" . hide-ifdef-block))
(define-key op-mode-menu-bar-map [OP-IFDEF show-ifdef-block]
  '("Show Enclosing Ifdef Blocks This Line" . show-ifdef-block))
(define-key op-mode-menu-bar-map [OP-IFDEF separator-thisblock]
  '("--")) ; ------------------------------------------------------------
(define-key op-mode-menu-bar-map [OP-IFDEF down-ifdef]
  '("Down Nested Visible Ifdef" . (lambda()(interactive)(byte-compiled-function_868121926)(search-forward "###" nil t))))
(define-key op-mode-menu-bar-map [OP-IFDEF up-ifdef]
  '("Up Nested Visible Ifdef" . (lambda()(interactive)(byte-compiled-function_421855559)(search-forward "###" nil t))))
(define-key op-mode-menu-bar-map [OP-IFDEF backward-ifdef]
  '("Beginning of Previous Visible Block This Level" .  (lambda()(interactive)(backward-ifdef)(search-forward "###" nil t))))
(define-key op-mode-menu-bar-map [OP-IFDEF forward-ifdef]
  '("End Of Next Visible Block This Level" . (lambda()(interactive)(byte-compiled-function_726788499)(search-forward "###" nil t))))
(define-key op-mode-menu-bar-map [OP-IFDEF previous-ifdef]
  '("Previous Ifdef Statement" .  (lambda()(interactive)(byte-compiled-function_306169596)(search-forward "###" nil t))))
(define-key op-mode-menu-bar-map [OP-IFDEF next-ifdef]
  '("Next Ifdef Statement" . (lambda()(interactive)(byte-compiled-function_190032434)(search-forward "###" nil t))))

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

(defun byte-compiled-function_617496182 ()
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
  (cons "OP-MISC" (make-sparse-keymap "OP-MISC")))


(define-key op-mode-menu-bar-map [OP-MISC p]
  '("OP Picture-Mode" . p))
(define-key op-mode-menu-bar-map [OP-MISC project-schedule-1]
  '("OP Vollkalender" . (lambda()(interactive)(byte-compiled-function_643324788 nil nil t))))
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
  '("automatic change mode OFF (for links only)" . (lambda()(interactive)(byte-compiled-function_156474084 t)(byte-compiled-function_156474084))))
(define-key op-mode-menu-bar-map [OP-MISC atm-on]
  '("Automatic Change Mode ON  (for links only)" . (lambda()(interactive)(byte-compiled-function_156474084 t))))

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
  (cons "OP-HEADINGS" (make-sparse-keymap "OP-HEADINGS")))

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
  (cons "OP-SHOW" (make-sparse-keymap "OP-SHOW")))

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

	))
  (if xemacsp
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
;;;;			  ["Save Test" (byte-compiled-function_51293477)]))
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
;;;;(defun byte-compiled-function_546619894 (map)
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



(add-menu-button '("File") ["Save Region Cryptet As..." (byte-compiled-function_51293477)])

(add-menu-button '("File") ["Save As..." 
			    (lambda()(interactive)
			      (if use-keymap-for-crypted-files 
				  (byte-compiled-function_199712133)
				(write-file buffer-file-name)))])
(add-menu-button '("File") ["Save Current Buffer"
			  (lambda()(interactive)
			    (if use-keymap-for-crypted-files 
				(byte-compiled-function_394138603)
			      (byte-compiled-function_252209645)))])
(add-menu-button '("Help") ["[OP] Outline-Project-Mode: Hilfe" (byte-compiled-function_756898759)])

;; bis hierher

(add-menu-button '("Edit") ["Backward Current Word" (byte-compiled-function_226574873)])
(add-menu-button '("Edit") ["Forward Current Word"  (byte-compiled-function_170283284)])


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
		   ["Outline Previous Same Level" outline-backward-same-level t]
		   ["Outline Next Same Level" outline-forward-same-level t]
		   ["Outline Previous" outline-previous-visible-heading t]
		   ["Outline Next" outline-next-visible-heading t]
		   ["Outline Up" outline-up-heading t]
		   ))

;--------------------

(defvar op-conv
;      (cons "OP-CONV" (make-keymap "OP-CONV"))
;    (cons "OP-CONV" (make-sparse-keymap "OP-CONV"))))
	'(	   ["xfig-Text von Cursorposition extrahieren" xfig-text-extractor t]
		   ["Corell EPS-File von Cursorposition extrahieren" corell-text-extractor t]
		   "----"
		   ["MPX-File von Cursorposition Outline-Form mit Kommentaren	[neg. Argument]"
		    ((lambda ()(interactive)(byte-compiled-function_257535182 -1))) t]
		   ["MPX-File von Cursorposition Outline-Form ohne Kommentare	[\"-\"-Argument]" 
		    ((lambda ()(interactive)(byte-compiled-function_257535182 '-))) t]
		   ["MPX-File von Cursorposition mit Kommentaren	[pos. Argument]" 
		    ((lambda ()(interactive)(byte-compiled-function_257535182 1))) t]
		   ["MPX-File von Cursorposition extrahieren" mpx-text-extractor t]
		   "----"
		   ["Import this BrainForest file	(M-x from_BF)" from_BF t]
		   ["Export this OP file to BrainForest	(M-x to_BF)"   to_BF t]
		   ))


;--------------------

(defvar op-search
;      (cons "OP-SEARCH" (make-keymap "OP-SEARCH"))
;    (cons "OP-SEARCH" (make-sparse-keymap "OP-SEARCH"))))
	'(	   ["Isearch Backward Only In Headings" isearch-backward-in-headings t]
		   ["Isearch Forward Only In Headings" isearch-forward-in-headings t]
		   ["Backward To Current .Word." ((lambda()(interactive)(byte-compiled-function_502372405 t))) t]
		   ["Forward To Current .Word." ((lambda()(interactive)(byte-compiled-function_818994661 t))) t]

		   ["I-search Backward Visible Only" isearch-visible-backward t]
		   ["I-search Forward Visible Only" isearch-visible-forward t]

		   ["Backward Word Expand To Visible" search-word-backward-expand-to-visible t]
		   ["Forward Word Expand To Visible" search-word-forward-expand-to-visible t]
		   ["Backward Current Word" search-this-string-backward t]
		   ["Forward Current Word" search-this-string-forward t]
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
		    ((lambda()(interactive)(byte-compiled-function_868121926)(search-forward "###" nil t)))]
		   ["Up Nested Visible Ifdef" 
		    ((lambda()(interactive)(byte-compiled-function_421855559)(search-forward "###" nil t)))]
		   ["Beginning of Previous Visible Block This Level" 
		    ((lambda()(interactive)(backward-ifdef)(search-forward "###" nil t)))]
		   ["End Of Next Visible Block This Level" 
		    ((lambda()(interactive)(byte-compiled-function_726788499)(search-forward "###" nil t)))]
		   ["Previous Ifdef Statement" 
		    ((lambda()(interactive)(byte-compiled-function_306169596)(search-forward "###" nil t)))]
		   ["Next Ifdef Statement" 
		    ((lambda()(interactive)(byte-compiled-function_190032434)(search-forward "###" nil t)))]
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
		   ["Insert Ifdef End" 
		    ((lambda()(interactive) (insert "  ###bis hierher\n")(backward-char 1)))]
		   ["Insert Elseif" 
		    ((lambda()(interactive) (insert "  ###dagegen jedoch nicht\n")(backward-char 1)))]
		   ["Insert Ifdef Begin" insert-ifdef-begin t]
		   ))

(defun byte-compiled-function_617496182 ()
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
		   ["OP Vollkalender"      ((lambda()(interactive)(byte-compiled-function_643324788 nil nil t)))]
		   ["OP Projektzeitplan"   project-schedule t]
		   ["OP Kalenderplaner 	d" d t]
		   ["OP Kalender in einem extra Fenster" calendar-in-extra-window t]
		   ["OP Kalender       	c" c t]
		   "----"
		   ["Automatic Change Mode OFF (for links only)" 
		    ((lambda()(interactive)(byte-compiled-function_156474084 t)(byte-compiled-function_156474084)))]
		   ["Automatic Change Mode ON  (for links only)" 
		    ((lambda()(interactive)(byte-compiled-function_156474084 t)))]
		   "----"
		   ["Eval Hypermark" select-hyper-func t]
		   "----"
		   ["Reduce To One Whitespace" fixup-whitespace t]
		   ["Insert TAB" tab-to-tab-stop t]
		   "----"
		   ["Format Region Softly (with included files)" op-softindent-region t]
		   ["Format Region OP-Mode Like" op-indent-region t]
		   ["Force Indent Line" force-indent t]
		   ["Headline Indent" indent-like-last-headline t]
		   ["Standard Indent" re-indent t]
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
	'(	   ["Toggle Everything In Region [...]" ausblend-0 t]
		   ["Toggle F[ällt]W[eg] In Region " ausblend-1 t]
		   ["Toggle S[tändig] W[iederkehrendes] In Region " ausblend-2 t]
		   ["Toggle V[erschobenes] In Region " ausblend-2 t]
		   ["Toggle R[essourcen]" ausblend-4 t]
		   ["Toggle E[rledigtes] In Region" ausblend-1 t]
		   ["Toggle D[ringendes] In Region" ausblend-3 t]
		   ["Toggle T[erminabsprachen] In Region " ausblend-5 t]
		   "----"
		   ["Shift Region Down" op-shift-right t]
		   ["Shift Region Up" op-shift-left t]
		   "----"
		   ["Forward Same Level" outline-forward-jump-same-level t]
		   ["Backward Same Level" outline-backward-jump-same-level t]
		   ["Previous Same Level" outline-backward-same-level t]
		   ["Next Same Level" outline-forward-same-level t]
		   ["Previous" outline-previous-visible-heading t]
		   ["Next" outline-next-visible-heading t]
		   ["Up" outline-up-heading t]
		   ["Top" top-heading t]
		   ["Next Top" next-top-heading t]
		   ["Very Top" very-top-heading t]
		   ))

;--------------------

(defvar op-show
	'(	   ["Hide All" hide-all t]
		   ["Hide All Others" hide-other-hide t]
		   ["Hide Subtree" hide-subtree-hide t]
		   ["Only Headings" hide-body t]
		   ["Hide All Deeper Entries" hide-leaves-hide t]
		   ["Hide Entry" hide-entry-hide t]
		   "----"
		   ["Sublevels Till Level [ARG]" hide-sublevels-hide t]
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


;; Update von allen Menu-Bars
(set-menubar-dirty-flag)


(add-menu nil "OP-%_Search" op-search) ;S
(add-menu nil "OP-Sh%_ow" op-show) ;O
(add-menu nil "Out%_line" outline) ;L
(add-menu nil "OP-%_Headings" op-headings) ;H
(add-menu nil "OP-%_Invisible" op-invisible) ;I
(add-menu nil "OP-%_Misc" op-misc) ;M
(add-menu nil "OP-I%_nclude" op-include) ;N
(add-menu nil "OP-If%_Def" op-ifdef) ;D
(add-menu nil "OP-%_CVS" op-cvs) ;C
(add-menu nil "OP-Con%_v" op-conv) ;V
                                   ; SOLHIMNDCV

;;; Menüs vom outline Mode
(delete-menu-item '("Headings"))
(delete-menu-item '("Show"))
(delete-menu-item '("Hide"))


;;*****************************************************************************


(add-menu-button '("Help") ["[OP] Hyperlink Hilfe" (byte-compiled-function_795360437)])

(defun byte-compiled-function_795360437 ()
  (interactive)
  (switch-to-buffer "*op-hyperlink help*")
  (setq truncate-lines t)
  (insert-string "

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Kettenausführungen im Projekt-Mode des emacs
;

   ->FILE:op-hyper.el*defun;EVAL:date
   ->FILE:op-hyper.el*defun->;EVAL:date

;;;;;;;;;;;;;;; Beispielliste aus den Kommentaren (z.T. erweitert) ;;;
;
; ->>../op-project.el*defun
; Beispiele:
; 
;   ->>Wort sucht alle vorkommenden ganzen Wörter, ->Wort dieses nur in Überschriften
;                                                    {(AUßER DIESE EXISTIERT NICHT, dann wird der Pfeil wie ein 
;                                                     ->> behandelt !! (Grund: bessere Optik im File))
;                                                     Dieses Verhalten, beschrieben hier in der
;                                                     geschweifte Klammer kann durch vertauschen der
;                                                     Funktionen FILE:func und FILE:func2 geändert werden, siehe
;                                                     Kommentar über FILE:func2.}
;   ->*Wort dieses nur am Anfang einer Einser-Überschrift, entsprechend ->**Wort bei einer Zweier-Überschrift
;   ->Name* sucht in File \"Name\"
; 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


   ->>FILE:../op-project.el*defun     ; mehrmals auslösen gibt jeweils das nächste Muster
   ->>FILE:../op-project.el*defun->
   ->>FILE:defun
   ->>FILE:defun->
   ->>FILE:../op-project.el*          ; Sucht nur den anderen File aus
   ->>defun                           ; Defaultbutton: sucht im gleichen File das Wort \"defun\"
   ->>defun                           ; Defaultbutton: sucht im gleichen File das Wort \"defun\"
   ->>defun->                         ; dito, setzt den Cursor aber in ein anderes Fenster
   ->FILE:../op-project.el*defun      ; mehrmals auslösen gibt jeweils das nächste Muster
   ->FILE:../op-project.el*defun->
   ->FILE:defun
   ->FILE:defun->
   ->FILE:../op-project.el*           ; Sucht nur den anderen File aus
   ->defun                            ; Defaultbutton: sucht im gleichen File das Wort \"defun\"
   ->defun                            ; Defaultbutton: sucht im gleichen File das Wort \"defun\"
   ->defun->                          ; dito, setzt den Cursor aber in ein anderes Fenster
   ->>../op-project.el*               ; sucht den File ../op-project.el heraus
   ->../op-project.el*                ; sucht den File ../op-project heraus (-> geht hier nur, weil kein Einspring-
                                      ;           punkt vorhanden ist, eigentlich muesste hier ->> stehen

   ->../op-project.el                 ; doppelt verkürzte Form: wie oben, jedoch kann auch noch der Stern
                                      ; weggelassen werden, da hier der File über den / erkannt wird
   ->../op-project.el->               ; doppelt verkürzte Form: wie oben, jedoch kann auch noch der Stern
                                      ; weggelassen werden, da hier der File über den / erkannt wird
   ->op-consistent.el                 ; doppelt verkürzte Form: wie oben, jedoch kann auch noch der Stern
                                      ; weggelassen werden, da hier der File über den . und Buchstabe 
                                      ; als Verweis auf eine Extention gesucht wird
   ->op-consistent.el->               ; doppelt verkürzte Form: wie oben, jedoch kann auch noch der Stern
                                      ; weggelassen werden, da hier der File über den . und Buchstabe 
                                      ; als Verweis auf eine Extention gesucht wird

   ->>../op-project.el*defun->        ; in anderem File
   ->>FILE:[ab]test                   ; alle Buttons dürfen Regexe enthalten, jedoch keine Blanks
        atest btest ctest             ;    (bleibt bei atest und btest stehen, kennt aber kein ctest)
   ->>FILE:\"a test\"->                 ; Blanks usw. sind erlaubt, doch nur in Anführungsstrichen  (\"a test\")
   ->>FILE:\"\* test\"                  ; Achtgeben: Sterne gehören zu den Regexen, müssen daher gequotet werden
   ->>FILE:\"\* test\"->
   * test    $ test                   ; nur zum Testen

   ->>A:Müller                        ; private Kurzform für ->FILE:adressen.op*\"name\", Adressenfile als Variable hier im Code

   ->**Ueberschriftentest             ; steht ein * am Anfang des Musters, werden die Sterne nicht als Regexe interpretiert,
                                      ;    sondern als \"Uberschriftengerechtes Muster\" (hier: `**	Ueberschriftentest')
                                      ;    Verhalten: ist das Muster am Zeilenanfang zu finden, wird es auch dort genommen,
                                      ;    ansonsten geht es durch den normalen Text.
 
   ->Ueberschriftentest
   ->FILE:Ueberschriftentest

**	Ueberschriftentest

;
; Beispiele FUNC:
;  
   ->FUNC:funcfile*startpunkt
   ->FUNC:funcfile*Fakultät
   ->FUNC:startpunkt

;
; Beispiele EVAL:
;  
   ->EVAL:xeyes
   ->EVAL:xeyes&
   ->EVAL:\"date --version\"  
   ->EVAL:\"gcc -v\"
   ->EVAL:xfig*test.fig
   ->EVAL:\"xfig -bg red\"*test.fig    ; Argumente in Anführungsstrichen, Ausführungspunkt NUR vor dem 
                                     ; ersten Blank (beim \"->EVAL:xxxx\"
   ->EVAL:\"xfig -bg red\"*test.fig&   ; wie eben
   ->FILE:op-hyper.el*defun;EVAL:\"date --version\"  
   ->FILE:op-hyper.el*defun->;EVAL:\"date --version\"  
				      ; bei Kettenargumenten: EVAL immer an den Schluß,
                                     ;  andere Funktionen immer mit *-Zeichen. Nur je ein Buttontyp ist erlaubt.
   ->EVAL:test.fig                   ; Defaultextention ist einprogrammiert (siehe oben)
   ->!date                           ; Defaultbuttons
   ->!\"date --version\"
   ->!cat*op-hyper.el
  
;
; Beispiele REF:
;
   ->REF:../op-hyper/op-hyper.el*1button      ; Muster ist hier \"1button\", kann aber beliebig sein
   ->REF:../op-hyper/op-hyper.el*1button->
   ->REF:1button                              ; auch hier: mehrmaliges Auslösen ohne Bewegen des 
                                              ; Cursors im anderen Fenster sucht nächste Referenz
   ->REF:1button->

 die alle sollten hier enden:   ->1button:FILE:defun   ; Doppelpunkt ist günstig, aber nicht Pflicht:
                                ->1button#FILE:defun   ; geht z. B. auch

;
; Beispiele SQL:
;  
   ->SQL:sqlfile.sql*startpunkt
   ->SQL:sqlfile2.sql


;
; Beispiele (siehe auch die bei REF:func) REF-R:
;
   ->REF-R:../op-hyper/op-hyper.el*2button->      ; Muster ist hier \"2button\", kann aber beliebig sein

   ->REF-R:2button->

   ->2button:REF-R:3button->

 die alle sollten hier enden:   ->3button:FILE:das_hier_gibts_nicht   

; weiteres Beispiel: Loop zurück zum ersten Button wird nicht unendlich durchlaufen 
; (ideal zum Einlesen vieler Files !!!)

   ->4button:REF-R:5button->           
        ->5button:REF-R:6button->
             ->6button:REF-R:4button->

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


;;
;; Stichworte aus font-lock.el
;;


;;; laden der Faces
(require 'custom)
(require 'cus-edit)


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
	    (defconst op-font-lock-keywords
	      (list

	       '("^\\(\\*E\\|\\*FW\\).*[\r\n]" . widget-button-pressed-face)       ; Erledigtes in Hauptüberschrift
	       '("^\\(\\*\\*+E\\|\\*+FW\\).*[\r\n]" . gnus-group-news-2-empty-face); Erledigtes in Unterüberschrift
					                                           ;(generelle Doku: in op-menus.el)
	       '("^\\(\\*+V\\|\\*+S\\).*[\r\n]" . gnus-group-mail-3-face)          ; Verschobenes und ständig 
					                                           ;              wiederkehrendes
	       '("^\\*+\\-.*[\r\n]" . gnus-group-news-2-face)			   ; *---- - Zeilen
	       '("^\\*+\\+.*[\r\n]" . message-header-to-face)			   ; *++++ - Zeilen
	       '("^\\*+|.*[\r\n]" . custom-invalid-face)			   ; *|||| - Zeilen
	       '("^\\*+=.*[\r\n]" . display-time-mail-balloon-enhance-face)	   ; *==== - Zeilen
	       '("^\\*[^*+=-EFVDSR].*[\r\n]" . gnus-summary-high-ticked-face)      ; Hauptüberschrift ; F != FW !!!
	       '("^\\*\\*[^*+=-EFVDSR].*[\r\n]" . message-header-to-face)             ; **-Unterüberschrift ; F != FW !!!
	       '("^\\*\\*\\*+[^*+=-EFVDSR].*[\r\n]" . gnus-summary-high-unread-face)  ; Unterüberschriften
	       '("^\\*+D.*[\r\n]" . font-lock-warning-face)			   ; Dringendes

	       ;; speziell für MPX-MSP
	       '("{Nr\\..*}" . gnus-group-news-low-empty-face)                     ; goldenrot wäre besser
	       '("{Verantwortlich.*}" . gnus-group-news-low-empty-face)


	       ;; Doppelzeilenkombination für golden unterlegt -> xemacs: rot grau unterlegt
	       '("#T.*\\(#\\|[\r\n]\\)" . modeline-mousable)
	       '("#R.*\\(#\\|[\r\n]\\)" . custom-comment-tag-face)

	       ;; dunkelgrün
	       '("#[^TR].*\\(#\\|[\r\n]\\)" . font-lock-doc-string-face)

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
;; annulliere das emacs20-Kommando zum hiliten
(if xemacsp
    (defun byte-compiled-function_503165602 (&optional var)
      t))

;; und mache ein anderes hilit-repaint-command auf control-shift-l
(if xemacsp
    (progn
      (defun byte-compiled-function_126678878 (&optional var)
	(interactive)
	(save-excursion
	  (font-lock-fontify-region (point-min) (point-max))))

      (defun byte-compiled-function_982980884 ()
	(interactive)
	(save-excursion
	  (font-lock-fontify-region (region-beginning) (region-end))))))
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
(define-key outline-minor-mode-map [(control L)] 'byte-compiled-function_126678878)
(define-key outline-minor-mode-map [(control meta L)] 'byte-compiled-function_982980884)



(setq font-lock-keywords op-font-lock-keywords )
(put 'byte-compiled-function_382702076 'font-lock-defaults '(op-font-lock-keywords))


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

;      (byte-compiled-function_503165602 t)
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
      (if xemacsp
	  (byte-compiled-function_126678878)
	(byte-compiled-function_503165602))
      (byte-compiled-function_1000129612)
      (if (re-search-forward "^\\*" nil t)
	  (progn
	    (beginning-of-line)))))



(message "... und fertig mit dem Project-mode")
