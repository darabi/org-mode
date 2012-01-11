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
