;;; org-depends.el --- Hierarchical TODO dependencies for Org-mode
;; Copyright (C) 2008 Free Software Foundation, Inc.
;;
;; Author: John Wiegley <johnw at newartisans dot com>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://www.newartisans.com/
;; Version: 1.0
;;
;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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

(require 'org)

(defun org-depends-block-todo (change-plist)
  "Block turning an entry into a TODO.
This checks whether the current task should be blocked from state
changes.  Such blocking occurs when:

  1. The task has children which are not all in a completed state.

  2. A task has a parent with the property :ORDERED:, and there
     are siblings prior to the current task with incomplete
     status."
  (catch 'dont-block
    ;; If this task has children, and any are undone, it's blocked
    (save-excursion
      (outline-back-to-heading)
      (let ((this-level (funcall outline-level)))
	(outline-next-heading)
	(let ((child-level (funcall outline-level)))
	  (while (and (not (eobp))
		      (> child-level this-level))
	    ;; this todo has children, check whether they are all
	    ;; completed
	    (if (and (not (org-entry-is-done-p))
		     (org-entry-is-todo-p))
		(throw 'dont-block nil))
	    (outline-next-heading)
	    (setq child-level (funcall outline-level))))))
    ;; Otherwise, if the task's parent has the :ORDERED: property, and
    ;; any previous siblings are undone, it's blocked
    (save-excursion
      (outline-back-to-heading)
      (when (save-excursion
	      (ignore-errors
		(outline-up-heading 1)
		(org-entry-get (point) "ORDERED")))
	(let* ((this-level (funcall outline-level))
	       (current-level this-level))
	  (while (and (not (bobp))
		      (= current-level this-level))
	    (outline-previous-heading)
	    (setq current-level (funcall outline-level))
	    (if (= current-level this-level)
		;; this todo has children, check whether they are all
		;; completed
		(if (and (not (org-entry-is-done-p))
			 (org-entry-is-todo-p))
		    (throw 'dont-block nil)))))))
    t))					; don't block

(add-hook 'org-blocker-hook 'org-depends-block-todo)

(defface org-depends-dimmed-todo-face
  '((((background light)) (:foreground "grey50"))
    (((background dark)) (:foreground "grey50")))
  "Face used to hide leading stars in headlines.
The foreground color of this face should be equal to the background
color of the frame."
  :group 'org-depends)

(defun org-depends-dim-blocked-todos ()
  "Dim currently blocked TODO's in the agenda display."
  (interactive)
  (mapc (lambda (o) (if (eq (org-overlay-get o 'org-type) 'org-blocked-todo)
		   (org-delete-overlay o)))
	(org-overlays-in (point-min) (point-max)))
  (save-excursion
    (let ((inhibit-read-only t)
	  b e p ov h l)
      (goto-char (point-min))
      (while (let ((pos (next-single-property-change (point) 'todo-state)))
	       (and pos (goto-char (1+ pos))))
	(let ((marker (get-text-property (point) 'org-hd-marker)))
	  (when (and marker
		     (not (with-current-buffer (marker-buffer marker)
			    (save-excursion
			      (goto-char marker)
			      (org-depends-block-todo (list :type 'todo-state-change
							   :position marker
							   :from "TODO"
							   :to "DONE"))))))
	    (setq b (point) e (point-at-eol)
		  ov (org-make-overlay b e))
	    (org-overlay-put ov 'face 'org-depends-dimmed-todo-face)
	    (org-overlay-put ov 'org-type 'org-blocked-todo)))))))

(add-hook 'org-finalize-agenda-hook 'org-depends-dim-blocked-todos)

(provide 'org-depends)

;;; org-depends.el ends here
