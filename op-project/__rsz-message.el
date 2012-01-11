
;;
;;<http://www.splode.com/~friedman/software/emacs-lisp/src/rsz-message.el>
;;

;;; rsz-message.el --- dynamically resize echo area to fit messages

;;; Copyright (C) 1993 Noah S. Friedman

;;; Author: Noah Friedman <friedman@prep.ai.mit.edu>
;;; Maintainer: friedman@prep.ai.mit.edu
;;; Keywords: extensions, window display
;;; Status: requires GNU Emacs 19.  Kludgy and experimental.

;;; $Id: rsz-message.el,v 1.2 1994/03/31 18:32:20 friedman Exp $

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, you can either send email to this
;;; program's maintainer or write to: The Free Software Foundation,
;;; Inc.; 675 Massachusetts Avenue; Cambridge, MA 02139, USA.

;;; Commentary:

;;; The echo area is one of the more poorly implemented features in Emacs.
;;; It is simply a static array of characters which is copied into by
;;; functions like `message'.  If the message is longer than the allocated
;;; echo area or the frame width, the rest of the message is discarded.

;;; This package implements the echo area on top of the minibuffer instead,
;;; resizing the minibuffer when necessary.  This is not the proper
;;; solution to the problem, but it is a stopgap measure.

;;; There is one serious, unavoidable problem with this implementation.
;;; Because the minibuffer display won't be updated until a sit-for or
;;; sleep-for happens, there is a redefinition of the `message' function
;;; which does this.  Unfortunately, that means that any function which
;;; calls `message' may be interrupted by a process filter, which probably
;;; breaks some important atomicity assumptions.

;;; Also, `message' is sometimes called from C primitives, which means some
;;; messages won't be resized.

;;; Code:


;; Do not set this variable directly.  Use the `rsz-message-enable',
;; `rsz-message-disable', or `rsz-message-toggle' functions instead.
(defvar rsz-message-enabled-p t)

;; These are internal and kludgy; don't frob them.
(defvar rsz-message-reg-beg nil)
(defvar rsz-message-reg-end nil)
(defvar rsz-message-win-height nil)
(make-variable-buffer-local 'rsz-message-reg-end)
(make-variable-buffer-local 'rsz-message-reg-end)
(make-variable-buffer-local 'rsz-message-win-height)

;; Save original definition
(and (subrp (symbol-function 'message))
     (fset 'message-subr (symbol-function 'message)))

;;;###autoload
(defun rsz-message (&optional fmt &rest args)
  "Print a message at the bottom of the screen.
The arguments to this function are the same as to the `format' function.

This function was provided by `rsz-message'.  
Normally the echo area and the minibuffer share the same portion of the
screen but are separate.  Since the echo area cannot be resized or wrapped,
this function temporarily inserts the message in a narrowed region of the
minibuffer window instead.

The `message' function is called from some C primitives which ignore any
lisp redefinition.  As a result, some messages may not properly size the
window.  There is nothing that can be done about this.  fnord"
  (rsz-message-cleanup)
  (and fmt
       (let* ((msg (apply 'format fmt args))
              buffer-undo-list)
         (save-excursion
           (save-window-excursion
             (select-window (minibuffer-window))
             (setq rsz-message-reg-beg (point-min)
                   rsz-message-reg-end (point-max)
                   rsz-message-win-height (window-height))
             (narrow-to-region (point) (progn (insert msg) (point))))
           (rsz-message-resize-window rsz-message-resize-window-exactly))
         (add-hook 'pre-command-hook 'rsz-message-resize-window-before-command)
         (or (input-pending-p) (sit-for 0))
         msg)))

(defun rsz-message-cleanup ()
  (let ((orig-window (selected-window))
        buffer-undo-list)
    (save-excursion
      (unwind-protect
          (progn
            (select-window (minibuffer-window))
            (and rsz-message-reg-beg
                 rsz-message-reg-end
                 (progn
                   (delete-region (point-min) (point-max))
                   (widen)
                   (narrow-to-region rsz-message-reg-beg rsz-message-reg-end)
                   (setq rsz-message-reg-beg nil
                         rsz-message-reg-end nil)))
            (and rsz-message-win-height
                 (progn
                   (enlarge-window 
                    (min 0 (- rsz-message-win-height (window-height))))
                   (setq rsz-message-win-height nil))))
        (select-window orig-window)
        (message-subr nil)))))

(defun rsz-message-resize-window-before-command ()
  (rsz-message-cleanup)
  (setq pre-command-hook 
        (delq 'rsz-message-resize-window-before-command pre-command-hook)))


;;;###autoload
(defun rsz-message-disable ()
  "Use normal input display in the minibuffer.
See help for `rsz-message-enable' for more information."
  (interactive)
  (defalias 'message (symbol-function 'message-subr)))

;;;###autoload
(defun rsz-message-enable ()
"Dynamically resize the echo area to contain its contents.

The function `rsz-message-disable' disables this package.
The function `rsz-message-toggle' toggles enabling of this package.
The function `rsz-message' is the multi-line version of `message'."
  (interactive)
  (defalias 'message 'rsz-message))

;;;###autoload
(defun rsz-message-toggle ()
  "Toggle the current state (enabled vs disabled) of rsz-message.
See help for `rsz-message-enable' for more information."
  (interactive)
  (setq rsz-message-enabled-p (not rsz-message-enabled-p))
  (if rsz-message-enabled-p
      (rsz-message-enable)
    (rsz-message-disable)))


(provide 'rsz-message)

(if rsz-message-enabled-p
    (rsz-message-enable)
  (rsz-message-disable))

;; rsz-message.el ends here
