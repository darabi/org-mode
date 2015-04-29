;;; -*- Mode: Emacs-Lisp -*-

;;;; Up and Down Lists and Strings

;;; This is an unreleased utility for paredit.el.
;;; Use at your own peril.

;;; This defines paredit variants of `down-list', `backward-up-list',
;;; `up-list', and `down-list', extending them so that moving up will
;;; move out of a string.  It does not yet extend them so that moving
;;; down will move into a string.

;;; Copyright (c) 2009, Taylor R. Campbell
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;; * Redistributions of source code must retain the above copyright
;;;   notice, this list of conditions and the following disclaimer.
;;;
;;; * Redistributions in binary form must reproduce the above copyright
;;;   notice, this list of conditions and the following disclaimer in
;;;   the documentation and/or other materials provided with the
;;;   distribution.
;;;
;;; * Neither the names of the authors nor the names of contributors
;;;   may be used to endorse or promote products derived from this
;;;   software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(defun paredit-up/down-list (n vertical-direction)
  (let ((horizontal-direction (if (> n 0) +1 -1)))
    (while (/= n 0)
      (goto-char
       (let ((state (paredit-current-parse-state)))
         (if (and (> vertical-direction 0) (paredit-in-string-p state))
             (let ((start+end (paredit-string-start+end-points state)))
               (if (> horizontal-direction 0)
                   (+ 1 (cdr start+end))
                   (car start+end)))
             (or (scan-lists (point) horizontal-direction vertical-direction)
                 (buffer-end horizontal-direction)))))
      (setq n (- n horizontal-direction)))))

(defun paredit-down-list (&optional argument)
  "Move forward down into a list.
With a positive argument, move forward down that many levels.
With a negative argument, move backward down that many levels."
  (interactive "p")
  (paredit-up/down-list (or argument +1) -1))

(defun paredit-backward-up-list (&optional argument)
  "Move backward up out of the enclosing list.
With a positive argument, move backward up that many levels.
With a negative argument, move forward up that many levels.
If in a string initially, that counts as one level."
  (interactive "p")
  (paredit-up/down-list (- 0 (or argument +1)) +1))

(defun paredit-up-list (&optional argument)
  "Move forward up out of the enclosing list.
With a positive argument, move forward up that many levels.
With a negative argument, move backward up that many levels.
If in a string initially, that counts as one level."
  (interactive "p")
  (paredit-up/down-list (or argument +1) +1))

(defun paredit-backward-down-list (&optional argument)
  "Move backward down into a list.
With a positive argument, move backward down that many levels.
With a negative argument, move forward down that many levels."
  (interactive "p")
  (paredit-up/down-list (- 0 (or argument +1)) -1))
