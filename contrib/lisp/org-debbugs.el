;;; org-debbugs.el --- Org-mode interface for the GNU bug tracker

;; Copyright (C) 2013 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.org>
;; Keywords: comm, hypermedia, maint, outlines

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides an interface to bug reports which are located
;; on the GNU bug tracker debbugs.gnu.org.  Its main purpose is to
;; show and manipulate bug reports as org-mode TODO items.

;; If you have `org-debbugs.el' in your load-path, you could enable
;; the bug tracker commands by the following lines in your ~/.emacs
;;
;;   (autoload 'org-debbugs "org-debbugs" "" 'interactive)
;;   (autoload 'org-debbugs-search "org-debbugs" "" 'interactive)
;;   (autoload 'org-debbugs-bugs "org-debbugs" "" 'interactive)

;; The bug tracker is called interactively by
;;
;;   M-x org-debbugs

;; It asks for the severities, for which bugs shall be shown. This can
;; be either just one severity, or a list of severities, separated by
;; comma.  Valid severities are "serious", "important", "normal",
;; "minor" or "wishlist".  Severities "critical" and "grave" are not
;; used, although configured on the GNU bug tracker.  If no severity
;; is given, all bugs are selected.

;; If a prefix is given to the command, more search parameters are
;; asked for, like packages (also a comma separated list, "org-mode" is
;; the default), or whether archived bugs shall be shown.

;; Another command is
;;
;;   M-x org-debbugs-search

;; It behaves like `org-debbugs', but asks at the beginning for a
;; search phrase to be used for full text search.  Additionally, it
;; asks for key-value pairs to filter bugs.  Keys are as described in
;; `debbugs-get-status', the corresponding value must be a regular
;; expression to match for.  The other parameters are as described in
;; `org-debbugs'.

;; The bug reports are downloaded from the bug tracker.  In order to
;; not generate too much load of the server, up to 500 bugs will be
;; downloaded at once.  If there are more hits, you will be asked to
;; change this limit, but please don't increase this number too much.

;; These default values could be changed also by customer options
;; `debbugs-gnu-default-severities', `org-debbugs-default-packages'
;; and `debbugs-gnu-default-hits-per-page'.

;; The commands create a TODO list.  Besides the usual handling of
;; TODO items, you could apply the following actions by the following
;; keystrokes:

;;   "C-c # c": Send a debbugs control message
;;   "C-c # d": Show bug attributes

;; The last entry in a TODO record is the link [[Messages]].  If you
;; follow this link, a Gnus ephemeral group is opened presenting all
;; related messages for this bug.  Here you could also send debbugs
;; control messages by keystroke "C".

;; Finally, if you simply want to list some bugs with known bug
;; numbers, call the command
;;
;;   M-x debbugs-gnu-bugs

;; The bug numbers to be shown shall be entered as comma separated list.

;; `org-debbugs.el' requires GNU Emacs 24.1 and GNU ELPA debbugs 0.4.

;;; Code:

(require 'debbugs-gnu)

(defgroup org-debbugs nil
  "Bug tracking with Org."
  :group 'org)

(defconst org-debbugs-severity-priority
  '(("serious" . "A")
    ("important" . "B")
    ("normal" . "C")
    ("minor" . "D")
    ("wishlist" . "E"))
  "Mapping of debbugs severities to TODO priorities.")

(defun org-debbugs-get-severity-priority (state)
  "Returns the TODO priority of STATE."
  (or (cdr (assoc (cdr (assq 'severity state))
		  org-debbugs-severity-priority))
      (cdr (assoc "minor" org-debbugs-severity-priority))))

(defconst org-debbugs-priority-faces
  '(("A" . org-warning)
    ("B" . org-warning))
  "Highlighting of prioritized TODO items.")

(defcustom org-debbugs-default-packages '("org-mode")
  "The list of packages to be searched for.
See `debbugs-gnu-all-packages' for a list of all available
packages."
  :type (get 'debbugs-gnu-default-packages 'custom-type)
  :group 'org-debbugs)

;; We do not add the bug numbers list to the elisp:link, because this
;; would be much too long.  Instead, this variable shall keep the bug
;; numbers.
(defvar org-debbugs-ids nil
  "The list of bug ids to be shown following the elisp link.")

;;;###autoload
(defun org-debbugs-search ()
  "Search for bugs interactively.
Search arguments are requested interactively.  The \"search
phrase\" is used for full text search in the bugs database.
Further key-value pairs are requested until an empty key is
returned."
  (interactive)

  (unwind-protect
      (let ((date-format "\\([[:digit:]]\\{4\\}\\)-\\([[:digit:]]\\{1,2\\}\\)-\\([[:digit:]]\\{1,2\\}\\)")
	    key val1 val2 phrase severities packages archivedp)

	;; Check for the phrase.
	(setq phrase (read-string debbugs-gnu-phrase-prompt))
	(if (zerop (length phrase))
	    (setq phrase nil)
	  (add-to-list 'debbugs-gnu-current-query (cons 'phrase phrase)))

	;; The other queries.
	(catch :finished
	  (while t
	    (setq key (completing-read
		       "Enter attribute: "
		       '("severity" "package" "tags" "submitter"
			 "subject" "status")
		       nil t))
	    (cond
	     ;; Server-side queries.
	     ((equal key "severity")
	      (setq
	       severities
	       (completing-read-multiple
		"Enter severities: " debbugs-gnu-all-severities nil t
		(mapconcat 'identity debbugs-gnu-default-severities ","))))

	     ((equal key "package")
	      (setq
	       packages
	       (completing-read-multiple
		"Enter packages: " debbugs-gnu-all-packages nil t
		(mapconcat 'identity debbugs-gnu-default-packages ","))))

	     ((member key '("tags" "subject"))
	      (setq val1 (read-string (format "Enter %s: " key)))
	      (when (not (zerop (length val1)))
		(add-to-list
		 'debbugs-gnu-current-query (cons (intern key) val1))))

	     ((equal key "submitter")
	      (setq val1 (read-string "Enter email address: "))
	      (when (not (zerop (length val1)))
		(add-to-list
		 'debbugs-gnu-current-query (cons (intern key) val1))))

	     ((equal key "status")
	      (setq
	       val1
	       (completing-read "Enter status: " '("done" "forwarded" "open")))
	      (when (not (zerop (length val1)))
		(add-to-list
		 'debbugs-gnu-current-query (cons (intern key) val1))))

	     ;; The End.
	     (t (throw :finished nil)))))

	;; Do the search.
	(org-debbugs severities packages))

    ;; Reset query and filter.
    (setq debbugs-gnu-current-query nil)))

;;;###autoload
(defun org-debbugs (severities &optional packages archivedp)
  "List all outstanding bugs."
  (interactive
   (let (severities archivedp)
     (list
      (setq severities
	    (completing-read-multiple
	     "Severities: " debbugs-gnu-all-severities nil t
	     (mapconcat 'identity debbugs-gnu-default-severities ",")))
      ;; The next parameters are asked only when there is a prefix.
      (if current-prefix-arg
	  (completing-read-multiple
	   "Packages: " debbugs-gnu-all-packages nil t
	   (mapconcat 'identity org-debbugs-default-packages ","))
	org-debbugs-default-packages)
      (when current-prefix-arg
	(setq archivedp (y-or-n-p "Show archived bugs?"))))))

  ;; Add queries.
  (dolist (severity (if (consp severities) severities (list severities)))
    (when (not (zerop (length severity)))
      (add-to-list 'debbugs-gnu-current-query (cons 'severity severity))))
  (dolist (package (if (consp packages) packages (list packages)))
    (when (not (zerop (length package)))
      (add-to-list 'debbugs-gnu-current-query (cons 'package package))))
  (when archivedp
    (add-to-list 'debbugs-gnu-current-query '(archive . "1")))

  (with-current-buffer (get-buffer-create "*Org Bugs*")
    (erase-buffer))

  (unwind-protect
      (let ((hits debbugs-gnu-default-hits-per-page))
	(setq org-debbugs-ids (org-debbugs-get-bugs debbugs-gnu-current-query))

	(when (> (length org-debbugs-ids) hits)
	  (let ((cursor-in-echo-area nil))
	    (setq hits
		  (string-to-number
		   (read-string
		    (format
		     "How many reports (available %d, default %d): "
		     (length org-debbugs-ids) hits)
		    nil
		    nil
		    (number-to-string hits))))))

	(org-debbugs-show-next-reports hits))

    ;; Reset query.
    (setq debbugs-gnu-current-query nil)))

(defun org-debbugs-get-bugs (query)
  "Retrieve bugs numbers from debbugs.gnu.org according search criteria."
  (let* ((debbugs-port "gnu.org")
	 (bugs (assoc 'bugs query))
	 (phrase (assoc 'phrase query))
	 args)
    ;; Compile query arguments.
    (unless query
      (dolist (elt org-debbugs-default-packages)
	(setq args (append args (list :package elt)))))
    (dolist (elt query)
      (setq args
	    (append
	     args
	     (if phrase
		 (cond
		  ((eq (car elt) 'phrase)
		   (list (list :phrase (cdr elt) :max 500)))
		  ((eq (car elt) 'date)
		   (list (list :date (cddr elt) (cadr elt)
			       :operator "NUMBT")))
		  (t
		   (list (list (intern (concat ":" (symbol-name (car elt))))
			       (cdr elt) :operator "ISTRINC"))))
	       (list (intern (concat ":" (symbol-name (car elt))))
		     (cdr elt))))))

    (sort
     (cond
      ;; If the query is just a list of bug numbers, we return them.
      (bugs (cdr bugs))
      ;; A full text query.
      (phrase
       (mapcar
	(lambda (x) (cdr (assoc "id" x)))
	(apply 'debbugs-search-est args)))
      ;; Otherwise, we retrieve the bugs from the server.
      (t (apply 'debbugs-get-bugs args)))
     ;; Sort function.
     '<)))

(defun org-debbugs-show-reports (bug-numbers)
  "Show bug reports as given in BUG-NUMBERS."
  (pop-to-buffer (get-buffer-create "*Org Bugs*"))
  (org-mode)
  (org-debbugs-mode 1)
  ;; FIXME: Does not show any effect.
  (set (make-local-variable 'org-priority-faces) org-debbugs-priority-faces)

  (let ((inhibit-read-only t)
	(debbugs-port "gnu.org"))

    (dolist (status
	     (sort
	      (apply 'debbugs-get-status bug-numbers)
	      (lambda (x y) (< (cdr (assq 'id x)) (cdr (assq 'id y))))))
      (let* ((beg (point))
	     (id (cdr (assq 'id status)))
	     (done (string-equal (cdr (assq 'pending status)) "done"))
	     (priority (org-debbugs-get-severity-priority status))
	     (archived (cdr (assq 'archived status)))
	     (tags (append (cdr (assq 'found_versions status))
			   (cdr (assq 'tags status))))
	     (subject (when (cdr (assq 'subject status))
			(decode-coding-string
			 (cdr (assq 'subject status)) 'utf-8)))
	     (date (cdr (assq 'date status)))
	     (last-modified (cdr (assq 'last_modified status)))
	     (originator (when (cdr (assq 'originator status))
			   (decode-coding-string
			    (cdr (assq 'originator status)) 'utf-8)))
	     (owner (when (cdr (assq 'owner status))
		      (decode-coding-string (cdr (assq 'owner status)) 'utf-8)))
	     (closed-by (when (cdr (assq 'done status))
			  (decode-coding-string
			   (cdr (assq 'done status)) 'utf-8)))
	     (merged (cdr (assq 'mergedwith status))))

	;; Handle tags.
	(when (string-match "^\\([0-9.]+\\); \\(.+\\)$" subject)
	  (add-to-list 'tags (match-string 1 subject))
	  (setq subject (match-string 2 subject)))
	(when archived
	  (add-to-list 'tags "ARCHIVE"))
	(setq tags
	      (mapcar
	       ;; Replace all invalid TAG characters by "_".
	       (lambda (x) (replace-regexp-in-string "[^A-Za-z0-9_@]" "_" x))
	       tags))

	;; Headline.
	(insert
	 (format
	  "* %s [#%s] %s %s\n"
	  (if done "DONE" "TODO")
	  priority subject
	  (if tags (mapconcat 'identity (append '("") tags '("")) ":") "")))

	;; Submitted.
	(when date
	  (insert
	   (format-time-string
	    "  [%Y-%m-%d %a] Submitted\n" (seconds-to-time date))))

	;; Properties.
	(insert "  :PROPERTIES:\n")
	(insert (format "  :DEBGUGS_ID: %s\n" id))
	(when merged
	  (insert
	   (format
	    "  :MERGED_WITH: %s\n"
	    (if (numberp merged)
		merged (mapconcat 'number-to-string merged " ")))))
	(insert (format "  :CREATOR: %s\n" originator))
	(when owner (insert (format "  :OWNER: %s\n" owner)))
	(when closed-by (insert (format "  :CLOSED_BY: %s\n" closed-by)))
	(insert "  :END:\n")

	;; Messages.
	(insert
	 "  [[elisp:(debbugs-gnu-select-report)][Messages]]\n")

	;; Last modified.
	(when last-modified
	  (insert
	   (format-time-string
	    "  [%Y-%m-%d %a] Last modified\n"
	    (seconds-to-time last-modified))))

	;; Add text properties.
	(add-text-properties beg (point) `(tabulated-list-id ,status))))

    (goto-char (point-min))
    (org-overview)
    (set-buffer-modified-p nil)))

(defun org-debbugs-show-next-reports (hits)
  "Show next HITS of bug reports."
  (with-current-buffer (get-buffer-create "*Org Bugs*")
    (save-excursion
      (goto-char (point-max))
      (forward-line -1)
      (delete-region (point) (point-max))
      (org-debbugs-show-reports
       (butlast org-debbugs-ids (- (length org-debbugs-ids) hits)))
      (setq org-debbugs-ids
	    (last org-debbugs-ids (- (length org-debbugs-ids) hits)))
      (when org-debbugs-ids
	(goto-char (point-max))
	(insert
	 (format
	  "[[elisp:(org-debbugs-show-next-reports %s)][Next bugs]]\n"
	  hits))))))

(defconst org-debbugs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c # c") 'debbugs-gnu-send-control-message)
    (define-key map (kbd "C-c # d") 'debbugs-gnu-display-status)
    map)
  "Keymap for the `org-debbugs-mode' minor mode.")

;; Make byte-compiler quiet.
(defvar gnus-posting-styles)

(define-minor-mode org-debbugs-mode
  "Minor mode for providing a debbugs interface in org-mode buffers.

\\{org-debbugs-mode-map}"
  :lighter " Debbugs" :keymap org-debbugs-mode-map
  (set (make-local-variable 'gnus-posting-styles)
       `((".*"
	  (eval
	   (when (buffer-live-p gnus-article-copy)
	     (with-current-buffer gnus-article-copy
	       (set (make-local-variable 'message-prune-recipient-rules)
		    '((".*@debbugs.*" "emacs-pretest-bug")
		      (".*@debbugs.*" "bug-gnu-emacs")
		      ("[0-9]+@debbugs.*" "submit@debbugs.gnu.org")
		      ("[0-9]+@debbugs.*" "quiet@debbugs.gnu.org")))
	       ;; `gnus-posting-styles' is eval'ed after
	       ;; `message-simplify-subject'.  So we cannot use m-s-s.
	       (setq subject ,debbugs-gnu-subject))))))))

;;;###autoload
(defun org-debbugs-bugs (&rest bugs)
  "List all BUGS, a list of bug numbers."
  (interactive
   (mapcar 'string-to-number
	   (completing-read-multiple "Bug numbers: " nil 'natnump)))
  (dolist (elt bugs)
    (unless (natnump elt) (signal 'wrong-type-argument (list 'natnump elt))))
  (add-to-list 'debbugs-gnu-current-query (cons 'bugs bugs))
  (org-debbugs nil))

;; TODO

;; - Refactor it in order to avoid code duplication with debbugs-gnu.el.

(provide 'org-debbugs)
