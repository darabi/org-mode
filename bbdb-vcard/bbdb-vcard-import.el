;;; bbdb-vcard-import.el -- import vCards into BBDB
;; 
;; Copyright (c) 2008 Marcus Crestani
;;
;; bbdb-vcard-import.el is free software you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; Author: Marcus Crestani <crestani@informatik.uni-tuebingen.de>
;; Created: 2008-01-03
;; Version: $Id: bbdb-vcard-import.el,v 1.6 2008/01/31 16:19:15 cvs Exp $
;; Keywords: vcard bbdb
;;
;; This requires vcard.el by NoahFriedman for the importer to work.
;;
;;    http://www.splode.com/~friedman/software/emacs-lisp/src/vcard.el
;;
;; The implementation is based on Christopher Smiths very simple
;; version of `bbdb-vcard-snarf-buffer':
;;
;;   http://www.emacswiki.org/cgi-bin/wiki/BbdbImporters#toc3
;;

;;; Commentary

;;
;; To import all vCards that are in the file ~/vCards.vcf do:
;;
;;	M-x bbdb-vcard-import RET ~/vCards.vcf RET
;;

;;; Todo

;;
;; STREET ADDRESSES and PHONE NUMBERS are not yet imported.  See
;; comment in `bbdb-vcard-merge'.
;;

;;; ChangeLog

;;
;; 2008-01-31  Marcus Crestani  <crestani@informatik.uni-tuebingen.de>
;;   - Do not enforce (type . "internet") for email addresses.
;; 
;; 2008-01-03  Marcus Crestani  <crestani@informatik.uni-tuebingen.de>
;;   - Initial version.
;;

;;; Code:

(require 'vcard)
(require 'bbdb)

(defvar bbdb-vcard-merged-records nil)

;; FIXME: this belongs in .emacs
(setf vcard-standard-filters 
      (remove 'vcard-filter-tel-normalize vcard-standard-filters))

(add-to-list 'bbdb-refile-notes-generate-alist '(luid . bbdb-refile-notes-remove-duplicates))
(add-to-list 'bbdb-refile-notes-generate-alist '(categories . bbdb-refile-notes-remove-duplicates))
(add-to-list 'bbdb-refile-notes-generate-alist '(notes . bbdb-refile-notes-remove-duplicates))

(defun bbdb-vcard-filter-empty-values (values)
  "Filter out empty values."
  (if (consp values)
      (if (string= "" (car values))
	  (bbdb-vcard-filter-empty-values (cdr values))
	(cons (car values) (bbdb-vcard-filter-empty-values (cdr values))))))

(defun bbdb-vcard-values (record field)
  "Return the values of an RECORD's FIELD; empty string entries are filtered out."
  (let ((values (vcard-values record (list field))))
    (if values
	(mapconcat 'identity 
		   (bbdb-vcard-filter-empty-values (car values))
		   ", ")
      "")))

(defun bbdb-vcard-get-emails (record)
  "Return a list of email addresses."
  (let ((pref (vcard-ref record '("email" ("type" . "pref"))))
	(rest (vcard-ref record '("email") '(("type" . "pref")))))
    (mapcar (lambda (entry) (car (cdr entry))) 
	    (if pref 
		(cons (car pref) rest)
	      rest))))

;; BEGIN:VCARD
;; version:2.1
;; n:Darabi;Kambiz;;;
;; fn:Kambiz Darabi
;; org:m-creations gmbh
;; tel:+49-1234-56789
;; tel;type=cell:987654321
;; tel;work:+49-6131-6224417
;; tel;work:+49-6131-6224417
;; tel;work:+49-6131-3272686
;; tel;fax:+49 1212 516396034
;; tel;voice:+989329540265
;; tel;work:+98 21 8884 7308-136
;; categories:Nicht abgelegt
;; END:VCARD


(setf testcard '((("version") "2.1") (("n") "Darabi" "Kambiz" "" "" "") (("fn") "Kambiz Darabi") (("org") "m-creations gmbh") (("tel" "work") "+49-6131-6224417") (("tel" "work") "+49-6131-3272686") (("tel" "fax") "+49 1212 516396034") (("tel" "voice") "+989329540265") (("tel" "work") "+98 21 8884 7308-136") (("note") ": grmblfx äöü") (("categories") "Nicht abgelegt")))

(bbdb-vcard-values testcard "note")

(setq bbdb-vcard-merged-records nil)

(bbdb-vcard-merge testcard)

(bbdb-search-simple "Kambiz Darabi" nil)

(defun test ()
  (let ((old-record (bbdb-search-simple "Kambiz Darabi" nil))
        (new-record ["Kambiz" "Darabi" ("Myself") "m-creations gmbh" (["Work" "+49-6131-6224417"] ["Work" "+49-6131-3272686"] ["Fax" "+49 1212 516396034"] ["Voice" "+989329540265"] ["Work" "+98 21 8884 7308-136"]) nil ("darabi@m-creations.com" "kambiz.darabi@sybase.com" "darabi@users.sourceforge.net" "darabi@m-creations.int") ((notes . ": grmblfx äöü"))]))

    (let ((n1 (bbdb-record-raw-notes new-record))
          (n2 (bbdb-record-raw-notes old-record))
          tmp)
      (or (equal n1 n2)
          (progn
            (or (listp n1) (setq n1 (list (cons 'notes n1))))
            (or (listp n2) (setq n2 (list (cons 'notes n2))))
            (while n2
              (if (setq tmp (assq (car (car n2)) n1))
                  (setcdr tmp
                          (funcall
                           (or (cdr (assq (car (car n2))
                                          bbdb-refile-notes-generate-alist))
                               bbdb-refile-notes-default-merge-function)
                           (cdr tmp) (cdr (car n2))))
		  (setq n1 (nconc n1 (list (car n2)))))
              (setq n2 (cdr n2)))
            (bbdb-record-set-raw-notes new-record n1))))))

(test)

(defun bbdb-vcard-get-phone-type (phone)
  "Return the type of the phone number (pref, if no explicit type)."
  (let* ((proplist (car phone))
         (type (cdr (assoc "type" proplist))))
    (or type
        (cadr proplist)
        'pref)))


(defun bbdb-vcard-get-phones (record)
  "Return a list of phone number objects."
  (message "b-v-get-phones: %S\n%S" record (vcard-ref record '("tel")))
  (let ((pref (vcard-ref record '("tel" ("type" . "pref"))))
	(rest (vcard-ref record '("tel") '(("type" . "pref")))))
    (mapcar (lambda (entry)
	      (let ((type (bbdb-vcard-get-phone-type entry)))
		(vector
		 (bbdb-vcard-translate type)
		 (cadr entry))))
	    (vcard-ref record '("tel")))))

;; ;; called with:

;; (setq testrec '(((version) 3.0) 
;;  ((n) Dummy Crash T Dr. Jun.) 
;;  ((fn) Dr. Crash T Dummy Jun.) 
;;  ((adr (type . home)) 1 Exthome Streethome Cityhome Statehome Ziphome Countryhome) 
;;  ((adr (type . work)) 3 Extbus Streetbus Citybus Statebus Zipbus Country bus) 
;;  ((ADR) 55116 extension Street Mainz Rlp Zipcode Germany) 
;;  ((tel (type . cell)) +1 646 11111111)))
 

;; (vcard-ref testrec '("adr" ("type" . "pref")))
;; (vcard-ref testrec '(ADR))

;; (car (car testrec))

;; (vcard-get-property '("adr" 55116 extension Street Mainz Rlp Zipcode Germany) "adr")
 
;; (vcard-proplist-all-properties (car testrec) '("adr"))

(defun bbdb-vcard-get-addresses (record)
  "Return a list of adress objects."
  (message "b-v-g-a: record %s" record)
  (let ((pref (vcard-ref record '("adr" ("type" . "pref"))))
	(rest (vcard-ref record '("adr") '(("type" . "pref")))))
    (message "b-v-g-a: pref %S" pref)
    (message "b-v-g-a: rest %s" rest)
    (let ((res (mapcar (lambda (entry)
	      (let ((proplist (car entry))
		    (phone (car (cdr entry))))
		(vector
		 (vcard-get-property proplist "type")
		 phone)))
	    (if pref
		(cons (car pref) rest)
	      rest))))
      (message "b-v-g-a: result %s" res)
      res)))


(defun bbdb-vcard-merge-interactively (name company nets addrs phones notes)
  "Interactively add a new record; this functions is an exact copy 
of \\[bbdb-merge-interactively] apart from the fact that it doesn't call
\\[bbdb-display-records] on the newly created record, but returns it."
  (let*
      ((f-l-name (bbdb-divide-name name))
       (firstname (car f-l-name))
       (lastname (nth 1 f-l-name))
       (aka nil)
       (new-record
        (vector firstname lastname aka company phones addrs
                (if (listp nets) nets (list nets)) 
                `((notes . ,notes))
                (make-vector bbdb-cache-length nil)))
       (old-record (bbdb-search-simple name nets)))
    (message "b-v-m-i: new rec note '%s' stringp %s" (elt new-record 7) (stringp (elt new-record 7)))
    (if old-record
	(progn
          (message "b-v-m-i: old rec note '%s' stringp %s" (bbdb-record-raw-notes old-record) (stringp (bbdb-record-raw-notes old-record)))
          (message "b-v-m-i: old-record %s" old-record)
	  (setq new-record (bbdb-merge-internally old-record new-record))
	  (bbdb-delete-record-internal old-record)))
    ;; create  new record
    (message "b-v-m-i: (stringp notes) %s" (stringp notes))
    (message "b-v-m-i: new-record %s" new-record)
    (message "b-v-m-i: note '%s' stringp %s" (elt new-record 7) (stringp (elt new-record 7)))
    (bbdb-invoke-hook 'bbdb-create-hook new-record)
    (bbdb-change-record new-record t)
    (bbdb-hash-record new-record)
    new-record))

(defun bbdb-merge-interactively (name company nets addrs phones notes)
  "Interactively add a new record; arguments same as \\[bbdb-create-internal]."
  (let*
      ((f-l-name (bbdb-divide-name name))
       (firstname (car f-l-name))
       (lastname (nth 1 f-l-name))
       (aka nil)
       (new-record
        (vector firstname lastname aka company phones addrs
                (if (listp nets) nets (list nets)) notes
                (make-vector bbdb-cache-length nil)))
       (old-record (bbdb-search-simple name nets)))
    (if old-record
    (progn
      (setq new-record (bbdb-merge-internally old-record new-record))
      (bbdb-delete-record-internal old-record)))
    ;; create  new record
    (bbdb-invoke-hook 'bbdb-create-hook new-record)
    (bbdb-change-record new-record t)
    (bbdb-hash-record new-record)
    (bbdb-display-records (list new-record))))

(defun bbdb-vcard-merge (record)
  "Merge data from vcard interactively into bbdb."
  (let* ((name (bbdb-vcard-values record "fn"))
	 (company (bbdb-vcard-values record "org"))
	 (net (bbdb-vcard-get-emails record))
	 (addrs (bbdb-vcard-get-addresses record))
	 (phones (bbdb-vcard-get-phones record))
	 (categories (bbdb-vcard-values record "categories"))
	 (notes (bbdb-vcard-values record "note"))
	 ;; FIXME, TODO: addrs and phones are not yet imported.  To do this
	 ;; right, figure out a way to map the several labels to
	 ;; `bbdb-default-label-list'.  Also, some phone number
	 ;; conversion may break the format of numbers.
	 ;; (new-record (bbdb-vcard-merge-interactively name company net addrs phones notes))
         (new-record (bbdb-vcard-merge-interactively name company net nil phones notes)))
    (message "b-v-m: notes '%s'" notes)    
    (setq bbdb-vcard-merged-records (append bbdb-vcard-merged-records 
					    (list new-record)))))

(defun bbdb-vcard-snarf-region (begin end)
  "Bbdb-snarf each match between the arguments BEGIN and END and return the
the position of the end of the last vcard as the region is modified during
parsing."
  (let ((record (vcard-parse-region begin end)))
    
    (bbdb-vcard-merge record)))

(defun bbdb-vcard-snarf-buffer (buf)
  "Traverse BUF via regex.  Bbdb-snarf against each match."
  (setq bbdb-vcard-merged-records nil)
  (let ((bbdb-current-buffer (current-buffer))
	(bbdb-current-point)
	(bbdb-next-point))
    (switch-to-buffer buf)
    (setq bbdb-current-point (point-min-marker))
    (setq bbdb-next-point (point-min-marker))
    (goto-char (marker-position  bbdb-current-point))
    (while (or (re-search-forward "^end:vcard[ \t]*$" (point-max) t)
	       (re-search-forward "^END:VCARD[ \t]*$" (point-max) t))
      (message "searching for end:vcard done. point %d, current-point %d, next-point %d, match-end %d " 
	       (point) 
	       (marker-position bbdb-current-point) 
	       (marker-position bbdb-next-point) 
	       (match-end 0))
      (set-marker bbdb-next-point (match-end 0))
      (bbdb-vcard-snarf-region (marker-position bbdb-current-point) (marker-position bbdb-next-point))
      (message "snarfed. point %d, current-point %d, next-point %d " (point) (marker-position bbdb-current-point) (marker-position bbdb-next-point))
      (switch-to-buffer buf)
      (goto-char (marker-position bbdb-next-point))
      (set-marker bbdb-current-point (point))
      (message "reset current-point. point %d, current-point %d, next-point %d " (point) (marker-position bbdb-current-point) (marker-position bbdb-next-point)))
    (switch-to-buffer bbdb-current-buffer)
    (bbdb-display-records bbdb-vcard-merged-records)))

(defun bbdb-vcard-snarf-current-buffer ()
  "Snarf the vcards in the current buffer."
  (interactive)
  (bbdb-vcard-snarf-buffer (current-buffer)))

(defun bbdb-vcard-import-current-buffer ()
  "Import the vcards in the current buffer into your bbdb."
  (interactive)
  (bbdb-vcard-snarf-current-buffer))

(defun bbdb-vcard-import (file)
  "Import the vcards in FILE into your bbdb."
  (interactive "FvCard file to read from: ")
  (let ((buffer (find-file file)))
    (bbdb-vcard-snarf-buffer buffer)
    (revert-buffer buffer)
    (kill-buffer buffer)))

(provide 'bbdb-vcard-import)
