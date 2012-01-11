;;; bbdb-syncml-vcard.el -- VCard routines for the bbdb-syncml package.
;; $Id: bbdb-syncml-vcard.el,v 1.2 2006/04/06 20:37:05 joergenb Exp $

;; Copyright (C) 2003-2004 Jørgen Binningsbø 

;; Author: Jørgen Binningsbø <jb@pvv.org>
;; Maintainer: Jørgen Binningsbø <jb@pvv.org>
;; Version: 
;; Created: Jan 25 2004
;; Keywords: syncml xml network
;; URL: 

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is a fork of bbdb-vcard-export located in the contrib/ folder
;; of the bbdb distribution.
;;
;; 2010-01-15, Kambiz Darabi: added bbdb-syncml-synchronize-vcard-dir 
;;                            to sync with a directory of vcard files
;;                            which then can be sync'd with opensync
(require 'bbdb-vcard-export)

(setq bbdb-vcard-export-name-function 'bbdb-syncml-vcard-filename)

(defun bbdb-syncml-synchronize-vcard-dir (&rest force-slow-sync) 
  "Synchronizes the bbdb database with the directory bbdb-syncml-vcard-dir.

If the given dir doesn't exist, it is created. Then, all bbdb records
are exported, creating one vcard file for each of them. If a record
doesn't have a unique id (luid), one is attached to it and the
global uid counter is incremented."
  (interactive)
  (unless (file-exists-p bbdb-syncml-vcard-dir) 
    (make-directory bbdb-syncml-vcard-dir)) 
  ;; do some initialization
  (message "vCard directory synchronization started...")   
  ;; check last sync time -get from .bbdb.syncml
  (setq syncml-previous-timestamp (bbdb-syncml-get-last-sync))
  (if (or (null syncml-previous-timestamp)
	  (string= "" syncml-previous-timestamp))
      (setq force-slow-sync 't))
  (setq syncml-current-timestamp (format-time-string "%Y%m%dT%H%M%SZ"))
  (message "Timestamp of last sync: %S" syncml-previous-timestamp)
  (message "Timestamp of this sync: %S" syncml-current-timestamp)

  ;; validate the luids in the bbdb and put all luids fond in a list.  will abort if inconsistencies are found.
  (setq bbdb-syncml-existing-luids (bbdb-syncml-validate-luids nil))

  ;; also ensure that the mapping file lists are up-to-date
  (setq bbdb-syncml-mapping-luid-list (bbdb-syncml-read-mapping-file))
  (message "LUIDs in BBDB        : %S" bbdb-syncml-existing-luids)
  (message "LUIDs in mapping file: %S" bbdb-syncml-mapping-luid-list)

  ;; FIXME darabi: read the modifications in bbdb-syncml-vcard-dir, before
  ;;               overwriting them with bbdb modifications

  ;; write the modifications to vcard dir
  (multiple-value-bind (added modified deleted) (bbdb-syncml-process-bbdb force-slow-sync)
    (message "added: %S" added)
    (message "modified: %S" modified)
    (message "deleted: %S" deleted)

    (bbdb-syncml-vcard-add-vcards added))

  ;; if all was successful, update the timestamp in the mapping file.
;;  (bbdb-syncml-write-mapping-file bbdb-syncml-pkg5-ok-luids)
  (bbdb-syncml-write-next-timestamp syncml-current-timestamp)
  
  (message "Synchronization complete!"))

(defun bbdb-syncml-vcard-add-vcards (added)
  "Adds the bbdb records in arg ADDED as vcard files to bbdb-syncml-vcard-dir"
  (dolist (luid added)
    (let ((record (car (bbdb-syncml-get-record-by-luid luid))))
      (if (null record)
          (error "Record <%d> cannot be retrieved with bbdb-syncml-get-record-by-luid" luid)
          (bbdb-vcard-do-record record bbdb-syncml-vcard-dir bbdb-syncml-vcard-coding-system)))))

(defun bbdb-syncml-vcard-filename (record)
  "Returns the luid of a record or signals an error, as
all exported bbdb records MUST have a unique id. bbdb-syncml-initialize
adds the luid field."
  (let ((luid (bbdb-record-getprop record 'luid)))
    (unless luid
      (error "Record has no unique id field. Have you run bbdb-syncml-initialize? Record: %s" record))
    (concat luid ".vcf")))
 
(setq testrec ["Crash T" "Dummy" ("dummy\\, crashy") "Crash Co" (["Mobile" 0 163 6670 1]) (["Home" ("Street line 1" "Street line 2") "Cityhome" "Statehome" "12345" "Emacs"] ["Office" ("Streetoffice line 1" "Streetoffice line 2") "Cityoffice" "Stateoffice" "12345" "Emacs"]) ("dummy@crash.com") ((notes . "additional comments") (creation-date . "2010-01-12") (timestamp . "2010-01-12")) nil])


(defun bbdb-syncml-vcard-get-bbdb-record-as-vcard-string (record)
  "Returns a VCARD2.1 formatted version of RECORD as a string"
  (let (
	(name (bbdb-record-name record))
	(first-name (elt record 0))
	(last-name (elt record 1))
	(company (elt record 3))
	(notes (bbdb-record-notes record))
	(phones (bbdb-record-phones record))
	(addresses (bbdb-record-addresses record))
	(luid (bbdb-record-getprop record 'luid))
	(net (bbdb-record-net record)))
    (concat 
     "BEGIN:VCARD\n"
     "VERSION:2.1\n"
     (concat "FN:" name "\n")
     (concat "N:" last-name ";" first-name "\n"
	     (if company (concat "ORG:" company "\n"))
	     (if notes (concat "NOTE:" notes "\n")))
     
     (concat "UID:" luid "\n")
     (if phones
	 (let (res)
	   (dolist (phone phones res)
	     (push (concat "TEL;" (bbdb-syncml-vcard-lookup-location-mapping
					(bbdb-phone-location phone))
			   ":"
			   (bbdb-phone-string phone) "\n")
		   res))
	   (mapconcat 'concat (cons "" res) "")))
     
     
     (if addresses
	 (while addresses
	   (concat (bbdb-vcard-export-address-string (car addresses)) "\n")
	   (setq addresses (cdr addresses))))
     (if net
	 (while net
	   (concat "EMAIL;TYPE=internet:" (car net) "\n")
	   (setq net (cdr net))))
     (concat "END:VCARD\n"))))


(defconst bbdb-syncml-vcard-phone-location-mappings
  '(("Mobile" "CELL")
    ("mobile" "CELL")
    ("mobil"  "CELL")
    ("Mobil"  "CELL")
    ("Home"   "HOME")
    ("home"   "HOME")
    ("heime"  "HOME")
    ("Heime"  "HOME")
    ("Voice"  "VOICE")
    ("voice"  "VOICE")
    ("work"   "WORK")
    ))

(defun bbdb-syncml-vcard-lookup-location-mapping (location)
  "Returns the text associated with the BBDB phone location LOCATION."

  (let (tmp (car (cdr (assoc location bbdb-syncml-vcard-phone-location-mappings))))
    (if (null tmp)
	(setq tmp "VOICE"))
    tmp))



(provide 'bbdb-syncml-vcard)	 
