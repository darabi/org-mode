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
