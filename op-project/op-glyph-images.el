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
