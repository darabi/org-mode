





(defun pdf-spool-buffer ()
  (interactive)
    (let ((filedir (file-name-directory (buffer-file-name (current-buffer))))
	  (filename (buffer-file-name (current-buffer))))
      (save-excursion
	(ps-spool-buffer)
	(switch-to-buffer "*PostScript*")
	(goto-char (point-min))
	(write-file (concat filename ".ps"))
	(kill-buffer (current-buffer)) 
	(if (equal system-type 'windows-nt)
	    (default-dir-real-shell-command (concat "%GVDIR%/ps2pdf.bat " (concat filename ".ps") (concat filename ".pdf")))
	  (default-dir-real-shell-command (concat "$GVDIR/ps2pdf.linux " (concat filename ".ps"))))
	(delete-file (concat filename ".ps")))
      (message (concat "File " (concat filename ".pdf") " written to file directory " filedir)))
    )



(defun pdf-spool-buffer-with-faces ()
  (interactive)
    
    (let ((filedir (file-name-directory (buffer-file-name (current-buffer))))
	  (filename (buffer-file-name (current-buffer))))
      (save-excursion
	(ps-spool-buffer-with-faces)
	(switch-to-buffer "*PostScript*")
	(goto-char (point-min))
	(write-file (concat filename ".ps"))
	(kill-buffer (current-buffer)) 
	(if (equal system-type 'windows-nt)
	    (default-dir-real-shell-command (concat "%GVDIR%/ps2pdf.bat " (concat filename ".ps") (concat filename ".pdf")))
	  (default-dir-real-shell-command (concat "$GVDIR/ps2pdf.linux " (concat filename ".ps"))))
	(delete-file (concat filename ".ps")))
      (message (concat "File " (concat filename ".pdf") " written to file directory " filedir)))
    )



(defun pdf-spool-region ()
  (interactive)
;  (if (buffer-modified-p)
;      (error "Please save buffer first: Undo problems might occur")
    
    (let ((filedir (file-name-directory (buffer-file-name (current-buffer))))
	  (filename (buffer-file-name (current-buffer))))
      (save-excursion
	(ps-spool-region (region-beginning) (region-end))
	(switch-to-buffer "*PostScript*")
	(goto-char (point-min))
	(write-file (concat filename ".ps"))
	(kill-buffer (current-buffer)) 
	(if (equal system-type 'windows-nt)
	    (default-dir-real-shell-command (concat "%GVDIR%/ps2pdf.bat " (concat filename ".ps") (concat filename ".pdf")))
	  (default-dir-real-shell-command (concat "$GVDIR/ps2pdf.linux " (concat filename ".ps"))))
	(delete-file (concat filename ".ps")))
      (message (concat "Region " (concat filename ".pdf") " written to file directory " filedir)))
    )



(defun pdf-spool-region-with-faces ()
  (interactive)
;  (if (buffer-modified-p)
;      (error "Please save buffer first: Undo problems might occur")
    
    (let ((filedir (file-name-directory (buffer-file-name (current-buffer))))
	  (filename (buffer-file-name (current-buffer))))
      (save-excursion
	(ps-spool-region-with-faces (region-beginning) (region-end))
	(switch-to-buffer "*PostScript*")
	(goto-char (point-min))
	(write-file (concat filename ".ps"))
	(kill-buffer (current-buffer)) 
	(if (equal system-type 'windows-nt)
	    (default-dir-real-shell-command (concat "%GVDIR%/ps2pdf.bat " (concat filename ".ps") (concat filename ".pdf")))
	  (default-dir-real-shell-command (concat "$GVDIR/ps2pdf.linux " (concat filename ".ps"))))
	(delete-file (concat filename ".ps")))
      (message (concat "File " (concat filename ".pdf") " written to file directory " filedir)))
    )






