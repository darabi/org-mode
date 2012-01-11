

(defun op-fill-paragraph-region ()
  (interactive)
  (save-excursion
    (let ((tmp-region-end nil))
      (setq tmp-region-end (save-excursion 
			     (goto-char (if (< (region-beginning)(region-end))(region-end)(region-beginning)))
			     (end-of-line)
			     (point)))
      (goto-char (if (< (region-beginning)(region-end))(region-beginning)(region-end)))
      (beginning-of-line)
      (newline)
      (previous-line 1)
      (save-match-data
	(re-indent))
      (beginning-of-line)
      (narrow-to-region (point) (1+ tmp-region-end))
      (fill-paragraph-or-region nil)
      (goto-char (point-min))
      (kill-line)
      (goto-char (point-max))
      (delete-char -1)
      (widen)
)))


