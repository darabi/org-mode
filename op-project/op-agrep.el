

(defvar current-buf (current-buffer))
(make-local-variable 'current-buffer)


(defun agrep (distance searchstring)
  (interactive "nNumber of allowed errors: \ns[Additional switches and] re-searchstring: ")
  (save-excursion
    (shell-command-on-region (point-min) (point-max)
			     (concat "agrep -n -" distance " " searchstring)))
  (setq current-buf (current-buffer))
  (other-window 1)
  (if (eq current-buf (current-buffer))
      (progn
	(other-window -1)
	(split-window-vertically)
	(other-window 1)))
  (switch-to-buffer "*agrep Occur*")
  (kill-buffer "*agrep Occur*")
  (switch-to-buffer "*Shell Command Output*")
  (rename-buffer "*agrep Occur*" t)
  (message
   (concat "agrep: Allowed Misspellings: " distance ", Searchstring: " searchstring))
  (local-set-key "\C-c\C-c" 'find-agrep-occur)
  (goto-char (point-min)))



(defun find-agrep-occur ()
  (interactive)
  (let ((begin) (end) (to-this-line))
    (save-excursion
      (beginning-of-line)
      (setq begin (point))
      (search-forward ":")
      (backward-char 1)
      (setq end (point)))
    (setq to-this-line (string-to-number (buffer-substring begin end)))
    (other-window -1)
    (goto-line to-this-line)))


