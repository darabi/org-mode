
(defun my-op-mode ()
  (interactive)
  (progn

  ;;(debug-on-entry 'lisp-interaction-mode) ; --hat menu-bar !!!

    (if (string= (getenv "HOME") "C:/")
	(progn
	  (y-or-n-p "Bitte erst die Variable HOME=\45HOMESHARE\45 definieren !!")
	  (kill-emacs)))
    (if gnuemacsp
	(setq inhibit-startup-message t))
    (if gnuemacsp (standard-display-european t))
    (setq scroll-step 1)
    (require 'cl)
    ;;(setq load-path (cons 
    ;;      (expand-file-name 
    ;;       "//magnolie.netz.klinik.uni-mainz.de/Programme/Emacs20/site-lisp/op-project") 
    ;;       load-path))
    (if gnuemacsp
	(setq load-path (cons (expand-file-name 
			       "U:/users/users01/walther/op-project")
			      (cons (expand-file-name 
				     "U:/users/users01/walther/op-project/op-calendar")
				    load-path))))
    (if xemacsp
	(setq load-path (cons (expand-file-name 
			       "U:/users/users01/walther/op-project")
			      (cons (expand-file-name 
				     "U:/users/users01/walther/op-project/op-calendar")
				    load-path))))
    (setq load--with--org-mode t)  ;; wenn t dann zuerst org-mode laden!
    (add-to-list 'load-path "U:/users/users01/walther/op-project/org-mode")
    (add-to-list 'load-path "U:/users/users01/walther/op-project/org-mode/xemacs")
    
    ;;
    ;; op-mode-Definitionen 
    ;;
    (defun project-mode()
      (interactive)
      (load-library "op-project"))
    (defun op-mode()
      (interactive)
      (load-library "op-project"))
    (autoload 'project-mode "op-project" "Automatic select project mode" t)
    (defun crypted-project-mode ()
      (load-library "op-project.crypt"))
    (defun noncrypted-project-mode()
      (load-library "op-project.noncrypted"))
    (setq auto-mode-alist
	  (nconc '(("\\.op\\.crypt$" . crypted-project-mode)
		   ("\\.op$"         . noncrypted-project-mode))
		 auto-mode-alist))
    (if gnuemacsp
	(require 'hilit19))
    (setq require-final-newline ??)
    (setq lpr-page-header-program "print")
    (setq lpr-command "print")

    ;; kleine Basisfunktion
    (defun convert-dos-newlines ()
      (interactive)
      (save-excursion
	(goto-char (point-min))
	(while (search-forward "\r" nil t)
	  (replace-match "" nil t))))
    )
  )
;;; (my-op-mode)
(my-op-mode)
;;
(message "op-mode")


