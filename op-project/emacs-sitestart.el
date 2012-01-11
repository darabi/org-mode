(setq load-path (cons (expand-file-name "~/op-project") load-path))
(defun project-mode()
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

