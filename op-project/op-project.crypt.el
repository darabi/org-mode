;
;
; Problem Reihenfolge: find-file-hook, auto-mode-alist, dann decryption 
;
; Diese simple Lösung hier ist sehr trickreich.
;
; SMW 2.3.1995
;
(defvar use-keymap-for-crypted-files nil)
(make-variable-buffer-local 'use-keymap-for-crypted-files)

(require 'op-crypt "op-crypt")


(if (file-exists-p buffer-file-truename)
    ;; hier wird erst der Logbuch-alarm ausgeschaltet, falls dieser schon definiert ist,
    ;; da sonst der Alarm sich staut während der Passwort-Abfrage
    (crypt-encrypt-region (point-min) (point-max) 
			  (crypt-read-string-no-echo "Password: ") t))

(discard-input)
(string-match "\\(.*/\\)*" (prin1-to-string (current-buffer) t))
(let ((save-string-tmp (match-end 0))
      (end-of-namestring (string-match "\\.crypt" (prin1-to-string (current-buffer) t))))
  (let ((new-buffer-name (substring (prin1-to-string (current-buffer) t) save-string-tmp end-of-namestring)))

;;;
;;; Administrating of two different buffers for crypt    
;;;
    (rename-buffer new-buffer-name)
    (set-buffer new-buffer-name)
    (setq buffer-file-name new-buffer-name)
    (setq buffer-file-truename new-buffer-name)
;;; trotz Decryption soll der File nicht als verändert gelten
    (set-buffer-modified-p nil)
    
;;;
;;; Aktivieren der Tastatur für gekryptete Files
    (setq use-keymap-for-crypted-files t)
;;;
;;; Projekt-Mode
;;;
    (project-mode)))
    
;(buffer-local-variables (get-buffer "adressen.op")) mit Lisp-Interaction-Mode im Scratch-Buffer


