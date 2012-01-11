
; Gegenstück zu dem File op-project.crypt.el
;
; SMW 5.4.1995
;
(require 'op-crypt "op-crypt")

;;;
;;; Aktivieren der jeweiligen BUFFER-lokalen Keymap durch eine Minor-Mode-List
;;;

(defvar use-keymap-for-crypted-files nil)
(make-variable-buffer-local 'use-keymap-for-crypted-files)
(setq use-keymap-for-crypted-files nil)
(discard-input)

(project-mode)

