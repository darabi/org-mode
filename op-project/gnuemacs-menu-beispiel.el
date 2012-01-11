(defvar pymacs-mode-map ()
  "Keymap for Emacs Lisp mode.
All commands in `shared-lisp-mode-map' are inherited by this map.")

(if pymacs-mode-map
    ()
  (let ((map (make-sparse-keymap "Pymacs")))
    (setq pymacs-mode-map (make-sparse-keymap))
    (set-keymap-parent pymacs-mode-map shared-lisp-mode-map)
    (define-key pymacs-mode-map "\e\t" 'lisp-complete-symbol)
    (define-key pymacs-mode-map "\e\C-x" 'eval-defun)
    (define-key pymacs-mode-map [menu-bar] (make-sparse-keymap))
    (define-key pymacs-mode-map [menu-bar pymacs]
      (cons "Pymacs" map))
    (define-key map [edebug-defun]
      '("Instrument Function for Debugging" . edebug-defun))
    (define-key map [byte-recompile]
      '("Byte-recompile Directory..." . byte-recompile-directory))))

  (use-local-map pymacs-mode-map)
  (defvar pymacs-mode-menu-bar-map (make-sparse-keymap))
  (menu-bar-update-buffers)
