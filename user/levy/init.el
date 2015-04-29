(unless (and (setq dwim-workspace (or (getenv "DWIM_WORKSPACE")
                                      (expand-file-name "~/workspace")))
             (file-exists-p dwim-workspace))
  (error "Could not find workspace directory (tried '%s'). Set/change the shell environment variable DWIM_WORKSPACE if you want to use something else." dwim-workspace))

(add-to-list 'load-path (expand-file-name (concat dwim-workspace "/hu.dwim.environment/emacs/")))
(require 'dwim-init)
(require 'dwim-key-bindings)

(global-set-key (kbd "<kp-begin>") (lambda () (interactive) (insert "\\")))

;;(global-set-key (kbd ">") (lambda () (interactive) (insert "|")))

(global-set-key (kbd "\\") 'dwim-switch-between-slime-repl-and-last-buffer)
(global-set-key (kbd "M-\\") 'dwim-switch-between-debug-and-last-source-buffer)
(global-set-key (kbd "C-\\") 'switch-to-other-buffer)
(global-set-key (kbd "M-t") 'toggle-truncate-lines)

(global-set-key (kbd "M-a") (lambda () (interactive) (insert "á")))
(global-set-key (kbd "M-e") (lambda () (interactive) (insert "é")))
(global-set-key (kbd "M-i") (lambda () (interactive) (insert "í")))
(global-set-key (kbd "M-o") (lambda () (interactive) (insert "ó")))
(global-set-key (kbd "C-. o") (lambda () (interactive) (insert "ö")))
(global-set-key (kbd "C-, o") (lambda () (interactive) (insert "ő")))
(global-set-key (kbd "M-u") (lambda () (interactive) (insert "ú")))
(global-set-key (kbd "C-. u") (lambda () (interactive) (insert "ü")))
(global-set-key (kbd "C-, u") (lambda () (interactive) (insert "ű")))

(global-set-key (kbd "M-z") 'redo)
(global-set-key (kbd "S-<delete>") 'kill-primary-selection)
(global-set-key (kbd "C-<insert>") 'copy-primary-selection)
(global-set-key (kbd "S-<insert>") 'yank-clipboard-selection)

(require 'sunrise-commander)
(require 'color-moccur)
(require 'moccur-edit)
(global-set-key (kbd "M-c") 'sunrise)

(setf undo-high-threshold 100000)
(setf undo-threshold 2000000)

(require 'pretty-unicode)

(setf slime-default-lisp 'sbcl-hu.dwim-development)
(setf slime-highlight-suppressed-forms t)

(dwim-redefine-ido-key (kbd "C-z") 'undo)
(dwim-redefine-ido-key (kbd "M-z") 'redo)

(add-hook 'slime-mode-hook (lambda ()
                             (outline-minor-mode)))

(setq shown t)

(global-set-key (kbd "C-M-o") (lambda ()
                                (interactive)
                                (make-local-variable 'shown)
                                (if shown
                                    (hide-body)
                                    (show-all))
                                (setq shown (not shown))))
