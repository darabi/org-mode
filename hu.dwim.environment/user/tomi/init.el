(unless (and (setq dwim-workspace (or (getenv "DWIM_WORKSPACE")
                                      (expand-file-name "~/workspace")))
             (file-exists-p dwim-workspace))
  (error "Could not find workspace directory (tried '%s'). Set/change the shell environment variable DWIM_WORKSPACE if you want to use something else." dwim-workspace))

(add-to-list 'load-path (expand-file-name (concat dwim-workspace "/hu.dwim.environment/emacs/")))
(require 'dwim-init)
(require 'dwim-key-bindings)

(add-hook 'lisp-mode-hook (lambda ()
                            (outline-minor-mode 1)))

(load "~/lib/emacs/tomi-init")

;; outline
(setq shown t)
(global-set-key (kbd "C-M-o") (lambda ()
                                (interactive)
                                (make-local-variable 'shown)
                                (if shown
                                    (hide-body)
                                    (show-all))
                                (setq shown (not shown))))
