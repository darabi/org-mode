(unless (and (setq dwim-workspace (or (getenv "DWIM_WORKSPACE")
                                      (expand-file-name "~/workspace")))
             (file-exists-p dwim-workspace))
  (error "Could not find workspace directory (tried '%s'). Set/change the shell environment variable DWIM_WORKSPACE if you want to use something else." dwim-workspace))

(add-to-list 'load-path (expand-file-name (concat dwim-workspace "/hu.dwim.environment/emacs/")))
(require 'dwim-init)
(require 'dwim-key-bindings)

;;(require 'w3m-load)

(setf minibuffer-message-timeout 0)

(global-set-key (kbd "M-b") 'redo)
(global-set-key (kbd "C-b") 'undo)

;;(global-set-key (kbd "M-C-c") 'copy-primary-selection)
;;(global-set-key (kbd "M-C-v") 'yank-clipboard-selection)
;;(global-set-key (kbd "M-C-x") 'kill-primary-selection)
(dwim-define-lisp-key (kbd "M-C-x") 'kill-primary-selection)
(dwim-define-lisp-key (kbd "C-c M-C-c") 'slime-eval-defun :elisp nil)

(dwim-define-lisp-key (kbd "C->") 'dwim-replace-sexp-at-point-with-clipboard)

(global-set-key (kbd "C-,") 'copy-primary-selection)
(global-set-key (kbd "C-.") 'yank-clipboard-selection)
(dwim-define-lisp-key [(meta ?\])] (lambda () (interactive) (insert "\)")))
(dwim-define-lisp-key [(meta ?\[)] (lambda () (interactive) (insert "\(")))
;;(global-set-key-set-key [(control insert)] 'kill-primary-selection)

(global-set-key (kbd "A--") 'switch-between-slime-repl-and-last-buffer)

(dwim-define-lisp-key [(control ?\8)] 'insert-parentheses)
(dwim-define-lisp-key [(control ?\9)] 'move-past-close-and-reindent)
(dwim-define-lisp-key (kbd "\(") 'self-insert-command)
(dwim-define-lisp-key (kbd "\)") 'self-insert-command)
(dwim-define-lisp-key (kbd "\]") 'self-insert-command)
(dwim-define-lisp-key (kbd "\[") 'self-insert-command)

(global-set-key (kbd "C--") 'dwim-switch-between-slime-repl-and-last-buffer)

;;(setq browse-url-firefox-program "/usr/local/bin/Icecat32")
(setq browse-url-firefox-program "firefox")

(dwim-define-lisp-key (kbd "C-M-,") 'dwim-copy-sexp-at-point)

(setq font-lock-verbose nil)
(setq slime-default-lisp 'sbcl-hu.dwim-development)
