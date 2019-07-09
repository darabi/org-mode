(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))

(require 'slime-autoloads)

(setq slime-contribs '(slime-fancy slime-asdf slime-tramp slime-repl slime-compiler-notes-tree))

(when (require 'my-x-functions "my-x-functions" nil)
  (save-excursion
    (unless (eq (window-system) 'w32)
      (my-set-x-font))))

(provide 'my-init)
