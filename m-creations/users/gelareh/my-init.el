(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))

(require 'slime-autoloads)

(setq slime-contribs '(slime-fancy slime-asdf slime-tramp slime-repl slime-compiler-notes-tree))

(provide 'my-init)
