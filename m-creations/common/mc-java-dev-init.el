;;;
;;; Copyright (c) 2020 m-creations gmbh, All rights reserved
;;;
;;; See LICENCE for details.

(setq use-package-always-ensure t)

(use-package projectile)
(use-package flycheck)
(use-package yasnippet :config (yas-global-mode))
(use-package lsp-mode :hook ((lsp-mode . lsp-enable-which-key-integration))
  :config (setq lsp-completion-enable-additional-text-edit t))

(use-package hydra)
(use-package company)

(use-package lsp-ui)
(use-package which-key :config (which-key-mode))
(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))
(use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))
(use-package dap-java :ensure nil)
(use-package posframe) ; for dap-ui-controls-mode
(use-package helm-lsp)
(use-package helm
  :config (helm-mode))

(use-package all-the-icons)
(use-package lsp-treemacs)

(require 'lsp-java)

(define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
(define-key lsp-mode-map [f12] #'lsp-treemacs-java-deps-list)
(define-key lsp-mode-map (kbd "C-1") #'lsp-execute-code-action)

(define-key lsp-treemacs-deps-list-mode-map [f12] #'lsp-treemacs-java-deps-list)

(add-hook 'java-mode-hook (lambda ()
                            (setq lsp-ui-doc-enable nil)
                            (lsp)
                            ;; (save-excursion (lsp-treemacs-java-deps-list))
                            ;; (lsp-treemacs-java-deps-follow)
                            (lsp-lens-mode)
                            (setq lsp-ui-doc-use-webkit nil)
                            (lsp-java-lens-mode)))

;; (add-hook 'lsp-treemacs-deps-list-mode (lambda ()
;;                                          (treemacs-toggle-fixed-width)))

(require 'package)

(package-initialize)

(setq package-selected-packages '(posframe flycheck lsp-mode hydra company lsp-ui which-key lsp-java dap-mode helm-lsp helm lsp-treemacs projectile yasnippet))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (package-install-selected-packages))

(add-hook 'prog-mode-hook 'linum-mode)

;; (add-hook 'prog-mode-hook 'lsp)

(define-key dap-ui-mode-map [f5] #'dap-step-in)
(define-key dap-ui-mode-map [f6] #'dap-next)
(define-key dap-ui-mode-map [f7] #'dap-step-out)
(define-key dap-ui-mode-map [f8] #'dap-continue)

(with-eval-after-load 'lsp-mode
  (setq gc-cons-threshold (* 100 1024 1024)
        read-process-output-max (* 1024 1024))

  (helm-mode)
  (which-key-mode)

  (require 'dap-java)
  (add-hook 'lsp-mode-hook 'lsp-enable-which-key-integration)
  (setq dap-auto-configure-features '(sessions locals breakpoints expressions controls tooltip repl))
  (dap-auto-configure-mode)
  (yas-global-mode))

(provide 'mc-java-dev-init)
