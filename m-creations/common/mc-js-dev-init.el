;;;
;;; Copyright (c) 2019 m-creations gmbh, All rights reserved
;;;

(require 'projectile)
(require 'js2-mode)
(require 'tide)
(require 'company)

;;; Javascript

;; for the flycheck validations, you need
;; npm install -g eslint eslint-plugin-angular eslint-config-angular babel-eslint
;;
;; on emacs 24, you also need to install the package let-alist from melpa
;; either with M-x package-list-packages, or C-u M-x package-install let-alist
;;
(add-hook 'js-mode-hook
          '(lambda () (js2-minor-mode)))

;; Typescript/Tide

(defun setup-tide-mode ()
  (interactive)
  (flycheck-mode +1)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))


;; formats the buffer before saving
(defun my-tide-mode-before-save-hook ()
  (when (eq major-mode 'tide-mode)
    (tide-format-before-save)))

(add-hook 'typescript-mode-hook #'my-tide-mode-before-save-hook)

(defun mc-setup-js2-mode ()
  (setq-default js2-basic-offset 2
                js2-indent-switch-body t
                js2-auto-indent-p t
                js2-global-externs '("angular")
                js2-indent-on-enter-key t
                flycheck-disabled-checkers '(javascript-jshint)
                flycheck-checkers '(javascript-eslint)
                flycheck-eslintrc "~/.eslintrc"
                flycheck-check-syntax-automatically '(save mode-enabled)
                ;; aligns annotation to the right hand side
                company-tooltip-align-annotations t)
  (add-to-list 'interpreter-mode-alist (cons "node" 'js2-mode))

  (flycheck-mode +1)
  (tide-setup)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(add-hook 'js2-mode-hook 'mc-setup-js2-mode)
(add-hook 'js2-minor-mode-hook 'mc-setup-js2-mode)

(add-hook 'js2-jsx-mode-hook
          '(lambda ()
             (message "jsx-mode-hook")
             (setq-local sgml-basic-offset js2-basic-offset)))

(add-hook 'flycheck-mode-hook
          '(lambda ()
             (require 'flycheck-js)
             (setq js2-strict-missing-semi-warning nil)
             (setq js2-missing-semi-one-line-override t)
             (mc-use-js-executables-from-node-modules)))

(provide 'mc-js-dev-init)
