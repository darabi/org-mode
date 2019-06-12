;;;
;;; Copyright (c) 2019 m-creations gmbh, All rights reserved
;;;

;; modify the ugly treemacs icons

(with-eval-after-load 'treemacs
  (with-eval-after-load 'all-the-icons
    (let ((all-the-icons-default-adjust 0)
          (tab-width 1))
      ;; Root icon
      (setq treemacs-icon-root-png
            (concat (all-the-icons-octicon "repo" :height 0.8 :v-adjust -0.2)  " "))
      ;; File icons
      (setq treemacs-icon-open-png
            (concat
             (all-the-icons-octicon "chevron-down" :height 0.8 :v-adjust 0.1)
             "\t"
             (all-the-icons-octicon "file-directory" :v-adjust 0)
             "\t")
            treemacs-icon-closed-png
            (concat
             (all-the-icons-octicon "chevron-right" :height 0.8
                                    :v-adjust 0.1 :face 'font-lock-doc-face)
             "\t"
             (all-the-icons-octicon "file-directory" :v-adjust 0 :face 'font-lock-doc-face)
             "\t"))
      ;; File type icons
      (setq treemacs-icons-hash (make-hash-table :size 200 :test #'equal)
            treemacs-icon-fallback (concat
                                    "\t\t"
                                    (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver
                                                          :height 0.8 :v-adjust 0.0)
                                    "\t")
            treemacs-icon-text treemacs-icon-fallback)

      (dolist (item all-the-icons-icon-alist)
        (let* ((extension (car item))
               (func (cadr item))
               (args (append (list (caddr item)) '(:v-adjust -0.05) (cdddr item)))
               (icon (apply func args))
               (key (s-replace-all '(("^" . "") ("\\" . "") ("$" . "") ("." . "")) extension))
               (value (concat "\t\t" icon "\t")))
          (unless (ht-get treemacs-icons-hash (s-replace-regexp "\\?" "" key))
            (ht-set! treemacs-icons-hash (s-replace-regexp "\\?" "" key) value))
          (unless (ht-get treemacs-icons-hash (s-replace-regexp ".\\?" "" key))
            (ht-set! treemacs-icons-hash (s-replace-regexp ".\\?" "" key) value)))))))

(require 'all-the-icons)

(unless (file-exists-p "~/.local/share/fonts/all-the-icons.ttf")
  (all-the-icons-install-fonts))

(require 'treemacs)

(add-hook 'treemacs-mode-hook '(lambda ()
                                 (custom-set-faces
                                  '(treemacs-git-modified-face ((t (:inherit variable-pitch :foreground "tomato"))))
                                  '(treemacs-root-face ((t (:inherit (variable-pitch font-lock-string-face) :weight bold :height 1.0))))
                                  '(variable-pitch ((t (:height 0.95))))
                                  '(hl-line ((t (:background "#104e8b")))))))

(provide 'mc-treemacs-init)
