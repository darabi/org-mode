(setq dwim-workspace (getenv "DWIM_WORKSPACE"))
(if (not dwim-workspace)
    (error "You MUST set the environment variable DWIM_WORKSPACE"))

(add-to-list 'load-path (expand-file-name (concat dwim-workspace "/hu.dwim.environment/emacs/")))
(require 'dwim-init)
;; (require 'dwim-key-bindings)

(dwim-define-lisp-key (kbd "C-c M-C-c") 'slime-eval-defun :elisp nil)

(global-set-key (kbd "C-,") 'copy-primary-selection)
(global-set-key (kbd "C-.") 'yank-clipboard-selection)
(global-set-key [(control insert)] 'kill-primary-selection)

;;(setq browse-url-firefox-program "/usr/local/bin/Icecat32")
(setq browse-url-firefox-program "firefox")


(define-key slime-macroexpansion-minor-mode-map (kbd "M-b")
  (lookup-key slime-macroexpansion-minor-mode-map (kbd "C-c <RET>")))

(dwim-define-lisp-key (kbd "C-M-,") 'dwim-copy-sexp)

(global-set-key (kbd "M-b") 'redo)
(global-set-key (kbd "C-b") 'undo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;


(dwim-define-lisp-key (kbd "C->") 'dwim-replace-sexp-at-point-with-clipboard)
(dwim-define-lisp-key (kbd "C-M-,") 'dwim-copy-sexp-at-point)
(dwim-define-lisp-key (kbd "M-C-.") 'dwim-paste-clipboard-at-last-change)

(add-hook 'bbdb-initialize-hook (lambda ()
                                  (setq bbdb-user-mail-names
                                        (regexp-opt '("kambiz.darabi@gmail.com")))))

;(add-hook 'lisp-mode-hook (lambda ()
;                            (setf slime-default-lisp 'sbcl-perec-development-image)))


;; (add-to-list 'load-path (expand-file-name "~/workspace/environment/emacs-addons/icicles/"))

;; (require 'icicles)

;; (icy-mode)

;; (unless icicle-fuzzy-completion-flag
;;   (icicle-toggle-fuzzy-completion))

;; (add-hook 'ido-define-mode-map-hook
;;           (lambda ()
;;             (flet ((redef (key function)
;;                      (define-key ido-completion-map key function)))
;;               (redef (kbd "M-p") 'previous-history-element)
;;               (redef (kbd "<up>") 'previous-history-element)
;;               (redef (kbd "M-n") 'next-history-element)
;;               (redef (kbd "<down>") 'next-history-element))))

(dwim-redefine-ido-key (kbd "C-b") 'undo)
(dwim-redefine-ido-key (kbd "M-b") 'redo)

(dwim-define-lisp-key [(tab)] 'slime-fuzzy-indent-and-complete-symbol :elisp nil)

;; (dwim-define-lisp-key [(control ?i)] 'slime-inspect :elisp nil)

;; (dwim-define-lisp-key [(tab)] (lambda ()
;;                            (interactive)
;;                            (ielm-complete-symbol)) :elisp nil)
(dwim-define-lisp-key (kbd "M-<delete>") 'paredit-forward-kill-word)
(dwim-define-lisp-key (kbd "M-<backspace>") 'paredit-backward-kill-word)
(dwim-define-lisp-key (kbd "M-<left>") (lambda () (interactive) (backward-sexp)))
(dwim-define-lisp-key (kbd "M-<down>") 'down-list)
(dwim-define-lisp-key (kbd "M-<up>") 'backward-up-list)
(dwim-define-lisp-key (kbd "M-<right>") (lambda () (interactive) (slime-forward-sexp)))

(when (boundp 'sgml-mode-hook)
  (add-hook 'sgml-mode-hook
            (lambda ()
              (flet ((def (key function)
                         (define-key sgml-mode-map key function)))
                (def (kbd "M-<right>") 'sgml-skip-tag-forward)
                (def (kbd "M-<left>") 'sgml-skip-tag-backward)))
            t))

(when (boundp 'nxml-mode-hook)
  (add-hook 'nxml-mode-hook
            (lambda ()
              (flet ((def (key function)
                         (define-key nxml-mode-map key function)))
                (def (kbd "M-<right>") 'nxml-forward-element)
                (def (kbd "M-<left>")  'nxml-backward-element)
                (def (kbd "M-<up>")    'nxml-backward-up-element)
                (def (kbd "M-<down>")  'nxml-down-element)))
            t))

(dwim-define-lisp-key (kbd "M-S-<right>") 'forward-sexp-mark)
(dwim-define-lisp-key (kbd "M-S-<left>") 'backward-sexp-mark)

(dwim-define-lisp-key (kbd "M-<insert>") 'dwim-copy-sexp-at-point)

(dwim-define-lisp-key [(meta shift ?f)] 'dwim-findr-search-sexp)

(dwim-define-lisp-key (kbd "M-;") 'paredit-comment-dwim)
(dwim-define-lisp-key (kbd "C-q") 'indent-sexp)
(dwim-define-lisp-key (kbd "M-q") 'slime-reindent-defun)
(dwim-define-lisp-key (kbd "C-k") 'kill-sexp)
(dwim-define-lisp-key (kbd "C-M-d") 'slime-repl-delete-from-input-history :buffers nil :elisp nil)
(dwim-define-lisp-key (kbd "C-M-d") 'slime-describe-symbol :repl nil :elisp nil)
(dwim-define-lisp-key (kbd "C-M-k") (lambda ()
                                      (interactive)
                                      (unless (hu.dwim.quasi-quote.after-sexp-separator-p)
                                        (ignore-errors (backward-sexp)))
                                      (kill-sexp 1)
                                      (dwim-forward-kill-whitespaces)
                                      (save-excursion
                                        (when (or (looking-at ")")
                                                  (looking-at ">")
                                                  (looking-at "\\]"))
                                          (dwim-backward-kill-whitespaces))
                                        (backward-char)
                                        (when (or (looking-at "(")
                                                  (looking-at "<")
                                                  (looking-at "\\["))
                                          (dwim-forward-kill-whitespaces))))
  :repl nil)

(dwim-define-lisp-key (kbd "C-j") (lambda ()
                                    (interactive)
                                    (let ((bounds (dwim-whitespace-bounds-at-point)))
                                      (when bounds
                                        (destructuring-bind (start end) bounds
                                          (if (string-match "\n" (buffer-substring start end))
                                              (progn
                                                (delete-region start end)
                                                (when (and (not (char-equal (following-char) ?\) ))
                                                           (not (char-equal (preceding-char) ?\( )))
                                                  (insert-string " ")))
                                              (paredit-newline)))))))

(dwim-define-lisp-key [(control t)] 'transpose-sexps)
(dwim-define-lisp-key [(control shift t)] (lambda ()
                                            (interactive)
                                            (transpose-sexps 1)
                                            (backward-sexp 2)))
;;; (global-set-key (kbd "C--") 'universal-argument) ; we use C-u, its original mapping
(dwim-define-lisp-key [(meta up)] 'backward-up-list)
;;; (dwim-define-lisp-key (kbd "C-u") 'paredit-raise-sexp)
;; jump to last mark without prefix, set mark with prefix. this inverts the original behaviour wrt prefixing.
;; (dwim-define-lisp-key (kbd "C-S-SPC") (lambda ()
;;                                       (interactive)
;;                                       (if current-prefix-arg
;;                                           (set-mark-command 1)
;;                                           (set-mark-command nil))))
(dwim-define-lisp-key (kbd "M-SPC") (lambda ()
                                      (interactive)
                                      (push-mark)))
(dwim-define-lisp-key (kbd "M-C-u") 'paredit-splice-sexp)
;;; (dwim-define-lisp-key (kbd "C-w") (lambda (n)
;;;                                     (interactive "P")
;;;                                     (hu.dwim.quasi-quote.wrap-selection-or-sexp nil n)))
;;; (dwim-define-lisp-key (kbd "C-S-w") (lambda (n)
;;;                                       (interactive "P")
;;;                                       (hu.dwim.quasi-quote.wrap-selection-or-sexp t n)))

(dwim-define-lisp-key (kbd "M-C-f") 'slime-search-definitions)
(dwim-define-lisp-key (kbd "C-c RET") 'slime-macroexpand-1 :repl nil)

(dwim-define-lisp-key (kbd "C-c C-c") 'dwim-slime-compile-defun :elisp nil :repl nil)

(dwim-define-lisp-key (kbd "C-M-k") 'slime-repl-kill-input :buffers nil :elisp nil)
(dwim-define-lisp-key (kbd "C-M-r") (lambda ()
                                      (interactive)
                                      (slime-restart-inferior-lisp)
                                      (slime-repl-clear-buffer))
                      :buffers nil :elisp nil)
(dwim-define-lisp-key (kbd "C-M-c") 'slime-repl-clear-buffer :buffers nil :elisp nil)
(dwim-define-lisp-key (kbd "C-c C-c") 'eval-defun :elisp nil :repl nil)

(global-set-key (kbd "M-f") 'findr-search)
(global-set-key (kbd "C-n") 'tags-loop-continue)

;; C-o is normally bound to open-line which inserts a newline
;; at point and stays there
(global-set-key (kbd "C-o") 'other-window)
(define-key Buffer-menu-mode-map (kbd "C-o") 'other-window)
(define-key dired-mode-map (kbd "C-o") 'other-window)

;; usually: (move-to-window-line arg) 
(global-set-key (kbd "M-r") 'query-replace)
(global-set-key [(meta shift r)] 'dwim-query-replace-current-sexp)
(global-set-key [(control shift f)] 'findr)
(global-set-key [(control shift s)] 'findr-search)
(global-set-key [(control shift r)] 'findr-query-replace)
(global-set-key [(control shift i)] (lambda ()
				      (interactive)
				      (slime-selector ?i)))
(global-set-key [(control shift t)] (lambda ()
				      (interactive)
				      (slime-selector ?t)))
(global-set-key [(control shift c)] (lambda ()
				      (interactive)
				      (slime-selector ?c)))

;; darabi: C-M-l locks the gnome session
;; (global-set-key (kbd "C-M-l") 'recenter)
(global-set-key (kbd "C-M-u") 'universal-argument)
(global-set-key (kbd "C-l") 'switch-to-other-buffer)
;; (global-set-key [(control e)] 'delete-window)
(global-set-key [(control shift e)] 'delete-other-windows)
(global-set-key (kbd "C-p") 'dwim-kill-this-buffer-and-window)
(define-key dired-mode-map (kbd "C-p") 'dwim-kill-this-buffer-and-window)
(global-set-key (kbd "C-M-\\") 'slime-selector)
;; (global-set-key (kbd "S-<delete>") 'kill-primary-selection)
;; (global-set-key (kbd "C-<insert>") 'copy-primary-selection)
;; (global-set-key (kbd "S-<insert>") 'yank-clipboard-selection)

;;; Fonts

(defun my-lisp-mode-fonts ()
  (font-lock-add-keywords
   'lisp-mode
   ;; atdoc keywords
   `(("@\\(a\\|aboutclass\\|aboutfun\\|arg\\|b\\|class\\|code\\|em\\|fun\\|itemize\\|pre\\|return\\|section\\|see\\|see-constructor\\|see-slot\\|short\\|variable\\)\\(\\[.*\\]\\)" 0 'font-lock-preprocessor-face t))) 'set)

(my-lisp-mode-fonts)
 
;; (font-lock-remove-keywords 'lisp-mode '(("\\(short\\|arg\\)")))
;; (font-lock-refresh-defaults)

(defun dwim/add-slime-impl-image (image-name &rest args)
  (let ((name (replace-regexp-in-string "hu\.dwim\." "" image-name)))
    (add-to-list 'slime-lisp-implementations 
                 `(,(intern name) 
                   (lambda ()
                     (dwim/build-sbcl-lisp-implementation-entry/image 
                      ,(concat image-name "_development") ,@args))))))


(dwim/add-slime-impl-image "hu.dwim.wiki" "--dynamic-space-size=200")
(dwim/add-slime-impl-image "hu.dwim.home")
(dwim/add-slime-impl-image "hu.dwim.perec")
(dwim/add-slime-impl-image "hu.dwim.quasi-quote")
(dwim/add-slime-impl-image "hu.dwim.rdbms")
(dwim/add-slime-impl-image "hu.dwim.reader")
(dwim/add-slime-impl-image "hu.dwim.web-server" "--dynamic-space-size=800mb")
(dwim/add-slime-impl-image "hu.dwim.presentation")
(dwim/add-slime-impl-image "rodin")
(dwim/add-slime-impl-image "rodin.peaches")
(dwim/add-slime-impl-image "alexandria")
(dwim/add-slime-impl-image "fluvium")
(dwim/add-slime-impl-image "nibbles")

(add-to-list 'slime-lisp-implementations `(acl ("~/opt/acl/alisp-modern" "-l"
                                                                  ,(concat dwim-workspace "/hu.dwim.environment/source/environment.lisp"))))

(setf slime-default-lisp 'web-server)

;; (setf slime-default-lisp 'rodin)
;; (setf slime-default-lisp 'rdbms)

;; (setf slime-default-lisp 'alexandria)

(defun my-slime-add-arg (lisp &rest args)
  (let* ((impl (slime-lookup-lisp-implementation slime-lisp-implementations lisp))
         (arglist (plist-get impl :program-args)))
    (append arglist (if (typep args 'list)
                        args
                        (list args)))))

