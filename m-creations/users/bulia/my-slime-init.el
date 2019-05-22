;; (require 'slime-docker)

;; (setq slime-docker-implementations '((sbcl ("--eval" "(ql:quickload :swank)" "--eval" "(progn (swank-loader:init) (setf swank::*loopback-interface* \"0.0.0.0\") (swank:create-server))")
;; 					   :image-name "mcreations/sbcl"
;; 					   :image-tag "1.4.1-mc-2017-10-23")))
(require 'slime)

(require 'dwim-slime)
(require 'dwim-util)

(require 'mc-lisp-indent)
(require 'mc-lisp-font-lock)

;; reset the dwim-init modification to these vars
(setq-default
 interprogram-cut-function (when (display-graphic-p) 'gui-select-text))

(dwim-define-lisp-key (kbd "C-c M-C-c") 'slime-eval-defun :elisp nil)

(define-key slime-macroexpansion-minor-mode-map (kbd "M-b")
  (lookup-key slime-macroexpansion-minor-mode-map (kbd "C-c <RET>")))

(dwim-define-lisp-key (kbd "C-M-,") 'dwim-copy-sexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (dwim-define-lisp-key (kbd "C->") 'dwim-replace-sexp-at-point-with-clipboard)
(dwim-define-lisp-key (kbd "C-M-,") 'dwim-copy-sexp-at-point)
(dwim-define-lisp-key (kbd "M-C-.") 'dwim-paste-clipboard-at-last-change)

(dwim-define-lisp-key [(tab)] 'slime-fuzzy-indent-and-complete-symbol :elisp nil)

(dwim-define-lisp-key (kbd "M-<delete>") 'paredit-forward-kill-word)
(dwim-define-lisp-key (kbd "M-<backspace>") 'paredit-backward-kill-word)
(dwim-define-lisp-key (kbd "M-<left>") 'paredit-forward-barf-sexp)
(dwim-define-lisp-key (kbd "M-<down>") 'down-list)
(dwim-define-lisp-key (kbd "M-<up>") 'backward-up-list)
(dwim-define-lisp-key (kbd "M-<right>") 'paredit-forward-slurp-sexp)

(when (boundp 'sgml-mode-hook)
  (add-hook 'sgml-mode-hook
            (lambda ()
              (cl-flet ((def (key function)
                         (define-key sgml-mode-map key function)))
                (def (kbd "M-<right>") 'sgml-skip-tag-forward)
                (def (kbd "M-<left>") 'sgml-skip-tag-backward)))
            t))

(when (boundp 'nxml-mode-hook)
  (add-hook 'nxml-mode-hook
            (lambda ()
              (cl-flet ((def (key function)
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

(global-set-key [(meta shift r)] 'dwim-query-replace-current-sexp)

(global-set-key [(control shift i)] (lambda ()
                                      (interactive)
                                      (slime-selector ?i)))
(global-set-key [(control shift t)] (lambda ()
                                      (interactive)
                                      (slime-selector ?t)))
(global-set-key [(control shift c)] (lambda ()
                                      (interactive)
                                      (slime-selector ?c)))

(global-set-key (kbd "C-M-\\") 'slime-selector)
;; (global-set-key (kbd "S-<delete>") 'kill-primary-selection)
;; (global-set-key (kbd "C-<insert>") 'copy-primary-selection)
;; (global-set-key (kbd "S-<insert>") 'yank-clipboard-selection)

(defun dwim/add-slime-impl-image (image-name &rest args)
  (let ((name (replace-regexp-in-string "hu\.dwim\." "" image-name)))
    (add-to-list 'slime-lisp-implementations
                 `(,(intern name)
                   (lambda ()
                     (dwim/build-sbcl-lisp-implementation-entry/image
                      ,(concat image-name "_development") ,@args))))))

;; clean the slime-lisp-implementations
(setq slime-lisp-implementations '((sbcl . "sbcl")))

(dwim/add-slime-impl-image "bernini")
(dwim/add-slime-impl-image "holz24")

(add-to-list 'slime-lisp-implementations `(sbcl-system ("/usr/bin/sbcl")))

;; (setf slime-default-lisp 'sbcl-system)
(setf slime-default-lisp 'holz24)


(defun kd-dwim-redefine-keys ()
  ;; C-right forward-word is important for me
  (define-key paredit-mode-map (kbd "M-<right>")
    'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd "C-<right>")
    'forward-word)
  (define-key paredit-mode-map (kbd "M-<left>")
    'paredit-forward-barf-sexp)
  (define-key paredit-mode-map (kbd "C-<left>")
    'backward-word))

(add-hook 'paredit-mode-hook
          '(lambda ()
            (kd-dwim-redefine-keys)))


(setf show-paren-mode t)
(setf show-paren-style 'parenthesis)

(provide 'my-slime-init)
