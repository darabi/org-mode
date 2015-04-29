;;;;;;
;;; key shortcuts - globals

;; Unset some keys I often press accidentally
(global-unset-key [(control x) (control z)])
(global-unset-key (kbd "M-<down>"))
(global-unset-key (kbd "C-v"))
(global-unset-key [(insert)])
(global-unset-key (kbd "C-x <left>"))
(global-unset-key (kbd "C-x <right>"))
(global-unset-key (kbd "C-x C-c"))

;; needed to differentiate from C-d
(global-set-key [delete] 'delete-char)

;; pc-select installs -nomark versions of these that does not push-mark
(global-set-key [(control home)] 'beginning-of-buffer)
(global-set-key [(control end)] 'end-of-buffer)

(global-set-key [(control k)] 'kill-word)
(global-set-key [(meta t)] 'toggle-truncate-lines)
(global-set-key [(control t)] 'transpose-words)
(global-set-key [(control shift t)] (lambda ()
                                      (interactive)
                                      (transpose-words 1)
                                      (backward-word 2)))

(global-set-key [(super l)] (lambda ()
                              (interactive)
                              (insert "λ")))

(global-set-key [(super shift l)] (lambda ()
                                    (interactive)
                                    (insert "Λ")))

(global-set-key [(meta insert)] 'overwrite-mode)

(global-set-key (kbd "M-<backspace>") 'backward-kill-word)
(global-set-key (kbd "M-<delete>") 'kill-word)

(global-set-key (kbd "C-M-<up>") (lambda ()
                                   (interactive)
                                   (scroll-down 5)))
(global-set-key (kbd "C-M-<down>") (lambda ()
                                     (interactive)
                                     (scroll-up 5)))
(global-set-key (kbd "C-M-<left>") (lambda ()
                                     (interactive)
                                     (scroll-right 20)))
(global-set-key (kbd "C-M-<right>") (lambda ()
                                      (interactive)
                                      (scroll-left 20)))

(global-set-key (kbd "C-h") 'goto-last-change-with-auto-marks)
(define-key isearch-mode-map (kbd "C-h") 'goto-last-change-with-auto-marks)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-z") 'undo)
(global-set-key [(control backspace)] 'undo)
(global-set-key [(shift delete)] 'kill-primary-selection)
(global-set-key (kbd "C-f") 'find-file)
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; also swich to the buffer list window
(global-set-key (kbd "C-b")     'dwim-list-buffers)
(global-set-key (kbd "C-x C-b") 'dwim-list-buffers)
(global-set-key (kbd "C-M-b")   'dwim-list-buffers)

;; select buffer and close buffer list window
(define-key Buffer-menu-mode-map (kbd "RET") 'Buffer-menu-select)
(define-key Buffer-menu-mode-map (kbd "q") 'dwim-kill-this-buffer-and-window)

(global-set-key (kbd "C-o") 'other-window)
(define-key Buffer-menu-mode-map (kbd "C-o") 'other-window)
(define-key dired-mode-map (kbd "C-o") 'other-window)

(global-set-key (kbd "M-r") 'query-replace)
(global-set-key [(meta shift r)] 'dwim-query-replace-current-sexp)
(global-set-key [(super s)] 'findr-search)
(global-set-key [(super r)] 'findr-query-replace)
(global-set-key [(super i)] 'dwim-switch-between-slime-inferior-lisp-and-last-buffer)
(global-set-key [(super t)] 'dwim-switch-between-slime-thread-list-and-last-buffer)
(global-set-key [(super c)] 'dwim-switch-between-slime-connection-list-and-last-buffer)
(global-set-key [(super p)] 'dwim-switch-between-slime-sprof-and-last-buffer)

(global-set-key (kbd "C-M-l") 'recenter)
(global-set-key (kbd "C-M-u") 'universal-argument)
(global-set-key (kbd "C-l") 'switch-to-other-buffer)
(global-set-key [(control e)] 'delete-window)
(global-set-key [(control shift e)] 'delete-other-windows)
(global-set-key (kbd "C-p") 'dwim-kill-this-buffer-and-window)
(define-key dired-mode-map (kbd "C-p") 'dwim-kill-this-buffer-and-window)
(global-set-key (kbd "C-M-\\") 'slime-selector)
(global-set-key (kbd "S-<delete>") 'kill-primary-selection)
(global-set-key (kbd "C-<insert>") 'copy-primary-selection)
(global-set-key (kbd "S-<insert>") 'yank-clipboard-selection)

(global-set-key (kbd "C-y") 'dwim-insert-and-pop-kill-ring-head)

;;(global-set-key (kbd "M-\\") (lambda () (interactive) (insert "|")))
;;(global-set-key (kbd "M-C-q") 'quoted-insert)

(global-set-key (kbd "M-k") 'dwim-kill-line)
(global-set-key [(control shift delete)] 'dwim-kill-line)

(global-set-key (kbd "C-/") 'dwim-switch-between-slime-repl-and-last-buffer)
(global-set-key (kbd "C-M-/") 'dwim-switch-between-elisp-repl-and-last-buffer)

(global-set-key (kbd "C-;") 'dwim-switch-between-debug-and-last-source-buffer)

(global-set-key (kbd "C-'") 'dwim-switch-between-slime-inspector-and-last-buffer)

(global-set-key [(control ?d)] 'dwim-duplicate-line-at-point)

(global-set-key [(control tab)] 'swbuff-switch-to-next-buffer)
(global-set-key [(control meta tab)] 'dabbrev-completion)

;; emacs24 WTF... this one doesn't work, only the second one
;; (global-set-key [(control shift tab)] 'swbuff-switch-to-previous-buffer)
(global-set-key (kbd "<C-S-iso-lefttab>") 'swbuff-switch-to-previous-buffer)

(define-key sldb-mode-map (kbd "<tab>") 'sldb-cycle)


;;;;;;
;;; IDO setup

(global-set-key "\M-x" 'ido-execute-extended-command)

(add-hook 'ido-setup-hook
          (lambda ()
            ;; in ido mode we want these to work just like in the global mapping
            (dwim-redefine-ido-key (kbd "C-o") 'other-window)
            ;; TODO the nest one seems not to work for some reason...
            (dwim-redefine-ido-key (kbd "C-r") 'ido-toggle-regexp)))

;;;;;;
;;; Slime setup

(eval-after-load 'slime
  '(progn
    (define-key slime-macroexpansion-minor-mode-map [remap undo] 'slime-macroexpand-undo)))


;;;;;;
;;; Lisp key binding, swap () with [], etc

(dwim-define-lisp-key (kbd "RET") 'paredit-newline :repl nil)
(define-key slime-repl-mode-map (kbd "M-RET") 'paredit-newline)
(dwim-define-lisp-key (kbd "M-;") 'paredit-comment-dwim)

(dwim-define-lisp-key (kbd "\"") 'paredit-doublequote)

;; it must be here, because somehow slime overrides global-set-key in the repl
(dwim-define-lisp-key (kbd "C-e") 'delete-window)

(dwim-define-lisp-key (kbd "[") 'paredit-open-round)
(dwim-define-lisp-key (kbd "]") 'paredit-close-round)
(dwim-define-lisp-key (kbd "\(") (lambda () (interactive) (insert "[")))
(dwim-define-lisp-key (kbd "\)") (lambda () (interactive) (insert "]")))
(dwim-define-lisp-key [(meta ?\])] (lambda () (interactive) (insert "\)")))
(dwim-define-lisp-key [(meta ?\[)] (lambda () (interactive) (insert "\(")))

;; binding fuzzy directly (as opposed to using through slime-indent) adds indent functionality also to TAB
(dwim-define-lisp-key [(tab)] 'slime-fuzzy-indent-and-complete-symbol :elisp nil :scheme nil)

(dwim-define-lisp-key [(control ?i)] 'slime-inspect :elisp nil)

(dwim-define-lisp-key [(tab)] 'complete-symbol :cl nil :scheme nil)
(dwim-define-lisp-key (kbd "M-<delete>") 'paredit-forward-kill-word)
(dwim-define-lisp-key (kbd "M-<backspace>") 'paredit-backward-kill-word)
(dwim-define-lisp-key (kbd "M-<left>") 'backward-sexp)
(dwim-define-lisp-key (kbd "M-<down>") 'paredit-down-list)
(dwim-define-lisp-key (kbd "M-<up>") 'paredit-backward-up-list)
(dwim-define-lisp-key (kbd "M-<right>") 'forward-sexp)

(when (boundp 'sgml-mode-hook)
  (add-hook 'sgml-mode-hook
            (lambda ()
              (flet ((def (key function)
                         (define-key sgml-mode-map key function)))
                (def (kbd "M-<right>") 'sgml-skip-tag-forward)
                (def (kbd "M-<left>") 'sgml-skip-tag-backward)))
            t))

(add-hook 'nxml-mode-hook
          (lambda ()
            (flet ((def (key function)
                       (define-key nxml-mode-map key function)))
              (def (kbd "M-<right>") 'nxml-forward-element)
              (def (kbd "M-<left>")  'nxml-backward-element)
              (def (kbd "M-<up>")    'nxml-backward-up-element)
              (def (kbd "M-<down>")  'nxml-down-element)
              (def (kbd "C-k")       'dwim-kill-xml-tag)
              (def (kbd "C-j")       'dwim-kill-whitespaces-or-insert-newline)))
          t)

;; not needed with newer emacs (23+ ?)
;(dwim-define-lisp-key (kbd "M-S-<right>") 'forward-sexp-mark)
;(dwim-define-lisp-key (kbd "M-S-<left>") 'backward-sexp-mark)

(dwim-define-lisp-key (kbd "M-<insert>") 'dwim-copy-sexp-at-point)

(dwim-define-lisp-key [(meta shift ?f)] 'dwim-findr-search-sexp)

(dwim-define-lisp-key (kbd "C-q") 'indent-sexp)
(dwim-define-lisp-key (kbd "M-q") 'slime-reindent-defun)
(dwim-define-lisp-key (kbd "C-k") 'kill-sexp)
(dwim-define-lisp-key (kbd "C-M-d") 'slime-repl-delete-from-input-history :buffers nil :elisp nil :scheme nil)
(dwim-define-lisp-key (kbd "C-M-d") 'slime-describe-symbol :repl nil :elisp nil)
(dwim-define-lisp-key (kbd "C-M-k") (lambda ()
                                      (interactive)
                                      (unless (hu.dwim.quasi-quote:after-sexp-separator-p)
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

(global-set-key (kbd "C-j") 'dwim-kill-whitespaces-or-insert-newline)
;; needed to override the slime REPL definition
(dwim-define-lisp-key (kbd "C-j") 'dwim-kill-whitespaces-or-insert-newline)

(dwim-define-lisp-key [(control t)] 'transpose-sexps)
(dwim-define-lisp-key [(control shift t)] (lambda ()
                                            (interactive)
                                            (transpose-sexps 1)
                                            (backward-sexp 2)))
(global-set-key (kbd "C--") 'universal-argument) ; we use C-u, its original mapping
(dwim-define-lisp-key [(meta up)] 'backward-up-list)
(dwim-define-lisp-key (kbd "C-u") 'paredit-raise-sexp)
;; jump to last mark without prefix, set mark with prefix. this inverts the original behaviour wrt prefixing.
(global-set-key (kbd "C-SPC") (lambda ()
                                (interactive)
                                (if current-prefix-arg
                                    (set-mark-command nil)
                                    (set-mark-command 1))))
(dwim-define-lisp-key (kbd "M-SPC") (lambda ()
                                      (interactive)
                                      (push-mark)))
(dwim-define-lisp-key (kbd "M-C-u") 'paredit-splice-sexp)
(dwim-define-lisp-key (kbd "C-w") (lambda (n)
                                    (interactive "P")
                                    (hu.dwim.quasi-quote:wrap-selection-or-sexp nil n)))
(dwim-define-lisp-key (kbd "C-S-w") (lambda (n)
                                      (interactive "P")
                                      (hu.dwim.quasi-quote:wrap-selection-or-sexp t n)))
;; TODO this is broken
(dwim-define-lisp-key (kbd "M-C-f") (lambda ()
                                      (interactive)
                                      (slime-find-definitions (slime-symbol-at-point))))
(dwim-define-lisp-key (kbd "C-c RET") (lambda ()
                                        (interactive)
                                        (pp-macroexpand-expression (read (slime-sexp-at-point))))
                      :elisp t :cl nil :scheme nil)

(dwim-define-lisp-key (kbd "C-c C-c") 'dwim-slime-compile-defun :elisp nil :repl nil)

(dwim-define-lisp-key (kbd "C-M-k") 'slime-repl-kill-input :buffers nil :elisp nil)
(dwim-define-lisp-key (kbd "C-M-r") (lambda ()
                                      (interactive)
                                      (slime-restart-inferior-lisp)
                                      (slime-repl-clear-buffer))
                      :buffers nil :elisp nil)
(dwim-define-lisp-key (kbd "C-M-c") 'slime-repl-clear-buffer :buffers nil :elisp nil :scheme nil)
(dwim-define-lisp-key (kbd "C-c C-c") 'eval-defun :elisp t :repl nil :scheme nil)

(global-set-key (kbd "M-f") 'findr-search)
(global-set-key (kbd "C-n") 'tags-loop-continue)

(provide 'dwim-key-bindings)
