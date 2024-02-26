(message "Loading my-init from %s" (buffer-file-name))

(setq calendar-week-start-day 1)

(setq show-paren-delay 0)
(show-paren-mode)
(setq vc-follow-symlinks t)
(setq x-select-enable-clipboard t)

;; load built-in cl.el first, as slime's cl-lib seems to conflict
;; with parts of it
(require 'cl)

;;;; Globally enable features

(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'downcase-region 'disabled nil)

;;;; X Selection

;; http://www.emacswiki.org/emacs/CopyAndPaste

; (transient-mark-mode 1)  ; Now on by default: makes the region act quite like the text "highlight" in many apps.
; (setq shift-select-mode t) ; Now on by default: allows shifted cursor-keys to control the region.
(setq mouse-drag-copy-region nil)  ; stops selection with a mouse being immediately injected to the kill ring
(setq x-select-enable-primary nil)  ; stops killing/yanking interacting with primary X11 selection
(setq x-select-enable-clipboard t)  ; makes killing/yanking interact with clipboard X11 selection

;; these will probably be already set to these values, leave them that way if so!
(setq interprogram-cut-function 'x-select-text)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; Shift movement selects region
(setq shift-select-mode t)
(setq unshifted-motion-keys-deselect-region t)
(delete-selection-mode t)

; You need an emacs with bug #902 fixed for this to work properly. It has now been fixed in CVS HEAD.
; it makes "highlight/middlebutton" style (X11 primary selection based) copy-paste work as expected
; if you're used to other modern apps (that is to say, the mere act of highlighting doesn't
; overwrite the clipboard or alter the kill ring, but you can paste in merely highlighted
; text with the mouse if you want to)
(setq select-active-regions t) ;  active region sets primary X11 selection
(global-set-key [mouse-2] 'mouse-yank-primary)  ; make mouse middle-click only paste from primary X11 selection, not clipboard and kill ring.

;; with this, doing an M-y will also affect the X11 clipboard, making emacs act as a sort of clipboard history, at
;; least of text you've pasted into it in the first place.
;; (setq yank-pop-change-selection t)  ; makes rotating the kill ring change the X11 clipboard.

;; stolen from
;; http://wttools.sourceforge.net/emacs-stuff/emacs.html
;;
;; load personal-funcs which contains my-color-theme etc.
;; (load-file "~/.emacs.d/site-lisp/personal-funcs.el")
(require 'personal-funcs)

(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
(if (getenv "DISPLAY")
    (setq browse-url-browser-function '(("http" . browse-url-firefox)
                                        ("file" . browse-url-firefox)))
    (setq browse-url-browser-function 'w3m-browse-url))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;; set a dedicated custom file so it doesn't mess with my ~/.emacs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(calendar-week-start-day 1)
 '(org-time-stamp-custom-formats (quote ("<%a %Y-%m-%d>" "<%Y-%m-%d>" "<%Y-%m-%d %H:%M>")))
;; '(paren-mode (quote paren) nil (paren))
 '(show-paren-delay 0)
 '(show-paren-mode (quote paren) nil (paren))
 '(vc-follow-symlinks t)
 '(x-select-enable-clipboard t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; GLOBAL KEYS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key "\C-xm" 'browse-url-at-point)
(global-set-key [f10] 'menu-bar-open)

;; cf. personal-funcs.el
;;
;; I find it annoying when backward-kill-word eats up half of the
;; previous line!
(global-set-key [C-backspace] 'kd-backward-kill-whitespace-or-word);
(global-set-key [M-backspace] 'kd-backward-kill-whitespace-or-word);
(global-set-key [?\C-\d] 'kd-backward-kill-whitespace-or-word);

;; whitespace-cleanup is often badly needed
(global-set-key "\C-cw" 'whitespace-cleanup)

;; string-insert-rectange
(global-set-key "\C-ci" 'string-insert-rectangle)

; this is the original setting
; (global-set-key [(control backspace)] 'backward-kill-word)

;;;; Key binding for switching to next and previous buffer
(global-set-key '[C-tab] 'bs-cycle-next)
(global-set-key [S-tab] 'bs-cycle-previous)
(global-set-key [C-iso-lefttab] 'bs-cycle-previous)

;; enlarge shrink a window horizontally
(global-set-key (kbd "C-}") 'enlarge-window-horizontally)
(global-set-key (kbd "C-{") 'shrink-window-horizontally)

;;;; Comment regions
;;; Well, it is good to have the same keybinding for commenting out
;;; region in all modes.
(global-set-key [?\C-c ?c] 'comment-region)
(global-set-key [?\C-c ?u] 'uncomment-region)

;;;; Movement in screen and other terminals
;; but in screen sessions, we need this
(global-set-key "\M-[1;5B"    'forward-paragraph)  ; Ctrl+down   => forward para
(global-set-key "\M-[1;5A"    'backward-paragraph) ; Ctrl+up     => backward para
(global-set-key "\M-[1;5C"    'forward-word)       ; Ctrl+right  => forward word
(global-set-key "\M-[1;5D"    'backward-word)      ; Ctrl+left   => backward word

;; and in rxvt, these:
; rxvt's keycodes for C-<left> and C-<right> are M-O d and M-O c,
(global-set-key [(meta O) (a)] 'backward-paragraph)
(global-set-key [(meta O) (b)] 'forward-paragraph)
(global-set-key [(meta O) (c)] 'forward-word)
(global-set-key [(meta O) (d)] 'backward-word)

;;; Scrolling

(setq smooth-scroll-margin 5)

;;;; Scrolling the other window
;; we need an interactive function for global-set-key (cf. below)
(defun kd-scroll-other-window-up (arg)
  (interactive "p")
  (scroll-other-window '-))

(global-set-key "\M-[5;3~"  'kd-scroll-other-window-up) ; M-<prior> (pg up)
(global-set-key "\M-[6;3~"  'scroll-other-window)      ; M-<next> (pg dn)

(global-set-key (kbd "C-,") 'copy-primary-selection)
(global-set-key (kbd "C-.") 'yank-clipboard-selection)
(global-set-key [(control insert)] 'kill-primary-selection)

(global-set-key (kbd "C-b") 'undo)
(global-set-key (kbd "M-b") 'redo)

(global-set-key (kbd "M-f") 'findr-search)
(global-set-key (kbd "C-n") 'tags-loop-continue)

;; C-o is normally bound to open-line which inserts a newline
;; at point and stays there
(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C-S-o") (lambda () (interactive) (other-window -1)))
(define-key Buffer-menu-mode-map (kbd "C-o") 'other-window)

(global-set-key (kbd "C-p") 'dwim-kill-this-buffer-and-window)

(add-hook 'dired-mode-hook (lambda ()
			     (define-key dired-mode-map (kbd "C-o") 'other-window)
			     (define-key dired-mode-map (kbd "C-p") 'dwim-kill-this-buffer-and-window)))

;; usually: (move-to-window-line arg)
(global-set-key (kbd "M-r") 'query-replace)
(global-set-key [(control shift f)] 'findr)
(global-set-key [(control shift s)] 'findr-search)
(global-set-key [(control shift r)] 'findr-query-replace)

;; darabi: C-M-l locks the gnome session
;; (global-set-key (kbd "C-M-l") 'recenter)
(global-set-key (kbd "C-M-u") 'universal-argument)
(global-set-key (kbd "C-l") 'switch-to-other-buffer)
;; (global-set-key [(control e)] 'delete-window)
(global-set-key [(control shift e)] 'delete-other-windows)

;; string-insert-rectangle

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;;; Color theme
;; ;; CUSTOM FACES are replaced by color-theme-darabi-[x|term]
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (eval-after-load "color-theme"
;;   '(progn
;;     (color-theme-initialize)
;;     (my-color-theme)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;;; LaTeX and friends
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (push '("\\.tex$" . LaTeX-mode) auto-mode-alist)

;; (add-hook 'LaTeX-mode-hook (lambda ()
;;                              (local-set-key "\C-x\C-j" 'okular-jump-to-line)
;;                              (setq LaTeX-command-style '(("" "%(PDF)%(latex) -synctex=1 %S%(PDFout)")))
;;                              (add-to-list 'TeX-output-view-style '("^pdf$" "." "okular --unique %s.pdf"))
;;                              (setq TeX-view-program-list '(("Okular" "okular --unique %s.pdf")))
;;                              (setq TeX-view-program-selection '((output-pdf "Okular") (output-dvi "Okular")))
;;                              (setq TeX-master 'dwim)
;;                              (TeX-PDF-mode)
;;                              (require 'okular-search)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;
;; ;;; ORG-MODE
;; ;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;;;; Loading and paths

;; (require 'org-install)
;; (add-to-list 'auto-mode-alist '("\\.org\\(-mode\\)?$" . org-mode))

;; ; you need to apt-get install ditaa to use it
;; (setq org-ditaa-jar-path "/usr/bin/ditaa")

;; (require 'info)
;; (push "~/.emacs.d/site-lisp/org-mode/doc" Info-default-directory-list)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; extension by Peter Jones pjones@pmade.com now part of org-mode contrib
;; cf ~/.emacs.d/site-lisp/org-invoice/org-invoice.el
(autoload 'org-invoice-report "org-invoice")
(autoload 'org-dblock-write:invoice "org-invoice")

;; ;; (add-to-list 'load-path "~/vc/org")
;; ; I prefer return to activate a link
;; ; this has to be set before org.el is loaded
(setq org-return-follows-link t)
(setq org-log-done t)

(use-package org)
(use-package helm)
(use-package helm-org
  :config (require 'my-org-init "my-org-init" nil))

(require 'org-tempo)
;; (require 'ox-odt)

;; (add-to-list 'auto-mode-alist '("\.org$" . org-mode))

;; specifying dirs is much safer, as new files
;; are automatically picked up
(setq org-agenda-files '("~/vc/org"
                         "~/vc/org/work"
                         "~/vc/org/life"))

(setq org-directory "~/vc/org")

(eval-after-load 'org
  '(progn
    ;; C-right forward-word is important for me
    (define-key org-mode-map (kbd "C-c t") 'org-set-tags)))

;; plantuml export for org-mode
;; active Org-babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (plantuml . t)
   (dot . t)))

(setq org-plantuml-jar-path "~/.emacs.d/plantuml/plantuml.jar")

(setq org-time-clocksum-format '(:hours "%02d" :require-hours t :minutes ":%02d" :require-minutes t))

;; sums are shown in hours instead of days (meaning 24 h) and hours
(setq org-duration-format (quote h:mm))

;;; org-capture and org-protocol

;; org-protocol and capturing from Chrome/Firefox
(require 'org-protocol)

(setq org-default-notes-file "~/vc/org/notes.org")

(setq org-capture-templates
      `(
        ("p" "Protocol" entry (file+headline ,(concat org-directory "/notes.org") "Inbox")
             "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
        ("L" "Protocol Link" entry (file+headline ,(concat org-directory "/notes.org") "Inbox")
             "* %? [[%:link][%:description]] \nCaptured On: %U")))

;; (add-to-list 'helm-completing-read-handlers-alist '(org-capture . helm-org-completing-read-tags))
;; (add-to-list 'helm-completing-read-handlers-alist '(org-set-tags . helm-org-completing-read-tags))

;; helm-org's tag multi-selection mechanism
;; https://github.com/emacs-helm/helm-org/issues/3

(add-hook 'org-mode-hook
          (lambda ()
            (require 'helm-org)
            (helm-mode)
            (add-to-list 'helm-completing-read-handlers-alist
                         '(org-set-tags-command . helm-org-completing-read-tags))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; ENCRYPTION
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun open-encrypted-file (fname)
  (interactive "Find file: n")
  (let ((buf (create-file-buffer fname)))
    (shell-command
     (concat "echo " (read-passwd "Decrypt password: ") " | bcrypt -o " fname)
     buf)
    (set-buffer buf)
    (kill-line)(kill-line)
    (toggle-read-only)
    (not-modified)))

;; password-generate.el is in .emacs.d/site-lisp/personal-funcs
(autoload 'password-generate "password-generate" "Return a string of LENGTH random characters." t)

(defun my-password-generate ()
  "Insert at point a random 8 character password (upper, lower, numbers)"
  (interactive)
  (insert (password-generate 8 t t t nil nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Git/Magit
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'magit-log-edit-mode-hook
          '(lambda ()
            (set-fill-column 60)))

(when (require 'my-x-functions "my-x-functions" nil)
  (save-excursion
    (when (eq 'x (window-system))
      (my-set-x-font))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Fonts
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-lisp-mode-fonts ()
  (font-lock-add-keywords
   'lisp-mode
   ;; atdoc keywords
   `(("@\\(a\\|aboutclass\\|aboutfun\\|arg\\|b\\|class\\|code\\|em\\|fun\\|itemize\\|pre\\|return\\|section\\|see\\|see-constructor\\|see-slot\\|short\\|variable\\)\\(\\[.*\\]\\)" 0 'font-lock-preprocessor-face t))) 'set)

(my-lisp-mode-fonts)

(font-lock-remove-keywords 'lisp-mode '(("\\(short\\|arg\\)")))
(font-lock-refresh-defaults)

;;; Kill ring browsing

;; M-y calls browse-kill-ring
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; LISP
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The SBCL binary and command-line arguments
;; this one doesn't print the SBCL greeting
;; (setq inferior-lisp-program "sbcl --dynamic-space-size 256 --noinform")
(setq inferior-lisp-program "sbcl --dynamic-space-size 512")

(defmacro define-lisp-mode-tab-fn (fn-name symbol-completion-fn)
  `(defun* ,fn-name ()
     "Calls lisp-indent-line. If point does not change (if no indentation was
performed, then slime-complete-symbol is called"
     (interactive)
     (let ((pos (point)))
       (lisp-indent-line)
       (when (= pos (point))
         (,symbol-completion-fn)))))

(define-lisp-mode-tab-fn dar-lisp-mode-tab lisp-complete-symbol)

(add-hook 'lisp-mode-hook
          (lambda ()
            (define-key lisp-mode-map [tab] 'dar-lisp-mode-tab)
            (define-key lisp-mode-map [?\C-c ?\C-r] 'eval-region)
            (define-key lisp-mode-map [?\C-c ?c] 'comment-region)
            (define-key lisp-mode-map [?\C-c ?c] 'comment-region)))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (define-key emacs-lisp-mode-map [tab] 'dar-lisp-mode-tab)
            (define-key emacs-lisp-mode-map [?\C-c ?\C-r] 'eval-region)
            (define-key emacs-lisp-mode-map [?\C-c ?c] 'comment-region)
            (define-key emacs-lisp-mode-map [?\C-c ?c] 'comment-region)))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (make-local-variable 'outline-regexp)
            (setq outline-regexp "^;;;+ ")
            (make-local-variable 'outline-heading-end-regexp)
            (setq outline-heading-end-regexp "\n")
            (outline-minor-mode 1)))

;; more ergonomic search for elisp definitions
;; cf. http://emacsredux.com/blog/2014/06/18/quickly-find-emacs-lisp-sources/
(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)

;; elisp-slime-nav gives you M-. M-, navigation in elisp sources
;; plus slime-describe-symbol C-c C-d C-d
;; cf. https://github.com/purcell/elisp-slime-nav
(require 'elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))

;; some customising
;; from http://boinkor.net/archives/2006/11/three_useful_emacs_hacks_for_l.html
;; Turning off the "unsafe local variable" warning
;; for common lisp source files. Several packages use the -*-
;; convention to set the major mode; some also set variables like
;; Package, Base and Syntax. Emacs 22 doesn't know that they're
;; harmless and prompts almost every time I open a new lisp
;; file. Ugh. Use this:
(put 'package 'safe-local-variable 'symbolp)
(put 'Package 'safe-local-variable 'symbolp)
(put 'syntax 'safe-local-variable 'symbolp)
(put 'Syntax 'safe-local-variable 'symbolp)
(put 'Base 'safe-local-variable 'integerp)
(put 'base 'safe-local-variable 'integerp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Slime
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dar-slime-mode-tab ()
  "Calls lisp-indent-line. If point does not change (if no indentation was
performed, then slime-complete-symbol is called"
  (interactive)
  (let ((pos (point)))
    (lisp-indent-line)
    (when (= pos (point))
      (slime-complete-symbol))))


(define-lisp-mode-tab-fn dar-slime-mode-tab slime-complete-symbol)

(add-hook 'slime-mode-hook
          (lambda ()
             (define-key slime-mode-map [tab] 'dar-slime-mode-tab)
             (define-key slime-mode-map [?\C-\)] 'slime-close-all-sexp)
             (define-key slime-mode-map [?\C-c ?c] 'comment-region)
             (define-key slime-mode-map [?\C-c ?u] 'uncomment-region)
             (unless (slime-connected-p)
               (save-excursion (slime)))))

(setq slime-net-coding-system 'utf-8-unix)

;; lisp buffers always use slime
;; (autoload 'slime "slime" "Superior Lisp Interaction Mode" t)
(require 'slime-autoloads)

(setq slime-contribs '(slime-asdf slime-tramp slime-repl))

(add-hook 'slime-compilation-finished-hook 'slime-maybe-list-compiler-notes)

(setq slime-tree-collapsed-severity-list '(:note))

;;; findr

(autoload 'findr "findr" "Find file name." t)

(define-key global-map [(control shift f)] 'findr)

(autoload 'findr-search "findr" "Find text in files." t)
(define-key global-map [(control shift s)] 'findr-search)

(autoload 'findr-query-replace "findr" "Replace text in files." t)
(define-key global-map [(control shift q)] 'findr-query-replace)

;;; never split the window vertically
(setq split-width-threshold 1000)

;;; XML handling

;; maven pom files
(add-to-list 'auto-mode-alist '("\\.pom\\(~\\|\\.~[0-9a-zA-Z_]+~\\)?\\'" . nxml-mode))

(defun kd-format-xml-buffer ()
  "Formats whole buffer with xmllint"
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    ;;(shell-command-on-region START END COMMAND &optional OUTPUT-BUFFER
    ;;                         REPLACE ERROR-BUFFER DISPLAY-ERROR-BUFFER)
    (shell-command-on-region 1 (buffer-size)
                             "xmllint --format -" (current-buffer) t nil t)))

(push '("<\\?xml " . nxml-mode) magic-mode-alist)

;;; It is always better to know current line and column number
(column-number-mode t)
(line-number-mode t)
(add-hook 'prog-mode-hook 'linum-mode)

;;; Make all yes-or-no questions as y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

;;; paredit

(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code."
  t)

(add-hook 'lisp-mode-hook
          '(lambda ()
            (paredit-mode +1)
            (require 'findr)))

(eval-after-load 'paredit
  '(progn
     ;; C-right forward-word is important for me
     (define-key paredit-mode-map (kbd "M-<right>")
       'paredit-forward-slurp-sexp)
     (define-key paredit-mode-map (kbd "C-<right>")
       'forward-word)
     (define-key paredit-mode-map (kbd "M-<left>")
       'paredit-forward-barf-sexp)
     (define-key paredit-mode-map (kbd "C-<left>")
       'backward-word)))


(add-hook 'slime-mode-hook
          '(lambda ()
	     ;; undo the changes in dwim init files
	     ))

(add-hook 'lisp-mode-hook
          '(lambda ()
             (abbrev-mode t)
             (paredit-mode +1)))

;;; Easy PGA encryption

;; setup easypga encryption support
(require 'epa-file)
(epa-file-enable)
(add-to-list 'auto-mode-alist '("\\.gpg\\(~\\|\\.~[0-9a-zA-Z_]+~\\)?\\'" nil epa-file))


(setq-default
 findr-file-name-regexp-history (list "\\(\\.lisp\\|\\.lsp\\|\\.asd\\|\\.el\\|\\.c\\|\\.h\\|\\.cpp\\|\\.tal\\|\\.css\\|\\.sh\\|\\.lua\\|\\.py\\|\\.go\\)$")
 findr-skip-directory-regexp "\\(^\\.backups\\|^_darcs\\|^\\.git\\|^CVS\\|^\\.svn\\|^www\\|^wwwroot\\|^node_modules\\)$"

 save-place t
 save-place-file "~/.emacs.d/places.el"

 undo-limit 3000000
 undo-strong-limit 5000000

 slime-repl-history-size 3000
 slime-kill-without-query-p t

 swbuff-clear-delay 3
 swbuff-clear-delay-ends-switching t
 swbuff-exclude-buffer-regexps '("^ " "\\*.*\\*")
 swbuff-separator " ### "

 indent-tabs-mode nil
 inhibit-startup-screen t
 signal-error-on-buffer-boundary nil

 ;; mostly to set LANG to have proper utf-8 encoding for file names
 tramp-remote-process-environment '("LANG=en_US.UTF-8" "LC_ALL=" "HISTFILE=$HOME/.tramp_history" "HISTSIZE=1" "TERM=dumb" "CDPATH=" "HISTORY=" "MAIL=" "MAILCHECK=" "MAILPATH=" "autocorrect=" "correct=")
 tramp-default-method "ssh"
 tramp-default-proxies-alist '(("ebr42.otm.gov.hu" "" "/ssh:%u@armageddon.intranet.netvisor.hu#2222:"))
 tramp-backup-directory-alist '((".*encrypt.*" . nil)
                                (".*vault" . nil)
                                (".*truecrypt.*" . nil)
                                ("." . "~/.backups/"))
 tramp-auto-save-directory "~/.backups/autosaves/"

 enable-local-variables :safe
 font-lock-maximum-size 500000
 )

(tool-bar-mode 0)

(add-hook 'global-whitespace-mode-hook
          (defun dwim/global-whitespace-mode-hook ()
            (setq whitespace-global-modes '(ada-mode asm-mode autoconf-mode awk-mode c-mode c++-mode cc-mode change-log-mode cperl-mode electric-nroff-mode emacs-lisp-mode f90-mode fortran-mode html-mode html3-mode java-mode jde-mode ksh-mode latex-mode LaTeX-mode lisp-mode m4-mode makefile-mode modula-2-mode nroff-mode objc-mode pascal-mode perl-mode prolog-mode python-mode scheme-mode sgml-mode sh-mode shell-script-mode simula-mode tcl-mode tex-mode texinfo-mode vrml-mode xml-mode lisp-mode)
                  whitespace-action '(auto-cleanup)
                  whitespace-style '(face trailing empty space-before-tab space-after-tab))))

;; The original whitespace style values; if face is included ... well, then faces are used
;; (face tabs spaces trailing lines space-before-tab newline indentation empty space-after-tab space-mark tab-mark newline-mark)

(global-whitespace-mode t)


;;; Safe file-local variables

(push (cons 'nxml-child-indent #'integerp) safe-local-variable-values)

;;;; Globally enable features

(put 'narrow-to-region 'disabled nil)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;;; GLOBAL KEYS
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key [f12] 'treemacs)
(global-set-key (kbd "<XF86Explorer>") 'treemacs)

;;; BBDB
(global-set-key [?\s-b] 'bbdb)

(setq org-clock-into-drawer nil)

(eval-after-load 'org-agenda
  '(progn
    (define-key org-agenda-mode-map "V" 'bh/view-next-project)
    (define-key org-agenda-mode-map "P" 'bh/narrow-to-project)
    (define-key org-agenda-mode-map "U" 'bh/narrow-up-one-level)
    (define-key org-agenda-mode-map "F" 'bh/restrict-to-file-or-follow)
    (define-key org-agenda-mode-map "N" 'bh/narrow-to-subtree)
    (org-defkey org-agenda-mode-map "W" 'bh/widen)))

; Whether to prompt to confirm evaluation
; This may be dangerous - make sure you understand the consequences
; of setting this -- see the docstring for details
(setq org-confirm-babel-evaluate t)

(add-hook 'org-invoice-heading-hook 'my-invoice-heading-hook)

(setq org-time-clocksum-format '(:hours "%02d" :require-hours t :minutes ":%02d" :require-minutes t))

(require 'cl)
(require 'org-clock)

;; (defun org-dblock-write:invoice (params)
;;   (cl-flet ((fmttm (tm) (format-time-string (org-time-stamp-format t t) tm)))
;;     (let ((file (or (plist-get params :file) (buffer-file-name)))
;;           (start (seconds-to-time
;;                   (org-matcher-time (plist-get params :tstart))))
;;           (end (seconds-to-time (org-matcher-time (plist-get params :tend)))))
;;       (while (time-less-p start end)
;;         (let ((next-week (time-add start
;;                                    (date-to-time "1970-01-08T00:00Z")))
;;               (week-begin (line-beginning-position))
;;               (week-minutes 0))
;;           (insert "\nWeekly Table from " (fmttm start) "\n")
;;           (insert "| Day of Week | Time |\n|-\n")
;;           (while (time-less-p start next-week)
;;             (let* ((next-day (time-add start (date-to-time "1970-01-02T00:00Z")))
;;                    (minutes
;;                     (with-current-buffer (find-file-noselect file)
;;                       (cadr (org-clock-get-table-data
;;                              file
;;                              (list :maxlevel 0
;;                                    :tstart (fmttm start)
;;                                    :tend (fmttm next-day)))))))
;;               (insert "|" (format-time-string "%a" start)
;;                       "|" (format "%d" minutes)
;;                       "|\n")
;;               (org-table-align)
;;               (incf week-minutes minutes)
;;               (setq start next-day)))
;;           (when (equal week-minutes 0)
;;             (delete-region week-begin (line-beginning-position))))))))

(defun org-dblock-write:invoice (params)
  "Write the standard clocktable."
  (setq params (org-combine-plists org-clocktable-defaults params))
  (catch 'exit
    (let* ((scope (plist-get params :scope))
	   (files (pcase scope
		    (`agenda
		     (org-agenda-files t))
		    (`agenda-with-archives
		     (org-add-archive-files (org-agenda-files t)))
		    (`file-with-archives
		     (and buffer-file-name
			  (org-add-archive-files (list buffer-file-name))))
		    ((or `nil `file `subtree `tree
			 (and (pred symbolp)
			      (guard (string-match "\\`tree\\([0-9]+\\)\\'"
						   (symbol-name scope)))))
		     (or (buffer-file-name (buffer-base-buffer))
			 (current-buffer)))
		    ((pred functionp) (funcall scope))
		    ((pred consp) scope)
		    (_ (user-error "Unknown scope: %S" scope))))
	   (block (plist-get params :block))
	   (ts (plist-get params :tstart))
	   (te (plist-get params :tend))
	   (ws (plist-get params :wstart))
	   (ms (plist-get params :mstart))
	   (step (plist-get params :step))
	   (hide-files (plist-get params :hidefiles))
	   (formatter (or (plist-get params :formatter)
			  org-clock-clocktable-formatter
			  'org-clocktable-write-default))
	   cc)
      ;; Check if we need to do steps
      (when block
	;; Get the range text for the header
	(setq cc (org-clock-special-range block nil t ws ms)
	      ts (car cc)
	      te (nth 1 cc)))
      (when step
	;; Write many tables, in steps
	(unless (or block (and ts te))
	  (error "Clocktable `:step' can only be used with `:block' or `:tstart,:end'"))
	(org-clocktable-steps params)
	(throw 'exit nil))

      (org-agenda-prepare-buffers (if (consp files) files (list files)))

      (let ((origin (point))
	    (tables
	     (if (consp files)
		 (mapcar (lambda (file)
			   (with-current-buffer (find-buffer-visiting file)
			     (save-excursion
			       (save-restriction
				 (org-clock-get-table-data file params)))))
			 files)
	       ;; Get the right restriction for the scope.
	       (save-restriction
		 (cond
		  ((not scope))	     ;use the restriction as it is now
		  ((eq scope 'file) (widen))
		  ((eq scope 'subtree) (org-narrow-to-subtree))
		  ((eq scope 'tree)
		   (while (org-up-heading-safe))
		   (org-narrow-to-subtree))
		  ((and (symbolp scope)
			(string-match "\\`tree\\([0-9]+\\)\\'"
				      (symbol-name scope)))
		   (let ((level (string-to-number
				 (match-string 1 (symbol-name scope)))))
		     (catch 'exit
		       (while (org-up-heading-safe)
			 (looking-at org-outline-regexp)
			 (when (<= (org-reduced-level (funcall outline-level))
				   level)
			   (throw 'exit nil))))
		     (org-narrow-to-subtree))))
		 (list (org-clock-get-table-data nil params)))))
	    (multifile
	     ;; Even though `file-with-archives' can consist of
	     ;; multiple files, we consider this is one extended file
	     ;; instead.
	     (and (not hide-files)
		  (consp files)
		  (not (eq scope 'file-with-archives)))))
        (message "Calling formatter %s with tables %s" formatter tables)
	(funcall formatter
		 origin
		 tables
		 (org-combine-plists params `(:multifile ,multifile)))))))

(defun my-org-invoice-report (&optional arg)
  "Update or create an org table containing a report about clocked time.

If point is inside an existing clocktable block, update it.
Otherwise, insert a new one.

The new table inherits its properties from the variable
`org-clock-clocktable-default-properties'.  The scope of the
clocktable, when not specified in the previous variable, is
`subtree' when the function is called from within a subtree, and
`file' elsewhere.

When called with a prefix argument, move to the first clock table
in the buffer and update it."
  (interactive "P")
  (error "Was ist los?")
  (org-clock-remove-overlays)
  (when arg
    (org-find-dblock "invoice")
    (org-show-entry))
  (pcase (org-in-clocktable-p)
    (`nil
     (org-create-dblock
      (org-combine-plists
       (list :scope (if (org-before-first-heading-p) 'file 'subtree))
       org-clock-clocktable-default-properties
       '(:name "invoice"))))
    (start (goto-char start)))
  (org-update-dblock))

(org-dynamic-block-define "invoice" #'my-org-invoice-report)

;; (defun org-set-tags-command-multiple (orig &optional arg)
;;   (cl-letf (((symbol-function #'completing-read)
;;              (lambda (prompt collection &optional predicate require-match initial-input
;;                              hist def inherit-input-method)
;;                (when initial-input
;;                  (setq initial-input
;;                        (replace-regexp-in-string
;;                         ":" ","
;;                         (replace-regexp-in-string
;;                          "\\`:" "" initial-input))))
;;                (let ((res (completing-read-multiple
;;                            prompt collection predicate require-match initial-input
;;                            hist def inherit-input-method)))
;;                  (mapconcat #'identity res ":")))))
;;     (let ((current-prefix-arg arg))
;;       (call-interactively orig))))

; (advice-add #'org-set-tags-command :around #'org-set-tags-command-multiple)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;;; BBDB
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; this is needed by lookout.el (outlook bbdb importer)
;; (require 'bbdb)
;; ;; (require 'bbdb-hooks)

;; ;; (setq lookout-bbdb-mapping-table 'lookout-bbdb-mapping-table-outlook-german)

;; ; don't try to parse the phone number
;; (setq bbdb-phone-style nil)

;; (setq bbdb-file-coding-system 'utf-8)

;; ;; initialise bbdb gnus support
;; (require 'bbdb-gnus)
;; (bbdb-insinuate-gnus)

;; ;; Bcc myself on all my outgoing mail...  That's not ideal, but it
;; ;; will do for the moment.
;; ;; this one is for gnus:
;; (setq message-default-headers
;; "From: Kambiz Darabi <darabi@m-creations.com>
;; Bcc: Myself <darabi@m-creations.int>
;; Content-Type: text/plain; charset=utf-8")

;; ;; and this one for mail sent out of bbdb
;; (setq mail-default-headers (concat message-default-headers "\n"))

;; (setq message-forward-before-signature nil)
;; (setq message-signature-separator "")
;; (setq message-signature-insert-empty-line t)

;; (setq message-signature nil)
;; (setq message-forward-before-signature nil)
;; (setq message-signature-separator "")
;; (setq message-signature-insert-empty-line t)

;; (add-hook 'message-mode-hook
;;           '(lambda ()
;;              (setq comment-empty-lines t)
;;              (define-key message-mode-map [(control c) (control meta c)]
;;                'my-message-send-and-org-gnus-store-link)))

;; ; How to more easily deal with new people from a company for which
;; ; I've already got a lot of people with the same address info.  To
;; ; be submitted to the bbdb mailing list some day.


;; ;;; pre BBDB3 functions: must be ported to bbdb 3

;; ;; (defvar bbdb-omit-fields-on-duplicate '(pilot-id))

;; ;; (defun bbdb-duplicate-and-edit-record (record newname)
;; ;;   "Duplicates the current record, prompting for a new value for name."
;; ;;   (interactive (list (bbdb-get-record "Record to start from: ")
;; ;;                      (read-string "New Name: ")))
;; ;;   (let ((omit-fields bbdb-omit-fields-on-duplicate)
;; ;;         (newnotes (bbdb-record-raw-notes record)))
;; ;;     (while omit-fields
;; ;;       (setq newnotes (remove (assoc (car omit-fields) newnotes) newnotes))
;; ;;       (setq omit-fields (cdr omit-fields)))
;; ;;     (bbdb-create-internal newname (bbdb-record-company record)
;; ;;                           (bbdb-record-net record)
;; ;;                           (bbdb-record-addresses record)
;; ;;                           (bbdb-record-phones record)
;; ;;                           newnotes)))

;; ;; ;;;; bbdb-vard-export
;; ;; (require 'bbdb-vcard-export)

;; ;; (mapc (lambda (e) (push e bbdb-vcard-translation-table))
;; ;;       '(("Telefon geschäftlich" . "work")
;; ;;         ("Mobiltelefon" . "cell")
;; ;;         ("Fax geschäftlich" . "fax")
;; ;;         ("Address 1" . "PREF")
;; ;;         ("Address 2" . "PREF")
;; ;;         ("Office" . "WORK")
;; ;;         ("Post" . "PREF")
;; ;;         ("nil" . "")
;; ;;         (nil . "")))

;;; Helm setup
; (require 'helm-ido-like)
; (helm-ido-like)

;; ;;;;;;
;; ;;; IDO setup

(require 'ido)

;; (setq ;; ido-everywhere t
;;       ido-enable-flex-matching t
;;       ido-max-directory-size 100000)

;; ;;; just like execute-extended-command bound to M-x but uses ido lookup
;; (setq ido-execute-extended-command-cache nil)
;; (defun ido-execute-extended-command ()
;;   (interactive)
;;   (call-interactively
;;    (intern
;;     (ido-completing-read
;;      "M-x "
;;      (progn
;;        (unless ido-execute-extended-command-cache
;;          (mapatoms (lambda (s)
;;                      (when (commandp s)
;;                        (setq ido-execute-extended-command-cache
;;                              (cons (symbol-name s) ido-execute-extended-command-cache))))))
;;        ido-execute-extended-command-cache)))))

(defun dwim-redefine-ido-key (key function)
  (define-key ido-common-completion-map key function)
  (dolist (map (list ido-buffer-completion-map
                     ido-file-completion-map
                     ido-file-dir-completion-map))
    (define-key map key nil)))

;; (add-hook 'ido-define-mode-map-hook
;;           (lambda ()
;;             (cl-flet ((redef (key function)
;;                      (define-key ido-completion-map key function)))
;;               (redef (kbd "M-p") 'previous-history-element)
;;               (redef (kbd "<up>") 'previous-history-element)
;;               (redef (kbd "M-n") 'next-history-element)
;;               (redef (kbd "<down>") 'next-history-element))))

;; (ido-mode 1)

;; (defun my-attach-directory-images ()
;;   "Attach all jpeg images in a directory to the current mail message"
;;   (interactive)
;;   (let ((dir (ido-read-directory-name "Welches Verzeichnis? " "~/Pictures" nil t "appt")))
;;     (dolist (f (directory-files dir t "\\(\\.jpg\\|\\.JPG\\)" nil))
;;       (mml-attach-file f "image/jpeg" nil "attachment"))))

;; ;; setup easypga encryption support
;; (require 'epa-file)
;; (epa-file-enable)
;; (add-to-list 'auto-mode-alist '("\\.gpg\\(~\\|\\.~[0-9a-zA-Z_]+~\\)?\\'" nil epa-file))

;; ;; s/mime does not use the system certificates by default
;; (setq smime-certificate-directory "/etc/ssl/certs")

;; ;; text/calendar (vcalendar) support for gnus

;; ;; ;; http://groups.google.com/group/gnu.emacs.gnus/browse_thread/thread/93471c1c6f4c785b/058991ad4fb9a58d
;; ;; ;; also cf. http://ozymandias.dk/emacs/org-import-calendar.el

;; ;; (add-to-list 'mm-inlined-types "text/calendar")
;; ;; (add-to-list 'mm-automatic-display "text/calendar")
;; ;; (add-to-list 'mm-inline-media-tests '("text/calendar" mm-inline-text-calendar identity))

;; (require 'icalendar)
;; (require 'boxquote)

;; (defun mm-inline-text-calendar (handle)
;;   (with-temp-buffer
;;       (mm-insert-part handle)
;;       (save-window-excursion
;;         (setq text (lala (icalendar--read-element nil nil)))))
;;   (boxquote-text text)
;;   (mm-insert-inline handle "\n"))

;; (defun lala (temp)
;;   (setq elems '(ORGANIZER ATTENDEE SUMMARY DTSTART DTEND DESCRIPTION)
;;         e (car (icalendar--all-events temp)))
;;   (let (value)
;;     (dolist (el elems)
;;       (setq value (format (concat "%s: %s\n" value)
;;                           el
;;                           (icalendar--get-event-property e el))))
;;     (replace-regexp-in-string "\\\\," ","  (replace-regexp-in-string "\\\\n" "\n"  value))))


;; (setq bbdb-time-internal-format "%Y-%m-%dT%T%z")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; BBDB stuff END
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; ;;; Mail

;; ;; ;; allow for searching of imap folders
;; ;; ; (require 'nnir)


;; ;; ;;    Jack Vinson <jvinson@cheux.ecs.umass.edu> writes:
;; ;; ;;    For multiple signature files, I advise the message-insert-signature
;; ;; ;;    function to set message-signature-file to a random file from my
;; ;; ;;    signature directory: (I have files that look like sig1 sig2 etc in
;; ;; ;;    that directory).
;; ;; ;; random insertion of .signature file
;; ;; ;; Thanks to Glenn R Coombs: glenn@prl.philips.co.uk
;; ;; ;; (defvar grc-signature-dir   "~/.sig/")
;; ;; ;; (defvar grc-signature-base  "sig")

;; ;; ;; (defadvice message-insert-signature (before random-mail-sig-ag act comp)
;; ;; ;;   "Change the value of message-signature-file each time
;; ;; ;; `message-insert-signature' is called."
;; ;; ;;   (let ((files (file-name-all-completions
;; ;; ;;                 grc-signature-base (expand-file-name grc-signature-dir))))
;; ;; ;;     (if files (let ((file (nth (random (length files)) files)))
;; ;; ;;                 (setq message-signature-file (concat grc-signature-dir file))
;; ;; ;;                 ))))

;; ;; ;;; namazu full text search engine is disabled because I didn't have enough time to experiment with it

;; ;; (require 'namazu)

;; ;; (setq namazu-dir-alist
;; ;;       '(("mail" . "~/.cache/namazu/mail")
;; ;;         ("chat" . "~/.cache/namazu/chat")))

;; ;; (setq namazu-default-dir "~/.cache/namazu/chat")
;; ;; ;; show the newest entries first
;; ;; (setq namazu-argument '("-H" "--late"))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; MozRepl
;; ;;
;; ;; old mozrepl config
;; ;;
;; ;; (add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode))
;; ;; (autoload 'run-mozilla "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
;; ;; (add-hook 'javascript-mode-hook 'javascript-custom-setup)
;; ;; (defun javascript-custom-setup ()
;; ;;   (moz-minor-mode 1))

;; ;; now using espresso.el for js files (.emacs.d/site-lisp/mozrepl/espresso.el)
;; ;; (add-to-list 'auto-mode-alist '("\\.js\\(on\\)?\\'" . espresso-mode))
;; ;; (autoload 'espresso-mode "espresso" nil t)

;; ;; (autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)

;; ;; (add-hook 'espresso-mode-hook 'espresso-custom-setup)
;; ;; (defun espresso-custom-setup ()
;; ;;   (moz-minor-mode 1)
;; ;;   (setq espresso-indent-level 2))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; ERC
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'notifications)

;; (defun erc-global-notify (match-type nick message)
;;   (message "ERC (%s) %s" nick message))

;; ;; (defun erc-global-notify (match-type nick message)
;; ;;   "Notify when a message is received."
;; ;;   (notifications-notify
;; ;;    :title nick
;; ;;    :body message
;; ;;    :app-name "ERC"
;; ;;    :timeout -1
;; ;;    :app-icon "/usr/share/notify-osd/icons/hicolor/scalable/status/notification-message-im.svg"
;; ;;    :urgency 'low

;; ;;    ;; this a freedesktop sound
;; ;;    :sound-name "message-new-instant"
;; ;;    ;; which is at this location in Debian
;; ;;    :sound-file "/usr/share/sounds/freedesktop/stereo/message-new-instant.oga"

;; ;;    ;; :resident t interferes with sounds, which might be a bug
;; ;;    :resident nil))

;; (add-hook 'erc-mode-hook
;;           '(lambda ()
;;             (require 'erc-log)
;;             (erc-log-enable)
;;             ;; save log after each message in erc
;;             (add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)
;;             ;; Add current time to every new line
;;             (erc-timestamp-mode t)
;;             ;; Notify me if someone calls me
;;             (erc-match-mode t)
;;             ;; notify me about these keywords
;;             (setq erc-keywords (append '("kami" "attila" "dwim") erc-pals))
;;             (require 'notifications)
;;             (add-hook 'erc-text-matched-hook 'erc-global-notify)))

;; ;; ;; Some basic settings for erc package
;; (setq erc-server "irc.libera.chat"
;;       ;; port 6697 is for erc-tls, 6667 without tls
;;       erc-port 6697
;;       erc-nick "kami_"
;;       erc-pals '("attila_lendvai" "levy" "levente_meszaros")
;;       erc-save-buffer-on-part t
;;       erc-log-channels t
;;       erc-log-channels-directory "~/.irclogs"
;;       erc-log-file-coding-system 'utf-8
;;       erc-generate-log-file-name-function 'erc-generate-log-file-name-with-date)

;; (defun my-erc ()
;;   "Connect to IRC."
;;   (interactive)
;;   (erc-tls :server "irc.oftc.net" :port 6697
;;            :nick "kambiz" :full-name "kambiz"
;;            ;; :client-certificate '("~/.ssl/oftc.net.kambiz.key"
;;            ;;                      "~/.ssl/oftc.net.kambiz..crt")
;;            )
;;   (erc-tls :server "irc.libera.chat" :port 6697
;;            :nick "kamileon" :full-name "kamileon")
;;   (setq erc-autojoin-channels-alist '(("irc.libera.chat" ) ;; "#emacs" "#screen" "#ion")
;;                                       ("irc.oftc.net" )))) ;; "#debian"))))

;;; DWIM

;; (setf slime-default-lisp 'fluvium)
(setf slime-default-lisp 'system-sbcl)

;; undo the changes in dwim init files
(show-paren-mode t)
(setq show-paren-style 'parenthesis)

;; I need match-paren much more often than back-to-indentation
(global-set-key [?\M-m] 'match-paren)

(setq common-lisp-hyperspec-root "file:///usr/share/doc/hyperspec/")

(add-hook 'lisp-mode-hook
          '(lambda ()
            (abbrev-mode t)
            (paredit-mode +1)
            ;; dash is not a word delimiter any more
            (modify-syntax-entry ?- "w")))

;; ;;; We use left and right guillemet for quasi-quoted strings
;; (global-set-key [?\C-<] 'insert-left-guillemet)
;; (global-set-key [?\C->] 'insert-right-guillemet)

;; ;; sql
;; (add-hook 'sql-interactive-mode-hook
;;           (lambda ()
;;             (setq sql-mysql-login-params (append sql-mysql-login-params '(port)))))

;; (defun my-sql ()
;;   (interactive)
;;   (setq sql-mysql-login-params (append sql-mysql-login-params '(port)))
;;   (sql-mysql))

;; ;;; Confluence
;; (setq confluence-url "https://confluence.prime-research.local/rpc/xmlrpc")
;; ;; (setq gnutls-verify-error '(("confluence.prime-research.local" nil)))
;; ;; (setq gnutls-log-level 2)

;;; Local Variables
(setf enable-local-variables t)


;;; Add Marmalade and Melpa package archives
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; don't initialise all packages automatically
(setq package-enable-at-startup nil)


;; (defvar *my-box-tramp-path*
;;   "/ssh:darabi@galen:")

;; (defvar *current-tramp-path* nil)

;; (defun connect-to-host (path)
;;   (setq *current-tramp-path* path)
;;   (setq slime-translate-from-lisp-filename-function
;;     (lambda (f)
;;       (concat *current-tramp-path* f)))
;;   (setq slime-translate-to-lisp-filename-function
;;     (lambda (f)
;;       (substring f (length *current-tramp-path*))))
;;   (slime-connect "localhost" 4005))

;; (defun my-box-slime ()
;;   (interactive)
;;   (connect-to-host *my-box-tramp-path*))

;; (defun my-box-homedir ()
;;   (interactive)
;;   (find-file (concat *server-tramp-path* "/home/darabi/")))

;; ;; http://stackoverflow.com/questions/23924306/arabic-glyphs-in-emacs
;; ;;
;; ;; to find your fonts: (dolist (f (font-family-list)) (print f))
;; ;;
;; ;; these are also OK for Farsi: "Iranian Sans", "Neirizi"
;; ;;
;; (when window-system (set-fontset-font "fontset-default" '(#x600 . #x6ff) "Droid Arabic Naskh"))

;; (defun my-sort-maven-dependencies ()
;;   (interactive)
;;   (save-excursion
;;     (beginning-of-buffer)
;;     (when (re-search-forward "<dependencies>")
;;       (let ((start (point)))
;;         (while (and (not (looking-at "dependencies"))
;;                     (re-search-forward ">[\n[:space:]]*?<\\([^d]\\)"))
;;           (replace-match "><\\1" t nil))
;;         (sort-lines nil start (point))
;;         (goto-char start)
;;         ;; (indent-according-to-mode)
;;         (save-excursion
;;           (while (re-search-forward "><" nil t)
;;             (replace-match ">\n<")))
;;         (while (not (eq (point) (point-max)))
;;           (forward-line 1)
;;           (indent-according-to-mode))))))


;;; Markdown
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;; ;; Typescript/Tide

;; (defun setup-tide-mode ()
;;   (interactive)
;;   (require 'company)
;;   (require 'tide)
;;   (tide-setup)
;;   (flycheck-mode +1)
;;   (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;   (eldoc-mode +1)
;;   (tide-hl-identifier-mode +1)
;;   ;; company is an optional dependency. You have to
;;   ;; install it separately via package-install
;;   ;; `M-x package-install [ret] company`
;;   (company-mode +1))


;; ;; formats the buffer before saving
;; (defun my-tide-mode-before-save-hook ()
;;   (when (eq major-mode 'tide-mode)
;;     (tide-format-before-save)))

;; (add-hook 'typescript-mode-hook #'my-tide-mode-before-save-hook)

;; ;; aligns annotation to the right hand side
;; (setq company-tooltip-align-annotations t)

(require 'mc-licence)

;; (require 'mc-doom-theme)
;; (load-theme 'doom-sourcerer t)

(require 'sanityinc-tomorrow-eighties-theme)
(load-theme 'sanityinc-tomorrow-eighties t)

;; ;; expand-region https://github.com/magnars/expand-region.el
;; (require 'expand-region)

;; (defun my-shrink-region ()
;;   (interactive)
;;   (er/expand-region -1))

;; (global-set-key (kbd "C-'") 'er/expand-region)
;; (global-set-key (kbd "C-;") 'my-shrink-region)

;; ;; projectile
;; ;; (require 'projectile)
;; ;; (setq projectile-project-search-path '("~/common-lisp/holz24"))

;; ;; Heidelpay JIRA
;; (setq jiralib-url "https://jira.hpchd.loc")

;; (require 'default-text-scale)
;; (default-text-scale-mode)

(provide 'my-init)
