(message "Loading my-init from %s" (buffer-file-name))

(setq calendar-week-start-day 1)
(setq org-time-stamp-custom-formats (quote ("<%a %Y-%m-%d>" "<%Y-%m-%d>" "<%Y-%m
-%d %H:%M>")))

(setq show-paren-delay 0)
(show-paren-mode)
(setq vc-follow-symlinks t)
(setq x-select-enable-clipboard t)

;; load built-in cl.el first, as slime's cl-lib seems to conflict
;; with parts of it
(require 'cl)

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
;;(load-file "~/.emacs.d/site-lisp/personal-funcs.el")
(require 'personal-funcs)

(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
(if (getenv "DISPLAY")
    (setq browse-url-browser-function '(("http" . browse-url-firefox)
                                        ("file" . browse-url-firefox)))
    (setq browse-url-browser-function 'w3m-browse-url))

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

; this is the original setting
; (global-set-key [(control backspace)] 'backward-kill-word)

;;;; Key binding for switching to next and previous buffer
(global-set-key '[C-tab] 'bs-cycle-next)
(global-set-key [S-tab] 'bs-cycle-previous)
(global-set-key [C-iso-lefttab] 'bs-cycle-previous)

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

;; (global-set-key "\C-cl" 'org-store-link)
;; (global-set-key "\C-ca" 'org-agenda)
;; (global-set-key "\C-cb" 'org-iswitchb)

;; ;; (add-to-list 'load-path "~/vc/org")
;; ; I prefer return to activate a link
;; ; this has to be set before org.el is loaded
;; (setq org-return-follows-link t)
;; (setq org-log-done t)

;; (require 'my-org-init "my-org-init" nil)

;; (require 'org)
;; (require 'ox-odt)

;; (add-to-list 'auto-mode-alist '("\.org$" . org-mode))

;; ;; specifying dirs is much safer, as new files
;; ;; are automatically picked up
;; (setq org-agenda-files '("~/vc/org"
;;                          "~/vc/org/work"
;;                          "~/vc/org/life"))

;; (setq org-directory "~/vc/org")

;; (eval-after-load 'org
;;   '(progn
;;     ;; C-right forward-word is important for me
;;     (define-key org-mode-map (kbd "C-c t") 'org-set-tags)))

;; ;; plantuml export for org-mode
;; ;; active Org-babel languages
;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((emacs-lisp . t)
;;    (plantuml . t)
;;    (dot . t)))

;; (setq org-plantuml-jar-path "~/.emacs.d/plantuml/plantuml.jar")

;; (setq org-time-clocksum-format '(:hours "%02d" :require-hours t :minutes ":%02d" :require-minutes t))


;;;; Globally enable features

(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq-default indent-tabs-mode nil)

(when (require 'my-x-functions "my-x-functions" nil)
  (save-excursion
    (unless (eq (window-system) 'w32)
      (my-set-x-font))))

(require 'my-slime-init "my-slime-init" t)

(show-paren-mode t)
(setq show-paren-style 'parenthesis)

;; I need match-paren much more often than back-to-indentation
(global-set-key [?\M-m] 'match-paren)

(setq common-lisp-hyperspec-root "file:///home/franke/cl/HyperSpec/")

(add-hook 'lisp-mode-hook
          '(lambda ()
            (abbrev-mode t)
            (paredit-mode -1)
            ;; dash is not a word delimiter any more
            (modify-syntax-entry ?- "w")))

;;; We use left and right guillemet for quasi-quoted strings
(global-set-key [?\C-<] 'insert-left-guillemet)
(global-set-key [?\C->] 'insert-right-guillemet)


;;; Local Variables
(setf enable-local-variables t)


;;; Add Marmalade and Melpa package archives
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; don't initialise all packages automatically
(setq package-enable-at-startup nil)


;;; Markdown
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;; Typescript/Tide

(defun setup-tide-mode ()
  (interactive)
  (require 'company)
  (require 'tide)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))


;; formats the buffer before saving
(defun my-tide-mode-before-save-hook ()
  (when (eq major-mode 'tide-mode)
    (tide-format-before-save)))

(add-hook 'typescript-mode-hook #'my-tide-mode-before-save-hook)

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

(require 'mc-licence)

(provide 'my-init)
