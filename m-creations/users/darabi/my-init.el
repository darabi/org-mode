(message "Loading my-init from %s" (buffer-file-name))

(setq calendar-week-start-day 1)
(setq org-time-stamp-custom-formats (quote ("<%a %Y-%m-%d>" "<%Y-%m-%d>" "<%Y-%m-%d %H:%M>")))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Color theme
;; CUSTOM FACES are replaced by color-theme-darabi-[x|term]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-after-load "color-theme"
  '(progn
    (color-theme-initialize)
    (my-color-theme)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;; changes added by custom
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

;;;; Globally enable features

(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'downcase-region 'disabled nil)

;; stolen from
;; http://wttools.sourceforge.net/emacs-stuff/emacs.html
;;
;; load personal-funcs which contains my-color-theme etc.
;;(load-file "~/.emacs.d/site-lisp/personal-funcs.el")
(require 'personal-funcs)

;;(setq browse-url-firefox-program "/usr/local/bin/Icecat32")
(setq browse-url-firefox-program "firefox")

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

;;; Scrolling

(setq smooth-scroll-margin 5)

;;;; Scrolling the other window
;; we need an interactive function for global-set-key (cf. below)
(defun kd-scroll-other-window-up (arg)
  (interactive "p")
  (scroll-other-window '-))

(global-set-key "\M-[5;3~"  'kd-scroll-other-window-up) ; M-<prior> (pg up)
(global-set-key "\M-[6;3~"  'scroll-other-window)      ; M-<next> (pg dn)


(global-set-key (kbd "C-b") 'undo)
(global-set-key (kbd "M-b") 'redo)

(global-set-key (kbd "M-f") 'findr-search)
(global-set-key (kbd "C-n") 'tags-loop-continue)

;; C-o is normally bound to open-line which inserts a newline
;; at point and stays there
(global-set-key (kbd "C-o") 'other-window)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Color theme
;; CUSTOM FACES are replaced by color-theme-darabi-[x|term]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-after-load "color-theme"
  '(progn
    (color-theme-initialize)
    (my-color-theme)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; LaTeX and friends
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(push '("\\.tex$" . LaTeX-mode) auto-mode-alist)

(add-hook 'LaTeX-mode-hook (lambda ()
                             (local-set-key "\C-x\C-j" 'okular-jump-to-line)
                             (setq LaTeX-command-style '(("" "%(PDF)%(latex) -synctex=1 %S%(PDFout)")))
                             (add-to-list 'TeX-output-view-style '("^pdf$" "." "okular --unique %s.pdf"))
                             (setq TeX-view-program-list '(("Okular" "okular --unique %s.pdf")))
                             (setq TeX-view-program-selection '((output-pdf "Okular") (output-dvi "Okular")))
                             (setq TeX-master 'dwim)
                             (TeX-PDF-mode)
                             (require 'okular-search)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ORG-MODE
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Loading and paths

(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\(-mode\\)?$" . org-mode))

; you need to apt-get install ditaa to use it
(setq org-ditaa-jar-path "/usr/bin/ditaa")

(require 'info)
(push "~/.emacs.d/site-lisp/org-mode/doc" Info-default-directory-list)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; extension by Peter Jones pjones@pmade.com now part of org-mode contrib
;; cf ~/.emacs.d/site-lisp/org-mode/contrib/lisp/org-invoice.el
(autoload 'org-invoice-report "org-invoice")
(autoload 'org-dblock-write:invoice "org-invoice")

;; (add-to-list 'load-path "~/vc/org")
; I prefer return to activate a link
; this has to be set before org.el is loaded
(setq org-return-follows-link t)
(setq org-log-done t)

(require 'my-org-init "my-org-init" nil)

(require 'org)
(require 'ox-odt)

(add-to-list 'auto-mode-alist '("\.org$" . org-mode))

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

;; password-generate.el is in .emacs.d/site-lisp/
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
    (unless (eq (window-system) 'w32)
      (my-set-x-font))))

;;; Fonts

(defun my-lisp-mode-fonts ()
  (font-lock-add-keywords
   'lisp-mode
   ;; atdoc keywords
   `(("@\\(a\\|aboutclass\\|aboutfun\\|arg\\|b\\|class\\|code\\|em\\|fun\\|itemize\\|pre\\|return\\|section\\|see\\|see-constructor\\|see-slot\\|short\\|variable\\)\\(\\[.*\\]\\)" 0 'font-lock-preprocessor-face t))) 'set)

(my-lisp-mode-fonts)

;; (font-lock-remove-keywords 'lisp-mode '(("\\(short\\|arg\\)")))
;; (font-lock-refresh-defaults)

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
;; SLIME
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun dar-slime-mode-tab ()
;;   "Calls lisp-indent-line. If point does not change (if no indentation was
;; performed, then slime-complete-symbol is called"
;;   (interactive)
;;   (let ((pos (point)))
;;     (lisp-indent-line)
;;     (when (= pos (point))
;;       (slime-complete-symbol))))


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

(setq slime-contribs '(slime-fancy slime-asdf slime-tramp slime-repl slime-compiler-notes-tree))

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

;;; sqlplus
(setenv "ORACLE_HOME" "/usr/lib/oracle/xe/app/oracle/product/10.2.0/server")
(setq sqlplus-command "/usr/lib/oracle/xe/app/oracle/product/10.2.0/server/bin/sqlplus")

;;; Prime Confluence
(setq confluence-url "https://confluence.prime-research.local:8443/rpc/xmlrpc")

;;; Javascript

;; ;; Tern http://ternjs.net
;; ;; (install with npm install -g tern)
;; ;; if not present in PATH, don't add it to the load path/js2 hook
;; (let ((path (replace-regexp-in-string "/bin/tern[\n \t]*$" "" (shell-command-to-string "which tern"))))
;;   (unless (eq 0 (length path))
;;     ;; tern config files are json
;;     (add-to-list 'auto-mode-alist '("\\.tern-config\\'" . js-mode))
;;     (add-to-list 'auto-mode-alist '("\\.tern-project\\'" . js-mode))
;;     (add-to-list 'load-path (concat path "/lib/node_modules/tern/emacs"))
;;     (autoload 'tern-mode "tern.el" nil t)
;;     (add-hook 'js2-mode-hook (lambda () (tern-mode t)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; changes added by custom
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(bbdb-get-only-first-address-p nil)
 ; '(bee-bigloo "/opt/bigloo/bin/bigloo")
 ; '(imap-ssl-program (quote ("openssl s_client -quiet -ssl3 -connect %s:%p" "openssl s_client -quiet -ssl2 -connect %s:%p")))
 ; '(spam-use-bogofilter t)
 '(tool-bar-mode nil)
 '(use-file-dialog nil))

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
                  whitespace-style '(face trailing empty tabs))))

(global-whitespace-mode t)


;;; Safe file-local variables

(push (cons 'nxml-child-indent #'integerp) safe-local-variable-values)

;;;; Globally enable features

(put 'narrow-to-region 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; GLOBAL KEYS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; planner global key bindings
(global-set-key [f12] 'plan)
(global-set-key [f11] 'planner-create-task-from-buffer)

;;; BBDB
(global-set-key [?\s-b] 'bbdb)

;;; ORG-MODE

;; specifying dirs is much safer, as new files
;; are automatically picked up
(setq org-agenda-files '("~/vc/org"
                         "~/vc/org/work"
                         "~/vc/org/life"))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; BBDB
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this is needed by lookout.el (outlook bbdb importer)
(require 'bbdb)
;; (require 'bbdb-hooks)

;; (setq lookout-bbdb-mapping-table 'lookout-bbdb-mapping-table-outlook-german)

; don't try to parse the phone number
(setq bbdb-phone-style nil)

(setq bbdb-file-coding-system 'utf-8)

;; initialise bbdb gnus support
(require 'bbdb-gnus)
(bbdb-insinuate-gnus)

;; Bcc myself on all my outgoing mail...  That's not ideal, but it
;; will do for the moment.
;; this one is for gnus:
(setq message-default-headers
"From: Kambiz Darabi <darabi@m-creations.com>
Bcc: Myself <darabi@m-creations.int>
Content-Type: text/plain; charset=utf-8")

;; and this one for mail sent out of bbdb
(setq mail-default-headers (concat message-default-headers "\n"))

(setq message-forward-before-signature nil)
(setq message-signature-separator "")
(setq message-signature-insert-empty-line t)

(setq message-signature nil)
(setq message-forward-before-signature nil)
(setq message-signature-separator "")
(setq message-signature-insert-empty-line t)

(add-hook 'message-mode-hook
          '(lambda ()
             (setq comment-empty-lines t)
             (define-key message-mode-map [(control c) (control meta c)]
               'my-message-send-and-org-gnus-store-link)))

; How to more easily deal with new people from a company for which
; I've already got a lot of people with the same address info.  To
; be submitted to the bbdb mailing list some day.


;;; pre BBDB3 functions: must be ported to bbdb 3

;; (defvar bbdb-omit-fields-on-duplicate '(pilot-id))

;; (defun bbdb-duplicate-and-edit-record (record newname)
;;   "Duplicates the current record, prompting for a new value for name."
;;   (interactive (list (bbdb-get-record "Record to start from: ")
;;                      (read-string "New Name: ")))
;;   (let ((omit-fields bbdb-omit-fields-on-duplicate)
;;         (newnotes (bbdb-record-raw-notes record)))
;;     (while omit-fields
;;       (setq newnotes (remove (assoc (car omit-fields) newnotes) newnotes))
;;       (setq omit-fields (cdr omit-fields)))
;;     (bbdb-create-internal newname (bbdb-record-company record)
;;                           (bbdb-record-net record)
;;                           (bbdb-record-addresses record)
;;                           (bbdb-record-phones record)
;;                           newnotes)))

;; ;;;; bbdb-vard-export
;; (require 'bbdb-vcard-export)

;; (mapc (lambda (e) (push e bbdb-vcard-translation-table))
;;       '(("Telefon geschäftlich" . "work")
;;         ("Mobiltelefon" . "cell")
;;         ("Fax geschäftlich" . "fax")
;;         ("Address 1" . "PREF")
;;         ("Address 2" . "PREF")
;;         ("Office" . "WORK")
;;         ("Post" . "PREF")
;;         ("nil" . "")
;;         (nil . "")))


;;;;;;
;;; IDO setup

(require 'ido)

(setq ido-everywhere t
      ido-enable-flex-matching t
      ido-max-directory-size 100000)

;;; just like execute-extended-command bound to M-x but uses ido lookup
(setq ido-execute-extended-command-cache nil)
(defun ido-execute-extended-command ()
  (interactive)
  (call-interactively
   (intern
    (ido-completing-read
     "M-x "
     (progn
       (unless ido-execute-extended-command-cache
         (mapatoms (lambda (s)
                     (when (commandp s)
                       (setq ido-execute-extended-command-cache
                             (cons (symbol-name s) ido-execute-extended-command-cache))))))
       ido-execute-extended-command-cache)))))

(defun dwim-redefine-ido-key (key function)
  (define-key ido-common-completion-map key function)
  (dolist (map (list ido-buffer-completion-map
                     ido-file-completion-map
                     ido-file-dir-completion-map))
    (define-key map key nil)))

(add-hook 'ido-define-mode-map-hook
          (lambda ()
            (cl-flet ((redef (key function)
                     (define-key ido-completion-map key function)))
              (redef (kbd "M-p") 'previous-history-element)
              (redef (kbd "<up>") 'previous-history-element)
              (redef (kbd "M-n") 'next-history-element)
              (redef (kbd "<down>") 'next-history-element))))

(ido-mode 1)

(defun my-attach-directory-images ()
  "Attach all jpeg images in a directory to the current mail message"
  (interactive)
  (let ((dir (ido-read-directory-name "Welches Verzeichnis? " "~/Pictures" nil t "appt")))
    (dolist (f (directory-files dir t "\\(\\.jpg\\|\\.JPG\\)" nil))
      (mml-attach-file f "image/jpeg" nil "attachment"))))

;; setup easypga encryption support
(require 'epa-file)
(epa-file-enable)
(add-to-list 'auto-mode-alist '("\\.gpg\\(~\\|\\.~[0-9a-zA-Z_]+~\\)?\\'" nil epa-file))

;; s/mime does not use the system certificates by default
(setq smime-certificate-directory "/etc/ssl/certs")

;; text/calendar (vcalendar) support for gnus

;; http://groups.google.com/group/gnu.emacs.gnus/browse_thread/thread/93471c1c6f4c785b/058991ad4fb9a58d
;; also cf. http://ozymandias.dk/emacs/org-import-calendar.el

(add-to-list 'mm-inlined-types "text/calendar")
(add-to-list 'mm-automatic-display "text/calendar")
(add-to-list 'mm-inline-media-tests '("text/calendar" mm-inline-text-calendar identity))

(require 'icalendar)
(require 'boxquote)

(defun mm-inline-text-calendar (handle)
  (with-temp-buffer
      (mm-insert-part handle)
      (save-window-excursion
        (setq text (lala (icalendar--read-element nil nil)))))
  (boxquote-text text)
  (mm-insert-inline handle "\n"))

(defun lala (temp)
  (setq elems '(ORGANIZER ATTENDEE SUMMARY DTSTART DTEND DESCRIPTION)
        e (car (icalendar--all-events temp)))
  (let (value)
    (dolist (el elems)
      (setq value (format (concat "%s: %s\n" value)
                          el
                          (icalendar--get-event-property e el))))
    (replace-regexp-in-string "\\\\," ","  (replace-regexp-in-string "\\\\n" "\n"  value))))


(setq bbdb-time-internal-format "%Y-%m-%dT%T%z")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BBDB stuff END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Mail

;; allow for searching of imap folders
; (require 'nnir)


;;    Jack Vinson <jvinson@cheux.ecs.umass.edu> writes:
;;    For multiple signature files, I advise the message-insert-signature
;;    function to set message-signature-file to a random file from my
;;    signature directory: (I have files that look like sig1 sig2 etc in
;;    that directory).
;; random insertion of .signature file
;; Thanks to Glenn R Coombs: glenn@prl.philips.co.uk
;; (defvar grc-signature-dir   "~/.sig/")
;; (defvar grc-signature-base  "sig")

;; (defadvice message-insert-signature (before random-mail-sig-ag act comp)
;;   "Change the value of message-signature-file each time
;; `message-insert-signature' is called."
;;   (let ((files (file-name-all-completions
;;                 grc-signature-base (expand-file-name grc-signature-dir))))
;;     (if files (let ((file (nth (random (length files)) files)))
;;                 (setq message-signature-file (concat grc-signature-dir file))
;;                 ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; LISP
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The SBCL binary and command-line arguments
;; this one doesn't print the SBCL greeting
;; (setq inferior-lisp-program "sbcl --dynamic-space-size 256 --noinform")
(setq inferior-lisp-program "sbcl --dynamic-space-size 512")
;;(setq inferior-lisp-program "/home/darabi/bin/ploom")

;; I would like to activate stella immediately
;; but this leads to an error from swank, because *load-path* is nil
;;(setq inferior-lisp-program "powerloom")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; SLIME
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun slime-powerloom ()
  "start slime with an sbcl image containing powerloom"
  (interactive)
  (let ((inferior-lisp-program "/home/darabi/bin/powerloom"))
    (slime)))

(defun slime-clim ()
  "start slime with an sbcl image containing current mcclim"
  (interactive)
  (let ((inferior-lisp-program "/home/darabi/wrk/lisp/mcclim/current/sbcl-clim"))
    (slime)))

; (slime-setup :autodoc t :typeout-frame t)
; (slime-setup '(slime-fancy slime-asdf slime-tramp))

;; (push "~/.emacs.d/site-lisp/slime/doc" Info-default-directory-list)

;; ;;; namazu full text search engine is disabled because I didn't have enough time to experiment with it

;; (require 'namazu)

;; (setq namazu-dir-alist
;;       '(("mail" . "~/.cache/namazu/mail")
;;         ("chat" . "~/.cache/namazu/chat")))

;; (setq namazu-default-dir "~/.cache/namazu/chat")
;; ;; show the newest entries first
;; (setq namazu-argument '("-H" "--late"))

;;; never split the window vertically
(setq split-width-threshold 1000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MozRepl
;;
;; old mozrepl config
;;
;; (add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode))
;; (autoload 'run-mozilla "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
;; (add-hook 'javascript-mode-hook 'javascript-custom-setup)
;; (defun javascript-custom-setup ()
;;   (moz-minor-mode 1))

;; now using espresso.el for js files (.emacs.d/site-lisp/mozrepl/espresso.el)
;; (add-to-list 'auto-mode-alist '("\\.js\\(on\\)?\\'" . espresso-mode))
;; (autoload 'espresso-mode "espresso" nil t)

;; (autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)

;; (add-hook 'espresso-mode-hook 'espresso-custom-setup)
;; (defun espresso-custom-setup ()
;;   (moz-minor-mode 1)
;;   (setq espresso-indent-level 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ERC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun erc-global-notify (match-type nick message)
  "Notify when a message is received."
  (notifications-notify
   :title nick
   :body message
   :app-name "ERC"
   :timeout -1
   :app-icon "/usr/share/notify-osd/icons/gnome/scalable/status/notification-message-im.svg"
   :urgency 'low

   ;; this a freedesktop sound
   :sound-name "message-new-instant"
   ;; which is at this location in Debian
   ;; :sound-file "/usr/share/sounds/freedesktop/stereo/message-new-instant.oga"

   ;; :resident t interferes with sounds, which might be a bug
   :resident nil))

(add-hook 'erc-mode-hook
          '(lambda ()
            (require 'erc-log)
            (erc-log-enable)
            ;; save log after each message in erc
            (add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)
            ;; Add current time to every new line
            (erc-timestamp-mode t)
            ;; Notify me if someone calls me
            (erc-match-mode t)
            ;; notify me about these keywords
            (setq erc-keywords (append '("kami" "attila" "dwim") erc-pals))
            (require 'notifications)
            (add-hook 'erc-text-matched-hook 'erc-global-notify)))

;; ;; Some basic settings for erc package
(setq erc-server "irc.eu.freenode.net"
      erc-port 6667
      erc-nick "kami"
      erc-pals '("attila_lendvai" "levy" "levente_meszaros")
      erc-save-buffer-on-part t
      erc-log-channels t
      erc-log-channels-directory "~/.irclogs"
      erc-log-file-coding-system 'utf-8
      erc-generate-log-file-name-function 'erc-generate-log-file-name-with-date)

;;;
;;; Jabber
;;;

(autoload 'jabber-connect "jabber"
  "Jabber Client"
  t)

;; sefroyek
(setq tanpad-gw1 "84.47.228.121")
;; datak
(setq tanpad-gw2 "109.162.158.231")

(setq jabber-invalid-certificate-servers (list "ivr.tanpad.com" tanpad-gw1 tanpad-gw2))

(setq jabber-account-list '(("kambiz@m-creations.net"
                             (:network-server . dev.m-creations.net)
                             (:port . 5222)
                             (:connection-type . ssl))
                            ("darabi@jabber.org"
                             (:port . 5222)
                             (:connection-type . ssl))))

(defun my-jabber-switch-account ()
  (interactive)
  (require 'jabber)
  (let* ((accounts
          '(("chalmers" ("jabber.cd.chalmers.se" nil "USERNAME"))
            ("80" ("jabber80.com" 80 "USERNAME"))))
         (acc (cadr (assoc (completing-read "Account: " accounts nil t) accounts)))
         (server (nth 0 acc))
         (user (nth 2 acc))
         (port (nth 1 acc)))
    (message "%s %s %s" server user port)
    (when server
      (message "Switching to %s..." server)
      (jabber-disconnect)
      (setq jabber-server server
            jabber-port port
            jabber-username user)
      (jabber-connect))))

(defun libnotify-jabber-notify (from buf text proposed-alert)
  "(jabber.el hook) Notify of new Jabber chat messages via libnotify"
  (when (or jabber-message-alert-same-buffer
            (not (memq (selected-window) (get-buffer-window-list buf))))
    (if (jabber-muc-sender-p from)
        (erc-global-notify 'pal (format "(PM) %s"
                                        (jabber-jid-displayname (jabber-jid-user from)))
                           (format "%s: %s" (jabber-jid-resource from) text)))
    (erc-global-notify 'pal (format "%s" (jabber-jid-displayname from))
                       text)))

(add-hook 'jabber-alert-message-hooks 'libnotify-jabber-notify)


;;; DWIM

;; (setf slime-default-lisp 'fluvium)
(setf slime-default-lisp 'web-server)

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

;;; We use left and right guillemet for quasi-quoted strings
(global-set-key [?\C-<] 'insert-left-guillemet)
(global-set-key [?\C->] 'insert-right-guillemet)

;;; Trac

(autoload 'trac-wiki "trac-wiki" "Trac wiki editing entry-point." t)

(eval-after-load "trac-wiki"
  '(progn
    (trac-wiki-define-multiple-projects
     '("tanpad" "paybox" "procom" "voicecash" "java" "mdoc")
     "https://trac.tanpad.com/" t)))

;;; sqlplus
(setenv "ORACLE_HOME" "/usr/lib/oracle/xe/app/oracle/product/10.2.0/server")
(setq sqlplus-command "/usr/lib/oracle/xe/app/oracle/product/10.2.0/server/bin/sqlplus")

;; sql
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (setq sql-mysql-login-params (append sql-mysql-login-params '(port)))))

(defun my-sql ()
  (interactive)
  (setq sql-mysql-login-params (append sql-mysql-login-params '(port)))
  (sql-mysql))

;;; Confluence
(setq confluence-url "https://confluence.prime-research.local/rpc/xmlrpc")
;; (setq gnutls-verify-error '(("confluence.prime-research.local" nil)))
;; (setq gnutls-log-level 2)

;;; Local Variables
(setf enable-local-variables t)


;;; Add Marmalade and Melpa package archives
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; don't initialise all packages automatically
(setq package-enable-at-startup nil)


(defvar *my-box-tramp-path*
  "/ssh:darabi@galen:")

(defvar *current-tramp-path* nil)

(defun connect-to-host (path)
  (setq *current-tramp-path* path)
  (setq slime-translate-from-lisp-filename-function
    (lambda (f)
      (concat *current-tramp-path* f)))
  (setq slime-translate-to-lisp-filename-function
    (lambda (f)
      (substring f (length *current-tramp-path*))))
  (slime-connect "localhost" 4005))

(defun my-box-slime ()
  (interactive)
  (connect-to-host *my-box-tramp-path*))

(defun my-box-homedir ()
  (interactive)
  (find-file (concat *server-tramp-path* "/home/darabi/")))

;; http://stackoverflow.com/questions/23924306/arabic-glyphs-in-emacs
;;
;; to find your fonts: (dolist (f (font-family-list)) (print f))
;;
;; these are also OK for Farsi: "Iranian Sans", "Neirizi"
;;
(when window-system (set-fontset-font "fontset-default" '(#x600 . #x6ff) "Droid Arabic Naskh"))

(defun my-sort-maven-dependencies ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (when (re-search-forward "<dependencies>")
      (let ((start (point)))
        (while (and (not (looking-at "dependencies"))
                    (re-search-forward ">[\n[:space:]]*?<\\([^d]\\)"))
          (replace-match "><\\1" t nil))
        (sort-lines nil start (point))
        (goto-char start)
        ;; (indent-according-to-mode)
        (save-excursion
          (while (re-search-forward "><" nil t)
            (replace-match ">\n<")))
        (while (not (eq (point) (point-max)))
          (forward-line 1)
          (indent-according-to-mode))))))


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

(require 'mc-doom-theme)

(load-theme 'doom-sourcerer t)

;; expand-region https://github.com/magnars/expand-region.el
(require 'expand-region)

(defun my-shrink-region ()
  (interactive)
  (er/expand-region -1))

(global-set-key (kbd "C-'") 'er/expand-region)
(global-set-key (kbd "C-;") 'my-shrink-region)

;; projectile
(require 'projectile)
(setq projectile-project-search-path '("~/common-lisp/holz24"))

(provide 'my-init)
