(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(dolist (p '(
             ;; (helm-ido-like :type git :host github :repo "clemera/helm-ido-like-guide" :branch "master" :commit "28cee63") ;; make helm behave more like ido
             (org :fork (:type git :host github :repo "m-creations/org-mode" :local-repo "org" :branch "release_9.6.23-kd-org-clock-timelist"))
             (ox-jira :type git :host github :repo "stig/ox-jira.el" :branch "trunk" :commit "0bd966ba") ;; export Org markup to Jira (needed by ejira)
             ;; (ejira :type git :host github :repo "nyyManni/ejira" :branch "master" :commit "8675aaef") ;; org-mode Jira integration (better than org-jira)
             adoc-mode
             all-the-icons
             apheleia ;; source code autoformatter
             bbdb
             browse-kill-ring
             company-lsp
             color-theme
             color-theme-sanityinc-tomorrow
             dash
             default-text-scale ;; increase/decrease font scaling in window
             dockerfile-mode
             editorconfig
             eglot ;; language server protocol LSP client https://github.com/joaotavora/eglot
             elisp-slime-nav ;; slime style navigation with M-. and M-, in elisp buffers
             exec-path-from-shell
             expand-region ;; js
             external-completion ;; https://elpa.gnu.org/packages/external-completion.html mirrored at https://github.com/emacs-straight/external-completion
             findr ;; instead of grep
             flycheck
             groovy-mode
             helm
             helm-flx
             helm-fuzzier
             helm-lsp
             helm-org
             helm-smex
             helm-swoop
             hydra
             jedi
             js2-mode
             keepass-mode
             kubel
             kubernetes
             lsp-docker
             lsp-java
             lsp-ui
             (magit-section :branch "main") ;; A Git porcelain inside Emacs.
             (git-commit :branch "main") ;; A Git porcelain inside Emacs.
             (magit :branch "main") ;; A Git porcelain inside Emacs.
             magit-lfs ;; Magit plugin for Git LFS
             magit-svn ;; Git-Svn extension for Magit
             magit-todos ;; Show source file TODOs in Magit
             magithub ;; Magit interfaces for GitHub
             merlin ;; ocaml
             ob-browser ;; Render HTML in org-mode blocks.
             ob-restclient ;; org-babel functions for restclient-mode
             ob-sql-mode ;; SQL code blocks evaluated by sql-mode
             ob-tmux ;; Babel Support for Interactive Terminal
             org-rich-yank ;; paste with org-mode markup and link to source
             orgnav ;; Org tree navigation using helm
             paredit
             posframe
             projectile
             python-environment
             slime-docker
             smex
             tide ;; js
             tree-sitter ;; see https://emacs-tree-sitter.github.io/installation/ and https://tree-sitter.github.io/tree-sitter/
             tree-sitter-langs
             tuareg ;; ocaml
             w3m
             yasnippet
             ))
  (straight-use-package p))

;; auto-format different source code files extremely intelligently
;; https://github.com/radian-software/apheleia
(use-package apheleia
  :straight t
  :ensure t
  :config
  (apheleia-global-mode +1))

(use-package tree-sitter
  :straight t
  :ensure t
  :config
  ;; activate tree-sitter on any buffer containing code for which it has a parser available
  (global-tree-sitter-mode)
  ;; you can easily see the difference tree-sitter-hl-mode makes for python, ts or tsx
  ;; by switching on and off
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :straight t
  :ensure t
  :after tree-sitter)

(load-theme 'sanityinc-tomorrow-eighties t)

(custom-theme-set-faces 'sanityinc-tomorrow-eighties
                        '(region ((t (:background "#DF0000")))))

(set-face-attribute 'secondary-selection nil :background "lightyellow" :foreground "black")


;; shows all colors and faces in a buffer

;; (custom-theme-visit-theme 'sanityinc-tomorrow-eighties)

;; (straight-use-package 'tide)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;; Add Marmalade and Melpa package archives
                                        ; (require 'package)
                                        ; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
                                        ; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; don't initialise all packages automatically
;; (setq package-enable-at-startup nil)

;; do not update quelpa repos on startup
                                        ; (setq quelpa-update-melpa-p nil)
;; do not perform a shallow clone
                                        ; (setq quelpa-git-clone-depth nil)

                                        ; (package-initialize)

;; (quelpa '(company-lsp :repo "tigersoldier/company-lsp" :fetcher github))
;; (quelpa '(dap-mode :repo "emacs-lsp/dap-mode" :fetcher github))
;; (quelpa '(helm-lsp :repo "emacs-lsp/helm-lsp" :fetcher github))
;; (quelpa '(lsp-mode :repo "emacs-lsp/lsp-mode" :fetcher github))
;; (quelpa '(lsp-ivy :repo "emacs-lsp/lsp-ivy" :fetcher github))
;; (quelpa '(lsp-java :repo "emacs-lsp/lsp-java" :fetcher github))
;; (quelpa '(lsp-treemacs :repo "emacs-lsp/lsp-treemacs" :fetcher github))
;; (quelpa '(lsp-ui :repo "emacs-lsp/lsp-ui" :fetcher github))

;; the 'local part indicates that the checked out branch at
;; ~/.emacs.d/quelpa/build/lsp-java
;; is used
;; (quelpa-upgrade '(lsp-java :repo "emacs-lsp/lsp-java" :fetcher github) 'local)
;; (quelpa-upgrade '(lsp-mode :repo "emacs-lsp/lsp-mode" :fetcher github) 'local)

;; (setq debug-on-error t)

(load-file "~/.emacs.d/site-lisp/dot-emacs")

(setq auth-sources '("~/.authinfo.gpg"))

;; (defun my-anonymise ()
;;   (interactive)
;;   (flet ((rep (srch rep)
;;               (while (search-forward srch nil t)
;;                 (replace-match rep))))
;;     (save-excursion
;;       (rep "hpc" "test"))
;;     (save-excursion
;;       (rep "m-creations.net" "example.com"))))

;; (setq org-id-track-globally t)

;; (setq request-curl-options '("-k"))

;; (use-package ejira
;;   :init
;;   (setq ;; jiralib2-url              "https://jira.hpchd.loc"
;;         ;; jiralib2-user-login-name  "Kambiz.Darabi.extern"
;;         jiralib2-url              "https://jira.xing.hh"
;;         jiralib2-user-login-name  "kambiz.darabi"
;;         ;; 8g6Z4M4sZ2Ft
;;         jiralib2-auth             'basic
;;         jiralib2-token            nil

;;         ejira-org-directory       "~/vc/org/work/jira-xing/"
;;         ejira-projects            '("HEI")

;;         ejira-priorities-alist    '(("Highest" . ?A)
;;                                     ("High"    . ?B)
;;                                     ("Medium"  . ?C)
;;                                     ("Low"     . ?D)
;;                                     ("Lowest"  . ?E))
;;         ejira-todo-states-alist   '(("To Do"       . 1)
;;                                     ("In Progress" . 2)
;;                                     ("Done"        . 3)))
;;   :config
;;   ;; Tries to auto-set custom fields by looking into /editmeta
;;   ;; of an issue and an epic.
;;   (add-hook 'jiralib2-post-login-hook #'ejira-guess-epic-sprint-fields)

;;   ;; They can also be set manually if autoconfigure is not used.
;;   ;; (setq ejira-sprint-field       'customfield_10001
;;   ;;       ejira-epic-field         'customfield_10002
;;   ;;       ejira-epic-summary-field 'customfield_10004)

;;   (require 'ejira-agenda)

;;   ;; Make the issues visisble in your agenda by adding `ejira-org-directory'
;;   ;; into your `org-agenda-files'.
;;   (add-to-list 'org-agenda-files ejira-org-directory)

;;   ;; Add an agenda view to browse the issues that
;;   (org-add-agenda-custom-command
;;    '("j" "My JIRA issues"
;;      ((ejira-jql "resolution = unresolved and assignee = currentUser()"
;;                  ((org-agenda-overriding-header "Assigned to me")))))))

;; (defun setup-tide-mode ()
;;   (interactive)
;;   ;; (setq tide-tsserver-process-environment '("TSS_LOG=-level verbose -file /tmp/tss.log"))
;;   (tide-setup)
;;   ;;(setq flycheck-check-syntax-automatically '(save mode-enabled))
;;   (eldoc-mode +1)
;;   (tide-hl-identifier-mode +1)
;;   ;; company is an optional dependency. You have to
;;   ;; install it separately via package-install
;;   ;; `M-x package-install [ret] company`
;;   )

;; (require 'eldoc)
;; (require 'tide)

;; (setup-tide-mode)

;; (use-package tide
;;   :ensure t
;;   :bind (("M-." . tide-jump-to-definition)
;;          ("M-," . tide-jump-back)
;;          )

;;   :config
;;   (setup-tide-mode)
;;   ;; aligns annotation to the right hand side
;;   (setq company-tooltip-align-annotations t)

;;   ;; formats the buffer before saving
;;   ;;(add-hook 'before-save-hook 'tide-format-before-save)

;;   ;; configure javascript-tide checker to run after your default javascript checker
;;   (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)

;;   (add-hook 'web-mode-hook
;;             (lambda ()
;;               (when (string-equal "js" (file-name-extension buffer-file-name))
;;                 (setup-tide-mode))))
;;   ;; configure jsx-tide checker to run after your default jsx checker
;;   (flycheck-add-mode 'javascript-eslint 'web-mode)
;;   (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)
;;   )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(calendar-week-start-day 1)
 '(custom-safe-themes
   '("628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "816bacf37139d6204b761fea0d25f7f2f43b94affa14aa4598bce46157c160c2" "99c86852decaeb0c6f51ce8bd46e4906a4f28ab4c5b201bdc3fdf85b24f88518" "fede08d0f23fc0612a8354e0cf800c9ecae47ec8f32c5f29da841fe090dfc450" "ffac21ab88a0f4603969a24b96993bd73a13fe0989db7ed76d94c305891fad64" "ed4c48eb91d07c2e447b445e2491ef17e9b326d43a60022297fd56af4749e772" "dd4628d6c2d1f84ad7908c859797b24cc6239dfe7d71b3363ccdd2b88963f336" "3be1f5387122b935a26e02795196bc90860c57a62940f768f138b02383d9a257" "04790c9929eacf32d508b84d34e80ad2ee233f13f17767190531b8b350b9ef22" "80930c775cef2a97f2305bae6737a1c736079fdcc62a6fdf7b55de669fbbcd13" "cabc32838ccceea97404f6fcb7ce791c6e38491fd19baa0fcfb336dcc5f6e23c" "59ba50f24540958f33699a5247255d10f34dd812f3975837e3eddccdc4caa32e" "45a8b89e995faa5c69aa79920acff5d7cb14978fbf140cdd53621b09d782edcf" "f5f3a6fb685fe5e1587bafd07db3bf25a0655f3ddc579ed9d331b6b19827ea46" "41eb3fe4c6b80c7ad156a8c52e9dd6093e8856c7bbf2b92cc3a4108ceb385087" "542e6fee85eea8e47243a5647358c344111aa9c04510394720a3108803c8ddd1" "44961a9303c92926740fc4121829c32abca38ba3a91897a4eab2aa3b7634bed4" "f984e2f9765a69f7394527b44eaa28052ff3664a505f9ec9c60c088ca4e9fc0b" "12670281275ea7c1b42d0a548a584e23b9c4e1d2dabb747fd5e2d692bcd0d39b" "808b47c5c5583b5e439d8532da736b5e6b0552f6e89f8dafaab5631aace601dd" "6daa09c8c2c68de3ff1b83694115231faa7e650fdbb668bc76275f0f2ce2a437" "fee4e306d9070a55dce4d8e9d92d28bd9efe92625d2ba9d4d654fc9cd8113b7f" "4feee83c4fbbe8b827650d0f9af4ba7da903a5d117d849a3ccee88262805f40d" "aea30125ef2e48831f46695418677b9d676c3babf43959c8e978c0ad672a7329" "fc7fd2530b82a722ceb5b211f9e732d15ad41d5306c011253a0ba43aaf93dccc" "16dd114a84d0aeccc5ad6fd64752a11ea2e841e3853234f19dc02a7b91f5d661" "36282815a2eaab9ba67d7653cf23b1a4e230e4907c7f110eebf3cdf1445d8370" "ecfd522bd04e43c16e58bd8af7991bc9583b8e56286ea0959a428b3d7991bbd8" "dcdd1471fde79899ae47152d090e3551b889edf4b46f00df36d653adc2bf550d" default))
 '(helm-completion-style 'emacs)
 '(org-time-stamp-custom-formats '("<%a %Y-%m-%d>" "<%Y-%m-%d>" "<%Y-%m-%d %H:%M>"))
 '(package-selected-packages
   '(expand-region doom-themes boxquote bbdb findr elisp-slime-nav browse-kill-ring paredit lsp-ivy magit-lfs magit-svn magit color-theme-sanityinc-tomorrow zenburn-theme base16-theme dracula-theme dap-mode lsp-ui flycheck projectile orgnav org-rich-yank org quelpa-use-package quelpa ob-tmux ob-sql-mode ob-restclient ob-browser lsp-treemacs helm helm-lsp ace-window dh-elpa which-key web-mode use-package undercover spinner popup pfuture org-jira lv lsp-java let-alist json-mode js2-refactor go-eldoc find-file-in-project exec-path-from-shell elpy buttercup bui async assess))
 '(safe-local-variable-values '((tabs-width . 4)))
 '(select-enable-clipboard t)
 '(show-paren-delay 0)
 '(show-paren-mode 'paren nil (paren))
 '(vc-follow-symlinks t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
