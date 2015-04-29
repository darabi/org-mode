(unless (and (setq dwim-workspace (or (getenv "DWIM_WORKSPACE")
                                      (expand-file-name "~/workspace")))
             (file-exists-p dwim-workspace))
  (error "Could not find workspace directory (tried '%s'). Set/change the shell environment variable DWIM_WORKSPACE if you want to use something else." dwim-workspace))

(add-to-list 'load-path (expand-file-name (concat dwim-workspace "/hu.dwim.environment/emacs/")))
(require 'dwim-init)
(require 'dwim-key-bindings)

;;;
;;; edit-server (for e.g. the chrome edit-in-emacs plugin)
;;;
(add-to-list 'load-path (expand-file-name (concat dwim-workspace "/emacs/emacs_chrome/servers")))
(require 'edit-server)
(edit-server-start)

;; to deal with the stupid new gmail compose code that uses html internally even in plain text mode
(add-to-list 'load-path (expand-file-name (concat dwim-workspace "/emacs/edit-server-htmlize")))
(autoload 'edit-server-maybe-dehtmlize-buffer "edit-server-htmlize" "edit-server-htmlize" t)
(autoload 'edit-server-maybe-htmlize-buffer   "edit-server-htmlize" "edit-server-htmlize" t)
(add-hook 'edit-server-start-hook 'edit-server-maybe-dehtmlize-buffer)
(add-hook 'edit-server-done-hook  'edit-server-maybe-htmlize-buffer)

;; KLUDGE ideally the above installed autoloads should be enough, but
;; we cannot install the local load path early enough for
;; customizations, so we cannot use customization for setting
;; EDIT-SERVER-HTMLIZE-URL-REGEXP.
(require 'edit-server-htmlize)
(setq edit-server-htmlize-url-regexp
      "^\\(mail\\.google\\.com/mail/\\|groups\\.google\\.com/forum/\\)")


(add-to-list 'load-path (expand-file-name (concat dwim-workspace "/emacs/expand-region")))
(require 'expand-region)
(global-set-key (kbd "C-@") 'er/expand-region)

;;; backups

(pushnew '(".*/notes/.*" . ".backups")
         backup-directory-alist)

;;; email integration
(setq message-signature 'attila/message-signature)
(setq mu4e-compose-signature 'attila/message-signature)
;; TODO this breaks mu4e signatures currently (setq message-signature-insert-empty-line nil)

(defun attila/message-signature ()
  (shell-command-to-string "~/bin/update-signature")
  (with-temp-buffer
    (insert-file-contents "~/.signature")
    (buffer-string)))

(defun attila/message-insert-signature ()
  (interactive)
  (save-excursion
    (message-goto-signature)
    (unless (eql (point) (buffer-end 1))
      (previous-line)
      (kill-region (point) (buffer-end 1)))
    (message-insert-signature))
  (message-goto-signature))

(when (memq 'mu4e features)
  (message "WARNING: mu4e is already loaded when we try to load our own")
  (sit-for 2))
(add-to-list 'load-path (expand-file-name (concat dwim-workspace "/emacs/mu/mu4e/")))
(require 'mu4e)

(global-set-key [(super m)] 'mu4e)
(global-set-key [(control c) (control w)] 'attila/message-insert-signature)
(define-key message-mode-map [(control c) tab] 'completion-help-at-point)
(define-key message-mode-map [(control c) (control w)] 'attila/message-insert-signature)
;; mu4e-compose-mode-map

(add-hook 'message-setup-hook 'mml-secure-message-sign-pgpmime)
(add-hook 'mu4e-compose-mode-hook 'mml-secure-message-sign-pgpmime)

(setq mu4e-maildir "~/Maildir")
;; FIXME this may save unencrypted drafts... check.
(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
(setq mu4e-trash-folder  "/[Gmail].Trash")
(setq mu4e-sent-messages-behavior 'delete)
(setq mu4e-get-mail-command "offlineimap -q")
(setq message-kill-buffer-on-exit t)
(setq mu4e-view-show-images t
      mu4e-view-image-max-width 800)
(setq mu4e-attachment-dir "~/Desktop")

(setq mu4e-maildir-shortcuts
      '( ("/INBOX"               . ?i)
         ("/[Gmail].Sent Mail"   . ?s)
         ("/[Gmail].Trash"       . ?t)
         ("/[Gmail].All Mail"    . ?a)))

(setq user-mail-address "attila.lendvai@gmail.com"
      user-full-name    "Attila Lendvai")

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

;;; gpg integration
(require 'epa-file)
(epa-file-enable)

(defun attila/kill-gpg-buffers ()
  ;; automatically delete *.gpg buffers that has been displayed longer than 60 seconds ago
  ;; install with: (run-with-idle-timer 60 t 'attila/kill-gpg-buffers)
  (interactive)
  (let ((buffers-killed 0))
    ;; (message "Looking for .gpg buffers among '%s' buffers" (length (buffer-list)))
    (dolist (buffer (copy-list (buffer-list)))
      (with-current-buffer buffer
        ;; (message "Looking at '%s' buffers" (buffer-name buffer))
        (when (and (string-match ".*\.gpg$" (buffer-name))
                   buffer-file-name
                   (> (buffer-size) 0))
          (let* ((current-time (current-time))
                 (last-displayed-time (or buffer-display-time
                                          (progn
                                            (display-buffer (current-buffer))
                                            buffer-display-time)))
                 (last-modified-time (if (numberp (visited-file-modtime))
                                         (list 0 0)
                                         (visited-file-modtime)))
                 (last-activity-time (if (time-less-p last-displayed-time
                                                      last-modified-time)
                                         last-modified-time
                                         last-displayed-time))
                 (time-since-last-activity (time-to-seconds (time-subtract current-time last-activity-time))))
            ;; (message "'%s': last-displayed-time '%s', last-modified-time '%s', last-activity-time '%s', time-since-last-activity '%s'"
            ;;          (buffer-name buffer) last-displayed-time last-modified-time last-activity-time time-since-last-activity)
            (if (> time-since-last-activity
                   90)
                (progn
                  (message "Auto-killing .gpg buffer '%s' because it was last active %s seconds ago" (buffer-name buffer) time-since-last-activity)
                  (when (buffer-modified-p buffer)
                    (save-buffer)
                    (message "Autosaved .gpg buffer '%s'" (buffer-name buffer)))
                  (kill-buffer buffer)
                  (incf buffers-killed))
                (progn
                  ;; (message "Not killing .gpg buffer '%s' yet, it was active %s seconds ago" (buffer-name buffer) time-since-last-activity)
                  ))))))
    (unless (zerop buffers-killed)
      (message "%s .gpg buffers have been killed (and potentially autosaved)" buffers-killed))))

(run-with-timer 15 15 'attila/kill-gpg-buffers)
;; (cancel-function-timers 'attila/kill-gpg-buffers)

(global-set-key [(control next)] 'end-of-buffer)
(global-set-key [(control prior)] 'beginning-of-buffer)

(require 'bbdb-loaddefs "~/workspace/emacs/bbdb/lisp/bbdb-loaddefs.el")
(global-set-key [(super b)] 'bbdb)
(require 'bbdb)
(require 'bbdb-mua)

(add-hook 'gnus-startup-hook (lambda ()
                               (define-key gnus-summary-mode-map (kbd "C-o") 'other-window)
                               (require 'bbdb)
                               (bbdb-initialize 'gnus)
                               (setq bbdb-complete-name-allow-cycling t
                                     bbdb-always-add-addresses t
                                     bbdb/mail-auto-create-p t)
                               (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)))

;; BEGIN:VCARD
;; VERSION:2.1
;; N;ENCODING=QUOTED-PRINTABLE;CHARSET=UTF-8:;=
;; Korlan=20Smagulova;;;
;; TEL;VOICE;CELL:+77012214191
;; END:VCARD

(defun attila/export-bbdb-to-vcard ()
  (bbdb-with-db-buffer
    (dolist (record bbdb-records)
      (let ((entry-level1 (format "BEGIN:VCARD
VERSION:3.0
N:%s%%s;%s;;;
TEL;VOICE;CELL:%%s
END:VCARD

" (bbdb-record-firstname record) (bbdb-record-lastname record)))
            (phone-records (bbdb-record-phone record)))
        (setq phone-records (sort (copy-seq phone-records) (lambda (e1 e2)
                                                             (let ((l1 (bbdb-phone-label e1))
                                                                   (l2 (bbdb-phone-label e2)))
                                                               (or (string= l1 "Main")
                                                                   (string= l1 "Mobile")
                                                                   (string< l1 l2))))))
        (dolist (phone phone-records)
          (princ (format entry-level1
                         (if (> (length phone-records) 1)
                             (concatenate 'string "(" (subseq (bbdb-phone-label phone) 0 1) ")")
                             "")
                         (bbdb-phone-string phone))))))))

(defun attila/delete-bbdb-xfields (field-name)
  (bbdb-with-db-buffer
    (dolist (record bbdb-records)
      (bbdb-record-set-xfield record field-name nil)
      (bbdb-change-record record))
    (bbdb-save)))

;;(setf minibuffer-message-timeout 0)

(global-set-key (kbd "M-b") 'redo)
(global-set-key (kbd "C-b") 'undo)

(global-set-key (kbd "C-,") 'copy-primary-selection)
(global-set-key (kbd "C-.") 'yank-clipboard-selection)
(global-set-key [(control insert)] 'kill-primary-selection)

(dwim-define-lisp-key (kbd "C->") 'dwim-paste-clipboard-at-last-change)
(dwim-define-lisp-key (kbd "C-M-,") 'dwim-copy-sexp-at-point)
(dwim-define-lisp-key (kbd "C-M-.") 'dwim-replace-sexp-at-point-with-clipboard)

;; KLUDGE to deal with the 'hyperspect' debian package
(let ((buffer (find-buffer-visiting "/usr/share/doc/hyperspec/Data/Map_Sym.txt")))
  (when buffer
    (kill-buffer buffer)))

;;(setq browse-url-firefox-program "/usr/local/bin/Icecat32")
;;(setq browse-url-firefox-program "firefox")

(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "chromium")

;; http://random-state.net/log/3512630740.html
(setq font-lock-verbose nil)

(setq slime-default-lisp 'sbcl-hu.dwim-development)

(let ((dir "/media/store/work/"))
  (when (file-exists-p dir)
    (dolist (entry (dwim/build-sbcl-lisp-implementation-entries/from-installed-binaries dir))
      (pushnew entry slime-lisp-implementations :test 'equal))))

;(add-hook 'bbdb-initialize-hook (lambda ()
;                                  (setq bbdb-user-mail-names
;                                        (regexp-opt '("attila.lendvai@gmail.com")))))

;; TODO promote to shared settings?
(global-set-key [(control meta ?,)] 'dwim-copy-word-at-point)
(global-set-key [(control meta ?.)] 'dwim-replace-word-at-point-with-clipboard)

(defun ati-c-setup ()
  (setq c-default-style "linux"
        c-basic-offset 4)
  (modify-syntax-entry ?_ "w" c-mode-syntax-table) ; _ should not be a word separator
  (pushnew '(statement-case-intro . 0) c-offsets-alist)
  (pushnew '(substatement-open . 0) c-offsets-alist)
  (define-key c-mode-map [(control ?d)] 'dwim-duplicate-line-at-point)
  (define-key c-mode-map (kbd "[") (lambda () (interactive) (insert "(")))
  (define-key c-mode-map (kbd "]") (lambda () (interactive) (insert ")")))
  (define-key c-mode-map (kbd "\(") (lambda () (interactive) (insert "[")))
  (define-key c-mode-map (kbd "\)") (lambda () (interactive) (insert "]"))))

(add-hook 'c-mode-hook 'ati-c-setup)

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

(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(require 'lua-mode)

(defun* dwim-define-lua-key (key binding &key (repl t) (buffers t))
  (when buffers
    (define-key lua-mode-map key binding))
  ;; it's a goddamn generic commint-mode...
  ;;(when repl
  ;;  (define-key slime-repl-mode-map key binding))
  )

(dwim-define-lua-key (kbd "[") (lambda () (interactive) (insert "(")))
(dwim-define-lua-key (kbd "]") (lambda () (interactive) (insert ")")))
(dwim-define-lua-key (kbd "\(") (lambda () (interactive) (insert "[")))
(dwim-define-lua-key (kbd "\)") (lambda () (interactive) (insert "]")))

(dwim-define-lua-key (kbd "C-c C-c") (lambda ()
                                       (interactive)
                                       (save-excursion
                                         (let ((start (progn
                                                        (lua-beginning-of-proc)
                                                        (point)))
                                               (end (progn
                                                      (lua-end-of-proc)
                                                      (point)))
                                               (original-repl-buffer-size (buffer-size lua-process-buffer))
                                               (lua-always-show nil))
                                           (slime-flash-region start end)
                                           (lua-send-region start end)
                                           (when (> (- (buffer-size lua-process-buffer)
                                                       original-repl-buffer-size)
                                                    10)
                                             (display-buffer lua-process-buffer))))))

(dwim-define-lua-key (kbd "C-/") (lambda ()
                                   (interactive)
                                   (or (and lua-process
                                             (comint-check-proc lua-process-buffer))
                                        (lua-start-process lua-default-application))
                                   (dwim-switch-between-target-buffer-and-last-buffer lua-process-buffer)
                                   (goto-char (1+ (buffer-size lua-process-buffer)))))

(setq lua-default-application "/home/alendvai/workspace/lua-5.2.0/src/lua ")

(setq lua-default-application "/usr/bin/env")
(setq lua-default-command-switches '("LUA_PATH=/home/alendvai/workspace/luasocket2-hg/install/share/lua/5.2/?.lua"
                                     "LUA_CPATH=/home/alendvai/workspace/luasocket2-hg/install/lib/lua/5.2/?.so"
                                     "/home/alendvai/workspace/lua-5.2.0/src/lua"
                                     "-i"))

(defun attila/python-mode-hook ()
  (define-key python-mode-map (kbd "RET") 'newline-and-indent)
  (define-key python-mode-map (kbd "[") (lambda () (interactive) (insert "(")))
  (define-key python-mode-map (kbd "]") (lambda () (interactive) (insert ")")))
  (define-key python-mode-map (kbd "\(") (lambda () (interactive) (insert "[")))
  (define-key python-mode-map (kbd "\)") (lambda () (interactive) (insert "]"))))

(add-hook 'python-mode-hook 'attila/python-mode-hook)

(load (expand-file-name (concat dwim-workspace "/hu.dwim.environment/emacs/nxhtml/autostart.el")))

(setq enable-local-variables t)


