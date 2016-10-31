;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ORG-MODE
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; http://doc.norang.ca/org-mode.html#Setup
;;
;; I have the following custom key bindings set up for my emacs (sorted by frequency).
;; Key	For	Used
;; F12	Agenda (1 key less than C-c a)	Very Often
;; C-c b	Switch to org file	Very Often
;; F11	Goto currently clocked item	Very Often
;; C-M-r	Capture a task	Very Often
;; C-F11	Clock in a task (show menu with prefix)	Often
;; f9 g	Gnus - I check mail regularly	Often
;; f5	Show todo items for this subtree	Often
;; S-f5	Widen	Often
;; f9 b	Quick access to bbdb data	Often
;; f9 c	Calendar access	Often
;; C-S-f12	Save buffers and publish current project	Often
;; C-c l	Store a link for retrieval with C-c C-l	Often
;; f8	Go to next org file in org-agenda-files	Sometimes
;; f9 r	Boxquote selected region	Sometimes
;; f9 t	Insert inactive timestamp	Sometimes
;; f9 v	Toggle visible mode (for showing/editing links)	Sometimes
;; C-f9	Previous buffer	Sometimes
;; C-f10	Next buffer	Sometimes
;; C-x n r	Narrow to region	Sometimes
;; f9 f	Boxquote insert a file	Sometimes
;; f9 i	Info manual	Sometimes
;; f9 I	Punch Clock In	Sometimes
;; f9 O	Punch Clock Out	Sometimes
;; f9 o	Switch to org scratch buffer	Sometimes
;; f9 s	Switch to scratch buffer	Sometimes
;; C-c r	Capture a task (from my mobile phone)	Rare
;; f9 h	Hide other tasks	Rare
;; f7	Toggle line truncation/wrap	Rare
;; f9 T	Tabify region	Rare
;; f9 U	Untabify region	Rare
;; C-c a	Enter Agenda (minimal emacs testing)	Rare

;; Custom Key Bindings
(global-set-key (kbd "<f12>") 'org-agenda)
;; macbook f12
(global-set-key (kbd "<down-mouse-3>") 'org-agenda)

(global-set-key (kbd "<f5>") 'bh/org-todo)
(global-set-key (kbd "<S-f5>") 'bh/widen)
(global-set-key (kbd "<f7>") 'bh/set-truncate-lines)
(global-set-key (kbd "<f8>") 'org-cycle-agenda-files)
(global-set-key (kbd "<f9> <f9>") 'bh/show-org-agenda)
(global-set-key (kbd "<f9> b") 'bbdb)
(global-set-key (kbd "<f9> c") 'calendar)
(global-set-key (kbd "<f9> f") 'boxquote-insert-file)
(global-set-key (kbd "<f9> g") 'gnus)
(global-set-key (kbd "<f9> h") 'bh/hide-other)
(global-set-key (kbd "<f9> n") 'org-narrow-to-subtree)
(global-set-key (kbd "<f9> w") 'widen)
(global-set-key (kbd "<f9> u") 'bh/narrow-up-one-level)

(global-set-key (kbd "<f9> I") 'bh/punch-in)
(global-set-key (kbd "<f9> O") 'bh/punch-out)

(global-set-key (kbd "<f9> o") 'bh/make-org-scratch)

(global-set-key (kbd "<f9> r") 'boxquote-region)
(global-set-key (kbd "<f9> s") 'bh/switch-to-scratch)

(global-set-key (kbd "<f9> t") 'bh/insert-inactive-timestamp)
(global-set-key (kbd "<f9> T") 'tabify)
(global-set-key (kbd "<f9> U") 'untabify)

(global-set-key (kbd "<f9> v") 'visible-mode)
(global-set-key (kbd "<f9> SPC") 'bh/clock-in-last-task)
(global-set-key (kbd "C-<f9>") 'previous-buffer)
(global-set-key (kbd "M-<f9>") 'org-toggle-inline-images)
(global-set-key (kbd "C-x n r") 'narrow-to-region)

(global-set-key (kbd "C-<f10>") 'next-buffer)
(global-set-key (kbd "<f11>") 'org-clock-goto)
(global-set-key (kbd "C-<f11>") 'org-clock-in)
(global-set-key (kbd "C-s-<f12>") 'bh/save-then-publish)
(global-set-key (kbd "C-M-r") 'org-capture)
(global-set-key (kbd "C-c r") 'org-capture)

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, and org-protocol
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/git/org/refile.org")
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
              ("r" "respond" entry (file "~/git/org/refile.org")
               "* TODO Respond to %:from on %:subject\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
              ("n" "note" entry (file "~/git/org/refile.org")
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("j" "Journal" entry (file+datetree "~/git/org/diary.org")
               "* %?\n%U\n" :clock-in t :clock-resume t)
              ("w" "org-protocol" entry (file "~/git/org/refile.org")
               "* TODO Review %c\n%U\n" :immediate-finish t)
              ("p" "Phone call" entry (file "~/git/org/refile.org")
               "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
              ("h" "Habit" entry (file "~/git/org/refile.org")
               "* NEXT %?\n%U\n%a\nSCHEDULED: %t .+1d/3d\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

(global-set-key (kbd "<f5>") 'bh/org-todo)

(defun bh/org-todo (arg)
  (interactive "p")
  (if (equal arg 4)
      (save-restriction
        (bh/narrow-to-org-subtree)
        (org-show-todo-tree nil))
    (bh/narrow-to-org-subtree)
    (org-show-todo-tree nil)))

(global-set-key (kbd "<S-f5>") 'bh/widen)

(defun bh/widen ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (org-agenda-remove-restriction-lock)
    (widen)
    (org-agenda-remove-restriction-lock)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "W" 'bh/widen))
          'append)

(defun bh/restrict-to-file-or-follow (arg)
  "Set agenda restriction to 'file or with argument invoke follow mode.
I don't use follow mode very often but I restrict to file all the time
so change the default 'F' binding in the agenda to allow both"
  (interactive "p")
  (if (equal arg 4)
      (org-agenda-follow-mode)
    (if (equal major-mode 'org-agenda-mode)
        (bh/set-agenda-restriction-lock 4)
      (widen))))

(defun bh/narrow-to-project ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
        (bh/narrow-to-org-project))
    (bh/narrow-to-org-project)))

(defun bh/narrow-to-org-subtree ()
  (widen)
  (org-narrow-to-subtree)
  (save-restriction
    (org-agenda-set-restriction-lock)))

(defun bh/narrow-to-subtree ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (org-with-point-at (org-get-at-bol 'org-hd-marker)
        (bh/narrow-to-org-subtree))
    (bh/narrow-to-org-subtree)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "N" 'bh/narrow-to-subtree))
          'append)

(defun bh/narrow-up-one-org-level ()
  (widen)
  (save-excursion
    (outline-up-heading 1 'invisible-ok)
    (bh/narrow-to-org-subtree)))

(defun bh/set-agenda-restriction-lock (arg)
  "Set restriction lock to current task subtree or file if prefix is specified"
  (interactive "p")
  (let* ((pom (bh/get-pom-from-agenda-restriction-or-point))
         (tags (org-with-point-at pom (org-get-tags-at))))
    (let ((restriction-type (if (equal arg 4) 'file 'subtree)))
      (save-restriction
        (cond
         ((and (equal major-mode 'org-agenda-mode) pom)
          (org-with-point-at pom
            (org-agenda-set-restriction-lock restriction-type)))
         ((and (equal major-mode 'org-mode) (org-before-first-heading-p))
          (org-agenda-set-restriction-lock 'file))
         (pom
          (org-with-point-at pom
            (org-agenda-set-restriction-lock restriction-type))))))))

(defun bh/get-pom-from-agenda-restriction-or-point ()
  (or (org-get-at-bol 'org-hd-marker)
      (and (marker-position org-agenda-restrict-begin) org-agenda-restrict-begin)
      (and (equal major-mode 'org-mode) (point))
      org-clock-marker))

(defun bh/narrow-up-one-level ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
        (bh/narrow-up-one-org-level))
    (bh/narrow-up-one-org-level)))

(defun bh/show-org-agenda ()
  (interactive)
  (switch-to-buffer "*Org Agenda*")
  (delete-other-windows))

(defvar bh/current-view-project nil)

(defun bh/view-next-project ()
  (interactive)
  (unless (marker-position org-agenda-restrict-begin)
    (goto-char (point-min))
    (setq bh/current-view-project (point)))
  (bh/widen)
  (goto-char bh/current-view-project)
  (forward-visible-line 1)
  (while (and (< (point) (point-max))
              (or (not (org-get-at-bol 'org-hd-marker))
                  (org-with-point-at (org-get-at-bol 'org-hd-marker)
                    (or (not (bh/is-project-p))
                        (bh/is-project-subtree-p)))))
    (forward-visible-line 1))
  (setq bh/current-view-project (point))
  (if (org-get-at-bol 'org-hd-marker)
      (bh/narrow-to-project)
      (message "All projects viewed.")
      (ding)))

(defun bh/org-todo (arg)
  (interactive "p")
  (if (equal arg 4)
      (save-restriction
        (bh/narrow-to-org-subtree)
        (org-show-todo-tree nil))
    (bh/narrow-to-org-subtree)
    (org-show-todo-tree nil)))

(defun bh/hide-other ()
  (interactive)
  (save-excursion
    (org-back-to-heading 'invisible-ok)
    (hide-other)
    (org-cycle)
    (org-cycle)
    (org-cycle)))

(defun bh/set-truncate-lines ()
  "Toggle value of truncate-lines and refresh window display."
  (interactive)
  (setq truncate-lines (not truncate-lines))
  ;; now refresh window display (an idiom from simple.el):
  (save-excursion
    (set-window-start (selected-window)
                      (window-start (selected-window)))))

(defun bh/make-org-scratch ()
  (interactive)
  (find-file "/tmp/publish/scratch.org")
  (gnus-make-directory "/tmp/publish"))

(defun bh/switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

;; Remove empty LOGBOOK drawers on clock out
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "LOGBOOK" (point))))

;; http://article.gmane.org/gmane.emacs.orgmode/8920
(defun org-gnus-store-link ()
  "Store a link to a Gnus folder or message.  Modified to store
  message IDs."
  (cond
   ((eq major-mode 'gnus-group-mode)
    (let ((group (cond ((fboundp 'gnus-group-group-name) ; depending on Gnus
                        (gnus-group-group-name))         ; version
                       ((fboundp 'gnus-group-name)
                        (gnus-group-name))
                       (t "???")))
          desc link)
      (unless group (error "Not on a group"))
      (org-store-link-props :type "gnus" :group group)
      (setq desc (concat
                  (if (org-xor current-prefix-arg
                               org-usenet-links-prefer-google)
                      "http://groups.google.com/groups?group="
                    "gnus:")
                  group)
            link (org-make-link desc))
      (org-add-link-props :link link :description desc)
      link))

   ((memq major-mode '(gnus-summary-mode gnus-article-mode))
    (and (eq major-mode 'gnus-article-mode) (gnus-article-show-summary))
    (let* ((group gnus-newsgroup-name)
           (article (gnus-summary-article-number))
           (header (gnus-summary-article-header article))
           (from (mail-header-from header))
           (message-id (mail-header-id header))
           (date (mail-header-date header))
           (subject (gnus-summary-subject-string))
           desc link)
      (org-store-link-props :type "gnus" :from from :subject subject
                            :message-id message-id :group group)
      (setq desc (org-email-link-description))
      (if (org-xor current-prefix-arg org-usenet-links-prefer-google)
          (setq link
                (concat
                 desc "\n  "
                 (format "http://groups.google.com/groups?as_umsgid=%s"
                         (org-fixup-message-id-for-http message-id))))
        (setq link (org-make-link "gnus:" group "#" message-id)))
      (org-add-link-props :link link :description desc)
      link))))

(defun org-gnus-follow-link (&optional group article)
  "Follow a Gnus link to GROUP and ARTICLE."
  (require 'gnus)
  (funcall (cdr (assq 'gnus org-link-frame-setup)))
  (if gnus-other-frame-object (select-frame gnus-other-frame-object))
  (cond ((and group article)
         (gnus-group-read-group 1 nil group)
         (gnus-summary-goto-article article nil t))
        (group (gnus-group-jump-to-group group))))

;; cf. http://orgmode.org/worg/org-hacks.html
(defun my-message-send-and-org-gnus-store-link (&optional arg)
  "Send message with `message-send-and-exit' and store org link to message copy.
If multiple groups appear in the Gcc header, the link refers to
the copy in the last group."
  (interactive "P")
    (save-excursion
      (save-restriction
        (message-narrow-to-headers)
        (let ((gcc (car (last
                         (message-unquote-tokens
                          (message-tokenize-header
                           (mail-fetch-field "gcc" nil t) " ,")))))
              (buf (current-buffer))
              (message-kill-buffer-on-exit nil)
              id to from subject desc link newsgroup xarchive)
        (message-send-and-exit arg)
        (or
         ;; gcc group found ...
         (and gcc
              (save-current-buffer
                (progn (set-buffer buf)
                       (setq id (org-remove-angle-brackets
                                 (mail-fetch-field "Message-ID")))
                       (setq to (mail-fetch-field "To"))
                       (setq from (mail-fetch-field "From"))
                       (setq subject (mail-fetch-field "Subject"))))
              (org-store-link-props :type "gnus" :from from :subject subject
                                    :message-id id :group gcc :to to)
              (setq desc (org-email-link-description))
              (setq link (org-gnus-article-link
                          gcc newsgroup id xarchive))
              (setq org-stored-links
                    (cons (list link desc) org-stored-links)))
         ;; no gcc group found ...
         (message "Can not create Org link: No Gcc header found."))))))

(defun my-invoice-heading-hook ()
  "Is called when org-invoice is collecting data from a heading.
With a small modification of org-clock-sum, we add a property
:org-clock-invoice-data to the heading text, and here we collect
that information. See ``ORG-CLOCK-SUM``"
  (message "my-invoice-heading-hook is called with %s" org-invoice-current-item)
  (push (cons 'timelist (get-text-property (point) :org-clock-invoice-data)) org-invoice-current-item))

(provide 'my-org-init)
