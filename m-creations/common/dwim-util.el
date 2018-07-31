(require 'cl)

(defun dwim-cycle-marks ()
  (interactive)
  ;; it's almost like (set-mark-command t) but that screws up marks when C-x C-x is used meanwhile
  (if nil
      (exchange-point-and-mark (not current-prefix-arg)) ; this branch preserves the current point
      (progn
        (when (and (first mark-ring)
                   (equal (point) (marker-position (first mark-ring))))
          (pop-mark)
          (set-mark (car mark-ring)))
        (goto-char (or (mark) (point)))))
  (pop-mark)
  (set-mark (car mark-ring))
  (deactivate-mark))

(defun dwim-mark/push-point ()
  (interactive)
  (set-mark (point))
  (push-mark)
  (deactivate-mark))

(defun dwim-clear-mark-ring ()
  (interactive)
  (setq mark-ring nil)
  (when current-prefix-arg
    (setq global-mark-ring nil))
  (set-mark (point))
  (deactivate-mark))

(defun dwim-list-buffers ()
  "List buffers negating the meaning of the prefixing *and* switch to the buffer list window."
  (interactive)
  (list-buffers (not current-prefix-arg))
  (let* ((buffer (get-buffer "*Buffer List*"))
         (window (get-buffer-window buffer)))
    (when window
      (select-window window)
      (set-window-point (get-buffer-window) 0)))
  ;;(isearch-forward)
  )

(defun dwim-window-deletable-p (window)
  "Return t if WINDOW is deletable, meaning that WINDOW is alive
and not a minibuffer's window, plus there is two or more windows."
  (and (window-live-p window)
       (not (window-minibuffer-p window))
       (not (one-window-p))))

(defun dwim-kill-this-buffer-and-window ()
  (interactive)
  (kill-this-buffer)
  (when (dwim-window-deletable-p (get-buffer-window (current-buffer)))
    (delete-window)))

(defun dwim-query-replace-read-to-arg (prompt from regexp-flag &optional noerror)
  (unless noerror
    (barf-if-buffer-read-only))
  (let ((to (if (consp from)
                (prog1
                    (cdr from)
                  (setq from (car from)))
                (query-replace-read-to from prompt regexp-flag))))
    (list from to current-prefix-arg)))

(defun dwim-query-replace-current-sexp (from-string to-string &optional delimited start end)
  (interactive (let ((common (dwim-query-replace-read-to-arg
                              (if (and transient-mark-mode mark-active)
                                  "Query replace in region"
                                  "Query replace")
                              (slime-sexp-at-point)
                              nil)))
		 (list (nth 0 common) (nth 1 common) (nth 2 common)
		       ;; These are done separately here
		       ;; so that command-history will record these expressions
		       ;; rather than the values they had this time.
		       (if (and transient-mark-mode mark-active)
			   (region-beginning))
		       (if (and transient-mark-mode mark-active)
			   (region-end)))))
  (beginning-of-defun)
  (perform-replace from-string to-string t nil delimited nil nil start end))

;; map these xemacs functions to their equivalents in gnu emacs
(unless (fboundp 'copy-primary-selection)
  (defun copy-primary-selection (beg end)
    (interactive "r")
    (if (region-active-p)
        (progn
          (let ((x-select-enable-clipboard t))
            (x-select-text (buffer-substring-no-properties beg end)))
          (deactivate-mark))
        (error "Region is not active"))
    ;;(call-interactively 'cua-copy-region)
    ))

(unless (fboundp 'kill-primary-selection)
  (defun kill-primary-selection (beg end)
    (interactive "r")
    (let ((x-select-enable-clipboard t))
      (x-select-text (buffer-substring-no-properties beg end)))
    (delete-region (point) (mark))
    (deactivate-mark)
    ;;(call-interactively 'cua-cut-region)
    ))

(unless (fboundp 'yank-clipboard-selection)
  (defun yank-clipboard-selection ()
    (interactive)
    (when mark-active
      (delete-region (point) (mark))
      (deactivate-mark))
    (let ((x-select-enable-clipboard t))
      (insert (or (x-cut-buffer-or-selection-value)
                  x-last-selected-text-clipboard)))
    ;;(x-clipboard-yank)
    ;;(call-interactively 'cua-paste)
    ))

(unless (fboundp 'switch-to-other-buffer)
  (defun switch-to-other-buffer ()
    "Switch to other-buffer in current window"
    (interactive)
    (switch-to-buffer (other-buffer))))

;; paste the last killed stuff and drop it from the kill-ring
;; killing and yank behaves as a stack this way
(defun dwim-insert-and-pop-kill-ring-head ()
  (interactive)
  (let ((str (pop kill-ring))
        (point-backup))
    (when str
      (when (char-equal (elt str (1- (length str))) ?\n)
        (setf point-backup (point))
        (beginning-of-line))
      (insert str)
      (when point-backup
        (goto-char point-backup)))))

(defun dwim-switch-between-target-buffer-and-last-buffer (target-buffer)
  (if (and target-buffer
           (not (zerop (buffer-size target-buffer))))
      (if (eq (current-buffer) target-buffer)
          (command-execute 'switch-to-other-buffer) ;or (switch-to-buffer (slime-recently-visited-buffer 'lisp-mode))
          (switch-to-buffer target-buffer))
      (message "No such buffer is available, or it's empty...")))

(defun dwim-switch-between-slime-inspector-and-last-buffer ()
  (interactive)
  (dwim-switch-between-target-buffer-and-last-buffer (slime-inspector-buffer)))

(defun dwim-switch-between-slime-repl-and-last-buffer ()
  (interactive)
  (with-current-buffer
      (dwim-switch-between-target-buffer-and-last-buffer (slime-output-buffer))
    ;; disable jumping to the input area
    ;;(when (and (eq (current-buffer) (slime-output-buffer))
    ;;           (< (point) slime-repl-prompt-start-mark))
    ;;  (goto-char (point-max)))
    ))

(defun dwim-restart-inferior-lisp ()
  (interactive)
  (slime-restart-inferior-lisp)
  (slime-repl-clear-buffer))

(defun dwim-switch-between-slime-inferior-lisp-and-last-buffer ()
  (interactive)
  (dwim-switch-between-target-buffer-and-last-buffer (process-buffer (slime-process))))

(defun dwim-switch-between-slime-sprof-and-last-buffer ()
  (interactive)
  (let ((target-buffer (get-buffer (slime-buffer-name :sprof))))
    (cond
      ((and target-buffer
            (eq (current-buffer) target-buffer))
       (dwim-switch-between-target-buffer-and-last-buffer target-buffer))
      (target-buffer
       (switch-to-buffer target-buffer))
      (t
       (slime-sprof-browser)))))

(defun dwim-switch-between-slime-selector-and-last-buffer (buffer-name slime-selector-key)
  (interactive)
  (let ((target-buffer (get-buffer buffer-name)))
    (if (and target-buffer
             (eq (current-buffer) target-buffer))
        (dwim-switch-between-target-buffer-and-last-buffer target-buffer)
        (progn
          (slime-selector slime-selector-key nil t)
          (switch-to-buffer (get-buffer buffer-name))))))

(defun dwim-switch-between-slime-thread-list-and-last-buffer ()
  (interactive)
  (dwim-switch-between-slime-selector-and-last-buffer slime-threads-buffer-name ?t))

(defun dwim-switch-between-slime-connection-list-and-last-buffer ()
  (interactive)
  (dwim-switch-between-slime-selector-and-last-buffer slime-connections-buffer-name ?c))

(defun dwim-switch-between-elisp-repl-and-last-buffer ()
  (interactive)
  (if (eq major-mode 'inferior-emacs-lisp-mode)
      (command-execute 'switch-to-other-buffer) ;or (switch-to-buffer (slime-recently-visited-buffer 'lisp-mode))
      (ielm)))

(defun dwim-switch-between-debug-and-last-source-buffer ()
  (interactive)
  (let ((debug-buffer (progn
                        (unless (sldb-get-default-buffer)
                          (error "No debugger buffer"))
                        (sldb-get-default-buffer))))
    (if (eq (current-buffer) debug-buffer)
        (switch-to-buffer (slime-recently-visited-buffer 'lisp-mode))
        (switch-to-buffer debug-buffer))))

;;;;;;
;;; editing functions

(defun dwim-duplicate-line-at-point ()
  (interactive)
  (let ((old-point (point))
        (line-start)
        (line-end))
    (if (and transient-mark-mode
             mark-active)
        (let ((region-beginning (region-beginning))
              (region-end (region-end)))
          (goto-char region-beginning)
          (beginning-of-line)
          (setf line-start (point))
          (goto-char region-end)
          (if (bolp)
              (backward-char)
              (end-of-line))
          (setf line-end (1+ (point)))
          (deactivate-mark))
        (progn
          (end-of-line)
          (setf line-end (1+ (point)))
          (beginning-of-line)
          (setf line-start (point))))
    (next-line 1)
    (beginning-of-line)
    (insert-buffer-substring (current-buffer) line-start line-end)
    (if (> (point) old-point)
        (progn
          (goto-char old-point)
          (next-line 1)))))

(defun dwim-whitespacep (char)
  (or (char-equal char ?\ )
      (char-equal char ?\t)
      (char-equal char ?\n)
      (char-equal char ?\^L)))

(defun dwim-forward-kill-whitespaces ()
  (interactive)
  (dwim-kill-whitespaces% :backward nil))

(defun dwim-backward-kill-whitespaces ()
  (interactive)
  (dwim-kill-whitespaces% :forward nil))

(defun dwim-kill-whitespaces ()
  (interactive)
  (dwim-kill-whitespaces%))

(defun* dwim-whitespace-bounds-at-point (&key (backward t) (forward t))
  (when (or (dwim-whitespacep (following-char))
            (dwim-whitespacep (preceding-char)))
    (let ((start (point))
          (end (point)))
      (when backward
        (while (slime-point-moves-p
                 (skip-syntax-backward " ")
                 (when (bolp) (backward-char))))
        (setf start (point)))
      (when forward
        (while (slime-point-moves-p
                 (skip-syntax-forward " ")
                 (when (eolp) (forward-char))))
        (setf end (point)))
      (when (< start end)
        (list start end)))))

(defun* dwim-kill-whitespaces% (&key (backward t) (forward t))
 "Delete whitespaces (controlled by forwardp and backwardp) at position
  and insert a space if not looking-at a paren."
 (let ((bounds (dwim-whitespace-bounds-at-point :backward backward :forward forward)))
   (when bounds
     (destructuring-bind (start end) bounds
       (delete-region start end)
       t))))

(defun dwim-kill-line (&optional count)
  "Kills a line, tries to keep the caret at the same position."
  (interactive)
  (let ((old-point (point))
        start
        end)
    (beginning-of-line)
    (setf start (point))
    (if count
        (forward-line count)
        (if (eobp)
            (signal 'end-of-buffer nil))
        (forward-line 1))
    (setf end (point))
    ;; do not store empty lines in the kill ring
    ;;(goto-char old-point)
    ;;(beginning-of-line)
    ;;(if (looking-at "[^	 ]*$")
    ;;    (delete-region start end)
    ;;    (kill-region start end))
    (kill-region start end)
    (end-of-line)
    (if (> (point) old-point)
        (goto-char old-point))))

(defun dwim-kill-whitespaces-or-insert-newline ()
  (interactive)
  (let ((bounds (dwim-whitespace-bounds-at-point)))
    (if bounds
        (destructuring-bind (start end) bounds
          (if (string-match "\n" (buffer-substring start end))
              (progn
                (delete-region start end)
                (when (and (not (char-equal (following-char) ?\) ))
                           (not (char-equal (preceding-char) ?\( )))
                  (insert-string " ")))
              (paredit-newline)))
        (message "No whitespaces at point"))))

(defun dwim-kill-xml-tag ()
  (interactive)
  (let ((original-point (point))
        (start (point))
        (end (progn
               (nxml-forward-element)
               (point))))
    (delete-region start end)
    (when (looking-at "\n")
      (dwim-kill-line))
    (goto-char original-point)))

;; TODO think through this defaulting... maybe an &key (at '(:repl :cl :scheme :elisp ...))
(defun* dwim-define-lisp-key (key binding &key (repl t) (buffers t) (cl t) (elisp t) (scheme t))
  (when buffers
    (when cl
      (define-key (cond
                    ((boundp 'slime-editing-map)
                     slime-editing-map)
                    ((boundp 'slime-parent-map)
                     slime-parent-map)
                    (t slime-mode-map))
          key binding))
    (when elisp
      (define-key emacs-lisp-mode-map key binding))
    (when (and scheme (boundp 'scheme-mode-map))
      (define-key scheme-mode-map key binding)))
  (when repl
    (when cl
      (define-key slime-repl-mode-map key binding))
    (when (and elisp (boundp 'ielm-map))
      (define-key ielm-map key binding))))

(defun dwim-copy-sexp-at-point ()
  (interactive)
  (let ((sexp (slime-sexp-at-point)))
    (if (featurep 'xemacs)
        (own-clipboard sexp)
        (let ((x-select-enable-clipboard t))
          (x-select-text sexp)))))

(defun dwim-copy-word-at-point ()
  (interactive)
  (let ((word (word-at-point)))
    (if (featurep 'xemacs)
        (own-clipboard word)
        (let ((x-select-enable-clipboard t))
          (x-select-text word)))))

(defun dwim-findr-search-sexp (files dir)
  (interactive (list (findr-read-file-regexp)
                     (findr-read-starting-directory)))
  (let ((sexp (slime-sexp-at-point)))
    (findr-search (regexp-quote sexp) files dir)))

(defun dwim-replace-sexp-at-point-with-clipboard ()
  (interactive)
  (let ((point (point))) ; hrm, save-excursion somehow doesn't work here
    (unless (hu.dwim.quasi-quote:after-sexp-separator-p)
      (beginning-of-thing (if (slime-symbol-at-point)
                              'slime-symbol
                              'sexp)))
    (yank-clipboard-selection)
    (kill-sexp)
    (goto-char point)))

(defun dwim-replace-word-at-point-with-clipboard ()
  (interactive)
  (let ((point (point))) ; hrm, save-excursion somehow doesn't work here
    (beginning-of-thing 'word)
    (yank-clipboard-selection)
    (kill-word 1)
    (goto-char point)))

(defun dwim-paste-clipboard-at-last-change ()
  (interactive)
  (goto-last-change-with-auto-marks)
  (yank-clipboard-selection))

(defun dwim-describe-face-at-point ()
  "Return face used at point."
  (interactive)
  (let ((face (get-char-property (point) 'face)))
    (if (listp face)
        (progn
          (message (format "Full value: %s" (prin1-to-string face)))
          (setq face (car face))))
    (if (featurep 'xemacs)
        (hyper-describe-face face)
        (describe-face face))))

;;;
;;; Convert source files in input-dir recursively to *.html in output-dir with the same dir structure
;;;
(defun* dwim-htmlize-source-files (input-dir output-dir &key
                                             (file-name-regexp "\\(\.lisp\\|\\.tal\\|.css\\)$")
                                             file-path-exclusion-regexp
                                             file-path-inclusion-regexp)
  (require 'htmlize)
  (setf input-dir (file-name-as-directory (file-truename input-dir)))
  (setf output-dir (file-name-as-directory (file-truename output-dir)))
  ;;(insert "input-dir: " input-dir ", output-dir: " output-dir "\n")
  (let ((files (findr file-name-regexp input-dir)))
    (unless (file-exists-p output-dir)
      (make-directory output-dir t))
    (dolist (file files)
      (when (and (= (mismatch file input-dir)
                    (length input-dir))
                 (or (not file-path-exclusion-regexp)
                     (not (string-match file-path-exclusion-regexp file))
                     (and file-path-inclusion-regexp
                          (string-match file-path-inclusion-regexp file))))
        (let* ((file-path (file-name-directory file))
               (file-relative-path (subseq file-path
                                           (length input-dir)
                                           (length file-path)))
               (file-output-dir (concatenate 'string
                                             output-dir
                                             file-relative-path)))
          ;;(insert "file-path: " file-path ", file-relative-path: " file-relative-path
          ;;        ",file-output-dir: " file-output-dir "\n")
          (dwim-htmlize-source-file file file-output-dir))))))

(defun dwim-htmlize-source-file (input-file output-dir)
  (setf output-dir (file-name-as-directory output-dir))
  (unless (file-exists-p output-dir)
    (make-directory output-dir t))
  (with-current-buffer (find-file-noselect input-file)
    (with-current-buffer (htmlize-buffer)
      (set-visited-file-name
       (concatenate 'string
                    output-dir
                    ;;(file-name-sans-extension
                    ;; (file-name-nondirectory file))
                    (file-name-nondirectory file)
                    ".html")
       t)
      (save-buffer)
      (kill-buffer nil))))

(provide 'dwim-util)
