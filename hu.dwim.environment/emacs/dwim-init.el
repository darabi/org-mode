(unless (or dwim-workspace
	    (and (setq dwim-workspace (or (getenv "DWIM_WORKSPACE")
					  (expand-file-name "~/workspace")))
		 (file-exists-p dwim-workspace)))
  (error "Could not find workspace directory (tried '%s'). Set/change the shell environment variable DWIM_WORKSPACE if you want to use something else." dwim-workspace))

(add-to-list 'load-path (expand-file-name (concat dwim-workspace "/hu.dwim.environment/emacs/")))
(add-to-list 'load-path (expand-file-name (concat dwim-workspace "/hu.dwim.def/emacs/")))
(add-to-list 'load-path (expand-file-name (concat dwim-workspace "/hu.dwim.syntax-sugar/emacs/")))
(add-to-list 'load-path (expand-file-name (concat dwim-workspace "/hu.dwim.quasi-quote/emacs/")))
(add-to-list 'load-path (expand-file-name (concat dwim-workspace "/hu.dwim.logger/emacs/")))
(let ((swank-directory (or (getenv "DWIM_SWANK_DIRECTORY")
                           "slime/")))
  (add-to-list 'load-path (expand-file-name (concat dwim-workspace "/" swank-directory))))

(require 'dwim-util)

(require 'cl)
(require 'darcsum)
(require 'redo)                ; additional alternative undo behaviour
(require 'ielm)
(require 'dired)
(require 'tramp)
(require 'nxml-mode)
(require 'whitespace)
(require 'goto-last-change)
(require 'hu.dwim.def)
(require 'hu.dwim.quasi-quote)
(require 'hu.dwim.syntax-sugar)
(require 'hu.dwim.logger)
(require 'paredit)
(require 'swbuff)
(require 'findr)
(require 'saveplace)
(require 'slime)
(require 'scheme)
(require 'smooth-scrolling)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(modify-syntax-entry ?- "w") ; now '-' is not considered a word-delimiter

(setq smooth-scroll-margin 5)

(add-hook 'css-mode-hook
          (defun dwim/css-mode-hook ()
            (setq css-indent-offset 2)))

(push '("<\\?xml " . nxml-mode) magic-mode-alist)

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;(setq debug-on-error t)

;; (setq file-coding-system-alist
;;       (append (list
;;                '("\\.patch" . binary)
;;                '("\\.bbdb" . iso-2022-8)
;;                '("\\.lisp" . utf-8)
;;                '("\\.asd" . utf-8)
;;                ;; I would like to open most files in utf-8 mode by default,
;;                ;; unfortunately this is not usable for XEmacs because
;;                ;; file-coding-system-alist overrides
;;                ;; -*- coding: foo -*- cookies in files
;;                ;;
;;                ;; and all the rest is utf-8:
;;                ;;'("" . utf-8)
;;                )
;;               file-coding-system-alist))

;;(setq browse-url-firefox-program "/path/to/fire-fox")
(setq browse-url-browser-function 'browse-url-firefox)


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

(ido-mode 1)

;;;;;;
;;; Slime setup

(defun dwim/build-sbcl-lisp-implementation-entries/from-installed-binaries (base-path)
  (let* ((regexp "^sbcl-\\([0-9]+\\).\\([0-9]+\\).\\([0-9]+\\).*-linux$")
         (dirs (directory-files base-path nil regexp))
         (result '()))
    (dolist (dir dirs)
      (assert (string-match regexp dir))
      (let ((v1 (match-string 1 dir))
            (v2 (match-string 2 dir))
            (v3 (match-string 3 dir)))
        (pushnew `(,(intern (concat "sbcl-" v1 "." v2 "." v3)) ("bash" ,(concat base-path dir "/run-sbcl.sh")) :init dwim/slime-init-command)
                 result)))
    result))

(defun dwim/build-sbcl-lisp-implementation-entry ()
  "Creates a command line for starting SBCL using run-sbcl.sh"
  `(("bash" ,(expand-file-name (concat dwim-workspace "/sbcl/run-sbcl.sh"))
            ;; "--no-userinit"
            )
    :init dwim/slime-init-command))

(defun dwim/build-sbcl-lisp-implementation-entry/image (&optional image-name &rest args)
  "Creates a command line for starting sbcl with run-sbcl"
  (let* ((cache-dir-name "~/.cache/common-lisp/")
         (cache-root (progn
                       (make-directory cache-dir-name t)
                       (expand-file-name cache-dir-name)))
         (cache-directories (sort (copy-list (file-name-all-completions "sbcl-" cache-root))
                                  'string<))
         (cache-directory (car (last cache-directories)))
         (image-path (if image-name
                         (if (file-exists-p image-name)
                             image-name
                             (concatenate 'string cache-root cache-directory image-name))
                         (expand-file-name (read-file-name "Core file: " cache-root nil t))))
         (program-and-args (list image-path)))
    (dolist (arg args)
      (append program-and-args arg))
    (when (> (length cache-directories) 1)
      (warn "There are multiple sbcl directories in '~/.cache/common-lisp/'! Using '%s'." cache-directory))
    `(,program-and-args
      :init dwim/slime-init-command)))

(defun dwim/slime-init-command (port-filename coding-system)
  (let ((loader (if (file-name-absolute-p slime-backend)
                    slime-backend
                    (concat slime-path slime-backend))))
    ;; Return a single form to avoid problems with buffered input.
    (format "%S\n\n"
            `(progn
               (require :asdf)
               (load ,(slime-to-lisp-filename (expand-file-name loader))
                     :verbose t)
               (funcall (read-from-string "swank-loader:init"))
               (funcall (read-from-string "swank:swank-require")
                        (append
                         '(:swank-asdf
                           :swank-fancy-inspector
                           :swank-fuzzy
                           :swank-indentation
                           :swank-presentations
                           :swank-clipboard
                           :swank-sprof
                           :swank-repl
                           :swank-c-p-c)
                         (when (find :sbcl *features*)
                           '(:swank-sbcl-exts))))
               (funcall (read-from-string "swank:start-server")
                        ,(slime-to-lisp-filename port-filename))))))

(eval-after-load 'slime
  '(progn
    (if (functionp 'slime-require)
        (slime-setup '(slime-asdf
                       slime-sbcl-exts
                       slime-fancy
                       slime-tramp
                       slime-sprof))
        (slime-setup :autodoc t))
    (define-key slime-macroexpansion-minor-mode-map [remap undo] 'slime-macroexpand-undo)
    (setq lisp-simple-loop-indentation 2
          lisp-loop-keyword-indentation 6
          lisp-loop-forms-indentation 6)
    (setq inferior-lisp-program "sbcl"
          slime-default-lisp 'sbcl ; to select which lisp to start by default
          slime-fuzzy-explanation ""
          slime-protocol-version 'ignore ; avoid annoying message when swank and slime dates don't exaclty match
          sldb-initial-restart-limit 15
          slime-complete-symbol*-fancy t
          slime-repl-history-remove-duplicates t
          lisp-indent-function 'common-lisp-indent-function
          slime-complete-symbol-function 'slime-fuzzy-complete-symbol
          slime-startup-animation nil
          ;; instead: "sudo apt-get install hyperspec" common-lisp-hyperspec-root "file:///usr/share/doc/clhs-7.0/HyperSpec/"
          slime-autodoc-use-multiline-p t
          slime-net-coding-system 'utf-8-unix
          slime-enable-evaluate-in-emacs t
          slime-outline-mode-in-events-buffer t)
    (setq slime-lisp-implementations
     `((installed-sbcl ("sbcl"))
       (installed-alisp ("alisp"))
       (sbcl dwim/build-sbcl-lisp-implementation-entry)
       (sbcl-image dwim/build-sbcl-lisp-implementation-entry/image)
       (sbcl-hu.dwim-development ,(lambda ()
                                    (dwim/build-sbcl-lisp-implementation-entry/image "hu.dwim_development")))
       (sbcl-hu.dwim.rdbms-development ,(lambda ()
                                          (dwim/build-sbcl-lisp-implementation-entry/image "hu.dwim.rdbms_development")))
       (sbcl-nafi.development-image ,(lambda ()
                                       (dwim/build-sbcl-lisp-implementation-entry/image "nafi_development")))
       ;; -K is only the terminal encoding, you may also need this in in .ccl-init.lisp: (setf ccl:*default-external-format* (make-external-format :character-encoding :utf-8 :line-termination :unix))
       (ccl ("ccl" "-K" "utf-8"))
       (mkcl ("mkcl"))
       ;; for reference: "clisp -ansi -q -K full -m 32M -I -Efile ISO-8859-15 -Epathname ISO-8859-1 -Eterminal UTF-8 -Emisc UTF-8 -Eforeign ISO-8859-1"
       (clisp ("clisp"
               "-i"
               ,(concat dwim-workspace "/hu.dwim.environment/source/environment.lisp")))))))

(add-to-list 'auto-mode-alist '("\\.xcvb\\'" . (lisp-mode font-lock-mode slime-mode)))
(add-to-list 'auto-mode-alist '("\\.asd\\'" . (lisp-mode font-lock-mode slime-mode)))

(defface font-lock-todo-face
   '((((class color) (background light)) (:foreground "Red" :weight bold)))
  "Face for the lambda character."
  :group 'font-lock-faces)

(defun dwim-shared-lisp-mode-hook ()
  ;; for M-q in comments
  ;;(setq fill-column 100) set in custom.el, it only gets buffer local this way

  (font-lock-add-keywords
   'lisp-mode
   `(("[^-]\\(FIXME\\|TODO\\|KLUDGE\\|QUESTION\\|WARNING\\|#[+-]debug\\)" 1 'font-lock-todo-face t)
     ("[(:]\\(in-package\\|in-suite\\|not-yet-implemented\\|ignore-errors\\|read-from-string\\|eval\\|production-only.?\\|break[^)]?\\|break/inspect.?\\|break/print.?\\|print\\)[ 	\n()]" 1 'font-lock-todo-face t)
     ("[(:]\\(debug-only.? .*\\))" 1 'font-lock-logger-expression-face t)
     ("debug-only.?" 0 'font-lock-logger-expression-face t)
     ("\\*debug-io\\*\\|\\*trace-output\\*" 0 'font-lock-todo-face t)
     (,(concatenate 'string "[ 	\n()]" (regexp-opt '("nil" "t" "#t" "#f" "true" "false" "undefined" "it") t)
                    ;; TODO fix #t #t #t alternating
                    "[ 	\n()]")
       1 font-lock-constant-face)
     (,(concatenate 'string "([ 	\n]*" (regexp-opt '("and" "or" "not" "xor") t) "[ 	\n()]")
       1 font-lock-builtin-face)
     ;;(,(concatenate 'string "(" (regexp-opt '("action") t) "[ 	\n()]")
     ;; 1 font-lock-builtin-face)
     ("(\\(<[^ 	\n()]*:[^ 	\n()]+\\)[ 	\n()]" 1 font-lock-preprocessor-face)
     ("[^ 	]*(\\(def\\([^ 	]\\|\\(const\\(\\|ant\\)\\|ine-key\\(\\|-after\\)\\|var\\|parameter\\|custom\\)\\|\\(suite.?\\|[^ 	\n()]*?class.?\\|entry-point\\|layer.?\\|ine-condition\\|condition.?\\|component.?\\|struct\\|type\\|view\\|renderer\\|ine-form\\|ine-syntax-node\\|ine-dynamic-context.?\\)\\|\\([^ 	\n()]+\\)\\)\\)\\>[ 	'(]*\\([^ 	\n()]+\\)?"
      (1 font-lock-keyword-face)
      (8
       (cond
         ((match-beginning 3)
          'font-lock-variable-name-face)
         ((match-beginning 6)
          'font-lock-type-face)
         (t
          'font-lock-function-name-face))
       nil t))
     (,(concatenate 'string "[(']\\(" (regexp-opt '("iter" "bind" "aif" "if-bind" "awhen" "when-bind"
                                                    "while" "until" "aprog1" "prog1-bind" "named-lambda"
                                                    "values" "append" "list" "list*" "unwind-protect-case" "setf" "setq"
                                                    "null" "apply" "funcall" "cons"
                                                    ) t)
                     "\\)[ 	\n()]")
       1 font-lock-keyword-face)
;;;       (,(concatenate 'string "[(']\\(" (regexp-opt '("progn") t)
;;;                      "\\)[ 	\n()]")
;;;         1 font-lock-comment-face)
     (,(concatenate 'string "[(']\\(" (regexp-opt '("catch" "throw" "return" "next-iteration" "call-next-method" "call-next-layered-method" "is" "signals" "not-signals" "finishes"
                                                    "handle-otherwise" "handle-otherwise*" "delay" "delay*" "force") t)
                    "\\|with.*?-lock.*?\\|recurse.*?\\)[ 	\n()]")
       1 font-lock-builtin-face t)
     ("(\\(block\\|return-from\\)[ 	\n()]+\\(.*?\\)[ 	\n()]"
      (1 font-lock-builtin-face)
      (2 font-lock-function-name-face nil t))
     (,(concatenate 'string "[ 	\n()]\\(" (regexp-opt '("this" "self") t) "\\)[ 	\n()]")
       1 font-lock-builtin-face t)
     (,(concatenate 'string "[ 	\n()]\\(-[-/a-zA-Z0-9]+-\\)") ;; -foo-
       1 font-lock-preprocessor-face t)
     ("\\<:\\sw+\\>" 0 font-lock-keyword-face prepend)))

  (let ((overrides
         '((:default-initargs (&rest))
           (define-backend-method defmethod)
           (define-layered-function defgeneric)
           (defmethod/cc defmethod)
           (defgeneric/cc defgeneric)
           (define-layered-method defmethod)
           (define-application-method defmethod)
           (define-form-method defmethod)
           (define-dynamic-context defclass)
           (defclass* defclass)
           (defcondition* defcondition)
           (def (4 4 (&whole 4 &rest 2) &body))
           (defresources (4 &rest (&whole 2 &lambda &body)))
           (make-xml-element (4 &lambda &body)))))
    (dolist (el overrides)
      (put (first el) 'common-lisp-indent-function
           (if (symbolp (second el))
               (get (second el) 'common-lisp-indent-function)
               (second el))))))

;(defun ebr42-connect-to-all-nodes ()
;  (interactive)
;  (loop for port upfrom 6001 below 6010 do
;        (slime-connect "localhost" port)))

(defun dwim-slime-compile-defun ()
  (interactive)
  (if current-prefix-arg
      (if (y-or-n-p (format "Compile defun in all %d connections? " (length slime-net-processes)))
          (progn
            (dolist (connection slime-net-processes)
              (let ((slime-dispatching-connection connection))
                (apply #'slime-compile-region (slime-region-for-defun-at-point)))
              (sleep-for 2))
            (message "Mass compilation finished."))
          (message "Mass compilation aborted."))
      (call-interactively 'slime-compile-defun)))

(setq-default
 findr-file-name-regexp-history (list "\\(\\.lisp\\|\\.asd\\|\\.el\\|\\.c\\|\\.h\\|\\.cpp\\|\\.tal\\|\\.css\\|\\.sh\\|\\.lua\\|\\.py\\)$")
 findr-skip-directory-regexp "\\(/\\.backups$\\|/_darcs$\\|/\\.git$\\|/CVS$\\|/\\.svn$\\)\\|/www$\\|/wwwroot$"

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
                                (".*truecrypt.*" . nil)
                                ("." . "~/.backups/"))
 tramp-auto-save-directory "~/.backups/autosaves/"

 shift-select-mode t

 enable-local-variables :safe
 font-lock-maximum-size 500000

 unshifted-motion-keys-deselect-region t
 x-select-enable-clipboard t
 interprogram-cut-function nil ; so that kill does not clobber the clipboard
 )

(delete-selection-mode t)
(tool-bar-mode 0)

(add-hook 'global-whitespace-mode-hook
          (defun dwim/global-whitespace-mode-hook ()
            (setq whitespace-global-modes '(ada-mode asm-mode autoconf-mode awk-mode c-mode c++-mode cc-mode change-log-mode cperl-mode electric-nroff-mode emacs-lisp-mode f90-mode fortran-mode html-mode html3-mode java-mode jde-mode ksh-mode latex-mode LaTeX-mode lisp-mode m4-mode makefile-mode modula-2-mode nroff-mode objc-mode pascal-mode perl-mode prolog-mode python-mode scheme-mode sgml-mode sh-mode shell-script-mode simula-mode tcl-mode tex-mode texinfo-mode vrml-mode xml-mode lisp-mode)
                  whitespace-indent-tabs-mode nil
                  whitespace-action '(auto-cleanup)
                  whitespace-style '(trailing empty tabs indentation)
                  show-trailing-whitespace t)))

(global-whitespace-mode t)

(add-hook 'show-paren-mode-hook
          (defun dwim/show-paren-mode-hook ()
            (setq show-paren-priority -1)
            (setq show-paren-style 'expression)))

(show-paren-mode t)

(custom-set-faces
 '(secondary-selection ((t (:background "#ffffd0")))) ; this makes the slime C-c C-c highlight less irritating
 '(swbuff-current-buffer-face ((t (:foreground "red" :bold t :underline t))))
 '(trailing-whitespace ((((class color) (background light)) (:background "#fff0f0"))))
 '(whitespace-trailing ((((class color) (background light)) (:background "#fff0f0")))))

;; RELOAD FILES LAST EDITED
;(autoload 'save-current-configuration "revive" "Save status" t)
;(autoload 'resume "revive" "Resume Emacs" t)
;(autoload 'wipe "revive" "Wipe Emacs" t)

;(resume)

(if (featurep 'xemacs)
    (progn
      (require 'backup-dir)
      (setq bkup-backup-directory-info
            `((t . (,(expand-file-name "~/.backups/") ok-create full-path prepend-name)))))
    (setq backup-by-copying t           ; don't clobber symlinks
          backup-directory-alist '((".*encrypt.*" . nil)
                                   (".*truecrypt.*" . nil)
                                   ("." . "~/.backups"))
          delete-old-versions t
          kept-new-versions 5
          kept-old-versions 5
          version-control t))

(add-hook 'lisp-mode-hook 'dwim-shared-lisp-mode-hook)

;; disable it, because it makes output dog slow with presentations
;(add-hook 'slime-repl-mode-hook
;          (lambda ()
;            (setf parse-sexp-lookup-properties nil)))

(provide 'dwim-init)
