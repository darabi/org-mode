;;
;; Setup font lock keywords for lisp-mode
;;
;; (c) m-creations gmbh 2018
;;

(defface font-lock-todo-face
   '((((class color) (background light)) (:foreground "Red" :weight bold)))
  "Face for the lambda character."
  :group 'font-lock-faces)

(defun mc-lisp-font-lock-hook ()
  (font-lock-add-keywords
   'lisp-mode
   `(("[^-]\\(FIXME\\|TODO\\|KLUDGE\\|QUESTION\\|WARNING\\|#[+-]debug\\)" 1 'font-lock-todo-face t)
     ("[(:]\\(in-package\\|in-suite\\|not-yet-implemented\\|not-yet-implemented/crucial-api\\|ignore-errors\\|read-from-string\\|eval\\|production-only.?\\|break[^)]?\\|break/inspect.?\\|break/print.?\\|print\\)[ 	\n()]" 1 'font-lock-todo-face t)
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
                                                    "handle-otherwise" "handle-otherwise*" "delay" "delay*" "force" "nest") t)
                    "\\|with.*?-lock.*?\\|recurse.*?\\)[ 	\n()]")
       1 font-lock-builtin-face t)
     ("(\\(block\\|return-from\\)[ 	\n()]+\\(.*?\\)[ 	\n()]"
      (1 font-lock-builtin-face)
      (2 font-lock-function-name-face nil t))
     (,(concatenate 'string "[ 	\n()]\\(" (regexp-opt '("this" "self") t) "\\)[ 	\n()]")
       1 font-lock-builtin-face t)
     (,(concatenate 'string "[ 	\n()]\\(-[-/a-zA-Z0-9]+-\\)") ;; -foo-
       1 font-lock-preprocessor-face t)
     ("\\<:\\sw+\\>" 0 font-lock-keyword-face prepend))))

(add-hook 'lisp-mode-hook 'mc-lisp-font-lock-hook)

(provide 'mc-lisp-font-lock)
