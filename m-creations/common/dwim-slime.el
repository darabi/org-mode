;;;
;;; This file contains parts of the Emacs functions in hu.dwim.environment
;;;
;;; http://dwim.hu/live/hu.dwim.environment/emacs
;;;
;;; which is licensed as Public Domain Software, or alternatively
;;; under the MIT licence.

(require 'dwim-util)
(require 'slime)
(require 'slime-repl)

(defun add-emacs.d-swank-to-asdf-source-registry ()
  (let ((conf "~/.config/common-lisp/source-registry.conf.d/10-m-creations-emacs.d-swank.conf"))
    (mkdir (file-name-directory conf) t)
    (write-region
     "
;; AUTOMATICALLY GENERATED FILE - DO NOT EDIT
;; (cf. ~/.emacs.d/site-lisp/m-creations/common/dwim-slime.el)
;;
;; This file configures ASDF to use the slime/swank version
;; which is managed in the m-creations emacs repo

 (:tree (:home \".emacs.d/site-lisp/slime\"))
" nil conf)))

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
  `(("bash" ,(or (ignore-errors dwim-sbcl-binary)
                 (expand-file-name (concat dwim-common-lisp-workspace "/sbcl/run-sbcl.sh")))
            ;; "--no-userinit"
            "--dynamic-space-size" "1024"
            )
    :init dwim/slime-init-command))

(defun dwim/build-sbcl-lisp-implementation-entry/image (&optional image-name)
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
                         (expand-file-name (read-file-name "Core file: " cache-root nil t)))))
    (when (> (length cache-directories) 1)
      (warn "There are multiple sbcl directories in '~/.cache/common-lisp/'! Using '%s'." cache-directory))
    `((,image-path "--no-userinit" )
      :init dwim/slime-init-command)))

(defun dwim/slime-init-command (port-filename coding-system)
  (let ((loader (if (file-name-absolute-p slime-backend)
                    slime-backend
                    (concat slime-path slime-backend))))
    ;; Return a single form to avoid problems with buffered input.
    (format "%S\n\n"
            `(progn
               (require :asdf)
               (funcall (read-from-string "asdf:load-system") :swank)
               (funcall (read-from-string "swank:swank-require")
                        (append
                         '(:swank-asdf
                           :swank-fancy-inspector
                           :swank-fuzzy
                           :swank-presentations
                           :swank-clipboard
                           :swank-sprof
                           :swank-repl
                           :swank-c-p-c)
                         (when (find :sbcl *features*)
                           '(:swank-sbcl-exts))))
               (funcall (read-from-string "swank:start-server")
                        ,(slime-to-lisp-filename port-filename))))))

(provide 'dwim-slime)
