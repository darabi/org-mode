;; Location: http://wttools.sourceforge.net/emacs-stuff/personal-funcs.html

;;    Artur Hefczyc sample, working .emacs file
;;    http://wttools.sf.net/
;;    (C) 2003, Artur Hefczyc
;;
;;    This library is free software; you can redistribute it and/or
;;    modify it under the terms of the GNU Lesser General Public
;;    License as published by the Free Software Foundation;
;;    version 2.1 of the License.
;;
;;    This library is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;    Lesser General Public License for more details.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    Some of stuuf stored here are created on my own and some
;;    were found on the Internet.
;;    I hope I don't break their authors permisions. If some one
;;    will find I do, please let me know.
;;    I also don't remember where I have found these functions and
;;    who is origin author.
;;    I will try to complete these lack if possible.
;;

(require 'color-theme)

;;; I prefer to delete whole line regardless where cursor is
;;; Define function doing this
;;; Author: Artur Hefczyc
(defun kill-whole-line (arg)
  "Kill whole line regardless where kursor is"
  (interactive "p")
  (move-to-column 0)
  (kill-line))

;;; Small but really nice and useful function inserting current
;;; date and time under cursor
;;; Is there any function doing this built-in emacs?
;;; Let me know if you know
;;; Author: Artur Hefczyc
(defun insert-current-time (arg)
  "Insert current time string at current position"
  (interactive "p")
  (insert (current-time-string)))

;;; Very useful function allowing to jump to matching parent
;;; Author: unknown yet
(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;;; This is my own emacs color theme definition.
(defun my-color-theme ()
  "Color theme by Artur Hefczyc, created 2003-04-30. Modified by Kambiz Darabi."
  (interactive)
  (if (eq 'x window-system)
      (color-theme-darabi-x)
    (color-theme-darabi-term)))


(defun color-theme-darabi-old ()
  "Color theme by Laurent Michel, created 2001-05-24.
Includes custom, fl, font-lock, gnus, message, widget."
  (interactive)
  (color-theme-install
   '(color-theme-darabi-old
     ((background-color . "grey15")
      (background-mode . dark)
      (border-color . "black")
      (cursor-color . "LightGoldenrod")
      (foreground-color . "limegreen")
      (mouse-color . "yellow"))
     (default
       ((t (:stipple nil :background "grey15" :foreground "limegreen"
                     :inverse-video nil :box nil :strike-through nil
                     :overline nil :underline nil :slant normal
                     :weight normal :height 80 :width normal
                     :family "outline-courier new"))))
     (border ((t (:background "black"))))
     (comint-highlight-input ((t (:bold t :weight bold))))
     (comint-highlight-prompt ((t (:foreground "cyan"))))
     (cursor ((t (:background "LightGoldenrod"))))
     (custom-button-face
      ((t (:background "lightgrey" :foreground "black"
                       :box (:line-width 2 :style released-button)))))
     (custom-button-pressed-face
      ((t (:background "lightgrey" :foreground "black"
                       :box (:line-width 2 :style pressed-button)))))
     (custom-changed-face ((t (:background "blue" :foreground "white"))))
     (custom-comment-face ((t (:background "dim gray"))))
     (custom-comment-tag-face ((t (:foreground "gray80"))))
     (custom-documentation-face ((t (nil))))
     (custom-face-tag-face
      ((t (:bold t :weight bold :height 1.2 :family "helv"))))
     (custom-group-tag-face
      ((t (:bold t :foreground "light blue"
                 :weight bold :height 1.2))))
     (custom-group-tag-face-1
      ((t (:bold t :foreground "pink" :weight bold
                 :height 1.2 :family "helv"))))
     (custom-invalid-face ((t (:background "red" :foreground "yellow"))))
     (custom-modified-face ((t (:background "blue" :foreground "white"))))
     (custom-rogue-face ((t (:background "black" :foreground "pink"))))
     (custom-saved-face ((t (:underline t))))
     (custom-set-face ((t (:background "gray15" :foreground "limegreen"))))
     (custom-state-face ((t (:foreground "lime green"))))
     (custom-variable-button-face ((t (:bold t :underline t :weight bold))))
     (custom-variable-tag-face
      ((t (:bold t :foreground "light blue"
                 :weight bold :height 1.2 :family "helv"))))
     (erc-action-face
      ((t (:bold t :background "gray15"
                 :foreground "lightyellow" :weight bold))))
     (erc-bold-face
      ((t (:bold t :background "gray15" :foreground "gray77" :weight bold))))
     (erc-current-nick-face
      ((t (:bold t :background "gray15" :foreground "lightblue" :weight bold))))
     (erc-dangerous-host-face ((t (:background "gray15" :foreground "red"))))
     (erc-default-face ((t (:background "Gray15" :foreground "gray77"))))
     (erc-direct-msg-face ((t (:background "gray15" :foreground "gray90"))))
     (erc-error-face ((t (:background "gray15" :foreground "red"))))
     (erc-fool-face ((t (:background "gray15" :foreground "gray40"))))
     (erc-input-face ((t (:background "gray15" :foreground "gray77"))))
     (erc-inverse-face ((t (:background "gray77" :foreground "Gray15"))))
     (erc-keyword-face
      ((t (:bold t :background "gray15"
                 :foreground "lightgreen" :weight bold))))
     (erc-nick-default-face
      ((t (:bold t :background "gray15" :foreground "Magenta" :weight bold))))
     (erc-nick-msg-face
      ((t (:bold t :background "gray15" :foreground "orange" :weight bold))))
     (erc-notice-face
      ((t (:bold t :background "gray15" :foreground "lightBlue" :weight bold))))
     (erc-pal-face
      ((t (:bold t :background "gray15" :foreground "white" :weight bold))))
     (erc-prompt-face
      ((t (:bold t :background "lightBlue2" :foreground "Black" :weight bold))))
     (erc-timestamp-face
      ((t (:bold t :background "gray15" :foreground "green" :weight bold))))
     (erc-underline-face
      ((t (:background "gray15" :foreground "gray77" :underline t))))
     (extra-whitespace-face ((t (:background "pale green"))))
     (fixed-pitch ((t (:family "courier"))))
     (font-lock-builtin-face ((t (:foreground "LightSlateBlue"))))
     (font-lock-comment-face ((t (:foreground "lightgray"))))
     (font-lock-constant-face ((t (:foreground "yellow"))))
     (font-lock-doc-face ((t (:foreground "LightSalmon"))))
     (font-lock-function-name-face ((t (:foreground "greenyellow"))))
     (font-lock-keyword-face ((t (:foreground "DeepSkyBlue"))))
     (font-lock-string-face ((t (:foreground "LightGoldenrodYellow"))))
     (font-lock-type-face ((t (:foreground "cyan"))))
     (font-lock-variable-name-face ((t (:foreground "lightcyan"))))
     (font-lock-warning-face ((t (:bold t :foreground "Pink" :weight bold))))
     (fringe ((t (:background "grey10"))))
     (header-line ((t (:background "grey20" :foreground "grey90" :box nil))))
     (highlight ((t (:background "darkolivegreen"))))
     (jde-bug-breakpoint-cursor ((t (:background "brown" :foreground "cyan"))))
     (jde-db-active-breakpoint-face ((t (:background "red" :foreground "black"))))
     (jde-db-requested-breakpoint-face
      ((t (:background "yellow" :foreground "black"))))
     (jde-db-spec-breakpoint-face ((t (:background "green" :foreground "black"))))
     (jde-java-font-lock-api-face ((t (:foreground "light goldenrod"))))
     (jde-java-font-lock-bold-face ((t (:bold t :weight bold))))
     (jde-java-font-lock-code-face ((t (nil))))
     (jde-java-font-lock-constant-face ((t (:foreground "Aquamarine"))))
     (jde-java-font-lock-doc-tag-face ((t (:foreground "light coral"))))
     (jde-java-font-lock-italic-face ((t (:italic t :slant italic))))
     (jde-java-font-lock-link-face
      ((t (:foreground "blue" :underline t :slant normal))))
     (jde-java-font-lock-modifier-face ((t (:foreground "LightSteelBlue"))))
     (jde-java-font-lock-number-face ((t (:foreground "LightSalmon"))))
     (jde-java-font-lock-operator-face ((t (:foreground "medium blue"))))
     (jde-java-font-lock-package-face ((t (:foreground "steelblue1"))))
     (jde-java-font-lock-pre-face ((t (nil))))
     (jde-java-font-lock-underline-face ((t (:underline t))))
     (menu ((t (nil))))
     (mode-line
      ((t (:background "grey75" :foreground "black"
                       :box (:line-width -1 :style released-button)))))
     (mouse ((t (:background "yellow"))))
     (region ((t (:background "blue3"))))
     (scroll-bar ((t (nil))))
     (secondary-selection ((t (:background "SkyBlue4"))))
     (ecb-default-highlight-face ((t (:background "gray40" :height 1.0))))

;;;      (default ((t (:stipple nil :background "dark slate grey" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :family "b&h-lucidatypewriter"))))           ;;;;;;;;;;;;;;
;;;      (gnus-header-content ((t (:foreground "lime green" :slant normal))))           ;;;;;;;;;;;;;;
;;;      (gnus-header-name ((((class color) (background dark)) (:foreground "spring green"))))           ;;;;;;;;;;;;;;
;;;      (gnus-header-subject ((((class color) (background dark)) (:foreground "bisk1" :weight bold))))           ;;;;;;;;;;;;;;
;;;      (message-header-name ((((class color) (background dark)) (:foreground "spring green"))))           ;;;;;;;;;;;;;;
;;;      (message-header-subject ((((class color) (background dark)) (:foreground "bisque1" :weight bold))))           ;;;;;;;;;;;;;;
;;;      (message-separator ((((class color) (background dark)) (:foreground "cadet blue"))))           ;;;;;;;;;;;;;;
;;;      (muse-header-1 ((t (:weight bold :height 1.4))))           ;;;;;;;;;;;;;;
;;;      (paren-face-match ((((class color)) (:background "green"))))           ;;;;;;;;;;;;;;
;;;      (paren-face-mismatch ((((class color)) (:foreground "white" :background "red"))))           ;;;;;;;;;;;;;;
;;;      (paren-match ((t (:background "green"))))           ;;;;;;;;;;;;;;
;;;      (paren-mismatch ((t (:background "red"))))           ;;;;;;;;;;;;;;
;;;      (region ((((class color) (min-colors 88) (background dark)) (:background "SlateBlue3"))))           ;;;;;;;;;;;;;;
;;;      (show-paren-match ((((class color)) (:background "green"))))           ;;;;;;;;;;;;;;
;;;      (show-paren-mismatch ((((class color)) (:background "red"))))           ;;;;;;;;;;;;;;
;;;      (ude-font-lock-face-10 ((t (:foreground "#a9aeed" :weight bold))))           ;;;;;;;;;;;;;;
;;;      (variable-pitch ((t nil)))))           ;;;;;;;;;;;;;;
     )))

(defun color-theme-darabi-x ()
  "Color theme for an Emacs running under X by Artur Hefczyc, created 2003-04-30."
  (interactive)
  (color-theme-install
   '(color-theme-darabi-x
     ((background-color . "black")
      (background-mode . dark)
      (border-color . "black")
      (cursor-color . "yellow")
      (foreground-color . "white")
      (mouse-color . "sienna1"))
     ((gnus-mouse-face . highlight)
      (list-matching-lines-face . bold)
      (view-highlight-face . highlight))
     (default ((t (nil))))

     ;; the following is alphabetically sorted for easier lookup
     (bold ((t (:bold t))))
     (bold-italic ((t (:italic t :bold t))))
     (custom-button-face ((t (nil))))
     (custom-changed-face ((t (:background "blue" :foreground "white"))))
     (custom-documentation-face ((t (nil))))
     (custom-face-tag-face ((t (:underline t))))
     (custom-group-tag-face ((t (:underline t :foreground "light blue"))))
     (custom-group-tag-face-1 ((t (:underline t :foreground "pink"))))
     (custom-invalid-face ((t (:background "red" :foreground "yellow"))))
     (custom-modified-face ((t (:background "blue" :foreground "white"))))
     (custom-rogue-face ((t (:background "black" :foreground "pink"))))
     (custom-saved-face ((t (:underline t))))
     (custom-set-face ((t (:background "white" :foreground "blue"))))
     (custom-state-face ((t (:foreground "lime green"))))
     (custom-variable-button-face ((t (:underline t :bold t))))
     (custom-variable-tag-face ((t (:underline t :foreground "light blue"))))
     (fl-comment-face ((t (:foreground "pink"))))
     (fl-doc-string-face ((t (:foreground "purple"))))
     (fl-function-name-face ((t (:foreground "red"))))
     (fl-keyword-face ((t (:foreground "cyan"))))
     (fl-string-face ((t (:foreground "green"))))
     (fl-type-face ((t (:foreground "yellow"))))
     (font-lock-builtin-face ((t (:foreground "LightSteelBlue"))))
     (font-lock-comment-face ((t (:foreground "OrangeRed"))))
     (font-lock-constant-face ((t (:foreground "Aquamarine"))))
     (font-lock-doc-string-face ((t (:foreground "LightSalmon"))))
     (font-lock-function-name-face ((t (:foreground "LightSkyBlue"))))
     (font-lock-keyword-face ((t (:foreground "Cyan"))))
     (font-lock-preprocessor-face ((t (:foreground "Aquamarine"))))
     (font-lock-reference-face ((t (:foreground "LightSteelBlue"))))
     (font-lock-string-face ((t (:foreground "LightSalmon"))))
     (font-lock-type-face ((t (:foreground "PaleGreen"))))
     (font-lock-variable-name-face ((t (:foreground "LightGoldenrod"))))
     (font-lock-warning-face ((t (:bold t :foreground "Pink"))))
     (gnus-cite-attribution-face ((t (:italic t))))
     (gnus-cite-face-1 ((t (:bold t :foreground "deep sky blue"))))
     (gnus-cite-face-10 ((t (:foreground "medium purple"))))
     (gnus-cite-face-11 ((t (:foreground "turquoise"))))
     (gnus-cite-face-2 ((t (:bold t :foreground "cyan"))))
     (gnus-cite-face-3 ((t (:bold t :foreground "gold"))))
     (gnus-cite-face-4 ((t (:foreground "light pink"))))
     (gnus-cite-face-5 ((t (:foreground "pale green"))))
     (gnus-cite-face-6 ((t (:bold t :foreground "chocolate"))))
     (gnus-cite-face-7 ((t (:foreground "orange"))))
     (gnus-cite-face-8 ((t (:foreground "magenta"))))
     (gnus-cite-face-9 ((t (:foreground "violet"))))
     (gnus-emphasis-bold ((t (:bold t))))
     (gnus-emphasis-bold-italic ((t (:italic t :bold t))))
     (gnus-emphasis-highlight-words ((t (:background "black" :foreground "yellow"))))
     (gnus-emphasis-italic ((t (:italic t))))
     (gnus-emphasis-underline ((t (:underline t))))
     (gnus-emphasis-underline-bold ((t (:underline t :bold t))))
     (gnus-emphasis-underline-bold-italic ((t (:underline t :italic t :bold t))))
     (gnus-emphasis-underline-italic ((t (:underline t :italic t))))
     (gnus-group-mail-1-empty-face ((t (:foreground "aquamarine1"))))
     (gnus-group-mail-1-face ((t (:bold t :foreground "aquamarine1"))))
     (gnus-group-mail-2-empty-face ((t (:foreground "aquamarine2"))))
     (gnus-group-mail-2-face ((t (:bold t :foreground "aquamarine2"))))
     (gnus-group-mail-3-empty-face ((t (:foreground "aquamarine3"))))
     (gnus-group-mail-3-face ((t (:bold t :foreground "aquamarine3"))))
     (gnus-group-mail-low-empty-face ((t (:foreground "aquamarine4"))))
     (gnus-group-mail-low-face ((t (:bold t :foreground "aquamarine4"))))
     (gnus-group-news-1-empty-face ((t (:foreground "PaleTurquoise"))))
     (gnus-group-news-1-face ((t (:bold t :foreground "PaleTurquoise"))))
     (gnus-group-news-2-empty-face ((t (:foreground "turquoise"))))
     (gnus-group-news-2-face ((t (:bold t :foreground "turquoise"))))
     (gnus-group-news-3-empty-face ((t (nil))))
     (gnus-group-news-3-face ((t (:bold t))))
     (gnus-group-news-4-empty-face ((t (nil))))
     (gnus-group-news-4-face ((t (:bold t))))
     (gnus-group-news-5-empty-face ((t (nil))))
     (gnus-group-news-5-face ((t (:bold t))))
     (gnus-group-news-6-empty-face ((t (nil))))
     (gnus-group-news-6-face ((t (:bold t))))
     (gnus-group-news-low-empty-face ((t (:foreground "DarkTurquoise"))))
     (gnus-group-news-low-face ((t (:bold t :foreground "DarkTurquoise"))))
     (gnus-header-content-face ((t (:italic t :foreground "forest green"))))
     (gnus-header-from-face ((t (:bold t :foreground "spring green"))))
     (gnus-header-name-face ((t (:foreground "deep sky blue"))))
     (gnus-header-newsgroups-face ((t (:italic t :bold t :foreground "purple"))))
     (gnus-header-subject-face ((t (:bold t :foreground "orange"))))
     (gnus-signature-face ((t (:bold t :foreground "khaki"))))
     (gnus-splash-face ((t (:foreground "Brown"))))
     (gnus-summary-cancelled-face ((t (:background "black" :foreground "yellow"))))
     (gnus-summary-high-ancient-face ((t (:bold t :foreground "SkyBlue"))))
     (gnus-summary-high-read-face ((t (:bold t :foreground "PaleGreen"))))
     (gnus-summary-high-ticked-face ((t (:bold t :foreground "pink"))))
     (gnus-summary-high-unread-face ((t (:bold t))))
     (gnus-summary-low-ancient-face ((t (:italic t :foreground "SkyBlue"))))
     (gnus-summary-low-read-face ((t (:italic t :foreground "PaleGreen"))))
     (gnus-summary-low-ticked-face ((t (:italic t :foreground "pink"))))
     (gnus-summary-low-unread-face ((t (:italic t))))
     (gnus-summary-normal-ancient-face ((t (:foreground "SkyBlue"))))
     (gnus-summary-normal-read-face ((t (:foreground "PaleGreen"))))
     (gnus-summary-normal-ticked-face ((t (:foreground "pink"))))
     (gnus-summary-normal-unread-face ((t (nil))))
     (gnus-summary-selected-face ((t (:underline t))))
     (highlight ((t (:background "darkolivegreen"))))
     (isearch ((t (:background "blue"))))
     (italic ((t (:italic t))))
     (message-cited-text-face ((t (:bold t :foreground "red"))))
     (message-header-cc-face ((t (:bold t :foreground "green4"))))
     (message-header-name-face ((t (:bold t :foreground "orange"))))
     (message-header-newsgroups-face ((t (:bold t :foreground "violet"))))
     (message-header-other-face ((t (:bold t :foreground "chocolate"))))
     (message-header-subject-face ((t (:bold t :foreground "yellow"))))
     (message-header-to-face ((t (:bold t :foreground "cyan"))))
     (message-header-xheader-face ((t (:bold t :foreground "light blue"))))
     (message-mml-face ((t (:bold t :background "Green3"))))
     (message-separator-face ((t (:foreground "blue3"))))
     (modeline ((t (:background "white" :foreground "black"))))
     (modeline-buffer-id ((t (:background "white" :foreground "black"))))
     (modeline-mousable ((t (:background "white" :foreground "black"))))
     (modeline-mousable-minor-mode ((t (:background "white" :foreground "black"))))
     (primary-selection ((t (:background "blue"))))
     (region ((t (:background "blue"))))
     (secondary-selection ((t (:background "yellow" :foreground "black"))))
     (swbuff-current-buffer-face ((t (:foreground "red" :bold t :underline t))))
     (trailing-whitespace ((((class color) (background light)) (:background "#fff0f0"))))
     (underline ((t (:underline t))))
     (whitespace-trailing ((((class color) (background light)) (:background "#fff0f0"))))
     (widget-button-face ((t (:bold t))))
     (widget-button-pressed-face ((t (:foreground "red"))))
     (widget-documentation-face ((t (:foreground "lime green"))))
     (widget-field-face ((t (:background "dim gray"))))
     (widget-inactive-face ((t (:foreground "light gray"))))
     (widget-single-line-field-face ((t (:background "dim gray"))))
     (zmacs-region ((t (:background "blue"))))
     ;; end of sorted lines
     )))

(defun color-theme-darabi-term ()
  "Color theme for a terminal session by Kambiz Darabi."
  (interactive)
  (color-theme-install
   '(color-theme-darabi-term
     ((background-color . "grey15")
      (background-mode . dark)
      (border-color . "black")
      (cursor-color . "LightGoldenrod")
      (foreground-color . "limegreen")
      (mouse-color . "yellow"))
     (default
       ((t (:stipple nil :background "grey15" :foreground "limegreen"
                     :inverse-video nil :box nil :strike-through nil
                     :overline nil :underline nil :slant normal
                     :weight normal :height 98 :width normal
                     :family "outline-courier new"))))
     (border ((t (:background "black"))))
     (comint-highlight-input ((t (:bold t :weight bold))))
     (comint-highlight-prompt ((t (:foreground "cyan"))))
     (cursor ((t (:background "LightGoldenrod"))))
     (custom-button-face
      ((t (:background "lightgrey" :foreground "black"
                       :box (:line-width 2 :style released-button)))))
     (custom-button-pressed-face
      ((t (:background "lightgrey" :foreground "black"
                       :box (:line-width 2 :style pressed-button)))))
     (custom-changed-face ((t (:background "blue" :foreground "white"))))
     (custom-comment-face ((t (:background "dim gray"))))
     (custom-comment-tag-face ((t (:foreground "gray80"))))
     (custom-documentation-face ((t (nil))))
     (custom-face-tag-face
      ((t (:bold t :weight bold :height 1.2 :family "helv"))))
     (custom-group-tag-face
      ((t (:bold t :foreground "light blue"
                 :weight bold :height 1.2))))
     (custom-group-tag-face-1
      ((t (:bold t :foreground "pink" :weight bold
                 :height 1.2 :family "helv"))))
     (custom-invalid-face ((t (:background "red" :foreground "yellow"))))
     (custom-modified-face ((t (:background "blue" :foreground "white"))))
     (custom-rogue-face ((t (:background "black" :foreground "pink"))))
     (custom-saved-face ((t (:underline t))))
     (custom-set-face ((t (:background "gray15" :foreground "limegreen"))))
     (custom-state-face ((t (:foreground "lime green"))))
     (custom-variable-button-face ((t (:bold t :underline t :weight bold))))
     (custom-variable-tag-face
      ((t (:bold t :foreground "light blue"
                 :weight bold :height 1.2 :family "helv"))))
     (erc-action-face
      ((t (:bold t :background "gray15"
                 :foreground "lightyellow" :weight bold))))
     (erc-bold-face
      ((t (:bold t :background "gray15" :foreground "gray77" :weight bold))))
     (erc-current-nick-face
      ((t (:bold t :background "gray15" :foreground "lightblue" :weight bold))))
     (erc-dangerous-host-face ((t (:background "gray15" :foreground "red"))))
     (erc-default-face ((t (:background "Gray15" :foreground "gray77"))))
     (erc-direct-msg-face ((t (:background "gray15" :foreground "gray90"))))
     (erc-error-face ((t (:background "gray15" :foreground "red"))))
     (erc-fool-face ((t (:background "gray15" :foreground "gray40"))))
     (erc-input-face ((t (:background "gray15" :foreground "gray77"))))
     (erc-inverse-face ((t (:background "gray77" :foreground "Gray15"))))
     (erc-keyword-face
      ((t (:bold t :background "gray15"
                 :foreground "lightgreen" :weight bold))))
     (erc-nick-default-face
      ((t (:bold t :background "gray15" :foreground "Magenta" :weight bold))))
     (erc-nick-msg-face
      ((t (:bold t :background "gray15" :foreground "orange" :weight bold))))
     (erc-notice-face
      ((t (:bold t :background "gray15" :foreground "lightBlue" :weight bold))))
     (erc-pal-face
      ((t (:bold t :background "gray15" :foreground "white" :weight bold))))
     (erc-prompt-face
      ((t (:bold t :background "lightBlue2" :foreground "Black" :weight bold))))
     (erc-timestamp-face
      ((t (:bold t :background "gray15" :foreground "green" :weight bold))))
     (erc-underline-face
      ((t (:background "gray15" :foreground "gray77" :underline t))))
     (extra-whitespace-face ((t (:background "pale green"))))
     (fixed-pitch ((t (:family "courier"))))
     (font-lock-builtin-face ((t (:foreground "LightSlateBlue"))))
     (font-lock-comment-face ((t (:foreground "lightgray"))))
     (font-lock-constant-face ((t (:foreground "yellow"))))
     (font-lock-doc-face ((t (:foreground "LightSalmon"))))
     (font-lock-function-name-face ((t (:foreground "greenyellow"))))
     (font-lock-keyword-face ((t (:foreground "DeepSkyBlue"))))
     (font-lock-string-face ((t (:foreground "LightGoldenrodYellow"))))
     (font-lock-type-face ((t (:foreground "cyan"))))
     (font-lock-variable-name-face ((t (:foreground "lightcyan"))))
     (font-lock-warning-face ((t (:bold t :foreground "Pink" :weight bold))))
     (fringe ((t (:background "grey10"))))
     (header-line ((t (:background "grey20" :foreground "grey90" :box nil))))
     (highlight ((t (:background "darkolivegreen"))))
     (jde-bug-breakpoint-cursor ((t (:background "brown" :foreground "cyan"))))
     (jde-db-active-breakpoint-face ((t (:background "red" :foreground "black"))))
     (jde-db-requested-breakpoint-face
      ((t (:background "yellow" :foreground "black"))))
     (jde-db-spec-breakpoint-face ((t (:background "green" :foreground "black"))))
     (jde-java-font-lock-api-face ((t (:foreground "light goldenrod"))))
     (jde-java-font-lock-bold-face ((t (:bold t :weight bold))))
     (jde-java-font-lock-code-face ((t (nil))))
     (jde-java-font-lock-constant-face ((t (:foreground "Aquamarine"))))
     (jde-java-font-lock-doc-tag-face ((t (:foreground "light coral"))))
     (jde-java-font-lock-italic-face ((t (:italic t :slant italic))))
     (jde-java-font-lock-link-face
      ((t (:foreground "blue" :underline t :slant normal))))
     (jde-java-font-lock-modifier-face ((t (:foreground "LightSteelBlue"))))
     (jde-java-font-lock-number-face ((t (:foreground "LightSalmon"))))
     (jde-java-font-lock-operator-face ((t (:foreground "medium blue"))))
     (jde-java-font-lock-package-face ((t (:foreground "steelblue1"))))
     (jde-java-font-lock-pre-face ((t (nil))))
     (jde-java-font-lock-underline-face ((t (:underline t))))
     (menu ((t (nil))))
     (mode-line
      ((t (:background "grey75" :foreground "black"
                       :box (:line-width -1 :style released-button)))))
     (mouse ((t (:background "yellow"))))
     (region ((t (:background "blue3"))))
     (scroll-bar ((t (nil))))
     (secondary-selection ((t (:background "SkyBlue4"))))
     (ecb-default-highlight-face ((t (:background "gray40" :height 1.0))))
     )))



;;  (custom-invalid ((t (:background "red" :foreground "black"))))                       ;;;;;;;;;;;;
;;  (erc-notice-face ((t (:background "gray15" :foreground "darkgray"))))                       ;;;;;;;;;;;;
;;  (gnus-header-content ((t (:foreground "lime green" :slant normal))))                       ;;;;;;;;;;;;
;;  (gnus-header-name ((((class color) (background dark)) (:foreground "spring green"))))                       ;;;;;;;;;;;;
;;  (gnus-header-subject ((((class color) (background dark)) (:foreground "bisk1" :weight bold))))                       ;;;;;;;;;;;;
;;  (highlight ((t (:background "darkolivegreen" :foreground "black"))))                       ;;;;;;;;;;;;
;;  (lazy-highlight ((((class color) (min-colors 8)) (:background "turquoise3" :foreground "black"))))                       ;;;;;;;;;;;;
;;  (menu ((t (:background "grey60" :foreground "black" :inverse-video t))))                       ;;;;;;;;;;;;
;;  (message-header-name ((((class color) (background dark)) (:foreground "spring green"))))                       ;;;;;;;;;;;;
;;  (message-header-subject ((((class color) (background dark)) (:foreground "bisque1" :weight bold))))                       ;;;;;;;;;;;;
;;  (message-separator ((((class color) (background dark)) (:foreground "cadet blue"))))                       ;;;;;;;;;;;;
;;  (muse-header-1 ((t (:weight bold :height 1.4))))                       ;;;;;;;;;;;;
;;  (org-hide ((((background dark)) (:background "black" :foreground "darkblue"))))                       ;;;;;;;;;;;;
;;  (paren-face-match ((((class color)) (:background "green"))))                       ;;;;;;;;;;;;
;;  (paren-face-mismatch ((((class color)) (:foreground "white" :background "red"))))                       ;;;;;;;;;;;;
;;  (paren-match ((t (:background "green"))))                       ;;;;;;;;;;;;
;;  (paren-mismatch ((t (:background "red"))))                       ;;;;;;;;;;;;
;;  (region ((((class color) (min-colors 88) (background dark)) (:background "SlateBlue3"))))                       ;;;;;;;;;;;;
;;  (secondary-selection ((t (:background "SkyBlue4" :foreground "black"))))                       ;;;;;;;;;;;;
;;  (show-paren-match ((((class color)) (:background "green"))))                       ;;;;;;;;;;;;
;;  (show-paren-mismatch ((((class color)) (:background "red"))))                       ;;;;;;;;;;;;
;;  (slime-highlight-edits-face ((((class color) (background dark)) (:background "blue"))))                       ;;;;;;;;;;;;
;;  (tool-bar ((default (:background "grey" :foreground "black" :box (:line-width 1 :style released-button))) (nil nil)))                       ;;;;;;;;;;;;
;;  (trailing-whitespace ((((class color) (background dark)) (:background "darkred" :foreground "white"))))                       ;;;;;;;;;;;;
;;  (ude-font-lock-face-10 ((t (:foreground "#a9aeed" :weight bold))))                       ;;;;;;;;;;;;
;;  (variable-pitch ((t nil)))                       ;;;;;;;;;;;;


;;; Order emacs to automaticaly save all IRC buffers when quiting
;;; Author: unknown yet
;; (defadvice save-buffers-kill-emacs (before save-logs (arg) activate)
;;   (save-some-buffers
;;    t (lambda ()
;;        (when (and (eq major-mode 'erc-mode) (not (null buffer-file-name)))))))

;;; Remove characters not allowed for Windows file system
;;; and create short log file name containing channel name and
;;; current day date.
;;; Author: unknown yet with Artur Hefczyc modifications
(defun my-erc-generate-log-file-name-short (buffer &optional target
                                                   nick server port)
  "This function uses the buffer-name as a file, with some replacing."
  (let* ((name (buffer-name buffer))
         (name (replace-regexp-in-string "|" "-" name)))
    (concat erc-log-channels-directory "/"
            name  "_" (format-time-string "%Y-%m-%d"))))

;;; Fast start on given server without any additional questions
;;; /join #geotools
;;; command is read from ~/.ercrc file, but how to do it
;;; without .ercrc file but explicity in erc-geotools function?
;;; Please let me know if you know how to set target channel name
;;; in .emacs file.
;;; Author: Artur Hefczyc
(defun erc-start-now ()
  "Start ERC client and log directly to default server without any questions."
  (interactive)
  (erc :server "irc.eu.freenode.net" :port 6667 :nick "kami" :full-name erc-user-full-name))

;;; Generate colorized HTML version of erc-log with
;;; faces set for light background - prefered by many
;;; people color for web pages
;;; Author: Artur Hefczyc
(defun erc-colorize-log ()
  "Generate html log file with colorized content - light background scheme"
  (interactive)
  (set-foreground-color "gray1")
  (set-background-color "lightyellow")
  (custom-set-faces
   '(erc-action-face
     ((t (:bold t :background "lightyellow" :foreground "gray1" :weight bold))))
   '(erc-bold-face
     ((t (:bold t :background "lightyellow" :foreground "gray1" :weight bold))))
   '(erc-current-nick-face
     ((t (:bold t :background "lightyellow"
                :foreground "darkblue" :weight bold))))
   '(erc-dangerous-host-face ((t (:background "lightyellow" :foreground "red"))))
   '(erc-default-face ((t (:background "Lightyellow" :foreground "gray1"))))
   '(erc-direct-msg-face ((t (:background "lightyellow" :foreground "black"))))
   '(erc-error-face ((t (:background "lightyellow" :foreground "red"))))
   '(erc-fool-face ((t (:background "lightyellow" :foreground "gray60"))))
   '(erc-input-face ((t (:background "lightyellow" :foreground "gray1"))))
   '(erc-inverse-face ((t (:background "gray1" :foreground "Lightyellow"))))
   '(erc-keyword-face
     ((t (:bold t :background "lightyellow"
                :foreground "darkgreen" :weight bold))))
   '(erc-nick-default-face
     ((t (:bold t :background "lightyellow"
                :foreground "darkMagenta" :weight bold))))
   '(erc-nick-msg-face
     ((t (:bold t :background "lightyellow" :foreground "brown" :weight bold))))
   '(erc-notice-face
     ((t (:bold t :background "lightyellow"
                :foreground "darkBlue" :weight bold))))
   '(erc-pal-face
     ((t (:bold t :background "lightyellow" :foreground "black" :weight bold))))
   '(erc-prompt-face
     ((t (:bold t :background "lightBlue2" :foreground "Black" :weight bold))))
   '(erc-timestamp-face
     ((t (:bold t :background "lightyellow"
                :foreground "darkgreen" :weight bold))))
   '(erc-underline-face
     ((t (:background "lightyellow" :foreground "gray1" :underline t))))
   )
  (htmlize-buffer (current-buffer))
  (set-background-color "gray15")
  (set-foreground-color "limegreen")
  (custom-set-faces
   '(erc-action-face
     ((t (:bold t :background "gray15"
                :foreground "lightyellow" :weight bold))))
   '(erc-bold-face
     ((t (:bold t :background "gray15" :foreground "gray77" :weight bold))))
   '(erc-current-nick-face
     ((t (:bold t :background "gray15" :foreground "lightblue" :weight bold))))
   '(erc-dangerous-host-face ((t (:background "gray15" :foreground "red"))))
   '(erc-default-face ((t (:background "Gray15" :foreground "gray77"))))
   '(erc-direct-msg-face ((t (:background "gray15" :foreground "gray90"))))
   '(erc-error-face ((t (:background "gray15" :foreground "red"))))
   '(erc-fool-face ((t (:background "gray15" :foreground "gray40"))))
   '(erc-input-face ((t (:background "gray15" :foreground "gray77"))))
   '(erc-inverse-face ((t (:background "gray77" :foreground "Gray15"))))
   '(erc-keyword-face
     ((t (:bold t :background "gray15" :foreground "lightgreen" :weight bold))))
   '(erc-nick-default-face
     ((t (:bold t :background "gray15" :foreground "Magenta" :weight bold))))
   '(erc-nick-msg-face
     ((t (:bold t :background "gray15" :foreground "orange" :weight bold))))
   '(erc-notice-face
     ((t (:bold t :background "gray15" :foreground "lightBlue" :weight bold))))
   '(erc-pal-face
     ((t (:bold t :background "gray15" :foreground "white" :weight bold))))
   '(erc-prompt-face
     ((t (:bold t :background "lightBlue2" :foreground "Black" :weight bold))))
   '(erc-timestamp-face
     ((t (:bold t :background "gray15" :foreground "green" :weight bold))))
   '(erc-underline-face
     ((t (:background "gray15" :foreground "gray77" :underline t))))
   )
  )

(defun kd-backward-kill-whitespace-or-word ()
  "Deletes any whitespace from point back to the end of the
previous word. Intended to be used instead of the binding
of <C-backspace>."
  (interactive)
  (let ((beg (point)))
    ; test the char before the current one
    (backward-char)
    (if (thing-at-point 'whitespace)
	(progn
	  ; as long as it is whitespace, go back
	  (while (and (> (point) 0)
		      (thing-at-point 'whitespace))
	    (backward-char))
	  ; found the last non-whitespace, so move
	  ; forward to not delete it
	  (forward-char)
	  (delete-region beg (point)))
      ; arg 1 means 'backward'
      (progn
	(forward-char)
	(backward-kill-word 1)))))

;; using guillemet symbols (for openarchitectureware)
(defun insert-left-guillemet ()
  (interactive)
  (insert-char 171 1))

(defun insert-right-guillemet ()
  (interactive)
  (insert-char 187 1))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; Display, buffers etc.
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (c) Tassilo Horn
;; http://tsdh.wordpress.com/2007/03/09/automatically-split-window-horizontally-if-current-window-is-wide-enough/
(setf my-display-buffer-width-threshold 185)

(setf my-display-buffer-same-window nil)

(defun my-display-buffer (buffer force-other-window)
  "If BUFFER is visible, select it.

If it's not visible and there's only one window, split the
current window and select BUFFER in the new window. If the
current window (before the split) is more than 165 columns wide,
split horizontally, else split vertically.

If the current buffer contains more than one window, select
BUFFER in the least recently used window.

This function returns the window which holds BUFFER.

FORCE-OTHER-WINDOW is ignored."
  (message "my-display-buffer called")
  (if (> (window-width) my-display-buffer-width-threshold)
      (message "horizontally")
      (message "vertically"))
  (or (get-buffer-window buffer)
      (if (one-window-p)
          (let ((new-win (if (> (window-width) my-display-buffer-width-threshold)
                             (split-window-horizontally)
                             (split-window-vertically))))
            (set-window-buffer new-win buffer)
            new-win)
          (let ((new-win (if my-display-buffer-same-window
                             (selected-window)
                             (get-lru-window))))
            (set-window-buffer new-win buffer)
            new-win))))

(defun nxml-format-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region begin end))
    (message "Ah, much better!"))

;;; START ERC CUSOMISATIONS

;;; the ERC customisations are taken from
;;; http://mwolson.org/projects/emacs-config/erc-init.el

;; Change fill column according to the width of the current frame
(defun my-erc-mode-stuff ()
  "Set fill column according to `frame-width'."
  (set (make-local-variable 'erc-fill-column) (- (frame-width) 10))
  (set (make-local-variable 'erc-timestamp-right-column)
       (1+ erc-fill-column)))
;; (add-hook 'erc-mode-hook 'my-erc-mode-stuff)

;;; Notify me when a keyword is matched (someone wants to reach me)

(defvar my-erc-page-message "%s is calling your name."
  "Format of message to display in dialog box")

(defvar my-erc-page-nick-alist nil
  "Alist of nicks and the last time they tried to trigger a
notification")

(defvar my-erc-page-timeout 30
  "Number of seconds that must elapse between notifications from
the same person.")

(defun my-erc-page-popup-notification (nick)
  (when window-system
    ;; must set default directory, otherwise start-process is unhappy
    ;; when this is something remote or nonexistent
    (let ((default-directory "~/"))
      ;; 8640000 milliseconds = 1 day
      (start-process "page-me" nil "notify-send"
                     "-u" "normal" "-t" "3600000" "ERC"
                     (format my-erc-page-message nick)))))

(defun my-erc-page-allowed (nick &optional delay)
  "Return non-nil if a notification should be made for NICK.
If DELAY is specified, it will be the minimum time in seconds
that can occur between two notifications.  The default is
`my-erc-page-timeout'."
  (unless delay (setq delay my-erc-page-timeout))
  (let ((cur-time (time-to-seconds (current-time)))
        (cur-assoc (assoc nick my-erc-page-nick-alist))
        (last-time nil))
    (if cur-assoc
        (progn
          (setq last-time (cdr cur-assoc))
          (setcdr cur-assoc cur-time)
          (> (abs (- cur-time last-time)) delay))
      (push (cons nick cur-time) my-erc-page-nick-alist)
      t)))

(defun my-erc-page-me (match-type nick message)
  "Notify the current user when someone sends a message that
matches a regexp in `erc-keywords'."
  (interactive)
  (when (and (eq match-type 'keyword)
             ;; I don't want to see anything from the erc server
             (not (string-match "\\`\\([sS]erver\\|localhost\\|root\\)" nick))
             ;; or bots
             (not (string-match "\\(^CIA[^!]*\\|bot\\|serv\\)!" nick))
             ;; or from those who abuse the system
             (my-erc-page-allowed nick))
    (my-erc-page-popup-notification nick)))
;; (add-hook 'erc-text-matched-hook 'my-erc-page-me)

(defun my-erc-page-me-PRIVMSG (proc parsed)
  (let ((nick (car (erc-parse-user (erc-response.sender parsed))))
        (target (car (erc-response.command-args parsed)))
        (msg (erc-response.contents parsed)))
    (when (and (erc-current-nick-p target)
               (not (erc-is-message-ctcp-and-not-action-p msg))
               (my-erc-page-allowed nick))
      (my-erc-page-popup-notification nick)
      nil)))
;; (add-hook 'erc-server-PRIVMSG-functions 'my-erc-page-me-PRIVMSG)

;;; Remove trailing whitespace in messages

(defun my-erc-remove-trailing-whitespace (proc parsed)
  "Remove trailing whitespace from the current message.
Some IM clients use an OTR plug-in that sends some annoying
trailing space to the screen, so we want to mop that up."
  (let ((msg (erc-response.contents parsed)))
    (when (stringp msg)
      (setf (erc-response.contents parsed)
            (erc-replace-regexp-in-string "[[:space:]]+\\'" "" msg))
      nil)))
;; (add-hook 'erc-server-PRIVMSG-functions 'my-erc-remove-trailing-whitespace)

;;; END ERC CUSOMISATIONS

;;; START EDIFF

;; Taken from StackOverflow
;;
;; http://stackoverflow.com/questions/1817370/using-ediff-as-git-mergetool/4613433#4613433
;;

;;
;; Setup for ediff.
;;
(require 'ediff)

(defvar ediff-after-quit-hooks nil
  "* Hooks to run after ediff or emerge is quit.")

(defadvice ediff-quit (after edit-after-quit-hooks activate)
  (run-hooks 'ediff-after-quit-hooks))

(setq git-mergetool-emacsclient-ediff-active nil)


(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

(defun local-ediff-before-setup-hook ()
  (setq local-ediff-saved-frame-configuration (current-frame-configuration))
  (setq local-ediff-saved-window-configuration (current-window-configuration))
  ;; (local-ediff-frame-maximize)
  (if git-mergetool-emacsclient-ediff-active
      (raise-frame)))

(defun local-ediff-quit-hook ()
  (set-frame-configuration local-ediff-saved-frame-configuration)
  (set-window-configuration local-ediff-saved-window-configuration))

(defun local-ediff-suspend-hook ()
  (set-frame-configuration local-ediff-saved-frame-configuration)
  (set-window-configuration local-ediff-saved-window-configuration))

(add-hook 'ediff-before-setup-hook 'local-ediff-before-setup-hook)
(add-hook 'ediff-quit-hook 'local-ediff-quit-hook 'append)
(add-hook 'ediff-suspend-hook 'local-ediff-suspend-hook 'append)

;; Useful for ediff merge from emacsclient.
(defun git-mergetool-emacsclient-ediff (local remote base merged)
  (setq git-mergetool-emacsclient-ediff-active t)
  (if (file-readable-p base)
      (ediff-merge-files-with-ancestor local remote base nil merged)
    (ediff-merge-files local remote nil merged))
  (recursive-edit))

(defun git-mergetool-emacsclient-ediff-after-quit-hook ()
  (exit-recursive-edit))

(add-hook 'ediff-after-quit-hooks 'git-mergetool-emacsclient-ediff-after-quit-hook 'append)

;;; END EDIFF

;;; opening a readonly file with sudo or su
(defun my-find-file-as-root ()
  "Opens the file associated with the current buffer using the 
tramp prefix /sudo:root@localhost:"
  (interactive)
  (let ((fname (buffer-file-name)))
    (if (not fname)
        (error "Current buffer has no associated file")
        (if (or (eq 0 (string-match "/su:" (buffer-file-name)))
                (eq 0 (string-match "/sudo:" (buffer-file-name))))
            (message "Current buffer already shows a /su: or /sudo: file.")
            (find-alternate-file (concat "/sudo:root@localhost:" fname))))))

;; https://github.com/m7d/elisp/blob/master/my-funcs.el

;; ID: 54eada16-ca15-3267-d16a-cd03d5b84de4
(defun get-bytes (file count)
  "Get the first COUNT bytes from FILE. Requires the head program
in your path. Useful for reading non-regular files like
/dev/random or /dev/urandom."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (call-process "head" file (current-buffer) nil
                  "-c" (number-to-string count))
    (substring (buffer-string) 0 count)))

;; Create UUIDs
;; ID: 90aebf38-b33a-314b-1198-c9bffea2f2a2
(defun uuid-create ()
  "Return a newly generated UUID. This uses a simple hashing of variable data."
  (let ((s (md5 (format "%s%s%s%s%s%s%s%s%s%s%s%s"
                        (user-uid)
                        (emacs-pid)
                        (system-name)
                        (user-full-name)
                        user-mail-address
                        (current-time)
                        (emacs-uptime)
                        (garbage-collect)
                        (random)
                        (buffer-list)
                        (recent-keys)
                        (if (file-exists-p "/dev/urandom")
                            (get-bytes "/dev/urandom" 16))))))
    (format "%s-%s-3%s-%s-%s"
            (substring s 0 8)
            (substring s 8 12)
            (substring s 13 16)
            (substring s 16 20)
            (substring s 20 32))))

(defun uuid-insert ()
  "Inserts a new UUID at the point."
  (interactive)
  (insert (uuid-create)))


;; http://www.emacswiki.org/emacs/DuplicateLines

(defun uniquify-region-lines (beg end)
  "Remove duplicate adjacent lines in region."
  (interactive "*r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
      (replace-match "\\1"))))
  
(defun uniquify-buffer-lines ()
  "Remove duplicate adjacent lines in the current buffer."
  (interactive)
  (uniquify-region-lines (point-min) (point-max)))

;;; Date/time functions

(defun millis-to-iso-date (millis)
  (interactive "NEnter java millisecond value: ")
  (let ((isodate (concat (format-time-string "%FT%T" (seconds-to-time (/ millis 1000))) (format ".%04dZ" (mod millis 1000)))))
    (message isodate)
    isodate))

(provide 'personal-funcs)
