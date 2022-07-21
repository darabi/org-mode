;;
;; Font handling in X
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Dynamic font (size) selection with xrandr
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-next-xrandr-screen-width ()
  (let ((start (re-search-forward " connected[^0-9]+" nil t)))
    (when start
      (let ((end (re-search-forward "x" nil t)))
        (when end
          (buffer-substring-no-properties start (decf end)))))))

(defun find-xrandr-screen-widths ()
  (let ((width)
        (widthlist))
    (while (setf width (find-next-xrandr-screen-width))
      (message "width %s" width)
      (push (string-to-number width) widthlist))
    widthlist))

(defun xrandr-max-width ()
  (let ((buffer "*Messages*")
        ;; this one prevents the buffer from appearing
        ;; as the output goes to the message area
        (max-mini-window-height 1.0))
    (if (eq 0 (shell-command "xrandr -q" buffer))
        (with-current-buffer buffer
          (let ((widths (find-xrandr-screen-widths))
                (maxwidth 0))
            (when widths
              (dolist (w widths)
                (setf maxwidth (max w maxwidth))))
            maxwidth))
        0)))


;; Possible fonts:
;; "Bitstream Vera Sans Mono Roman-12"
;; "Bitstream Vera Sans Mono Roman-13"
;; "Inconsolata-10"
;; "lucidasanstypewriter-12.5"

(defun my-set-x-font ()
  "Checks the width of the screen with xrandr and sets different fonts based on
width thresholds. Note that the fonts must either be present in system font folders
or in ~/.fonts"
  (interactive)
  (let ((maxwidth (x-display-pixel-width)))
    ;; (message "maxwidth %s" maxwidth)
    (case maxwidth
      (1280 (set-frame-font "Consolas-11" t))
      (1440 (set-frame-font "Inconsolata-11" t))
      (1680 (set-frame-font "Inconsolata-11" t))
      (1920 (set-frame-font "Consolas-11" t))
      (2560 (set-frame-font "Inconsolata-16" t))
      (3200 (set-frame-font "Inconsolata-12" t))
      (3840 (set-frame-font "Inconsolata-12" t))
      (0 (set-frame-font "Inconsolata-10.5" t)))))

(my-set-x-font)

(defun my-presentation-font (font-size)
  "Sets the color-theme to light background and the font to the argument font-size (default 21)"
  (interactive "NFont size: ")
  (set-frame-font (format "Inconsolata-%d" font-size t))
  (color-theme-snow))

(provide 'my-x-functions)
