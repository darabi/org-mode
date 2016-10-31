;;
;; Font handling in X
;;
(defun my-set-x-font ()
  "Checks the width of the screen with xrandr and sets different fonts based on
width thresholds. Note that the fonts must either be present in system font folders
or in ~/.fonts"
  (interactive)
  (let ((maxwidth (xrandr-max-width)))
    (message "maxwidth %s" maxwidth)
    (case maxwidth
      (1280 (set-default-font "Consolas-11" t))
      (1440 (set-default-font "Inconsolata-11" t))
      (1680 (set-default-font "Inconsolata-11" t))
      (1920 (set-default-font "Consolas-11" t))
      (0 (set-default-font "Inconsolata-10.5" t)))))

;; TODO: 9.5 is much better on an eee without external
;;       monitor, but how do we find out whether we
;;       have an external monitor connected, or not?
;; (set-default-font "Monospace-9.5" t)

;; (set-default-font "lucidasanstypewriter-12.5" t)

(provide 'my-x-functions)
