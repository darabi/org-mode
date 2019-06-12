;;;
;;; Copyright (c) 2019 m-creations gmbh, All rights reserved
;;;

;; initialises doom-themes and some 'integration' configuration
;;
;; cf. https://github.com/hlissner/emacs-doom-themes
;;
;; after loading this file, you can load one of the themes in
;;
;; ../../doom-themes/themes
;;
;; currently:
;;
;; - doom-challenger-deep
;; - doom-city-lights
;; - doom-dracula
;; - doom-Iosvkem
;; - doom-molokai
;; - doom-nord-light
;; - doom-nord
;; - doom-nova
;; - doom-one-light
;; - doom-one
;; - doom-opera-light
;; - doom-opera
;; - doom-peacock
;; - doom-solarized-light
;; - doom-sourcerer
;; - doom-spacegrey
;; - doom-tomorrow-day
;; - doom-tomorrow-night
;; - doom-vibrant
;;
;; e.g.:
;;
;; (load-theme 'doom-sourcerer t)

(require 'doom-themes)

;; Global settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled

;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)

;; Corrects (and improves) org-mode's native fontification.
(with-eval-after-load 'org-mode
  (with-eval-after-load 'doom-themes
    (doom-themes-org-config)))

;; treemacs users will like this
(with-eval-after-load 'treemacs
  (with-eval-after-load 'doom-themes
    (doom-themes-treemacs-config)))

(provide 'mc-doom-theme)
