(eval-when-compile
  (require 'ido))

;; Nice vertical display. Default setup gives me claustrophobia.
(when (boundp ido-vertical-mode)
  (ido-vertical-mode))

;; Allow flexible matching in ido
(when (boundp flx-ido-mode)
  (flx-ido-mode))
