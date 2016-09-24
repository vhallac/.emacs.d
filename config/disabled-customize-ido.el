(eval-when-compile
  (require 'ido))

;; Nice vertical display. Default setup gives me claustrophobia.
(when (functionp 'ido-vertical-mode)
  (ido-vertical-mode))

;; Allow flexible matching in ido
(when (functionp 'flx-ido-mode)
  (flx-ido-mode))
