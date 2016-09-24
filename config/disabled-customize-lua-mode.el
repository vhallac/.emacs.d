(add-hook 'lua-mode-hook '(lambda ()
  "Customizations for lua mode"
  (setq lua-electric-mode nil) ; Can't indent properly. At stay out of the way.
  (setq lua-indent-level 4)
  (choose-indent-type)
  (auto-fill-mode 1)
  (subword-mode 1)))
