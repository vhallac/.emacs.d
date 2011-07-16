(autoload 'lua-mode "lua-mode.el"
  "Major mode for editing LUA code." t)

(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))

(add-hook 'lua-mode-hook '(lambda ()
  "Customizations for lua mode"
  (setq lua-electric-mode nil) ; Can't indent properly. At stay out of the way.
  (setq lua-indent-level 4)
  (choose-indent-type)
  (auto-fill-mode 1)
  (subword-mode 1)))
