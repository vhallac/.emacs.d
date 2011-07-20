(autoload 'iedit "iedit"
  "Edit multiple regions with the same content simultaneously." t)

(define-key global-map (kbd "C-;") 'iedit-mode)
