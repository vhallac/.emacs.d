; Useful global keymap bindings
(define-key global-map (kbd "<end>") 'end-of-line)
(define-key global-map (kbd "<home>") 'beginning-of-line)
(define-key global-map (kbd "C-<end>") 'end-of-buffer)
(define-key global-map (kbd "C-<home>") 'beginning-of-buffer)
(define-key global-map (kbd "M-]") 'match-parenthesis)
(define-key global-map (kbd "M-s M-s") 'other-window)

(define-key global-map (kbd "C-c j c") 'jabber-connect-all)
