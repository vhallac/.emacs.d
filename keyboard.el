; Useful global keymap bindings
(define-key global-map (kbd "C-<tab>") 'other-window)
(define-key global-map (kbd "C-<f1>") 'bury-buffer)
(define-key global-map (kbd "<end>") 'end-of-line)
(define-key global-map (kbd "<home>") 'beginning-of-line)
(define-key global-map (kbd "C-<end>") 'end-of-buffer)
(define-key global-map (kbd "C-<home>") 'beginning-of-buffer)
(define-key global-map (kbd "C-j") 'delete-indentation)
(define-key global-map (kbd "C-S-w") 'delete-region)
(define-key global-map (kbd "M-g") 'goto-line)
(define-key global-map (kbd "<f9>") 'electric-buffer-list)
(define-key global-map (kbd "M-]") 'match-parenthesis)
(define-key global-map (kbd "M-s M-s") 'other-window)
(define-key global-map (kbd "M-1") 'delete-other-windows)
(define-key global-map (kbd "M-2") 'split-window-vertically)
(define-key global-map (kbd "M-3") 'split-window-horizontally)

(define-key global-map (kbd "C-x C-j C-c") 'jabber-connect-all)

(define-key global-map (kbd "C-M-r") 'org-capture)
(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "<f12>") 'org-agenda)
(define-key global-map (kbd "<f11> I") 'bh/clock-in)
(define-key global-map (kbd "<f11> O") 'bh/clock-out)

(define-key global-map (kbd "C-x g") 'magit-status)

(define-key global-map (kbd "C-;") 'mark-more-like-this-extended)
