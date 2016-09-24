(eval-when-compile
  (require 'helm-config))

(define-key global-map (kbd "C-x C-f") 'helm-find-files)
(define-key global-map (kbd "C-x b") 'helm-buffers-list)
(define-key global-map (kbd "M-x") 'helm-M-x)
