(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(require 'auto-complete)
(require 'auto-complete-config)

(add-hook 'python-mode-hook
          (lambda ()
            (setq ropemacs-enable-autoimport t)
            (require 'pymacs)
            (if (not (boundp 'ropemacs-mode))
                (pymacs-load "ropemacs" "rope-"))
            ;; Automatically save project python buffers before refactorings
            (setq ropemacs-confirm-saving 'nil)
            (auto-complete-mode 1)
            (ac-ropemacs-setup)
            (ropemacs-mode 1)))
