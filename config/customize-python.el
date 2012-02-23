(defvar ropemacs-enable-autoimport)
(defvar ropemacs-confirm-saving)

(autoload 'pymacs-load "pymacs.el")

(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(require 'auto-complete)
(require 'auto-complete-config)

(add-hook 'python-mode-hook
          (lambda ()
            (setq ropemacs-enable-autoimport t)
            (unless (featurep 'ropemacs)
	      (pymacs-load "ropemacs" "rope-" t)
	      (ropemacs-mode 1))
            ;; Automatically save project python buffers before refactorings
            (setq ropemacs-confirm-saving 'nil)
            (auto-complete-mode 1)
            (ac-ropemacs-setup)))
