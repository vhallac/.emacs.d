(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(autoload 'python-mode "python.el" "Python editing mode." t)

(defun ropemacs-start ()
  "Loads pymacs and ropemacs"
  (interactive)
  (require 'pymacs)
  (pymacs-load "ropemacs" "rope-")
  ;; Automatically save project python buffers before refactorings
  (setq ropemacs-confirm-saving 'nil))

(add-hook 'python-mode-hook
          (lambda ()
            (vtidy-mode t)))
