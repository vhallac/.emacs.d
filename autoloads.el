;;; All autoloads for globally accessible functions are defined here.
;;; In addition, when the configuration for the package is trivial (such as a
;;; single setq), the setup is done in here as well.
;;; For all other packages, the configuration is handled by the files
;;; ~/.emacs.d/config/customization-<file>.el.
(autoload 'choose-indent-type "choose-indent"
  "Choose indent type according to majority in file" t)

(autoload 'forth-mode "gforth.el")
(add-to-list 'auto-mode-alist '("\\.fs$" . forth-mode))
(autoload 'forth-block-mode "gforth.el")
(add-to-list 'auto-mode-alist '("\\.fb$" . forth-block-mode))

;; Display the color specifications in CSS with the specified color.
(add-hook 'css-mode-hook 'rainbow-mode)


;; Hooks to launch rinari. I am not adding any others, because I don't want
;; rinari to jump in for everything. Normally, I would have javascript, css,
;; etc. modes added here. In case rinari is no activated automatically,
;; executing `rinari-launch' will do the trick.
(setq rinari-major-modes (list 'ruby-mode-hook 'yaml-mode-hook 'rhtml-mode-hook
                               'rspec-mode-hook))

;; (autoload 'malabar-mode "malabar-mode" "An improved java-mode." t)
;; (add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))

;; (autoload 'eieio-defclass-autoload "eieio")
