;;; All autoloads for globally accessible functions are defined here.
;;; In addition, when the configuration for the package is trivial (such as a
;;; single setq), the setup is done in here as well.
;;; For all other packages, the configuration is handled by the files
;;; ~/.emacs.d/config/customization-<file>.el.
(autoload 'choose-indent-type "choose-indent"
  "Choose indent type according to majority in file" t)

(autoload 'erc "erc"
  "An Emacs Internet Relay Chat client" t)

(autoload 'forth-mode "gforth.el")
(add-to-list 'auto-mode-alist '("\\.fs$" . forth-mode))
(autoload 'forth-block-mode "gforth.el")
(add-to-list 'auto-mode-alist '("\\.fb$" . forth-block-mode))

(autoload 'gnus "gnus"
  "Mail and news reader" t)

(autoload 'nxml-mode "nxml-mode"
  "Massive XML goodness" t)
(add-to-list 'auto-mode-alist
             '("\\.\\(x[ms]l\\|rng\\|x?html?\\)\\'" . nxml-mode))

(autoload 'bh/clock-in "org-clock"
  "Custom clock-in function: loaded in customize-org-clock")
(autoload 'bh/clock-out "org-clock"
  "Custom clock-out function: loaded in customize-org-clock")

;; (add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\)$" . org-mode))

(autoload 'python-mode "python" "Python editing mode." t)
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))

;; Display the color specifications in CSS with the specified color.
(add-hook 'css-mode-hook 'rainbow-mode)

;; TODO: Move to rhtml-mode package
(autoload 'rhtml-mode "rhtml-mode"
  "Major mode for editing .html.erb files." t)
(add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . rhtml-mode))

(add-to-list 'auto-mode-alist '("\\(?:\\.\\(?:gemspec\\|r\\(?:ake\\|[ub]\\)\\)\\|Gemfile\\)\\$" . ruby-mode))

(autoload 'scheme-mode "scheme"
  "Gotta scheme sometimes" t)
(add-to-list 'auto-mode-alist '("\\.s\\(s\\|c[mh]\\)$" . scheme-mode))

(autoload 'virtualenv-activate "virtualenv"
  "Enable virtualenv usage for python" t)
(defvar virtualenv-use-ipython nil)

(autoload 'eclim-manage-projects "eclim"
  "Emacs-eclipse bridge" t)

(autoload 'global-eclim-mode "eclim"
  "Emacs-eclipse bridge - enable global mode" t)

(autoload 'start-eclimd "eclimd"
  "Manage eclimd from emacs" t)

;; Let groovy mode handle gradle files
(add-to-list 'auto-mode-alist '("\\.gradle$" . groovy-mode))

;; Hooks to launch rinari. I am not adding any others, because I don't want
;; rinari to jump in for everything. Normally, I would have javascript, css,
;; etc. modes added here. In case rinari is no activated automatically,
;; executing `rinari-launch' will do the trick.
(setq rinari-major-modes (list 'ruby-mode-hook 'yaml-mode-hook 'rhtml-mode-hook
                               'rspec-mode-hook))
