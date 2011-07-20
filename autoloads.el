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
(setq auto-mode-alist (cons '("\\.fs\\'" . forth-mode)
                            auto-mode-alist))
(autoload 'forth-block-mode "gforth.el")
(setq auto-mode-alist (cons '("\\.fb\\'" . forth-block-mode)
                            auto-mode-alist))

(autoload 'gnus "gnus"
  "Mail and news reader" t)

(autoload 'iedit-mode "iedit"
  "Edit multiple regions with the same content simultaneously." t)

(autoload 'jabber-connect-all "jabber" "Load Jabber client" t)

(autoload 'lua-mode "lua-mode.el"
  "Major mode for editing LUA code." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))

(autoload 'nxml-mode "nxml-mode"
  "Massive XML goodness" t)
(add-to-list 'auto-mode-alist
             '("\\.\\(x[ms]l\\|rng\\|x?html?\\)\\'" . nxml-mode))

(autoload 'org2blog/wp-mode "org2blog"
  "Blog posts from org mode" t)
(autoload 'bh/clock-in "org-clock"
  "Custom clock-in function: loaded in customize-org-clock")
(autoload 'bh/clock-out "org-clock"
  "Custom clock-out function: loaded in customize-org-clock")

(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\)$" . org-mode))

(autoload 'python-mode "python" "Python editing mode." t)
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))

(autoload 'scheme-mode "scheme"
  "Gotta scheme sometimes" t)
(add-to-list 'auto-mode-alist '("\\.s\\(s\\|c[mh]\\)$" . scheme-mode))

(autoload 'slime-mode "slime"
  "Superior LISP Interaction Mode" t)
(autoload 'slime "slime"
  "Superior LISP Interaction Mode" t)

(autoload 'virtualenv-activate "virtualenv"
  "Enable virtualenv usage for python" t)
(setq virtualenv-use-ipython nil)
