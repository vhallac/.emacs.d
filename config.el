
;; Set up customizations:
;; Each file of the form customize-<file>.el in the ~/.emacs.d/config directory
;; is loaded via a call to (require '<file-name>) when <file> is loaded.
(mapc (lambda (x)
        (let ((file-name
               (replace-regexp-in-string "^customize-\\|\\\.elc?$" ""
                                         (file-name-nondirectory x)) ))
          (eval-after-load file-name
            `(try-progn
              (concat "Cannot load configuration:" ,x)
              (load-file ,x)))))
      (directory-files "~/.emacs.d/config" t "^customize-.*el$"))

;; Use after-load hook to perform simple customizations of base packages.
(defun call-file-customizer (fname)
  "Call function customize-<file-name> if it is defined."
  (let* ((file-name (replace-regexp-in-string "^customize-\\|\\\.elc?$" ""
                                              (file-name-nondirectory fname)))
         (customize-func (intern (concat "customize-" file-name))))
    (when (functionp customize-func)
      (funcall customize-func))))

(add-hook 'after-load-functions 'call-file-customizer 'after)

;; And finally, activate all packages
(package-initialize)

(use-package helm
  :disabled
  :bind ( ("C-x C-f" . helm-find-files)
          ("C-x b" . helm-buffers-list)
          ("M-x"  . helm-M-x)))

(use-package ivy
  :config
  (ivy-mode)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "))

(use-package swiper
  :bind
  ("C-s" . swiper)
  :config
  (setq search-default-mode nil))

(use-package auth-source
  :config
  (when (and epg-gpg-program
             (file-exists-p epg-gpg-program)
             (file-executable-p epg-gpg-program))
    (setq auth-sources
          '( (:source "~/.emacs.d/.secrets/authinfo.gpg" :host t :protocol t)))))

(use-package gnutls
  :config
  (setq gnutls-min-prime-bits 1024))

(use-package lisp-mode
  :config
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode))

(use-package dired
  :config
  (add-hook 'dired-mode-hook
            (lambda ()
              (make-local-variable 'coding-system-for-read)
              (setq coding-system-for-read 'utf-8))))

(use-package gnus
  :commands gnus)

(use-package erc
  :commands erc
  :config
  (setq erc-dcc-mode t
        erc-dcc-verbose t
        erc-modules '(autojoin button completion dcc fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring stamp track))

  ;; If the DCC download directory is missing, create it.
  (if (not (file-exists-p erc-dcc-get-default-directory))
      (make-directory erc-dcc-get-default-directory t)))

(use-package nxml-mode
  :commands nxml-mode
  :bind (:map nxml-mode-map
              ("C-c k c" . comment-region))
  :mode ("\\.\\(x[ms]l\\|rng\\|x?html?\\)\\'" . nxml-mode)
  :config
  (setq nxml-child-indent 4
        nxml-outline-child-indent 4
        nxml-slash-auto-complete-flag nil)

  (add-hook 'nxml-mode-hook
            '(lambda ()
               (choose-indent-type)
               ;; Add my schema files to RNG search path
               (add-to-list 'rng-schema-locating-files
                            "~/.emacs.d/nxml-schemas/schemas.xml")
               (add-to-list 'rng-schema-locating-files
                            "~/.emacs.d/nxml-schemas/libvirt/schemas.xml"))))

(use-package python
  :commands python-mode
  :mode  ("\\.py$" . python-mode)
  :interpreter ("python" . python-mode)
  :config
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
              (ac-ropemacs-setup))))

(use-package pymacs
  :commands pymacs-load)

(use-package rhtml-mode
  :commands rhtml-mode
  :mode ("\\.html\\.erb\\'" . rhtml-mode))

(use-package ruby-mode
  :commands ruby-mode
  :mode ("\\(?:\\.\\(?:gemspec\\|r\\(?:ake\\|[ub]\\)\\)\\|Gemfile\\)\\$" . ruby-mode))

(use-package scheme
  :commands scheme-mode
  :mode ("\\.s\\(s\\|c[mh]\\)$" . scheme-mode))

(use-package virtualenv
  :commands virtualenv-activate
  :config (defvar virtualenv-use-ipython nil))

(use-package eclim
  :commands (eclim-manage-projects global-eclim-mode)
  :config
  (setq eclim-auto-save t 		; Need to save before analyzing class
        help-at-pt-display-when-idle t
        eclim-executable (concat eclipse-dir "eclim"))

  (help-at-pt-set-timer)

  (add-hook 'eclim-mode-hook
            (lambda ()
              (require 'auto-complete-config)
              (ac-config-default)
              (add-to-list 'ac-sources 'ac-source-eclim)))) ; ac-source-emacs-eclim is also available

(use-package eclimd
  :commands start-eclimd
  :config (setq eclimd-executable (concat eclipse-dir "eclimd")
                eclimd-default-workspace "~/work"))

(use-package groovy-mode
  :commands groovy-mode
  :mode ("\\.gradle$" . groovy-mode))

(use-package elisp-helper
  :commands vh-eval-and-replace
  :bind (("C-c e" . vh-eval-and-replace)))

(use-package magit
  :bind (("C-c g" . magit-status)))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'post-forward
                uniquify-separator ":"))

(use-package lua-mode
  :config
  (add-hook 'lua-mode-hook '(lambda ()
                              "Customizations for lua mode"
                              (setq lua-electric-mode nil
                                    lua-indent-level 4)
                              (choose-indent-type)
                              (auto-fill-mode 1)
                              (subword-mode 1))))

(use-package ledger-mode
  :commands ledger-mode
  :config
  (setq ledger-post-account-alignment-column 2
        ledger-clear-whole-transactions t
        ledger-complete-ignore-case t
        ledger-highlight-xact-under-point nil)

  (defadvice ledger-add-transaction (after remove-extra-newlines activate)
    "Clip the ever-growing \n series at end of file"
    (when (looking-at "\n\n\n")
      (delete-char 2))))

;; Org Babel
(use-package ob
  :config
  (setq org-babel-min-lines-for-block-output 999
        org-babel-results-keyword "results")

  (org-babel-do-load-languages 'org-babel-load-languages
                               '((ledger . t))))

(use-package org-capture
  :config
  (setq org-default-notes-file "~/org/refile.org"
        org-capture-templates  '(("w" "Web" entry
                                  (file+headline "~/org/home.org" "Firefox")
                                  "* TODO %c\n\n%i" :immediate-finish t)
                                 ("t" "TODO" entry
                                  (file+headline "~/org/refile.org" "Tasks")
                                  "* TODO %?\n  :PROPERTIES:\n  :CREATED: %U\n  :LINK: %a\n  :END:\n %i"
                                  :clock-in t :clock-resume t)
                                 ("n" "note" entry
                                  (file+headline "~/org/refile.org" "Notes")
                                  "* %? :NOTE:\n  %U\n  %a\n"
                                  :clock-in t :clock-resume t)
                                 ("q" "Quick note" item
                                  (file+headline "~/org/review.org" "Quick notes"))
                                 ("s" "Schedule" entry
                                  (file+headline "~/org/refile.org" "Quick Schedule")
                                  "* TODO %?\n  SCHEDULED: %t\n  %i")
                                 ("c" "Quick note on clocked task" item
                                  (clock))
                                 ("a" "appointment" entry
                                  (file+headline "~/org/appointments.org" "Appointments")
                                  "* %? :APPOINTMENT:\n %U")
                                 ("l" "Ledger entries")
                                 ("li" "Credit Card" plain
                                  (file+olp "~/org/finance.org" "Expenses" "Review")
                                  "%(subst-char-in-string ?- ?/ (org-read-date)) %^{Payee}\n  Liabilities:CC:ING Bonus  \n  Expenses:%^{Account}  %^{Amount}\n")
                                 ("lc" "Cash" plain
                                  (file+olp "~/org/finance.org" "Expenses" "Review")
                                  "%(subst-char-in-string ?- ?/ (org-read-date)) * %^{Payee}\n  Assets:Cash\n  Expenses:%^{Account}  %^{Amount}\n")))
  )
(use-package org-clock
  :bind (("C-c C-x C-j" . org-clock-goto))
  :config
  (require 'vh/org-clock-helper "vh-org-clock-helper")

  (setq org-clock-persist t
        ;; Yes it's long... but more is better ;)
        org-clock-history-length 28
        ;; Resume clocking task on clock-in if the clock is open
        org-clock-in-resume t
        ;; Save clock data in the CLOCK drawer and state changes and notes in the LOGBOOK drawer
        org-clock-into-drawer "CLOCK"
        ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks
        ;; with 0:00 duration
        org-clock-out-remove-zero-time-clocks t
        ;; Clock out when moving task to DONE
        org-clock-out-when-done t
        ;; Save the running clock and all clock history when exiting Emacs, load it on
        ;; startup
        org-clock-persist t
        ;; Enable auto clock resolution for finding open clocks
        org-clock-auto-clock-resolution 'when-no-clock-is-running
        ;; Include current clocking task in clock reports
        org-clock-report-include-clocking-task t
        ;; Change task state to NEXT from TODO when clocking in
        org-clock-in-switch-to-state 'bh/clock-in-to-next
        org-clock-modeline-total 'current)
  (org-clock-persistence-insinuate)
  (org-clock-load))

(use-package org-habit
  :config
  (setq org-habit-graph-column 80
        org-habit-show-habits-only-for-today nil))

(use-package org-mobile
  :config (setq org-mobile-directory "~/outgoing/mobileorg"))

(use-package projectile
  :config
  (projectile-register-project-type 'ant '("build.xml") "ant" "ant test")
  (projectile-register-project-type 'nodejs '("package.json") "npm --no-color build" "npm --no-color test")

  (setq projectile-project-root-files-functions '(projectile-root-top-down
                                                  projectile-root-bottom-up
                                                  projectile-root-top-down-recurring))
  (add-to-list 'projectile-project-root-files "build.xml")
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (projectile-global-mode))

(use-package counsel-projectile
  :after projectile
  :config
  (counsel-projectile-on))

(use-package ido
  :config
  (when (functionp 'ido-vertical-mode)
    (ido-vertical-mode))

  (when (functionp 'flx-ido-mode)
    (flx-ido-mode)))

(use-package escreen
  :bind (:map escreen-map
              ("l"  . escreen-display-screens)
              ("\\" . toggle-input-method))

  :config
  (require 'vh-escreen)
  (escreen-install)
  (add-hook 'escreen-goto-screen-hook 'escreen-display-screens))

(use-package javascript-mode
  :config
  (add-to-list 'compilation-error-regexp-alist '("^\\W+at\\ \\(.*\\)\\ (\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)" 2 3 4)))

(use-package recentf
  :bind (("C-x c f" . counsel-recentf)))
