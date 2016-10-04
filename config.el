
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



;; Configure individual packages here

(use-package ido
  :disabled
  :config
  (when (functionp 'ido-vertical-mode)
    (ido-vertical-mode))

  (when (functionp 'flx-ido-mode)
    (flx-ido-mode)))

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
  :bind ("C-s" . swiper)
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
  :defer
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
  :bind (("C-c e" . vh-eval-and-replace)))

(use-package magit
  :bind (("C-c g" . magit-status)))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'post-forward
                uniquify-separator ":"))


(use-package lua-mode
  :commands lua-mode
  :config
  (add-hook 'lua-mode-hook
            '(lambda ()
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

(use-package projectile
  :config
  (projectile-register-project-type 'ant '("build.xml") "ant" "ant test")
  (add-to-list 'projectile-project-root-files "build.xml")
  (projectile-register-project-type 'nodejs '("package.json") "npm --no-color build" "npm --no-color test")
  (add-to-list 'projectile-globally-ignored-directories "node_modules")

  (setq projectile-project-root-files-functions '(projectile-root-top-down
                                                  projectile-root-bottom-up
                                                  projectile-root-top-down-recurring))
  (projectile-global-mode))

(use-package counsel-projectile
  :after projectile
  :config
  (counsel-projectile-on))

(use-package escreen
  :bind (:map escreen-map
              ("l"  . escreen-display-screens)
              ("\\" . toggle-input-method))

  :config
  (require 'vh-escreen)
  (escreen-install)
  (add-hook 'escreen-goto-screen-hook 'escreen-display-screens))

(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :config
  (yas-reload-all)
  (add-hook 'js2-mode-hook #'yas-minor-mode-on)
  ;; This is for jasmine output. But not enough
  (add-to-list 'compilation-error-regexp-alist '("^\\W+at\\(.*\\)\\ (\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)" 2 3 4)))


(use-package org
  :defer
  :config
  (setq org-enforce-todo-checkbox-dependencies t
        org-enforce-todo-dependencies t
        org-hide-leading-stars t
        org-log-done 'time
        org-log-reschedule 'note
        org-log-redeadline 'note
        org-log-into-drawer "LOGBOOK"
        org-return-follows-link t
        org-special-ctrl-a/e t
        org-use-fast-todo-selection t
        ;; TODO sequences
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
                            (sequence "WAITING(w@/!)" "|" "HOLD(h@/!)"
                                      "CANCELLED(c@/!)" "PHONE" "MEETING")
                            (sequence "QUOTE(q!)" "QUOTED(Q!)" "|"
                                      "APPROVED(A@)" "EXPIRED(E@)" "REJECTED(R@)")
                            (sequence "OPEN(O)" "|" "CLOSED(C)"))
        org-todo-keyword-faces '(("TODO"      :foreground "red"          :weight bold)
                                 ("NEXT"      :foreground "blue"         :weight bold)
                                 ("DONE"      :foreground "forest green" :weight bold)
                                 ("WAITING"   :foreground "yellow"       :weight bold)
                                 ("HOLD"      :foreground "goldenrod"    :weight bold)
                                 ("CANCELLED" :foreground "orangered"    :weight bold)
                                 ("PHONE"     :foreground "forest green" :weight bold)
                                 ("MEETING"   :foreground "forest green" :weight bold)
                                 ("QUOTE"     :foreground "hotpink"      :weight bold)
                                 ("QUOTED"    :foreground "indianred1"   :weight bold)
                                 ("APPROVED"  :foreground "forest green" :weight bold)
                                 ("EXPIRED"   :foreground "olivedrab1"   :weight bold)
                                 ("REJECTED"  :foreground "olivedrab"    :weight bold)
                                 ("OPEN"      :foreground "magenta"      :weight bold)
                                 ("CLOSED"    :foreground "forest green" :weight bold))
        org-todo-state-tags-triggers '(("CANCELLED" ("CANCELLED" . t))
                                       ("WAITING" ("WAITING" . t))
                                       ("HOLD" ("WAITING" . t) ("HOLD" . t))
                                       (done ("WAITING") ("HOLD"))
                                       ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                                       ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                                       ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))
        org-treat-S-cursor-todo-selection-as-state-change nil
        ;; Targets include this file and any file contributing to the agenda -
        ;; up to 5 levels deep
        org-refile-targets '((org-agenda-files :maxlevel . 5)
                             (nil :maxlevel . 5))
        ;; Targets start with the file name - allows creating level 1 tasks
        org-refile-use-outline-path 'file
        ;; Targets complete in steps so we start with filename, TAB shows the
        ;; next level of targets etc
        org-outline-path-complete-in-steps t
        ;; Allow refile to create parent tasks with confirmation
        org-refile-allow-creating-parent-nodes 'confirm
        ;; Column view and estimates
        org-columns-default-format "%80ITEM(Task) %7TODO(To Do) %10Effort(Estim){:} %10CLOCKSUM{Total}"
        org-global-properties '(("Effort_ALL" . "0:0 0:10 0:30 1:00 2:00 3:00 4:00 8:00"))
        org-time-clocksum-format '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)
        ;; Mark a task as DONE when archiving
        org-archive-mark-done nil
        org-src-fontify-natively t
        org-time-clocksum-use-effort-durations t)
  (unbind-key "C-c ;" org-mode-map)
  (unbind-key "C-c C-x C-s" org-mode-map)
  (add-to-list 'org-modules 'org-habit)
  (flyspell-mode 1)
  (auto-fill-mode t))

(use-package org-agenda
  :bind (("C-c a" . org-agenda))
  :config
  (require 'org-helper)

  (setq org-agenda-start-on-weekday 6 ;Weeks start on saturday (for review purposes)

        org-agenda-include-all-todo t
        org-agenda-time-grid '((daily today) "----------------" (800 1000 1200 1400 1600 1800 2000))

        ;; Agenda log mode items to display (clock time only by default)
        org-agenda-log-mode-items '(clock)

        org-agenda-custom-commands
        '(("u" "Unscheduled" todo ""
           ((org-agenda-todo-ignore-scheduled t)))
          ("N" "Notes" tags "NOTE"
           ((org-agenda-overriding-header "Notes")
            (org-tags-match-list-sublevels t)))
          ("h" "Habits" tags-todo "STYLE=\"habit\""
           ((org-agenda-overriding-header "Habits")
            (org-agenda-sorting-strategy
             '(todo-state-down effort-up category-keep))))
          (" " "Agenda"
           ((agenda "" nil)
            (tags "REFILE"
                  ((org-agenda-overriding-header "Tasks to Refile")
                   (org-tags-match-list-sublevels nil)))
            (tags-todo "-CANCELLED/!"
                       ((org-agenda-overriding-header "Stuck Projects")
                        (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                        (org-agenda-sorting-strategy
                         '(priority-down category-keep))))
            (tags-todo "-HOLD-CANCELLED/!"
                       ((org-agenda-overriding-header "Projects")
                        (org-agenda-skip-function 'bh/skip-non-projects)
                        (org-agenda-sorting-strategy
                         '(priority-down category-keep))))
            (tags-todo "-CANCELLED/!NEXT"
                       ((org-agenda-overriding-header "Project Next Tasks")
                        (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                        (org-tags-match-list-sublevels t)
                        (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-sorting-strategy
                         '(priority-down todo-state-down effort-up category-keep))))
            (tags-todo "-REFILE-CANCELLED-WAITING/!"
                       ((org-agenda-overriding-header (if (marker-buffer org-agenda-restrict-begin) "Project Subtasks" "Standalone Tasks"))
                        (org-agenda-skip-function 'bh/skip-project-tasks-maybe)
                        (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-sorting-strategy
                         '(category-keep)))
                       (tags-todo "-CANCELLED+WAITING/!"
                                  ((org-agenda-overriding-header "Waiting and Postponed Tasks")
                                   (org-agenda-skip-function 'bh/skip-stuck-projects)
                                   (org-tags-match-list-sublevels nil)
                                   (org-agenda-todo-ignore-scheduled 'future)
                                   (org-agenda-todo-ignore-deadlines 'future))))
            (tags "-REFILE/"
                  ((org-agenda-overriding-header "Tasks to Archive")
                   (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                   (org-tags-match-list-sublevels nil))))
           nil)
          ("r" "Tasks to Refile" tags "REFILE"
           ((org-agenda-overriding-header "Tasks to Refile")
            (org-tags-match-list-sublevels nil)))
          ("#" "Stuck Projects" tags-todo "-CANCELLED/!"
           ((org-agenda-overriding-header "Stuck Projects")
            (org-agenda-skip-function 'bh/skip-non-stuck-projects)))
          ("n" "Next Tasks" tags-todo "-WAITING-CANCELLED/!NEXT"
           ((org-agenda-overriding-header "Next Tasks")
            (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
            (org-tags-match-list-sublevels t)
            (org-agenda-sorting-strategy
             '(todo-state-down effort-up category-keep))))
          ("R" "Tasks" tags-todo "-REFILE-CANCELLED/!-HOLD-WAITING"
           ((org-agenda-overriding-header "Tasks")
            (org-agenda-skip-function 'bh/skip-project-tasks-maybe)
            (org-agenda-sorting-strategy
             '(category-keep))))
          ("p" "Projects" tags-todo "-HOLD-CANCELLED/!"
           ((org-agenda-overriding-header "Projects")
            (org-agenda-skip-function 'bh/skip-non-projects)
            (org-agenda-sorting-strategy
             '(category-keep))))
          ("W" "Waiting Tasks" tags-todo "-CANCELLED+WAITING/!"
           ((org-agenda-overriding-header "Waiting and Postponed tasks"))
           (org-tags-match-list-sublevels nil))
          ("A" "Tasks to Archive" tags "-REFILE/"
           ((org-agenda-overriding-header "Tasks to Archive")
            (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
            (org-tags-match-list-sublevels nil)))
          ("w" "Weekly review" agenda ""
           ((org-agenda-span 7) (org-agenda-log-mode 1))))

        org-agenda-auto-exclude-function 'bh/org-auto-exclude-function

        ;; Reporting and logs

        ;; Agenda clock report parameters (no links, 2 levels deep)
        ;; C-c a < a v m b R

        org-agenda-clockreport-parameter-plist '(:link nil :maxlevel 2)
        org-agenda-day-face-function #'jd:org-agenda-day-face-holidays-function
        org-agenda-include-diary t)

  ;; Monkey patch agenda dimmed task function to skip tasks blocked by checkboxes
  (defadvice org-agenda-dim-blocked-tasks (around vh/org-agenda-dont-dim-checkbox-blocks activate)
    (let ((org-blocker-hook org-blocker-hook))
      (remove-hook 'org-blocker-hook 'org-block-todo-from-checkboxes)
      ad-do-it))

  ;; TODO: The code should be part of local config

    ;; Search all my org files
  ;; `recursive-directory-list' comes from ~/.emacs.d/loadpaths.el
  (setq org-agenda-text-search-extra-files
        (apply #'append (mapcar (lambda (dir)
                                  (directory-files dir t ".*\\.org$"))
                                (recursive-directory-list "~/org")))   )

  ;; Do not duplicate agenda files in extra files
  (mapc (lambda (agenda-file)
          (setq org-agenda-text-search-extra-files
                (delete agenda-file org-agenda-text-search-extra-files)))
        org-agenda-files))

(use-package ox
  :after org
  :bind (:map org-mode-map
              ("C-c C-p" . org-publish-current-project))
  :config
  (setq  org-publish-project-alist '(("org-notes-static"
                                      :base-directory "~/org/notes"
                                      :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
                                      :publishing-directory "~/org-publish/"
                                      :recursive t
                                      :publishing-function org-publish-attachment)
                                     ("org-notes"
                                      :base-directory "~/org/notes"
                                      :base-extension "org"
                                      :publishing-directory "~/org-publish/")
                                     ("org-notes-all"
                                      :components ("org-notes" "org-notes-static"))

                                     )
         org-publish-use-timestamps-flag nil
         org-html-head-extra (concat "<style type=\"text/css\">"
                                     "<!--/*--><![CDATA[/*><!--*/"
                                     "pre.src {overflow-x: auto; }"
                                     ".src { background-color: #f5deb3; color: #black;}"
                                     "/*]]>*/-->"
                                     "</style>")))

(use-package ob                         ;org-babel
  :after org
  :defer
  :config
  (setq org-babel-min-lines-for-block-output 999
        org-babel-results-keyword "results")

  (org-babel-do-load-languages 'org-babel-load-languages '((ledger . t))))

(use-package org-capture
  :bind (("C-c c" .  org-capture))
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
                                  "%(subst-char-in-string ?- ?/ (org-read-date)) * %^{Payee}\n  Assets:Cash\n  Expenses:%^{Account}  %^{Amount}\n"))))

(use-package org-clock
  :bind (("C-c C-x C-j" . org-clock-goto))
  :config
  (require 'org-helper)

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
  :after org-agenda
  :defer
  :config
  (setq org-habit-graph-column 80
        org-habit-show-habits-only-for-today nil))

(use-package org-mobile
  :commands (org-mobile-push org-mobile-pull)
  :config (setq org-mobile-directory "~/outgoing/mobileorg"))

(use-package gnus
  :commands gnus
  :config
  (setq gnus-select-method '(nntp "news.easynews.com")
        gnus-posting-styles '(((message-news-p)
                               (name "Vedat Hallac")
                               (address "vedat.hallac@mail.invalid"))
                              ("gmail-2"
                               (name "Dys@Bloodfeather")
                               (address "dys.wowace@gmail.com"))
                              ("gmail-android"
                               (name "Vedat Hallac")
                               (address "vedat@android.ciyiz.biz"))
                              ("gmail-pia"
                               (name "Vedat Hallaç")
                               (address "vedat.hallac@pia-team.com")))

        gnus-secondary-select-methods '( (nnimap "gmail-1"
                                                 (nnimap-address "imap.gmail.com")
                                                 (nnimap-server-port 993)
                                                 (nnimap-stream tls)
                                                 (nnimap-list-pattern ("INBOX" "*"))
                                                 (nnimap-expunge-on-close always)
                                                 (gnus-check-new-newsgroups nil)
                                                 (gnus-ignored-newsgroups "^to\\.\\|^[0-9. 	]+\\( \\|$\\)\\|^[\”]\”[#’()]"))
                                         (nnimap "gmail-2"
                                                 (nnimap-address "imap.gmail.com")
                                                 (nnimap-server-port 993)
                                                 (nnimap-stream tls)
                                                 (nnimap-list-pattern ("INBOX" "*"))
                                                 (nnimap-expunge-on-close always)
                                                 (gnus-check-new-newsgroups nil)
                                                 (gnus-ignored-newsgroups "^to\\.\\|^[0-9. 	]+\\( \\|$\\)\\|^[\”]\”[#’()]"))
                                         (nnimap "gmail-android"
                                                 (nnimap-address "imap.gmail.com")
                                                 (nnimap-server-port 993)
                                                 (nnimap-stream tls)
                                                 (nnimap-list-pattern ("INBOX" "*"))
                                                 (nnimap-expunge-on-close always)
                                                 (gnus-check-new-newsgroups nil)
                                                 (gnus-ignored-newsgroups "^to\\.\\|^[0-9. 	]+\\( \\|$\\)\\|^[\”]\”[#’()]"))
                                         (nnimap "gmail-pia"
                                                 (nnimap-address "imap.gmail.com")
                                                 (nnimap-server-port 993)
                                                 (nnimap-stream tls)
                                                 (nnimap-list-pattern ("INBOX" "*"))
                                                 (nnimap-expunge-on-close always)
                                                 (gnus-check-new-newsgroups nil)
                                                 (gnus-ignored-newsgroups "^to\\.\\|^[0-9. 	]+\\( \\|$\\)\\|^[\”]\”[#’()]")))

        gnus-use-adaptive-scoring '(word line)

        ;; TODO: Move to use-package gnus-score?
        gnus-score-expiry-days 60
        gnus-default-adaptive-score-alist '((gnus-unread-mark)
                                            (gnus-ticked-mark (from 40))
                                            (gnus-dormant-mark (from 50))
                                            (gnus-saved-mark (from 200) (subject 50))
                                            (gnus-del-mark (from -20) (subject -50))
                                            (gnus-read-mark (from 20) (subject 10))
                                            (gnus-killed-mark (from -10) (subject -30)))

        gnus-topic-line-format "%i[ %0{%(%n (new: %A)%)%} ]\n"
        mail-self-blind t               ; Add me to Bcc:
        mail-user-agent 'gnus-user-agent  ; Allow Gcc:

        ;; Work-around for GMail's internal folders: When the IMAP folder contains
        ;; characters [ and ] (actually any regexp character), the function
        ;; `gnus-score-find-bnews' cannot return the ADAPT file name. This causes ADAPT
        ;; files to be generated, but not used in these groups.
        ;; The following setting ensures these two characters are never used in ADAPT
        ;; file names.
        nnheader-file-name-translation-alist '((?[ . ?_) (?] . ?_))
        ;; see bbdb-mua-summary-unify-format-letter configuration for bbdb for uB
        gnus-summary-line-format "%U%R%z%I%(%[%4L: %-23,23uB%]%) %s\n"
        )

  (when (require 'bbdb nil t)
    (bbdb-initialize 'gnus)
    (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus))

  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode))

(use-package bbdb
  :defer
  :config
  (setq bbdb-update-records-p 'query
        bbdb-mua-update-interactive-p '(search . query)
        ;; bbdb-accept-message-alist '( ("From" . "@pia-team\.com")
        ;;                              ("From" . "@\\(?:milleni\\|turkcell\\)\.com\.tr"))
        bbdb/gnus-score-default 10
        ;; use %uB for names in gnus-summary-line-format configuration
        bbdb-mua-summary-unify-format-letter "B" ))

(use-package tls
  :defer
  :config
  (setq tls-program `(,(concat
                      (if (boundp 'openssl-prg)
                          openssl-prg
                        "openssl")
                      " s_client -connect %h:%p -no_ssl2 -ign_eof")))
  ;; TODO: Can I send mail without running gnus first?
  (require 'smtp-openssl))

(use-package mm-decode
  :defer
  :config
  (setq mm-text-html-renderer 'w3m
        mm-inline-text-html-with-images t
        mm-w3m-safe-url-regexp nil
        mm-inline-large-images t
        mm-coding-system-priorities '(utf-8)) )

(use-package smtpmail
  :defer
  :config
  (require 'email-helper)

  (setq mail-host-address "hallac.net"
        smtp-accounts '( (ssl "vedathallac@gmail.com" "gmail-1" "smtp.googlemail.com" 587)
                         (ssl "dys.wowace@gmail.com" "gmail-2" "smtp.googlemail.com" 587)
                         (ssl "vedat@android.ciyiz.biz" "gmail-android" "smtp.googlemail.com" 587)
                         (ssl "vedat.hallac@pia-team.com" "gmail-pia" "smtp.googlemail.com" 587)
                         (ssl "vedat@hallac.net" "hallac-net" "smtp.yandex.com" 587))))

(use-package message
  :bind (:map message-mode-map
         ("C-c o" . vh/message-edit-body-as-org)
         ("C-c h" . vh/message-org-to-html))
  :defer
  :config
  (setq message-alternative-emails (regexp-opt '("vedathallac@gmail.com"
                                                 "vedat.hallac@gmail.com"
                                                 "dys.wowace@gmail.com"
                                                 "vedat@android.ciyiz.biz"
                                                 "vedat@oyun.cuyuz.biz"
                                                 "vedathallac@yandex.com"
                                                 "vedat@hallac.net"
                                                 "vedat.hallac@pia-team.com")))

  (require 'email-helper)
  (require 'smtpmail)
  (when (require 'bbdb nil t)
    (bbdb-initialize 'message)
    (bbdb-insinuate-message)

    (setq bbdb-mua-pop-up nil
          bbdb-complete-mail-allow-cycling t)))

(use-package cc-mode
  :bind (:map c-mode-map
              ("C-c C-f" . c-helper-find-file)
              ("C-c C-v" . c-helper-find-include-file))
  :config
  (require 'c-helper)
  (setq c-echo-syntactic-information-p t
        c-electric-pound-behavior '(alignleft)
        ;; c-font-lock-extra-types '("FILE" "\\<[A-Z]+_[A-Z]*[a-z]+\\sw*\\>" "\\<[A-Za-z]\\sw*[a-z]\\sw*T\\>" "\\sw+_\\(\\|s\\)t" "\\<\\(u\\|\\)int\\(8\\|16\\|32\\|64\\)\\>" "THR_HANDLE" "\\sw+_STATUS" "bool" "true" "false" "BOOL" "TRUE" "FALSE")
        c-indent-comments-syntactically-p t
        ;;c-macro-cppflags "-I../include -I../../include -I../../../include -I ../../../../include -I../../../../../include"
        ;;c-macro-preprocessor "e:\\cygwin\\lib\\gcc-lib\\i686-pc-cygwin\\2.95.2\\cpp -C"
        c-macro-shrink-window-flag t)
  (add-hook 'c-mode-common-hook (lambda ()
                                  (auto-fill-mode t)
                                  (c-toggle-auto-hungry-state 1)
                                  (auto-complete-mode)))
  (add-hook 'c-mode-hook (lambda ()
                           (c-set-style "tda")))
  (add-hook 'java-mode-hook (lambda ()
                              (c-set-style "java-custom")))
  )

(use-package yasnippet
  :commands (yas-minor-mode yas-global-mode yas-reload-all))

(use-package midnight
  :config
    ;; run clean-buffer-list every 2 hours
  (defvar clean-buffer-list-timer (run-at-time t 7200 'clean-buffer-list)
    "Stores clean-buffer-list timer if there is one.

You can disable clean-buffer-list by (cancel-timer clean-buffer-list-timer).")

  ;; kill buffers if they were last disabled more than 15 minutes ago
  (setq clean-buffer-list-delay-special 900


        ;; kill everything, clean-buffer-list is very intelligent at not killing
        ;; unsaved buffer.
        clean-buffer-list-kill-regexps '("^.*$")

        ;; keep these buffer untouched
        clean-buffer-list-kill-never-buffer-names '("*Messages*" "*cmd*" "*scratch*"
                                                    "*w3m*" "*w3m-cache*"
                                                    "*Group*")
        clean-buffer-list-kill-never-regexps '("^\\*EMMS Playlist\\*.*$"
                                               "^\\*Article "
                                               "^\\*Summary "
                                               ".*\\.org")))
(use-package slime
  :config
  (add-to-list 'lisp-mode-hook 'slime-mode)

  (slime-setup)
  (add-to-list slime-lisp-implementations `((sbcl ("sbcl"))
                                   (cmucl ("lisp"))
                                   (openmcl ("openmcl"))
                                   (s48 ("scheme48") :init slime48-init-command)
                                   (s48-large ("scheme48" "-h" "80000000")
                                              :init slime48-init-command)
                                   (abcl ("abcl"))))
  ;;  (setq inferior-lisp-program "sbcl")
  )

(use-package ruby-mode
  :bind (:map ruby-mode-map
              ("C-x C-t" . ruby-compilation-this-rspec)
              ;;("C-c C-d" . yari-anything)
              ("#" . ruby-electric-strparam)
              ("C-M-u" . ruby-goto-containing-block-start)
              ("C-c b" . ruby-flip-containing-block-type))
  :config
  (require 'ruby-helper)
  ;;(autoload 'ruby-electric-mode "ruby-electric.el")
  ;;(autoload 'rinari-launch "rinari.el")
  ;;(autoload 'yari-anything "yari.el")
  (autoload 'word-at-point "thingatpt.el")

  ;;(require 'anything)
  (require 'auto-complete-config)
  ;;(require 'ruby-compilation-rspec)

  (require 'align)
  (defconst align-ruby-modes '(ruby-mode))
  (defconst ruby-align-rules-list
    '((ruby-comma-delimiter
       (regexp . ",\\(\\s-*\\)[^/ \t\n]")
       (modes  . align-ruby-modes)
       (repeat . t))
      (ruby-symbol-after-func
       (regexp . "^\\s-*\\w+\\(\\s-+\\):\\w+")
       (modes  . align-ruby-modes))))
  (add-to-list 'align-perl-modes 'ruby-mode)
  (add-to-list 'align-dq-string-modes 'ruby-mode)
  (add-to-list 'align-sq-string-modes 'ruby-mode)
  (add-to-list 'align-open-comment-modes 'ruby-mode)
  (dolist (it ruby-align-rules-list)
    (add-to-list 'align-rules-list it))

  (add-hook 'ruby-mode-hook (lambda ()
                              ;; (rinari-launch)
                              ;;(ruby-electric-mode t)
                              (auto-complete-mode t)
                              ;; Auto-complete fixups
                              (make-local-variable 'ac-ignores)
                              (add-to-list 'ac-ignores "end"))))

(use-package rspec-mode
  :commands rspec-mode
  :config
  (autoload 'ruby-electric-mode "ruby-electric.el")

  (add-hook 'rspec-mode-hook (lambda ()
                               ;;(require 'rinari)
                               ;;(require 'ruby-compilation-rspec)
                               (require 'auto-complete-config)
                               ;;(ruby-electric-mode t)
                               (auto-complete-mode t)
                               ;; Auto-complete fixups
                               (make-local-variable 'ac-ignores)
                               (add-to-list 'ac-ignores "end"))))

(use-package quantified
  :commands (quantified-text quantified-track quantified-share-org-subtree quantified-summarize-time)
  :config
  ;; Load my passwords so that I can login to quantified awesome
  (require 'secrets))

(use-package multiple-cursors
  :bind ( ("C-c m l" . mc/edit-lines)
          ("C-c m m" . mc/mark-more-like-this-extended)
          ("C-c m p" . mc/mark-previous-word-like-this)
          ("C-c m n" . mc/mark-next-word-like-this)
          ("C-c m P" . mc/mark-previous-symbol-like-this)
          ("C-c m N" . mc/mark-next-symbol-like-this)
          ("C-c m i" . mc/insert-numbers)
          ("C-c m s" . mc/mark-all-symbols-like-this-in-defun)
          ("C-c m S" . mc/mark-all-symbols-like-this)
          ("C-c m w" . mc/mark-all-symbols-like-this-in-defun)))
