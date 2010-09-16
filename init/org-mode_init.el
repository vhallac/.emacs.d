;; Is this how we set it up?
(require 'org-install)
(require 'org-protocol)

(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\)$" . org-mode))

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

;; TODO: f keys? Maybe move to something better for consoles?
(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "<f11> I") 'bh/clock-in)
(global-set-key (kbd "<f11> O") 'bh/clock-out)

(setq
 org-clock-persist t
 org-enforce-todo-checkbox-dependencies t
 org-enforce-todo-dependencies t
 org-hide-leading-stars t
 org-log-done 'time
 org-publish-project-alist '(("org-notes-static"
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
                              :components ("org-notes" "org-notes-static")))
 org-publish-use-timestamps-flag nil
 org-return-follows-link t
 org-special-ctrl-a/e t
 org-use-fast-todo-selection t)

;; TODO sequences
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
        (sequence "WAITING(w@/!)" "SOMEDAY(s!)" "|" "CANCELLED(c@/!)")
        (sequence "QUOTE(q!)" "QUOTED(Q!)" "|" "APPROVED(A@)" "EXPIRED(E@)" "REJECTED(R@)")
        (sequence "OPEN(O)" "|" "CLOSED(C)")))

(setq org-todo-keyword-faces
      (quote (("TODO"      :foreground "red"          :weight bold)
              ("NEXT"      :foreground "blue"         :weight bold)
              ("DONE"      :foreground "forest green" :weight bold)
              ("WAITING"   :foreground "yellow"       :weight bold)
              ("SOMEDAY"   :foreground "goldenrod"    :weight bold)
              ("CANCELLED" :foreground "orangered"    :weight bold)
              ("QUOTE"     :foreground "hotpink"      :weight bold)
              ("QUOTED"    :foreground "indianred1"   :weight bold)
              ("APPROVED"  :foreground "forest green" :weight bold)
              ("EXPIRED"   :foreground "olivedrab1"   :weight bold)
              ("REJECTED"  :foreground "olivedrab"    :weight bold)
              ("OPEN"      :foreground "magenta"      :weight bold)
              ("CLOSED"    :foreground "forest green" :weight bold))))

(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-todo-state-tags-triggers
      '(("CANCELLED" ("CANCELLED" . t))
        ("WAITING" ("WAITING" . t))
        ("SOMEDAY" ("WAITING" . t))
        (done ("WAITING"))
        ("TODO" ("WAITING") ("CANCELLED"))
        ("NEXT" ("WAITING"))
        ("DONE" ("WAITING") ("CANCELLED"))))

(defun home ()
  (interactive)
  (find-file "~/org/home.org"))

(defun work ()
  (interactive)
  (find-file "~/org/work.org"))

(defun vh/refile ()
  (interactive)
  (find-file "~/org/refile.org"))

(add-hook 'org-mode-hook
	  (lambda ()
        (define-key org-mode-map [(control c) (control p)]
          '(lambda ()
             (interactive "")
             (org-publish-current-project)))
        ;; yasnippet
        (make-variable-buffer-local 'yas/trigger-key)
        (setq yas/trigger-key [tab])
        (define-key yas/keymap [tab] 'yas/next-field-group)
        ;; flyspell mode to spell check everywhere
        (flyspell-mode 1)
        (auto-fill-mode t)
        (vtidy-mode 1)))

;; Remember and org-protocol setups
(setq org-default-notes-file "~/org/refile.org")

;;;  Load Org Remember Stuff
(setq org-capture-templates
      '(("w" "Web" entry
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
        ("a" "appointment" entry
         (file+headline "~/org/appointments.org" "Appointments")
         "* %? :APPOINTMENT:\n %U")))

;; I use C-M-r to start org-capture (r for 'remember'. I may need a better key.)
(global-set-key (kbd "C-M-r") 'org-capture)

(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)

;; clocking functions

;; Resume clocking tasks when emacs is restarted
(org-clock-persistence-insinuate)

(setq org-clock-in-switch-to-state 'bh/clock-in-to-next)

;;
;; Yes it's long... but more is better ;)
(setq org-clock-history-length 28)

;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)

;; Separate drawers for clocking and logs
(setq org-drawers '("PROPERTIES" "LOGBOOK" "CLOCK"))

;; Save clock data in the CLOCK drawer and state changes and notes in the
;; LOGBOOK drawer
(setq org-clock-into-drawer "CLOCK")

;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks
;; with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)

;; Save the running clock and all clock history when exiting Emacs, load it on
;; startup
(setq org-clock-persist t)

;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution 'when-no-clock-is-running)

;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)

(setq org-time-stamp-rounding-minutes '(15 15))

;; refile functions

; Use IDO for target completion
(setq org-completion-use-ido t)

; Targets include this file and any file contributing to the agenda - up to 5
; levels deep
(setq org-refile-targets '((org-agenda-files :maxlevel . 5)
                           (nil :maxlevel . 5)))

; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path 'file)

; Targets complete in steps so we start with filename, TAB shows the next level
; of targets etc
(setq org-outline-path-complete-in-steps t)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; Agenda setup

(setq org-agenda-files '("~/org/work.org"
                         "~/org/home.org"
                         "~/org/refile.org"
                         "~/org/uki.org"))

(setq org-agenda-include-all-todo t)
(setq org-agenda-time-grid '((daily today) "----------------" (800 1000 1200 1400 1600 1800 2000)))

;; Agenda log mode items to display (clock time only by default)
(setq org-agenda-log-mode-items '(clock))

(setq org-agenda-custom-commands
      '(("w" "Tasks waiting on something" tags "WAITING/!"
         ((org-use-tag-inheritance nil)
          (org-agenda-todo-ignore-scheduled nil)
          (org-agenda-todo-ignore-deadlines nil)
          (org-agenda-todo-ignore-with-date nil)
          (org-agenda-overriding-header "Waiting Tasks")))
        ("r" "Refile New Notes and Tasks" tags "LEVEL=1+REFILE"
         ((org-agenda-todo-ignore-with-date nil)
          (org-agenda-todo-ignore-deadlines nil)
          (org-agenda-todo-ignore-scheduled nil)
          (org-agenda-overriding-header "Tasks to Refile")))
        ("N" "Notes" tags "NOTE"
         ((org-agenda-overriding-header "Notes")))
        ("n" "Next" tags-todo "-WAITING-CANCELLED/!NEXT"
         ((org-agenda-overriding-header "Next Tasks")))
        ("p" "Projects" tags-todo "LEVEL=2-REFILE|LEVEL=1+REFILE/!-DONE-CANCELLED"
         ((org-agenda-skip-function 'bh/skip-non-projects)
          (org-agenda-overriding-header "Projects")))
        ("o" "Other (Non-Project) tasks" tags-todo "LEVEL=2-REFILE|LEVEL=1+REFILE/!-DONE-CANCELLED"
         ((org-agenda-skip-function 'bh/skip-projects)
          (org-agenda-overriding-header "Other Non-Project Tasks")))
        ("A" "Tasks to be Archived" tags "LEVEL=2-REFILE/DONE|CANCELLED"
         ((org-agenda-overriding-header "Tasks to Archive")))
        ("h" "Habits" tags "STYLE=\"habit\""
         ((org-agenda-todo-ignore-with-date nil)
          (org-agenda-todo-ignore-scheduled nil)
          (org-agenda-todo-ignore-deadlines nil)
          (org-agenda-overriding-header "Habits")))
        ("#" "Stuck Projects" tags-todo "LEVEL=2-REFILE|LEVEL=1+REFILE/!-DONE-CANCELLED"
         ((org-agenda-skip-function 'bh/skip-non-stuck-projects)
          (org-agenda-overriding-header "Stuck Projects")))
        ;; TODO: The following condition is wrong. I don't have a good selector
        ;; for clockable tasks yet.
        ("c" "Select default clocking task" tags "LEVEL=2-REFILE"
         ((org-agenda-skip-function
           '(org-agenda-skip-subtree-if 'notregexp "^\\*\\* Organization"))
          (org-agenda-overriding-header "Set default clocking task with C-u C-u I")))))

(setq org-agenda-auto-exclude-function 'bh/org-auto-exclude-function)

;; Reporting and logs

;; Agenda clock report parameters (no links, 2 levels deep)
;; C-c a < a v m b R
(setq org-agenda-clockreport-parameter-plist '(:link nil :maxlevel 2))

;; Column view and estimates
;(setq org-columns-default-format "%80ITEM(Task) %TAGS(Context) %7TODO(To Do) %10Effort(Estim){:} %10CLOCKSUM{Total}")
(setq org-columns-default-format "%80ITEM(Task) %10Effort(Estim){:} %10CLOCKSUM{Total}"
      org-global-properties '(("Effort_ALL" . "0:0 0:10 0:30 1:00 2:00 3:00 4:00 8:00")))

;; Archiving

(setq org-archive-mark-done nil)



;; Helper functions

(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (let ((has-subtask)
        (subtree-end (save-excursion (org-end-of-subtree t))))
    (save-excursion
      (forward-line 1)
      (while (and (not has-subtask)
                  (< (point) subtree-end)
                  (re-search-forward "^\*+ " subtree-end t))
        (when (member (org-get-todo-state) org-todo-keywords-1)
          (setq has-subtask t))))
    has-subtask))

(defun bh/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
         (has-next (save-excursion
                     (forward-line 1)
                     (and (< (point) subtree-end)
                          (re-search-forward "^\\*+ NEXT " subtree-end t)))))
    (if (and (bh/is-project-p) (not has-next))
        nil ; a stuck project, has subtasks but no next task
      subtree-end)))

(defun bh/skip-non-projects ()
  "Skip trees that are not projects"
  (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (bh/is-project-p)
        nil
      subtree-end)))

(defun bh/skip-projects ()
  "Skip trees that are projects"
  (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (bh/is-project-p)
        subtree-end
      nil)))

;; Remove empty CLOCK drawers on clock out
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "CLOCK" (point))))

;; Change task state to NEXT from TODO when clocking in
(defun bh/clock-in-to-next (kw)
  "Switch task from TODO to NEXT when clocking in.
Skips remember/capture tasks and tasks with subtasks"
  (if (and (string-equal kw "TODO")
           (not (or (string-equal "*Remember*" (buffer-name))
                    (string-prefix-p "CAPTURE-" (buffer-name)))))

      (let ((subtree-end (save-excursion (org-end-of-subtree t)))
            (has-subtask nil))
        (save-excursion
          (forward-line 1)
          (while (and (not has-subtask)
                      (< (point) subtree-end)
                      (re-search-forward "^\*+ " subtree-end t))
            (when (member (org-get-todo-state) org-not-done-keywords)
              (setq has-subtask t))))
        (when (not has-subtask)
          "NEXT"))))

;; Use the default task to clock in whenever you clock out
(setq bh/keep-clock-running nil)

;; Not quite sure how these will be integrated. Leave them in for now.
(defun bh/clock-in ()
  (interactive)
  (setq bh/keep-clock-running t)
  (if (marker-buffer org-clock-default-task)
      (unless (org-clock-is-active)
        (bh/clock-in-default-task))
    (unless (marker-buffer org-clock-default-task)
      (org-agenda nil "c"))))

(defun bh/clock-out ()
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out)))

(defun bh/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun bh/clock-out-maybe ()
  (when (and bh/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task))
    (bh/clock-in-default-task)))

(defun bh/weekday-p ()
  (let ((wday (nth 6 (decode-time))))
    (and (< wday 6) (> wday 0))))

(defun bh/working-p ()
  (let ((hour (nth 2 (decode-time))))
    (and (bh/weekday-p) (or (and (>= hour 8) (<= hour 11))
                           (and (>= hour 13) (<= hour 16))))))

(defun bh/org-auto-exclude-function (tag)
  (and (cond
       ((string= tag "@home")
        (bh/working-p))
       ((string= tag "@office")
        (not (bh/working-p)))
       ((or (string= tag "@errand") (string= tag "phone"))
        (let ((hour (nth 2 (decode-time))))
          (or (< hour 8) (> hour 21)))))
       (concat "-" tag)))

;; I don't use this -- but set it in case I forget to specify a location in a
;; future template.
;; org-capture still uses this variable
(setq org-remember-default-headline "Tasks")

;; For windows, remember to register the org-protocol:// handler via
;; Windows Registry Editor Version 5.00
;; [HKEY_CLASSES_ROOT\org-protocol]
;; @="URL:Org Protocol"
;; "URL Protocol"=""
;; [HKEY_CLASSES_ROOT\org-protocol\shell]
;; [HKEY_CLASSES_ROOT\org-protocol\shell\open]
;; [HKEY_CLASSES_ROOT\org-protocol\shell\open\command]
;; @="\"<path-to-emacs>/bin/emacsclient.exe\" \"%1\""
