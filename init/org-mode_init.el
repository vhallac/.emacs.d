;; Is this how we set it up?
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\)$" . org-mode))

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

;; TODO: f keys? Maybe move to something better for consoles?
(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "<f11> I") 'bh/clock-in)
(global-set-key (kbd "<f11> O") 'bh/clock-out)

(setq
 org-M-RET-may-split-line '((default))
 org-clock-in-resume t
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
(require 'remember)
(org-remember-insinuate)

;; Start clock in a remember buffer and switch back to previous clocking task on save
(add-hook 'remember-mode-hook 'org-clock-in 'append)
(add-hook 'org-remember-before-finalize-hook 'bh/clock-in-interrupted-task)

;; I use C-M-r to start org-remember
(global-set-key (kbd "C-M-r") 'org-remember)

;; Keep clocks running
(setq org-remember-clock-out-on-exit nil)

;; C-c C-c stores the note immediately
(setq org-remember-store-without-prompt t)

;; I don't use this -- but set it in case I forget to specify a location in a future template
(setq org-remember-default-headline "Tasks")

(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)

;; From:
;; http://tsdh.wordpress.com/2008/11/14/calling-org-remember-from-inside-conkeror/
;; Integrate with conkeror
;; TODO: Change to autoload
(require 'org-protocol)

(setq org-remember-templates
 '(("Web" ?w "* TODO %c\n\n%i%!" "~/org/home.org" "Firefox" nil)
   ("TODO1"  ?T "* TODO %?\n   %i\n %a" nil nil nil)
   ("TODO"  ?t "* TODO %?\n  :PROPERTIES:\n :created: %U\n :link: %a\n  :END:\n %i")
   ("note" ?n "* %? :NOTE:\n  %U\n  %a\n  :CLOCK:\n  :END:" nil bottom nil)
   ("appointment" ?a "* %?\n  %U" "~/git/org/todo.org" "Appointments" nil)))

(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)

;; For windows, remember to register the org-protocol:// handler via
;; Windows Registry Editor Version 5.00
;; [HKEY_CLASSES_ROOT\org-protocol]
;; @="URL:Org Protocol"
;; "URL Protocol"=""
;; [HKEY_CLASSES_ROOT\org-protocol\shell]
;; [HKEY_CLASSES_ROOT\org-protocol\shell\open]
;; [HKEY_CLASSES_ROOT\org-protocol\shell\open\command]
;; @="\"<path-to-emacs>/bin/emacsclient.exe\" \"%1\""

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
(setq org-clock-persist 'history)

;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution 'when-no-clock-is-running)

;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

(setq bh/keep-clock-running nil)

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
(setq org-refile-use-outline-path (quote file))

; Targets complete in steps so we start with filename, TAB shows the next level
; of targets etc
(setq org-outline-path-complete-in-steps t)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

; Use IDO only for buffers
; set ido-mode to buffer and ido-everywhere to t via the customize interface
; '(ido-mode (quote both) nil (ido))
; '(ido-everywhere t)

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
Skips remember tasks and tasks with subtasks"
  (if (and (string-equal kw "TODO")
           (not (string-equal (buffer-name) "*Remember*")))
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
  (when (and bh/keep-clock-running (not org-clock-clocking-in) (marker-buffer org-clock-default-task))
    (bh/clock-in-default-task)))

(defun bh/clock-in-interrupted-task ()
  "Clock in the interrupted task if there is one"
  (interactive)
  (let ((clock-in-to-task))
    (if (org-clock-is-active)
        (when (marker-buffer org-clock-interrupted-task)
          (if (equal org-clock-interrupted-task org-clock-hd-marker)
              (setq clock-in-to-task (cadr org-clock-history))
            (setq clock-in-to-task org-clock-interrupted-task))))
    (if clock-in-to-task
        (org-with-point-at clock-in-to-task
          (org-clock-in nil))
      (org-clock-out))))

(defun bh/weekday-p ()
  (let ((wday (nth 6 (decode-time))))
    (and (< wday 6) (> wday 0))))

(defun bh/working-p ()
  (let ((hour (nth 2 (decode-time))))
    (and (bh/weekday-p) (or (and (>= hour 8) (<= hour 11))
                           (and (>= hour 13) (<= hour 16))))))

(defun bh/network-p ()
  (= 0 (call-process "/bin/ping" nil nil nil
                     "-c1" "-q" "-t1" "norang.ca")))

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

