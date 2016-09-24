(eval-when-compile
  (require 'org-agenda)
  (require 'cl))

(declare-function org-agenda-files "org-agenda.el")

;; Helper functions
(require 'bh/org-agenda-helper "bh-org-agenda-helper")

;; Agenda setup
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
      org-agenda-include-diary t)

(setq org-agenda-day-face-function
      (defun jd:org-agenda-day-face-holidays-function (date)
        "Compute DATE face for holidays."
        (unless (org-agenda-todayp date)
          (dolist (file (org-agenda-files nil 'ifmode))
            (let ((face
                   (dolist (entry (org-agenda-get-day-entries file date))
                     (let ((category (with-temp-buffer
                                       (insert entry)
                                       (org-get-category (point-min)))))
                       (when (or (string= "Holidays" category)
                                 (string= "Vacation" category))
                         (return 'org-agenda-date-weekend))))))
              (when face (return face)))))))


;; Search all my org files
;; `recursive-directory-list' comes from ~/.emacs.d/loadpaths.el
(setq org-agenda-text-search-extra-files
  (apply #'append (mapcar (lambda (dir)
          (directory-files dir t ".*\\.org$"))
        (recursive-directory-list "~/org"))))

;; Do not duplicate agenda files in extra files
(mapc (lambda (agenda-file)
        (setq org-agenda-text-search-extra-files
              (delete agenda-file org-agenda-text-search-extra-files)))
      org-agenda-files)

;; Monkey patch agenda dimmed task function to skip tasks blocked by checkboxes
(defadvice org-agenda-dim-blocked-tasks (around vh/org-agenda-dont-dim-checkbox-blocks activate)
  (let ((org-blocker-hook org-blocker-hook))
    (remove-hook 'org-blocker-hook 'org-block-todo-from-checkboxes)
    ad-do-it))
