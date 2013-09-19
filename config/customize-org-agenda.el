(eval-when-compile
  (require 'org-agenda)
  (require 'cl))

(declare-function org-agenda-files "org-agenda.el")

;; Agenda setup

(setq org-agenda-files (mapcar #'expand-file-name
                               '("~/org/work.org"
                                 "~/org/home.org"
                                 "~/org/refile.org")))

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

(setq org-agenda-include-diary t)

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
