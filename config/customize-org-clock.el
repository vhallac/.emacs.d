(eval-when-compile
  (require 'org-clock))

(declare-function org-clock-out "org-clock.el")
(declare-function org-clock-in "org-clock.el")

 ;; Use the default task to clock in whenever you clock out
(defvar bh/keep-clock-running nil)

(setq
 org-clock-persist t
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

;;; clocking functions
;; Resume clocking tasks when emacs is restarted
(org-clock-persistence-insinuate)

(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)
(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)

;;; Helper functions
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
