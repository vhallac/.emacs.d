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

(provide 'vh/org-clock-helper)
