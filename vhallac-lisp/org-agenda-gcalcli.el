(defconst vh--gcalcli-executable "/usr/bin/gcalcli")

(defvar vh--calendar-config-paths '(("gcal.pers" . "/home/vedat/.cache/gcalcli.pers")
                                    ("gcal.work" . "/home/vedat/.cache/gcalcli.work")))

(defvar vh--use-am-pm nil)

(defvar  vh--gcalcli-cache-invalidate-seconds (* 60  60))

(defvar vh--gcalcli-agenda-cache nil)

(defun vh/agenda-find-date-end (&rest point)
  (save-excursion
    (goto-char (or point (point)))
    (let ((day (org-get-at-bol 'day)))
      (while (eq day (get-text-property (+ 1 (point-at-eol)) 'day))
        (forward-line)))
    (1+ (point-at-eol))))

(defun vh/agenda-goto-next-date ()
  (interactive)
  (goto-char (1+ (vh/agenda-find-date-end))))

(defun vh/agenda-narrow-to-day ()
  (interactive)
  (narrow-to-region (point-at-bol) (vh/agenda-find-date-end)))

(defun vh/append-to-day-reports ()
  ;; When clocking in, agenda narrows to single line, and finalize is called
  ;; Do not execute hook when this happens.
  ;; Alternative - widen, do your thing, and narrow back
  (when (not (and (eq (point-at-bol) (point-min))
                  (eq (point-at-eol) (point-max))))
    (when (org-agenda-check-type nil 'agenda)
      (save-excursion
        (goto-char (point-min))
        (vh/agenda-goto-next-date)
        (while (< (point) (point-max))
          (vh/agenda-narrow-to-day)
          (goto-char (point-max))
          (let ((start (point-max))
                (day (get-text-property (point-min) 'day)))
            (insert  (vh--get-gcalcli-org-agenda-of-day-cached day))
            (add-text-properties start (point) (list 'org-agenda-type 'agenda 'day day)))
          (widen)
          (goto-char (1+ (point))))))))

(defun vh--get-gcalcli-org-agenda-of-day-cached (date)
  (let ((agenda-entry (assoc date vh--gcalcli-agenda-cache)))
    (when (not agenda-entry)
      (setq agenda-entry (list date (time-to-seconds) nil))
      (setq vh--gcalcli-agenda-cache (cons agenda-entry vh--gcalcli-agenda-cache)))
    (when (or  (> (- (time-to-seconds) (cadr agenda-entry)) vh--gcalcli-cache-invalidate-seconds)
               (not  (caddr agenda-entry)))
      (setcar (cddr agenda-entry)  (vh--get-gcalcli-org-agenda-of-day date)))
    (caddr agenda-entry)))

(defun vh--get-gcalcli-org-agenda-of-day (date)
  (let* ((result (mapconcat (lambda (cfg)
                              (let ((agenda-str (vh--get-gcalcli-agenda-for date (cdr cfg))))
                                (if (string-match-p "^Traceback " agenda-str)
                                    nil
                                  (replace-regexp-in-string "^"
                                                            (format "  %-12s" (concat (car cfg) ": "))
                                                            (vh--line-trim agenda-str)))))
                            vh--calendar-config-paths "\n"))
         (trimmed (vh--line-trim result)))
    (if (and  trimmed (> (length trimmed) 0))
        (concat trimmed "\n" )
      "")))

(defun vh--line-trim (str)
  (replace-regexp-in-string "^\\(?:\\s-*\n+\\)+\\|\n+\\'" "" str))

(defun vh--get-gcalcli-agenda-for (date &optional config-path)
  (let* ((date-start (apply 'format "%d/%d/%d" (calendar-gregorian-from-absolute date)))
         (date-end (concat date-start " 12pm"))
         (gcalcli-params (concat (when config-path
                                   (concat " --configFolder=" config-path))
                                 " --nocolor "
                                 (if vh--use-am-pm "--nomilitary" "--military")
                                 " agenda"
                                 " '" date-start "' '" date-end "'"
                                 " | grep -vi 'no events found'"
                                 " | cut -c13-")))
    (let ((coding-system-for-read 'utf-8))
      (shell-command-to-string (concat vh--gcalcli-executable gcalcli-params)))))

(provide 'org-agenda-gcalcli)
