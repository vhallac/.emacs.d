;; Some helper functions to perform weekly reviews. Almost all code in here
;; comes from
;; http://dl.dropboxusercontent.com/u/3968124/sacha-emacs.html#weekly-review

;; Disabling quantifiedawesome.org use:
;; (require 'quantified)

(defun sacha/quantified-get-hours (category time-summary)
  "Return the number of hours based on the time summary."
  (if (stringp category)
      (if (assoc category time-summary)
          (/ (cdr (assoc category time-summary)) 3600.0)
        0)
    (apply '+ (mapcar
               (lambda (x)
                 (sacha/quantified-get-hours x time-summary)) category))))

(defun vh/hours-to-weekly-percentage (h) (/ h 1.68))

(defun vh/percent-of (h total)
  (if (eq total 'weekly)
      (/ h 1.68)
    (if (eq 0 total) 0.0
      (/ h (* total 0.01)))))

(defun vh/make-item-prefix (&optional indent &rest type)
  (let ((prefix (concat (make-string (or indent 0) ?\ ) "- ")))
    (if (eq (car type) 'checkbox)
        (concat prefix "[" (if (cadr type) "X" " ") "] ")
      prefix)))

(defun vh/extract-items (key items &optional indent &rest item-type)
  "Make a string from the tiems that belong to given key

KEY is a string to filter the items based on car of each element
in ITEMS. INDENT is the number of spaces to add before each line
in the generated string. ITEM-TYPE is either empty, or the
'checkbox followed by CHECKED, which indicates the state of the
generated checkbox.

Example:
 (vh/extract-items \"life-next'\" next-week-items 2 'checkbox nil)"
  (let ((prefix (apply 'vh/make-item-prefix indent item-type))
        (filtered (delq nil (mapcar
                             (lambda (x) (when (equal (car x) key) (cdr x)))
                             items))))
    (let ((retval (mapconcat 'identity (sort filtered 'string<) (concat "\n" prefix))))
      (if (and retval
               (> (length retval) 0))
          (concat prefix retval "\n")
        retval))))

(defun vh/review-base-date ()
  (apply 'encode-time (org-read-date-analyze "-fri" nil '(0 0 0))))

(defun vh/get-simple-weekly-report (last-week-items next-week-items)
  (let* ((base-date (vh/review-base-date))
         (start (format-time-string "%Y-%m-%d" (days-to-time (- (time-to-number-of-days base-date) 6))))
         (end (format-time-string "%Y-%m-%d" (days-to-time (1+ (time-to-number-of-days base-date))))))

    (concat
     "- *Paid Work*\n"
     (vh/extract-items "business" last-week-items 2 'checkbox t)
     (vh/extract-items "business-next" next-week-items 2 'checkbox nil)
     "\n"
     "  - *Build*\n"
     "  - *Mentor*\n"
     "  - *Research*\n"
     "- *Social*\n"
     (vh/extract-items "social" last-week-items 2 'checkbox t)
     (vh/extract-items "social-next" next-week-items 2 'checkbox nil)
     "\n"
     "- *Discretionary*\n"
     (vh/extract-items "life" last-week-items 2 'checkbox t)
     (vh/extract-items "life-next" next-week-items 2 'checkbox nil)
     "  - *Productive*\n"
     "  - *Read*\n"
     "  - *Play*\n"
     "  - *Exercise*\n")))

(defun sacha/get-weekly-report (quantified-items last-week-items next-week-items)
  (let* ((base-date (vh/review-base-date))
         (start (format-time-string "%Y-%m-%d" (days-to-time (- (time-to-number-of-days base-date) 6))))
         (end (format-time-string "%Y-%m-%d" (days-to-time (1+ (time-to-number-of-days base-date)))))
         (biz-time (sacha/quantified-get-hours "Paid Work" quantified-items)))

    (concat
     (format "- *Paid Work* (%.1fh - %d%%)\n"
             biz-time
             (vh/percent-of biz-time 'weekly))
     (vh/extract-items "business" last-week-items 2 'checkbox t)
     (vh/extract-items "business-next" next-week-items 2 'checkbox nil)
     "\n"
     (format "  - *Build* (%.1fh - %d%% of Business)\n"
             (sacha/quantified-get-hours "Business - Build" quantified-items)
             (vh/percent-of (sacha/quantified-get-hours "Business - Build" quantified-items) biz-time))
     (format "  - *Mentor* (%.1fh - %d%% of Business)\n"
             (sacha/quantified-get-hours "Business - Mentor" quantified-items)
             (vh/percent-of (sacha/quantified-get-hours "Business - Mentor" quantified-items) biz-time))
     (format "  - *Research* (%.1fh - %d%% of Business)\n"
             (sacha/quantified-get-hours "Business - Research" quantified-items)
             (vh/percent-of (sacha/quantified-get-hours "Business - Research" quantified-items) biz-time))
     (format "- *Social* (%.1fh - %d%%)\n"
             (sacha/quantified-get-hours '("People" "Discretionary - Family") quantified-items)
             (vh/percent-of (sacha/quantified-get-hours '("People" "Discretionary - Family") quantified-items) 'weekly))
     (vh/extract-items "social" last-week-items 2 'checkbox t)
     (vh/extract-items "social-next" next-week-items 2 'checkbox nil)
     "\n"
     (format "- *Discretionary - Productive* (%.1fh - %d%%)\n"
             (sacha/quantified-get-hours "Discretionary - Productive" quantified-items)
             (vh/percent-of (sacha/quantified-get-hours "Discretionary - Productive" quantified-items) 'weekly))
     (vh/extract-items "life" last-week-items 2 'checkbox t)
     (vh/extract-items "life-next" next-week-items 2 'checkbox nil)
     (format "- *Discretionary - Read* (%.1fh)\n"
             (sacha/quantified-get-hours "Discretionary - Read" quantified-items))
     (format "- *Discretionary - Play* (%.1fh - %d%%)\n"
             (sacha/quantified-get-hours "Discretionary - Play" quantified-items)
             (vh/percent-of (sacha/quantified-get-hours "Discretionary - Play" quantified-items) 'weekly))
     (format "- *Personal routines* (%.1fh - %d%%)\n"
             (sacha/quantified-get-hours "Personal" quantified-items)
             (vh/percent-of (sacha/quantified-get-hours "Personal" quantified-items) 'weekly))
     (format "- *Unpaid work* (%.1fh - %d%%)\n"
             (sacha/quantified-get-hours "Unpaid work" quantified-items)
             (vh/percent-of (sacha/quantified-get-hours "Unpaid work" quantified-items) 'weekly))
     (format "- *Sleep* (%.1fh - %d%% - average of %.1f per day)\n"
             (sacha/quantified-get-hours "Sleep" quantified-items)
             (vh/percent-of (sacha/quantified-get-hours "Sleep" quantified-items) 'weekly)
             (/ (sacha/quantified-get-hours "Sleep" quantified-items) 7)))))

(defvar sacha/review-category-key-map
  '(("routines" "skip")
    ("business" "business")
    ("people" "people")
    ("friend" "social")
    ("family" "social")
    ("default" "life")))

(defun vh/key-from-category (category)
  (cadr (or (assoc category sacha/review-category-key-map)
            (assoc "default" sacha/review-category-key-map))))

(defun sacha/get-week-items (regexp key-suffix)
  (let ((week-tasks (list)))
    (setq string (buffer-substring-no-properties (point-min) (point-max)))
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (let* ((category (match-string 1))
               (item (match-string 2))
               (key (concat (vh/key-from-category category) key-suffix)))
          (add-to-list 'week-tasks (cons key item)))))
    week-tasks))

(defun sacha/get-last-week-items ()
  (save-window-excursion
    (org-agenda nil "w")
    (org-agenda-later -1)
    (org-agenda-log-mode 16)
    (sacha/get-week-items "^  \\([^:]+\\): +.*?\\(?:State:.*?\\(?:TODO\\|NEXT\\|DONE\\)\\|Clocked:.*DONE\\) \\(.*?\\)\\(?:[       ]+\\(:[[:alnum:]_@#%:]+:\\)\\)?[        ]*$"
                          "")))

(defun sacha/get-next-week-items ()
  (save-window-excursion
    (org-agenda nil "w")
    (sacha/get-week-items "^  \\([^:]+\\): +\\(?:Sched[^:]+: +\\)?TODO \\(.*?\\)\\(?:[      ]+\\(:[[:alnum:]_@#%:]+:\\)\\)?[        ]*$"
                          "-next")))

(defun sacha/get-quantified-data ()
  ;; Disabling use of quantified.org
  (when nil
    (let* ((base-date (vh/review-base-date))
           (start (format-time-string "%Y-%m-%d" (days-to-time (- (time-to-number-of-days base-date) 6))))
           (end (format-time-string "%Y-%m-%d" (days-to-time (1+ (time-to-number-of-days base-date))))))
      (quantified-summarize-time start end))))

(defun sacha/org-summarize-focus-areas ()
  "Summarize previous and upcoming tasks as a list."
  (interactive)
  (let ((string (vh/get-simple-weekly-report (sacha/get-last-week-items)
                                             (sacha/get-next-week-items))))
    (if (called-interactively-p 'any)
        (insert string)
      string)))

(defun sacha/org-prepare-weekly-review ()
  "Prepare weekly review template."
  (interactive)
  (let ((base-date (vh/review-base-date))
        (org-agenda-files (remove
                           (expand-file-name "~/org/refile.org")
                           org-agenda-files)))
    (insert
     (concat
      "**** Weekly review: Week ending " (format-time-string "%B %e, %Y" base-date) "  :weekly:\n"
      "*Quick Notes*\n\n"
      "*Focus areas and time review*\n\n"
      (sacha/org-summarize-focus-areas)
      "\n"))))

(provide 'vh-reviews)
