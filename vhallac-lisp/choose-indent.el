(defun count-re-matches (strn)
  "Return number of matches STRING following the point.
      Continues until end of buffer. Also display the count as a message."
  (interactive (list (read-string "Enter string: ")))
  (save-excursion
    (goto-char (point-min))
    (let ((count -1))
      (while
          (progn
            (setq count (1+ count))
            (re-search-forward strn nil t)))
      count)))

(defun choose-indent-type ()
  "Counts the indentations with tabs and spaces, and chooses the winner."
  (interactive "")
  (let ((spaces (count-re-matches "^ +"))
        (tabs (count-re-matches "^\t+")))
    (if (> tabs spaces)
        (setq indent-tabs-mode t)
      (setq indent-tabs-mode nil))))
