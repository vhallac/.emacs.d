(provide 'vtidy)

; Make it work under emacs, too. :-)
(GNUEmacs
 (progn
   (if (not (functionp 'add-local-hook))
       (defun add-local-hook (HOOK FN APPEND) (add-hook HOOK FN APPEND t)))
   (if (not (functionp 'remove-local-hook))
       (defun remove-local-hook (HOOK FN) (remove-hook HOOK FN t)))))

(setq vtidy-old-line 1)
(make-variable-buffer-local 'vtidy-old-line)

(define-minor-mode vtidy-mode
  "Toggle VTidy mode.
With no argument, this command toggles the mode.
When ARG is specified, turn on the VTidy mode if it is positive, and
off otherwise.

When VTidy mode is enabled, all whitespaces at the end of lines will be
removed as you move the cursor out of the line."
  ; Initial value
  nil
  ; Mode menu
  " VTidy"
  ; Keymap
  nil

  (if vtidy-mode
      (progn
        (setq vtidy-old-line (line-number))
        (add-local-hook 'post-command-hook 'vtidy-trim-line-hook t))
    (remove-local-hook 'post-command-hook 'vtidy-trim-line-hook)))

(defun vtidy-trim-line-hook ()
  "This function is added to post-command-hook.

It checks the end of all visited lines, and trims whitespace at the end."
  (let ((vtidy-current-line (line-number)))
    (if vtidy-old-line
        (if (/= vtidy-old-line vtidy-current-line)
            (vtidy-trim-line-end vtidy-old-line vtidy-current-line)))
    (setq vtidy-old-line vtidy-current-line)))

(defun vtidy-trim-line-end (old-line new-line)
  "Remove spaces and tabs from the end of line"
  (let ((deactivate-mark nil))
    (save-excursion
      (save-match-data
        (forward-line (- old-line new-line))
        (if (re-search-forward "\\s-+$" (point-at-eol) 't)
            (replace-match "" 't nil))))))
