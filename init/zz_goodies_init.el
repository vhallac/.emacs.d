(GNUEmacs
 ;From locate.el locate-current-line-number
 (defun line-number ()
   "Return the current line number in current buffer."
   (+ (count-lines (point-min) (point))
      (if (eq (current-column) 0)
          1
        0)))
 (defun int-to-char (X) X)
 (defun set-buffer-tag-table (FILE)
   (visit-tags-table FILE t)))

; Default frame position
(if window-system
    (progn
      (GNUEmacs
       (set-cursor-color "light green"))))

; Lazy mode on: Type y or n instead of full "yes" or "no"
(fset 'yes-or-no-p 'y-or-n-p)

(add-hook 'after-make-frame-functions
          (lambda (frame)
             (when window-system
               (scroll-bar-mode -1)
               (set-cursor-color "light green"))
             (blink-cursor-mode -1)
             (setq transient-mark-mode t)
             (menu-bar-mode -1)
             (tool-bar-mode -1)))

; Make sure the hooks are run if we are not in daemon mode
(if (not (daemonp))
    (add-hook 'after-init-hook
              (lambda ()
                (run-hook-with-args 'after-make-frame-functions
                                    (selected-frame)))))

; Experimental setup extras
(GNUEmacs
 ;; no splash screen:
 (setq inhibit-startup-message t)
 (setq inhibit-splash-screen t)

 ;; Interactively do things
 (ido-mode t))

(defadvice kill-ring-save (before slick-copy activate compile) "When called
  interactively with no active region, copy a single line instead."
  (interactive (if mark-active (list (region-beginning) (region-end)) (message
  "Copied line") (list (line-beginning-position) (line-beginning-position
  2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
        (line-beginning-position 2)))))

 (defun insert-date ()
   "Inserts the current date at point"
   (interactive)
   (insert (format-time-string "%d/%m/%Y")))

;; Do not save duplicate kills into the kill ring
(setq kill-do-not-save-duplicates t)

;; Tab either indents, or does completion
(setq tab-always-indent 'complete)

;; browse-kill-ring mode for easy kill-ring history handling
;; Use M-y to paste from history
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

(add-hook 'before-save-hook
          'delete-trailing-whitespace)
