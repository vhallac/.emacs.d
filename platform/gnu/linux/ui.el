(defun setup-initial-frame (&optional frame)
  (when (window-system frame)
    (message "Adjusting!!!")
    (setup-ui-face frame)
    (if (>= (x-display-pixel-height) 800)
        (setq initial-frame-alist '((height . 55))))))

(defun setup-ui-frame (&optional frame)
  (when (window-system frame)
    ;; The calculation below is based on rough trial/error.
    (set-frame-height frame (/ (x-display-pixel-height) 19))
    (setup-ui-face frame)))

(defun setup-ui-face (&optional frame)
    ;; Emacs discards my font configuration from ~/.emacs.d/customizations.el
    ;; Adding a replica here to ensure it gets set properly.
  (when frame (select-frame frame))
  (set-face-font 'default "Envy Code R")
  (set-face-attribute 'default nil :height 98))

;; If not in daemon mode, just  setup te initial frame
;; Otherwise, add the hook to set the frame up
(if (not (daemonp))
    (setup-initial-frame)
  (add-hook 'after-make-frame-functions 'setup-ui-frame))
