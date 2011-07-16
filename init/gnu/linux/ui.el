(defun setup-initial-frame (&optional frame)
  (when window-system
    (message "Adjusting!!!")
    (if (>= (x-display-pixel-height) 800)
        (setq initial-frame-alist '((height . 55))))))

(defun setup-frame-height (&optional frame)
  (if window-system
    (set-frame-height frame 55)))

;; If not in daemon mode, just  setup te initial frame
;; Otherwise, add the hook to set the frame up
(if (not (daemonp))
    (setup-initial-frame)
  (add-hook 'after-make-frame-functions 'setup-frame-height))
