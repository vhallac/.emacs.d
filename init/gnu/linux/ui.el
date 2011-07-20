(defun setup-initial-frame (&optional frame)
  (when (window-system frame)
    (message "Adjusting!!!")
    (if (>= (x-display-pixel-height) 800)
        (setq initial-frame-alist '((height . 55))))))

(defun setup-frame-height (&optional frame)
  (when (window-system frame)
    ;; The calculation below is based on rough trial/error.
    (set-frame-height frame (/ (x-display-pixel-height) 19))
    ;; Emacs discards my font configuration from ~/.emacs.d/customizations.el
    ;; Adding a replica here to ensure it gets set properly.
    (set-face-attribute 'default nil :height 98 :foundry "outline" :family "Envy Code R")))

;; If not in daemon mode, just  setup te initial frame
;; Otherwise, add the hook to set the frame up
(if (not (daemonp))
    (setup-initial-frame)
  (add-hook 'after-make-frame-functions 'setup-frame-height))
