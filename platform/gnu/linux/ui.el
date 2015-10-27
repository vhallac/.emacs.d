(defun setup-initial-frame (&optional frame)
  (when (window-system frame)
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
  (let* ((ydpi (/ (* 25 (x-display-pixel-height)) (x-display-mm-height)))
         (height (if (> ydpi 100) 125 105)))
    (set-face-attribute 'default nil :height height)))

;; If not in daemon mode, just  setup te initial frame
;; Otherwise, add the hook to set the frame up
(if (not (daemonp))
    (setup-initial-frame)
  (add-hook 'after-make-frame-functions 'setup-ui-frame))

(defun wg/kludge-gpg-agent ()
  ;; For this to work, after you start gpg-agent, dump the contents of file
  ;; to /tmp/vedat.gpgagent.info file
  (with-temp-buffer
    (insert-file-contents "/tmp/vedat.gpgagent.info")
    (when (/= (point-min) (point-max))
      (setenv "GPG_AGENT_INFO" (buffer-substring (point-min) (point-at-eol)))))
  (if (display-graphic-p)
      (setenv "DISPLAY"
              (terminal-name))
    (setenv "GPG_TTY"
            (terminal-name))))

(add-hook 'window-configuration-change-hook 'wg/kludge-gpg-agent)
