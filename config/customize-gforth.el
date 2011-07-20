;; TODO: This is a windows-only setup. Use the path setup stuff.
(add-hook 'forth-mode-hook
          (function (lambda ()
                      (define-key forth-mode-map [f5]
                        (function (lambda ()
                                    (interactive)
                                    (run-forth
                                     (concat
                                      (file-name-as-directory gforth-dir)
                                      "gforth.exe")))))

                      ;; customize variables here:
                      (setq forth-indent-level 4)
                      (setq forth-minor-indent-level 2)
                      (setq forth-hilight-level 3))))

(if (boundp 'gforth-dir)
    (let ((info-dir (concat gforth-dir "/doc")))
      ;; Hack alert: Can't seem to make info do my bidding, so I'll bash it on
      ;; the head a bit.
      (setq Info-directory-list nil)
      (add-to-list 'Info-default-directory-list info-dir)))

