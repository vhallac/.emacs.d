(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(efs-ftp-program-args (quote ("-L" "-u")))
 '(efs-ftp-prompt-regexp "^.*> *")
 '(efs-use-passive-mode nil)
 '(message-send-mail-function (quote smtpmail-send-it)))


(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:background "#00005f" :foreground "white" :height 98 :foundry "outline" :family "Envy Code R"))))
 '(font-lock-string-face ((((class color) (min-colors 88) (background dark)) (:foreground "PeachPuff"))))
 '(highline-face ((t (:background "grey90"))))
 '(magit-item-highlight ((t (:inherit highlight-current-line-face))))
 '(mmm-code-submode-face ((t (:background "grey35"))))
 '(mmm-default-submode-face ((t (:background "grey15"))))
 '(quack-pltish-comment-face ((((class color) (background light)) (:foreground "green4" :slant italic :weight bold))))
 '(quack-pltish-defn-face ((t (:foreground "turquoise3" :weight bold))))
 '(quack-pltish-paren-face ((((class color) (background dark)) (:foreground "goldenrod3"))))
 '(quack-pltish-selfeval-face ((((class color) (background light)) (:foreground "SlateGray4" :weight bold))))
 '(quack-threesemi-semi-face ((((class color) (background light)) (:background "gray88" :foreground "green4" :weight bold))))
 '(quack-threesemi-text-face ((((class color) (background light)) (:background "gray88" :foreground "green4" :weight bold)))))
