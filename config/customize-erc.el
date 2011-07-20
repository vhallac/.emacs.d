(setq erc-dcc-mode t
      erc-dcc-verbose t
      erc-modules '(autojoin button completion dcc fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring stamp track))

;; If the DCC download directory is missing, create it.
(if (not (file-exists-p erc-dcc-get-default-directory))
    (make-directory erc-dcc-get-default-directory t))
