
(setq c-helper-global-search-list (recursive-directory-list "/usr/include")
      erc-dcc-get-default-directory "~/erc_dcc")

;; Got eclipse only in marvin
(if (equal "marvin" (downcase (car (split-string system-name "\\."))))
    (setq eclipse-dir "/opt/eclipse/"))

