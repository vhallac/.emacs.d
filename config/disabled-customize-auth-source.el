(eval-when-compile
  (require 'auth-source))

(when (and epg-gpg-program
           (file-exists-p epg-gpg-program)
           (file-executable-p epg-gpg-program))
  (setq auth-sources
        '( (:source "~/.emacs.d/.secrets/authinfo.gpg" :host t :protocol t))))
