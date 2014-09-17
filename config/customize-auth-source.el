(eval-when-compile
  (require 'auth-source))

(setq auth-sources
      '( (:source "~/.emacs.d/.secrets/authinfo.gpg" :host t :protocol t)))
