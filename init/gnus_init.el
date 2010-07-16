(require 'gnus)

(add-to-list 'gnus-secondary-select-methods
             '(nnimap "gmail"
                      (nnimap-address "imap.gmail.com")
                      (nnimap-server-port 993)
                      (nnimap-stream tls)))

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.googlemail.com" 587 nil nil))
      smtpmail-auth-credentials "~/.authinfo"
;;      smtpmail-auth-credentials '(("smtp.googlemail.com" 587 "vedathallac@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.googlemail.com"
      smtpmail-smtp-server "smtp.googlemail.com"
      smtpmail-smtp-service 587)

(if (boundp 'openssl-prg)
    (setq tls-program `(,(concat openssl-prg " s_client -connect %h:%p -no_ssl2 -ign_eof")))
  (setq tls-program "openssl s_client -connect %h:%p -no_ssl2 -ign_eof"))

;; Use openssl for TLS - gnutls behaves badly under Win32
(require 'smtp-openssl)
