(require 'gnus)

(setq user-full-name "Vedat Hallac"
      user-mail-address "vedathallac@gmail.com")

(setq auth-sources
      '( (:source "~/.emacs.d/.secrets/authinfo.gpg" :host t :protocol t)))

(setq gnus-select-method
      '(nntp "news.solani.org"))

(setq gnus-posting-styles
      '(
        (message-news-p (name "Vedat Hallac")
                        (address "vedat.hallac@mail.invalid"))
        ("gmail-2" 
         (name "Dys@Bloodfeather")
         (address "dys.wowace@gmail.com"))))

(setq gnus-secondary-select-methods
      (list
       '(nnimap "gmail-1"
                (nnimap-address "imap.gmail.com")
                (nnimap-server-port 993)
                (nnimap-stream tls)
                (nnimap-list-pattern ("INBOX" "*"))
                (nnimap-expunge-on-close always)
                (gnus-check-new-newsgroups nil)
                (gnus-ignored-newsgroups "^to\\.\\|^[0-9. 	]+\\( \\|$\\)\\|^[\”]\”[#’()]"))
       '(nnimap "gmail-2"
                (nnimap-address "imap.gmail.com")
                (nnimap-server-port 993)
                (nnimap-stream tls)
                (nnimap-list-pattern ("INBOX" "*"))
                (nnimap-expunge-on-close always)
                (gnus-check-new-newsgroups nil)
                (gnus-ignored-newsgroups "^to\\.\\|^[0-9. 	]+\\( \\|$\\)\\|^[\”]\”[#’()]"))))

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.googlemail.com" 587 nil nil))
      smtpmail-default-smtp-server "smtp.googlemail.com"
      smtpmail-smtp-server "smtp.googlemail.com"
      smtpmail-smtp-service 587)

(setq tls-program `(,(concat
                      (if (boundp 'openssl-prg)
                          openssl-prg
                        "openssl")
                      " s_client -connect %h:%p -no_ssl2 -ign_eof")))

;; Use openssl for TLS - gnutls behaves badly under Win32
(require 'smtp-openssl)

;; html mail stuff
(setq mm-text-html-renderer 'w3m)


(setq gnus-use-adaptive-scoring t)
(setq gnus-score-expiry-days 60)
(setq gnus-default-adaptive-score-alist
      '((gnus-unread-mark)
        (gnus-ticked-mark (from 4))
        (gnus-dormant-mark (from 5))
        (gnus-saved-mark (from 20) (subject 5))
        (gnus-del-mark (from -2) (subject -5))
        (gnus-read-mark (from 2) (subject 1))
        (gnus-killed-mark (from 0) (subject -3))))

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(setq gnus-topic-line-format "%i[ %0{%(%n (new: %A)%)%} ]\n")

;; TODO: Sending mail from gmail-2 doesn't work due to SMTP setup. The From:
;; address get overwritten by the mail address of the gmail-1 account.
