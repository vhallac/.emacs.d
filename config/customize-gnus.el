(eval-when-compile
  (require 'message)
  (require 'auth-source)
  (require 'gnus-msg)
  (require 'tls)
  (require 'gnus-score)
  (require 'gnus-topic)
  (require 'starttls)
  (require 'cl))

(require 'gnus)

(declare-function message-fetch-field "message.el")

(setq user-full-name "Vedat Hallac"
      user-mail-address "vedathallac@gmail.com")

(setq auth-sources
      '( (:source "~/.emacs.d/.secrets/authinfo.gpg" :host t :protocol t)))

(setq gnus-select-method
      '(nntp "news.easynews.com"))

(setq gnus-posting-styles
      '(((message-news-p)
         (name "Vedat Hallac")
         (address "vedat.hallac@mail.invalid"))
        ("gmail-2"
         (name "Dys@Bloodfeather")
         (address "dys.wowace@gmail.com"))
        ("gmail-android"
         (name "Vedat Hallac")
         (address "vedat@android.ciyiz.biz"))
        ("outlook-pia"
         (name "Vedat Hallaç")
         (address "vedat.hallac@pia-team.com"))))

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
                (gnus-ignored-newsgroups "^to\\.\\|^[0-9. 	]+\\( \\|$\\)\\|^[\”]\”[#’()]"))
       '(nnimap "gmail-android"
                (nnimap-address "imap.gmail.com")
                (nnimap-server-port 993)
                (nnimap-stream tls)
                (nnimap-list-pattern ("INBOX" "*"))
                (nnimap-expunge-on-close always)
                (gnus-check-new-newsgroups nil)
                (gnus-ignored-newsgroups "^to\\.\\|^[0-9. 	]+\\( \\|$\\)\\|^[\”]\”[#’()]"))
       '(nnimap "outlook-pia"
                (nnimap-address "imap-mail.outlook.com")
                (nnimaxsp-server-port 993)
                (nnimap-stream ssl)
                (nnimap-list-pattern ("INBOX" "*"))
                (nnimap-expunge-on-close always)
                (gnus-check-new-newsgroups nil)
                (gnus-ignored-newsgroups "^to\\.\\|^[0-9. 	]+\\( \\|$\\)\\|^[\”]\”[#’()]"))))

(setq tls-program `(,(concat
                      (if (boundp 'openssl-prg)
                          openssl-prg
                        "openssl")
                      " s_client -connect %h:%p -no_ssl2 -ign_eof")))

;; html mail and foreign characters
(setq mm-text-html-renderer 'w3m
      mm-inline-text-html-with-images t
      mm-w3m-safe-url-regexp nil
      mm-inline-large-images t)
(setq mm-coding-system-priorities '(utf-8))

;; Use openssl for TLS - gnutls behaves badly under Win32
(require 'smtp-openssl)

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

;; Set up multiple SMTP accounts
;; Available SMTP accounts.
;; Format for each entry is:
;; (mech address alias host port user password key cert)
;; - mech is one of 'plain, 'login, 'cram-md5, or 'ssl
;; - alias is the server's alias. This is used to support multiple accounts on
;;   same server. The authinfo entry should have a machine that matches the alias.
;; - address is the e-mail address that uses this server
;; - host is the SMTP host
;; - port is the port to connect to
;; - user is the user name for authentication. This parameter is optional.
;; - password is the password for authentication. This parameter is optional.
;; - key is ???
;; - cert is ??? (certificate of the server?)
(defvar smtp-accounts
  '( (ssl "vedathallac@gmail.com" "gmail-1" "smtp.googlemail.com" 587)
     (ssl "dys.wowace@gmail.com" "gmail-2" "smtp.googlemail.com" 587)
     (ssl "vedat@android.ciyiz.biz" "gmail-android" "smtp.googlemail.com" 587)
     (ssl "vedat.hallac@pia-team.com" "outlook-pia" "smtp.live.com" 587)))

(if (>= emacs-major-version 24)
    (defun set-smtp-common (alias server port &optional user password)
      ;; TODO: I need both alias and real server entries in my authinfo
      ;; for this method. I don't like it. Need a better way to handle it.
      (unless user
        (setq user (plist-get (car (auth-source-search :host alias
                                                       :port 587))
                              :user)))
      (setq smtpmail-smtp-user user
            smtpmail-smtp-server server
            smtpmail-smtp-service port))

  (defun set-smtp-common (alias server port &optional user password)
    "Set the common fields of the supplied parameters"
    (unless user
      (let ((port-name (format "%s" port)))
        (multiple-value-setq
            (user password)
          (auth-source-user-or-password '("login" "password") alias port-name))))
    (setq smtpmail-auth-credentials (list (list server port user password))
          smtpmail-smtp-server server
          smtpmail-smtp-service port)
    (message "Setting SMTP server to `%s:%s' for user `%s'." server port user)))

(defun set-smtp (mech alias server port &optional user password)
  "Set related SMTP variables for supplied parameters."
  (set-smtp-common alias server port user password)
  (setq smtpmail-auth-supported (list mech)
        smtpmail-starttls-credentials nil))

(defun set-smtp-ssl (alias server port &optional user password key cert)
  "Set related SMTP and SSL variables for supplied parameters."
  (set-smtp-common alias server port user password)
  (setq starttls-use-gnutls nil        ;use starttls-program
        starttls-extra-arguments nil
        smtpmail-starttls-credentials (list (list server port key cert))))

(defun change-smtp ()
  "Change the SMTP server according to the current from line."
  (save-excursion
    (loop with from = (save-restriction
                        (message-narrow-to-headers)
                        (message-fetch-field "from"))
          for (auth-mech address . auth-spec) in smtp-accounts
          when (string-match address from)
          do (cond
              ((memq auth-mech '(cram-md5 plain login))
               (return (apply 'set-smtp 'auth-mech auth-spec)))
              ((eql auth-mech 'ssl)
               (return (apply 'set-smtp-ssl auth-spec)))
              (t (error "Unrecognized SMTP auth. mechanism: `%s'." auth-mech)))
          finally (error "Cannot infer SMTP information."))))

(require 'smtpmail)

(defadvice smtpmail-via-smtp (around set-smtp-server-from-sender activate)
  ;; Not sure if this is the right way, but it seems to prevent the password
  ;; lingering around in the variable.
  (let ((smtpmail-auth-credentials nil))
    (with-current-buffer smtpmail-text-buffer
      (change-smtp))
    ad-do-it))

(setq gnus-use-adaptive-scoring '(word line))
;; This is easier than making virtual folders for inbox+sent, IMO
(setq mail-self-blind t
      mail-user-agent 'gnus-user-agent)

;; tells gnus to get new mail and also display all old mail
(define-key gnus-summary-mode-map (kbd "C-c C-c")
  (lambda ()
    (interactive)
    (gnus-summary-rescan-group 'all)))

(when (require 'bbdb nil t)
  (bbdb-initialize 'gnus)

  (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
  (setq bbdb-update-records-p 'query
        bbdb-mua-update-interactive-p '(search . query)
        ;; bbdb-accept-message-alist '( ("From" . "@pia-team\.com")
        ;;                              ("From" . "@\\(?:milleni\\|turkcell\\)\.com\.tr"))
        bbdb/gnus-score-default 10
        bbdb-mua-summary-unify-format-letter "B" ; use %uB for names
        gnus-summary-line-format "%U%R%z%I%(%[%4L: %-23,23uB%]%) %s\n"

        )
  )

(defun mimedown ()
  (interactive)
  (save-excursion
    (let ((default-process-coding-system '(utf-8 . utf-8)))
      (message-goto-body)
      (shell-command-on-region (point) (point-max) "~/bin/mimedown.py" nil t))))
