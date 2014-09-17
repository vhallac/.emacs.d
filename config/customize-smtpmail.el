(eval-when-compile
  (require 'smtpmail))

(setq mail-host-address "hallac.net")

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
     (ssl "vedat.hallac@pia-team.com" "outlook-pia" "smtp.live.com" 587)
     (ssl "vedat@hallac.net" "hallac-net" "smtp.yandex.com" 587)))

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

(defadvice smtpmail-via-smtp (around set-smtp-server-from-sender activate)
  ;; Not sure if this is the right way, but it seems to prevent the password
  ;; lingering around in the variable.
  (let ((smtpmail-auth-credentials nil))
    (with-current-buffer smtpmail-text-buffer
      (change-smtp))
    ad-do-it))
