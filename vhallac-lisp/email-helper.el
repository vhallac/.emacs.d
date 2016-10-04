(require 'smtpmail)

(defvar smtp-accounts
  '( (ssl "vedathallac@gmail.com" "gmail-1" "smtp.googlemail.com" 587)
     (ssl "dys.wowace@gmail.com" "gmail-2" "smtp.googlemail.com" 587)
     (ssl "vedat@android.ciyiz.biz" "gmail-android" "smtp.googlemail.com" 587)
     (ssl "vedat.hallac@pia-team.com" "gmail-pia" "smtp.googlemail.com" 587)
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
              (t (error "Unrecognized SMTP auth. mechanism: `%s'" auth-mech)))
          finally (error "Cannot infer SMTP information"))))

(defadvice smtpmail-via-smtp (around set-smtp-server-from-sender activate)
  "When sending smtp mail, replace credentials according to to From: field"
  ;; Not sure if this is the right way, but it seems to prevent the password
  ;; lingering around in the variable.
  (let ((smtpmail-auth-credentials nil))
    (with-current-buffer smtpmail-text-buffer
      (change-smtp))
    ad-do-it))

(defun mimedown ()
  "Add html mime part to message by interpreting body as markdown"
  (interactive)
  (save-excursion
    (let ((default-process-coding-system '(utf-8 . utf-8)))
      (message-goto-body)
      (shell-command-on-region (point) (point-max) "~/bin/mimedown.py" nil t))))

(defun vh/message-edit-body-as-org ()
  "Edit the body of the message in org-mode.

When I need to send an e-mail in HTML mode, I can easily edit in org-mode, then export using vh/message-org-to-html"
  (interactive)
  (let ((old-mode major-mode)
        (body-start (save-excursion
                      (message-goto-body)
                      (point))))
    (narrow-to-region body-start (point-max))
    ;; (setq vh-message-last-mode major-mode)
    (org-mode)
    (set (make-local-variable 'vh/message-last-mode)
         old-mode))
  (add-hook 'org-ctrl-c-ctrl-c-final-hook 'vh/message-back-to-message))

(defun vh/message-back-to-message ()
  "You don't need to call this usually. Just hitting 'C-c C-c' should take you out"
  (interactive)
  (when (and (boundp 'vh/message-last-mode)
             vh/message-last-mode)
    (widen)
    (funcall vh/message-last-mode)
    (setq vh/message-last-mode nil)
    (remove-hook 'org-ctrl-c-ctrl-c-final-hook 'vh/message-back-to-message)
    t))

(defun vh/message-org-to-html ()
  (interactive)
  (message-goto-body)
  (save-restriction
    (narrow-to-region (point) (point-max))
    (let ((text (org-export-as 'html)))
      (kill-region (point-min) (point-max))
      (mml-insert-multipart "alternative")
      (mml-insert-part "text/html")
      (insert (concat text "\n")))))

(defun vh/insert-pia-html-sig ()
  (interactive)
  (insert-string
   (base64-decode-string
    ;; Abusing base64 to avoid escaping the quotes. :)
    (concat "PGRpdiBzdHlsZT0icGFkZGluZy10b3A6N3B4O2ZvbnQtZmFtaWx5OidWZXJkYW5hJywnc2Fucy1z"
            "ZXJpZic7Zm9udC1zaXplOjhwdCI+PGEgaHJlZj0iaHR0cDovL3d3dy5waWEtdGVhbS5jb20vIiB0"
            "YXJnZXQ9Il9ibGFuayI+PGltZyBzcmM9Imh0dHA6Ly93d3cucGlhLXRlYW0uY29tL3dwLWNvbnRl"
            "bnQvdXBsb2Fkcy8yMDEyLzA5L2xvZ29fZmluYWwucG5nIiBoZWlnaHQ9IjcyIiB3aWR0aD0iMTYw"
            "Ii8+PC9hPjxociB3aWR0aD0iMTYwIiBhbGlnbj0ibGVmdCIvPjxwIGFsaWduPSJsZWZ0Ij48c3Bh"
            "biBzdHlsZT0iZm9udC1zaXplOjlwdCIgbGFuZz0iRU4tVVMiPjxiPkFobWV0IFZlZGF0IEhhbGxh"
            "JiMyMzE7PC9iPjwvc3Bhbj48YnIvPlNlbmlvciBTb2Z0d2FyZSBFbmdpbmVlcjxici8+PGJyLz5N"
            "OiArOTAgNTQxIDgzMyAyOCA4Mjxici8+VDogKzkwIDIxNiA2ODggNjkgNDE8YnIvPkJ1eWFrYSBB"
            "Vk0gLCBCIGJsb2sgS2F0IDEyIE5vOjcyPGJyLz4zNDc3MSBUZXBlJiMyNTI7c3QmIzI1MjsgLSAm"
            "IzIyMDttcmFuaXllIC0gJiMzMDQ7c3RhbmJ1bDxici8+PHNwYW4gc3R5bGU9ImNvbG9yOiMwMDY4"
            "Y2Y7Ij48YSBocmVmPSJtYWlsdG86dmVkYXQuaGFsbGFjQHBpYS10ZWFtLmNvbSI+PHU+dmVkYXQu"
            "aGFsbGFjQHBpYS10ZWFtLmNvbTwvdT48L2E+PC9zcGFuPjwvcD48L2Rpdj4="))))

(provide 'email-helper)
