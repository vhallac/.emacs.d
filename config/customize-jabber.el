(autoload 'jabber-connect-all "jabber" "Load Jabber client" t)

(define-key global-map (kbd "C-x C-j C-c") 'jabber-connect-all)

(setq jabber-account-list
      '(("vedathallac@gmail.com" 
         (:network-server  . "talk.google.com")
         (:machine-alias   . "gmail-1")
         (:connection-type . ssl))
        ("dys.wowace@gmail.com" 
         (:network-server  . "talk.google.com")
         (:machine-alias   . "gmail-2")
         (:connection-type . ssl))
        ("vhallac@chat.facebook.com" 
         (:network-server  . "chat.facebook.com")
         (:machine-alias   . "facebook")
         (:port            . 5222)
         (:connection-type . network))))

(defun make-jid-from-args (username server resource)
  "Construct a jid from username server and resource arguments."
  (let ((format-str "%s"))
    (when username
      (setq format-str (concat "%s@" format-str)))
    (when resource
      (setq format-str (concat format-str "/%s")))
    (apply 'format (cons format-str
                         (delq nil (list username server resource))))))

;; Modify jabber-connect to extract passwords from my encrypted authinfo
;; The entries I use are:
;; machine gtalk-1 login <unused> password a_password port 5223
;; machine gtalk-2 login <unused> password a_password port 5223
;; I defined a new keyword :machine-alias for jabber-account-list entries to
;; handle multiple users on the same server.
(defadvice jabber-connect (around get-password activate)
  "Read password out of a specified netrc compatible authinfo file"  
  (let ((password (ad-get-arg 4)))
    (unless password
      (let* ((username (ad-get-arg 0))
             (server (ad-get-arg 1))
             (resource (ad-get-arg 2))
             (jid (make-jid-from-args username server resource))
             (alist (cdr (assoc jid jabber-account-list)))
             (mach-alias (or (cdr (assq :machine-alias alist))
                             server))
             (port (or (ad-get-arg 6)
                       5223))
             (port-name (format "%s" port))
             (auth-passwd (car (auth-source-user-or-password '("password")
                                                        mach-alias port-name))))
        (when auth-passwd
          (ad-set-arg 4 auth-passwd))
          ad-do-it))))
