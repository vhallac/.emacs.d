;;; smtp-open-ssl.el --- Allow smptmail to use openssl

;; This file is not part of Emacs

;; Author: Phillip Lord <p.lord@russet.org.uk>
;; Maintainer: Phillip Lord <p.lord@russet.org.uk>
;; Website: http://www.russet.org.uk
;; Version: 0.3

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA. 

;;; Commentary:
;;
;; Many SMTP servers now require a secure channel of access for
;; interaction. Currently, smtpmail supports connection via a TLS
;; channel using the external gnutls package. This is opposed to Gnus
;; IMAP support which uses openssl. 
;;
;; Unfortunately, gnutls uses process signalling for out-of-band
;; communication. So, for example, the cygwin gnutls won't work with
;; NTEmacs, while openssl works fine. I attempting to hack this in (by
;; calling cygwin "kill" to do process handling), but failed. 
;;
;; This file adds support to smptmail.el to use openssl instead of
;; gnutls. 
;;
;; NOTE: this file uses advice on top of existing smptmail code base. So it's
;; a bit of a hack, although less of a hack than the first version

;;; Usage
;;
;; Put (require 'smtp-openssl) into your .emacs. Then configure smtpmail as normal. Say..

;; (setq smtpmail-smtp-server "smtp.domain.com")
;; (setq smtpmail-smtp-service 465)
;; (setq send-mail-function 'smtpmail-send-it)


;; (setq smtpmail-auth-credentials  ; or use ~/.authinfo
;;      '(("smtp.domain.com" 465 "username" "password")))


;;; Code

(require 'smtpmail)

;; options
(defvar smtp-openssl-active t
  "Use openssl as opposed to gnutls for TLS communication")


;; we can advice around smtpmail-open-stream, so do this rather than overwrite it. 

(defadvice smtpmail-open-stream
  (around smtp-openssl-advice-open activate)
  
  ;; we want to use openssl and for this stream we should be using
  ;; ssl. 
  (let ((process-buffer (ad-get-arg 0))
        (host (ad-get-arg 1))
        (port (ad-get-arg 2)))

    (if smtp-openssl-active
        ;; check return to see if process is sane. 
        (progn 
          (message "Opening SSL connection")
          (setq ad-return-value
                (start-process 
                 ;; open the process a buffer
                 "SMTP" process-buffer "openssl"
                 ;; use client mode for tunnelling
                 "s_client" 
                 ;; be quiet about it, or any ssl communication stuff will
                 ;; happen in band
                 "-quiet" 
                 ;; use tls1 and/or connection
                 "-tls1" "-connect"
                 (format "%s:%s" host port))))
      ;; just do the normal thing instead. 
      ad-do-it)))


;; this function overrides the default and removes openssl stuff from the
;; start of the connection, which doesn't seem to work.
(defadvice smtpmail-process-filter 
  (around smtp-openssl-process-filter activate)
  (let ((output (ad-get-arg 1)))
    ;; this unless removes openssl stuff
    (unless (and
             smtp-openssl-active
             (or
              (string-match "depth=" output)
              (string-match "verify" output)))
      ad-do-it)))


(provide 'smtp-openssl)


