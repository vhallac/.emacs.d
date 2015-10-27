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
        ("gmail-pia"
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
       '(nnimap "gmail-pia"
                (nnimap-address "imap.gmail.com")
                (nnimap-server-port 993)
                (nnimap-stream tls)
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

(setq gnus-score-expiry-days 60)
(setq gnus-default-adaptive-score-alist
      '((gnus-unread-mark)
        (gnus-ticked-mark (from 40))
        (gnus-dormant-mark (from 50))
        (gnus-saved-mark (from 200) (subject 50))
        (gnus-del-mark (from -20) (subject -50))
        (gnus-read-mark (from 20) (subject 10))
        (gnus-killed-mark (from -10) (subject -30))))

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(setq gnus-topic-line-format "%i[ %0{%(%n (new: %A)%)%} ]\n")

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

;; Work-around for GMail's internal folders: When the IMAP folder contains
;; characters [ and ] (actually any regexp character), the function
;; `gnus-score-find-bnews' cannot return the ADAPT file name. This causes ADAPT
;; files to be generated, but not used in these groups.
;; The following setting ensures these two characters are never used in ADAPT
;; file names.
(setq nnheader-file-name-translation-alist '((?[ . ?_) (?] . ?_)) )

(defun mimedown ()
  (interactive)
  (save-excursion
    (let ((default-process-coding-system '(utf-8 . utf-8)))
      (message-goto-body)
      (shell-command-on-region (point) (point-max) "~/bin/mimedown.py" nil t))))
