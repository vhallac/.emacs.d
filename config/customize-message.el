
(when (require 'bbdb nil t)
  (bbdb-initialize 'message)
  (bbdb-insinuate-message)

  (setq bbdb-mua-pop-up nil
        bbdb-complete-mail-allow-cycling t))

(setq message-alternative-emails (regexp-opt '("vedathallac@gmail.com"
                                               "vedat.hallac@gmail.com"
                                               "dys.wowace@gmail.com"
                                               "vedat@android.ciyiz.biz"
                                               "vedat@oyun.cuyuz.biz"
                                               "vedathallac@yandex.com"
                                               "vedat.hallac@pia-team.com")))
(defun vh-message-edit-body-as-org ()
  "Edit the body of the message in org-mode.

When I need to send an e-mail in HTML mode, I can easily edit in org-mode, then export using vh-message-org-to-html"
  (interactive)
  (let ((body-start (save-excursion
                       (message-goto-body)
                       (point))))
    (narrow-to-region body-start (point-max)))
  (set (make-local-variable 'vh-message-last-mode)
       major-mode)
  (setq vh-message-last-mode major-mode)
  (org-mode)
  (add-hook 'org-ctrl-c-ctrl-c-hook 'vh-message-back-to-message))

(defun vh-message-back-to-message ()
  "You don't need to call this usually. Just hitting 'C-c C-c' should take you out"
  (interactive)
  (when (and (boundp 'vh-message-last-mode)
             vh-message-last-mode)
    (widen)
    (funcall vh-message-last-mode)
    (setq vh-message-last-mode nil)
    (remove-hook 'org-ctrl-c-ctrl-c-hook 'vh-message-last-mode)
    t))

(defun vh-message-org-to-html ()
  (interactive)
  (message-goto-body)
  (save-restriction
    (narrow-to-region (point) (point-max))
    (let ((text (org-export-as 'html)))
      (kill-region (point-min) (point-max))
      (mml-insert-multipart "alternative")
      (mml-insert-part "text/html")
      (insert (concat text "\n")))))

(define-key message-mode-map (kbd "C-c o") 'vh-message-edit-body-as-org)
(define-key message-mode-map (kbd "C-c h") 'vh-message-org-to-html)
