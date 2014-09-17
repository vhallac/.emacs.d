(eval-when-compile
  (require 'message))

(require 'smtpmail)

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
                                               "vedat@hallac.net"
                                               "vedat.hallac@pia-team.com")))
(defun pia-insert-html-sig ()
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
