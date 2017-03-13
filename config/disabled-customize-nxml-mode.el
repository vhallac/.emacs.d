(eval-when-compile
  (require 'nxml-mode))

(setq
 nxml-child-indent 4
 nxml-outline-child-indent 4
 nxml-slash-auto-complete-flag nil)

(define-key nxml-mode-map (kbd "C-c k c") 'comment-region)

(add-hook 'nxml-mode-hook
          '(lambda ()
             (choose-indent-type)
             ;; Add my schema files to RNG search path
             (add-to-list 'rng-schema-locating-files
                          "~/.emacs.d/nxml-schemas/schemas.xml")
             (add-to-list 'rng-schema-locating-files
                          "~/.emacs.d/nxml-schemas/libvirt/schemas.xml")))

;; Add some mmm goodies for World of Warcraft XML files
;;  (require 'mmm-auto)
;;  (require 'mmm-vars)

;;  (mmm-add-group
;;   'emblua
;;   '((emblua-handler
;;      :submode lua-mode
;;     :front "<On\\([^\\>]+\\)>"
;;     :back "</On~1>"
;;     :save-matches 1
;;     :face mmm-code-submode-face)))

;; (add-to-list 'mmm-mode-ext-classes-alist '(nxml-mode nil emblua))
;; (setq mmm-global-mode 'maybe)
