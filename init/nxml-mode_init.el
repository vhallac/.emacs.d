;;--
;; Make sure nxml-mode can autoload
;;--
;;(load "rng-auto.el")

;;--
;; Load nxml-mode for files ending in .xml, .xsl, .rng, .xhtml
;;--
(add-to-list 'auto-mode-alist
             '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\|html\\|htm\\)\\'" . nxml-mode))

(setq
 nxml-child-indent 4
 nxml-outline-child-indent 4
 nxml-slash-auto-complete-flag t
 rng-schema-locating-files '("schemas.xml" "~/.emacs.d/nxml-schemas/schemas.xml"))

(add-hook 'nxml-mode-hook 
          '(lambda ()
             (choose-indent-type)
             (vtidy-mode t)))
                             

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
