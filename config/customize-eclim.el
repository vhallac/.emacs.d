(eval-when-compile
  (require 'eclim))

(setq eclim-auto-save t 		; Need to save before analyzing class
      help-at-pt-display-when-idle t
      eclim-executable (concat eclipse-dir "eclim")
)

(help-at-pt-set-timer)
(add-hook 'eclim-mode-hook
	  (lambda ()
	    (require 'auto-complete-config)
	    (ac-config-default)
	    (add-to-list 'ac-sources 'ac-source-eclim))) ; ac-source-emacs-eclim is also available

