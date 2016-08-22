(eval-when-compile
  (require 'malabar-mode)
  (require 'auto-complete)
  )

(c-add-style "java-custom"
	     '("java"
	       (c-offsets-alist . ((substatement-open . 0)
				   (arglist-cont-nonempty . (c-lineup-cascaded-calls
							     c-lineup-argcont))
				   (statement-cont . (c-lineup-cascaded-calls
						      c-lineup-assignments))))
	       (c-hanging-braces-alist . ((class-open after)
					  (inexpr-class-open after)
					  (inexpr-class-close before)
					  (defun-open after)
					  (inline-open after)
					  (substatement-open after)
					  (block-close . c-snug-do-while)))))


;; (require 'cedet)
;; (require 'semantic)
;; (load "semantic/loaddefs.el")

;; (add-hook 'malabar-mode-hook
;;           '(lambda ()
;;              (c-set-style "java-custom")
;;              (semantic-mode 1)
;;              (auto-complete-mode t)
;;              (setq ac-sources (append ac-sources '(ac-source-semantic)))))
