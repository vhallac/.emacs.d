(eval-when-compile
  (require 'groovy-mode))

(require 'groovy-electric)

(add-to-list 'groovy-mode-hook
	     '(lambda ()
		(groovy-electric-mode t)))
