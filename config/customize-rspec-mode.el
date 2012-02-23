(declare-function auto-complete-mode "auto-complete.el")

(autoload 'ruby-electric-mode "ruby-electric.el")

(add-hook 'rspec-mode-hook
          (lambda ()
	    (require 'rinari)
	    (require 'ruby-compilation-rspec)
	    (require 'auto-complete-config)
            (ruby-electric-mode t)
            (auto-complete-mode t)
            ;; Auto-complete fixups
            (make-local-variable 'ac-ignores)
            (add-to-list 'ac-ignores "end")))
