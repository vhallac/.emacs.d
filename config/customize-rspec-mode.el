(require 'rinari)
(require 'ruby-compilation-rspec)

(add-hook 'rspec-mode-hook
          (lambda ()
            (ruby-electric-mode t)
            (auto-complete-mode t)
            ;; Auto-complete fixups
            (make-local-variable 'ac-ignores)
            (add-to-list 'ac-ignores "end")))
