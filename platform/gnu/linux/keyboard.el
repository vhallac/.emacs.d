(add-hook 'after-make-frame-functions
          (lambda ()
            (when (window-system)
              (normal-erase-is-backspace-mode 1)
              (define-key key-translation-map (kbd "C-/") (kbd "C-_")))))
