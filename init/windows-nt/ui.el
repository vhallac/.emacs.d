(setq initial-frame-alist (append '((top . 20) (left . 1))
                                  (if (>= (x-display-pixel-height) 1024)
                                      (list (cons 'height 55)))))
