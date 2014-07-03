(eval-when-compile
  (require 'ace-window))

(defadvice ace-jump-list-visual-area (after remove-invisible activate compile)
  (setq ad-return-value
        (delq nil
              (mapcar (lambda (x)
                        (let ((f (aj-visual-area-frame x)))
                          (when
                              (and (frame-live-p f)
                                   (frame-visible-p f)
                                   (or (/= (frame-height f) 10)
                                       (/= (frame-width f) 10)))
                            x)))
                      ad-return-value))))
