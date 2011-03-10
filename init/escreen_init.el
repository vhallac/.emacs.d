(load "escreen")
(escreen-install)

;;(setq escreen-prefix-char "C-z")
;;(global-set-key escreen-prefix-char 'escreen-prefix)

;; Get the screen-data for the specified screen number
(defun vh-find-escreen-data-by-number (number)
  (car (delq nil
             (mapcar (lambda (x) (and (= (car x) number) x))
                     escreen-configuration-alist))))

;; Extract the buffer name for the given screen number
(defun vh-escreen-buffer-name (number)
  (let* ((screen-data (vh-find-escreen-data-by-number number))
         (data-map (escreen-configuration-data-map screen-data)))
    (escreen-configuration-data-map-critical-buffer-name
     (escreen-configuration-data-map-critical (car data-map)))))

;; add C- l to list screens with emphase for current one
(defun escreen-display-screens ()
  "what the name says"
  (interactive)
  (let ((escreens (escreen-get-active-screen-numbers))
        (screen-msg ""))

    (dolist (s escreens)
      (setq screen-msg
            (concat screen-msg
                    (let ((display-str (concat (number-to-string s)
                                               ":"
                                               (vh-escreen-buffer-name s))))
                      (if (= escreen-current-screen-number s)
                          (propertize display-str 'face 'bold-italic)
                        display-str))
                      " ")))
    (message "escreen: %s" screen-msg)))

(define-key escreen-map (kbd "l") 'escreen-display-screens)

(defadvice escreen-goto-last-screen (after escreen-goto-last-screen-show activate)
  (escreen-display-screens))

(defadvice escreen-create-screen (after escreen-create-screen-show activate)
  (escreen-display-screens))

(defadvice escreen-kill-screen (after escreen-kill-screen-show activate)
  (escreen-display-screens))

(add-hook 'escreen-goto-screen-hook 'escreen-display-screens)

;; Restore C-\ functionality
(define-key escreen-map (kbd "\\") 'toggle-input-method)
