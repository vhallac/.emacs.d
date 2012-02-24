(load "term/xterm")

(defun terminal-init-screen ()
  "Terminal initialization function for screen."
   ;; Use the xterm color initialization code.
   (xterm-register-default-colors)
   (tty-set-up-initial-frame-faces))

;; Define sone terminal key codes.
;; TODO: How compatible are these between different terminals?
(define-key function-key-map "\e[1;3A" '[M-up])
(define-key function-key-map "\e[1;5A" '[C-up])
(define-key function-key-map "\e[1;3B" '[M-down])
(define-key function-key-map "\e[1;5B" '[C-down])
(define-key function-key-map "\e[1;3C" '[M-right])
(define-key function-key-map "\e[1;5C" '[C-right])
(define-key function-key-map "\e[1;3D" '[M-left])
(define-key function-key-map "\e[1;5D" '[M-left])
(define-key function-key-map "\e[13~" '[f3])
(define-key function-key-map "\e[14~" '[f4])
;; TODO: Fix these on a unix terminal. Or fix the ones above.
(define-key key-translation-map (kbd "M-[ 1 ~") (kbd "<home>"))
(define-key key-translation-map (kbd "M-[ 1 ^") (kbd "C-<home>"))
(define-key key-translation-map (kbd "M-[ 1 ; 5 H") (kbd "C-<home>"))
(define-key key-translation-map (kbd "<select>") (kbd "<end>"))
(define-key key-translation-map (kbd "M-[ 4 ~") (kbd "<end>"))
(define-key key-translation-map (kbd "M-[ 4 ^") (kbd "C-<end>"))
(define-key key-translation-map (kbd "M-[ 1 ; 5 F") (kbd "C-<end>"))
