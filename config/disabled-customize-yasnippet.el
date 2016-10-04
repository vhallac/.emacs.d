(eval-when-compile
  (require 'yasnippet))

(declare-function yas/global-mode "yasnippet.el")

(setq yas/root-directory "~/.emacs.d/snippets")
(yas/load-directory yas/root-directory)
(setq yas/trigger-key "M-/")
(yas/global-mode 1)
(setq yas/prompt-functions 'yas/completing-prompt)
;; The checks and adds go from least wanted to most wanted. i.e., the last one
;; appears first when we check for all completion methods.
(when (and (boundp 'ido-mode) ido-mode)
  (when (require 'ido nil t)
    (add-to-list 'yas/prompt-functions 'yas/ido-prompt)))
(when (require 'dropdown-list nil t)
  (add-to-list 'yas/prompt-functions 'yas/dropdown-prompt))
