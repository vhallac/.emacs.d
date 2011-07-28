(setq yas/root-directory "~/.emacs.d/snippets")
(yas/load-directory yas/root-directory)
(setq yas/trigger-key "M-/")
(yas/global-mode 1)
(require 'dropdown-list)
(setq yas/prompt-functions '(yas/dropdown-prompt
                             yas/ido-prompt
                             yas/completing-prompt))
