(require 'rinari)
(require 'anything)
(require 'yari)
(require 'auto-complete)
(require 'ruby-electric)

(defun ruby-electric-strparam ()
  "Convert # to #{} when editing a string"
  (interactive)
  (insert "#")
  (when (and
         (looking-back "\".*")
         (looking-at ".*\""))
    (insert "{}")
    (backward-char 1)))

;; Setup align for ruby-mode
(require 'align)
(defconst align-ruby-modes '(ruby-mode))
(defconst ruby-align-rules-list
  '((ruby-comma-delimiter
     (regexp . ",\\(\\s-*\\)[^/ \t\n]")
     (modes  . align-ruby-modes)
     (repeat . t))
    (ruby-symbol-after-func
     (regexp . "^\\s-*\\w+\\(\\s-+\\):\\w+")
     (modes  . align-ruby-modes))))
(add-to-list 'align-perl-modes 'ruby-mode)
(add-to-list 'align-dq-string-modes 'ruby-mode)
(add-to-list 'align-sq-string-modes 'ruby-mode)
(add-to-list 'align-open-comment-modes 'ruby-mode)
(dolist (it ruby-align-rules-list)
  (add-to-list 'align-rules-list it))

(add-hook 'ruby-mode-hook
          (lambda ()
            (rinari-launch)
            (ruby-electric-mode t)
            (auto-complete-mode t)
            ;; Auto-complete fixups
            (make-local-variable 'ac-ignores)
            (add-to-list 'ac-ignores "end")))

(define-key ruby-mode-map (kbd "C-c d") 'yari-anything)
(define-key ruby-mode-map (kbd "<tab>") 'yas/expand)
(define-key ruby-mode-map (kbd "#") 'ruby-electric-strparam)
