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
(define-key ruby-mode-map (kbd "C-M-u") 'ruby-goto-containing-block-start)
(define-key ruby-mode-map (kbd "C-c b") 'ruby-flip-containing-block-type)

(defun ruby-get-containing-block ()
  (let ((pos (point))
        (block nil))
    (save-match-data
      (save-excursion
        (catch 'break
          ;; If in the middle of or at end of do, go back until at start
          (while (and (not (looking-at "do"))
                      (string-equal (word-at-point) "do"))
            (backward-char 1))
          ;; Keep searching for the containing block (i.e. the block that begins
          ;; before our point, and ends after it)
          (while (not block)
            (if (looking-at "do\\|{")
                (let ((start (point)))
                  (ruby-forward-sexp)
                  (if (> (point) pos)
                      (setq block (cons start (point)))
                    (goto-char start))))
            (if (not (search-backward-regexp "do\\|{" (point-min) t))
                (throw 'break nil))))))
        block))

(defun ruby-goto-containing-block-start ()
  (interactive)
  (let ((block (ruby-get-containing-block)))
    (if block
        (goto-char (car block)))))

(defun ruby-flip-containing-block-type ()
  (interactive)
  (save-excursion
    (let ((block (ruby-get-containing-block)))
      (goto-char (car block))
      (save-match-data
        (let ((strings (if (looking-at "do")
                           (cons
                            (if (= 3 (count-lines (car block) (cdr block)))
                                "do *\\(|[^|]+|\\)? *\n *\\(.*?\\) *\n *end"
                              "do *\\(|[^|]+|\\)? *\\(\\(.*\n?\\)+\\) *end")
                            "{ \\1 \\2 }")
                         (cons
                          "{ *\\(|[^|]+|\\)? *\\(\\(.*\n?\\)+\\) *}"
                          (if (= 1 (count-lines (car block) (cdr block)))
                              "do \\1\n\\2\nend"
                            "do \\1\\2end")))))
          (when (re-search-forward (car strings) (cdr block) t)
            (replace-match (cdr strings) t)
            (indent-region (match-beginning 0) (match-end 0))))))))
