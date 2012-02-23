(autoload 'ruby-electric-mode "ruby-electric.el")
(autoload 'rinari-launch "rinari.el")
(autoload 'yari-anything "yari.el")
(autoload 'word-at-point "thingatpt.el")

(require 'anything)
(require 'auto-complete-config)
(require 'ruby-compilation-rspec)

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

(define-key ruby-mode-map (kbd "C-x C-t") 'ruby-compilation-this-rspec)

(defconst spork-server-buffer-name "spork-server"
  "The name of the buffer that runs spork server")

(defun spork-inactive-p ()
  (let* ((spork-buffer (get-buffer (concat "*" spork-server-buffer-name "*")))
         (spork-process (and spork-buffer
                             (get-buffer-process spork-buffer))))
    (null spork-process)))

(defun start-spork ()
  (interactive)
  (if (and (spork-inactive-p)
           (executable-find "spork"))
      (ruby-compilation-do spork-server-buffer-name
                           (list "bundle" "exec" "spork"))))

(defun ruby-compilation-this-rspec ()
  (interactive)
  (if (or (null buffer-file-name)
          (not (string-match "_spec.rb$" buffer-file-name))
          (spork-inactive-p))
      (ruby-compilation-this-test)
    (let ((test-name (ruby-compilation-this-test-name)))
      (pop-to-buffer (ruby-compilation-do
                      (ruby-compilation-this-test-buffer-name test-name)
                      (list ruby-compilation-executable
                            "--drb"
                            (buffer-file-name)
                            ruby-compilation-test-name-flag test-name))))))

(define-key ruby-mode-map (kbd "C-c d") 'yari-anything)
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
      (save-restriction
        (narrow-to-region (car block) (cdr block))
        (goto-char (point-min))
        (if (re-search-forward "\\`do" nil t)
            (replace-match "{")
          (if (re-search-forward "\\`{" nil t)
              (replace-match "do")))
        (if (re-search-forward "end\\'" nil t)
            (replace-match "}")
          (if (re-search-forward "}\\'" nil t)
              (replace-match "end")))
        (save-match-data
          (goto-char (point-min))
          (when (looking-at "\\`{\\(?:[^\n]*\n\\)\\{2\\} *}\\'")
            (dotimes (cnt 2)
              (join-line t)
              (if (not (char-equal (char-after (point)) 32)) (insert " "))))
          (when (looking-at "\\`do *\\(\\(?:|[^|]+|\\)?\\)\\(?:[^\n]*?\\)\\( *end\\)\\'")
            (goto-char (match-end 1))
            (insert "\n")
            (goto-char (1+ (match-beginning 2)))
            (insert "\n"))
          (setq block (cons (point-min) (point-max)))))
      (indent-region (car block) (cdr block)))))
