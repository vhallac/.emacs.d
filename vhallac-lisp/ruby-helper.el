(defun ruby-electric-strparam ()
  "Convert # to #{} when editing a string"
  (interactive)
  (insert "#")
  (when (and
         (looking-back "\".*")
         (looking-at ".*\""))
    (insert "{}")
    (backward-char 1)))

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

(provide 'ruby-helper)
