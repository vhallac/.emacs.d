; The few macros below make it easier to code up version dependent stuff
(defmacro GNUEmacs (&rest x)
  (list 'if (string-match "GNU Emacs" (prin1-to-string (version))) (cons 'progn x)))
(defmacro GNUEmacs20 (&rest x)
  (list 'if (string-match "GNU Emacs 20" (prin1-to-string (version))) (cons 'progn x)))
(defmacro GNUEmacs21 (&rest x)
  (list 'if (string-match "GNU Emacs 21" (prin1-to-string (version))) (cons 'progn x)))
(defmacro XEmacs (&rest x)
  (list 'if (string-match "XEmacs 21" (prin1-to-string (version))) (cons 'progn x)))
(defmacro GNULinux (&rest x)
  (list 'if (string-match "linux" (prin1-to-string system-type)) (cons 'progn x)))
(defmacro Windows (&rest x)
  (list 'if (string-match "windows" (prin1-to-string system-type)) (cons 'progn x)))

; Make sure we get to edit a file. Even if some packages are missing.
(defmacro try-progn (err-msg &rest body)
  `(condition-case err
      (progn ,@body)
    (error (message-box (concat ,err-msg " %s.") (cdr err)))))

(defun load-files (path regexp)
  (mapcar (lambda (file)
                 (try-progn 
                  (concat "Cannot load file: " file)
                  (load-file file)))
          (sort (directory-files path t regexp)
                'string-lessp)))
