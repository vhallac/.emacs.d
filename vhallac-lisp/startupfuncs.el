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
