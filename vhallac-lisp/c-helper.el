(provide 'c-helper)
(require 'etags)

(setq c-helper-global-search-list nil)
(setq c-helper-find-file-history nil)
(setq c-helper-buffer-specific-dir-hook nil)

; Emacs compatibility functions
(GNUEmacs
    (progn
      (if (not (functionp 'buffer-tag-table-files))
          (defun buffer-tag-table-files ()
            (save-excursion
              (if (visit-tags-table-buffer)
                  (let ((tag-path (file-name-directory (buffer-file-name))))
                    (mapcar '(lambda (file) (convert-standard-filename 
                                             (concat tag-path file)))
                            (tags-table-files)))
                nil))))
      (if (not (functionp 'buffer-tag-table-list))
          (defun buffer-tag-table-list ()
            (save-excursion
              (if (visit-tags-table-buffer)
                  (buffer-file-name)))))))

(defun partial-file-path-match (full-path partial-path)
  "Compare a full (at least fuller) path against a sub-path.
If the trailing parts of two paths match, returns t. Otherwise, returns nil.
For example \"/usr/local/bin/emacs\" vs \"bin/emacs\" returns t."
  (let ((match t))
    (while (and match partial-path)
      (let ((full-last (file-name-nondirectory full-path))
            (partial-last (file-name-nondirectory partial-path)))
        (if (or (null partial-last)
                (string-equal partial-last ""))
            (setq partial-path nil)
          (setq match (string-equal full-last partial-last))
          (setq full-path (file-name-directory full-path))
          (setq partial-path (file-name-directory partial-path))
          (if full-path
              (setq full-path (directory-file-name full-path)))
          (if partial-path
              (setq partial-path (directory-file-name partial-path))))))
    match))

(defun c-helper-find-under-dirs (dirlist filename)
  "Locate the file under DIRLIST.
If the same file appears more than once in the directory list, the one closest
to the top list of directories is found."
  (let ((name nil))
    (while dirlist
      (let* ((dir (car dirlist))
             (contents (directory-files dir t))
             (files nil)
             (dirs nil))
        (mapcar '(lambda (name)
                   (cond ((and (file-directory-p name)
                               (not (member 
                                     (file-name-nondirectory name)
                                     '("." ".." "cvs" "CVS" "rcs" "RCS" ".svn"))))
                          (setq dirs (cons name dirs)))
                         ((and (not (file-directory-p name))
                               (file-readable-p name))
                          (setq files (cons (convert-standard-filename name) files))))
                   nil)
                contents)
        (while (and files (null name))
          (if (partial-file-path-match (car files) filename)
              (setq name (car files)))
          (setq files (cdr files)))
        (setq dirlist (append (cdr dirlist) dirs)))
      (if name
          (setq dirlist nil)))
    name))

(defun c-helper-find-in-tags (filename)
  "Locates a file in the buffer's tag files.
Returns the absolute path to the file, if found in the TAGS list,
otherwise return nil."
  (let ((files (buffer-tag-table-files))
        (name nil))
    (while (and files (null name))
      (if (partial-file-path-match (car files) filename)
          (setq name (car files)))
      (setq files (cdr files)))
    (if name
        (expand-file-name name))))

(defun c-helper-find-file (&optional filename)
  "Finds the file in the current include path.
See c-helper-include-path for the current include path."
  (interactive)
  (progn
	(if (or (not filename)
			(eq (string-width filename) 0))
		(setq filename (read-input "Please enter the file name: "
								   ""
								   c-helper-find-file-history
								   "")) )
	(let ((dirs (append c-helper-global-search-list
                        (if (functionp c-helper-buffer-specific-dir-hook)
                            (funcall c-helper-buffer-specific-dir-hook)
                          nil))))
	  ; Try to find in the tag list, if appropriate
	  (if (buffer-tag-table-list)
		  (let ((fname (c-helper-find-in-tags filename)))
			(if fname
				(progn
                  (if (> (count-windows) 1)
                      (find-file-other-window fname)
                    (find-file fname))
                  (return nil)))))

	  ; Otherwise, try the specified directories
	  (if dirs
		  (let ((fname (c-helper-find-under-dirs dirs filename)))
			(if fname
				(if (> (count-windows) 1)
					(find-file-other-window fname)
				  (find-file fname))
			  (error (concat "Cannot find file: " filename))))
		(error "Cannot construct search path")))))

(defun c-helper-find-include-file ()
  "Extracts the include file from the line under the point,
and finds it in the search path."
  (interactive)
  (save-excursion
	(beginning-of-line)
	(if (search-forward-regexp "#include\\s-*[\\\"<]\\(.*\\)[\\\">]"
							   (point-at-eol) ; limit
							   t ; noerror
							   )
		(let ((file-name (buffer-substring-no-properties
                          (match-beginning 1) (match-end 1))))
		  (if file-name
			  (c-helper-find-file file-name)
			(error "No file specified in the #include statement")))
	  (error "Not on a line with a #include statement"))))

