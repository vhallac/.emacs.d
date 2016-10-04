(eval-when-compile
  (require 'cl))

(require 'etags)

(defvar c-helper-global-search-list nil)
(defvar c-helper-find-file-history nil)
(defvar c-helper-buffer-specific-dir-hook nil)

; Emacs compatibility functions
(unless (fboundp 'buffer-tag-table-files)
    (defun buffer-tag-table-files ()
      (save-excursion
	(if (visit-tags-table-buffer)
	    (let ((tag-path (file-name-directory (buffer-file-name))))
	      (mapc '(lambda (file) (convert-standard-filename
				     (concat tag-path file)))
		    (tags-table-files)))
	  nil))))
(unless (fboundp 'buffer-tag-table-list)
    (defun buffer-tag-table-list ()
      (save-excursion
	(if (visit-tags-table-buffer)
	    (buffer-file-name)))))

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
        (mapc '(lambda (name)
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
		(setq filename (read-string "Please enter the file name: "
								   ""
								   'c-helper-find-file-history
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

(c-add-style "java-custom"
             '("java"
               (c-offsets-alist . ((substatement-open . 0)
                                   (arglist-cont-nonempty . (c-lineup-cascaded-calls
                                                             c-lineup-argcont))
                                   (statement-cont . (c-lineup-cascaded-calls
                                                      c-lineup-assignments))))
               (c-hanging-braces-alist . ((class-open after)
                                          (inexpr-class-open after)
                                          (inexpr-class-close before)
                                          (defun-open after)
                                          (inline-open after)
                                          (substatement-open after)
                                          (block-close . c-snug-do-while)))))

(c-add-style "tda" '(
     (c-basic-offset . 4)
     (c-comment-only-line-offset . 0)
     (c-block-comment-prefix . "*")
     (c-hanging-braces-alist     . ((substatement-open        before after)
                                    (brace-list-open          after)
                                    (brace-list-intro)
                                    (brace-entry-open         before)
                                    (brace-list-close  . vh/c-snug-array-close)
                                    (block-close       . c-snug-do-while)
                                    (class-open               after)
                                    (class-close              before)))
     (c-hanging-colons-alist     . ((case-label after)
                                    (label after)
                                    (member-init-intro before)
                                    (inher-intro)))
     (c-offsets-alist . ((topmost-intro         . 0)
                         (topmost-intro-cont    . 0)
                         (substatement          . +)
                         (substatement-open     . 0)
                         (case-label            . 0)
                         (label                 . 0)
                         (access-label          . -)
                         (inclass               . +)
                         (inline-open           . 0)
                         (cpp-macro-cont        . ++)
                         (arglist-intro         . c-lineup-arglist-intro-after-paren)
                         (arglist-cont          . c-lineup-arglist)
                         (arglist-cont-nonempty . c-lineup-arglist)
                         (arglist-close         . c-lineup-arglist)
                         (inextern-lang         . -)
                         (statement-cont        . vh/c-lineup-array-init)))
     (c-cleanup-list . (empty-defun-braces
                        list-close-comma
                        scope-operator
                        one-liner-defun
                        comment-close-slash))
     (c-hanging-semi&comma-criteria . (c-semi&comma-inside-parenlist))))

(c-add-style "eracom" '(
     (c-basic-offset . 4)
     (c-comment-only-line-offset . 0)
     (c-block-comment-prefix . "*")
     (c-hanging-braces-alist     . ((substatement-open        before after)
                                    (brace-list-open          after)
                                    (brace-list-intro)
                                    (brace-entry-open         before)
                                    (brace-list-close  . vh/c-snug-array-close)
                                    (block-close       . c-snug-do-while)
                                    (class-open               after)
                                    (class-close              before)))
     (c-hanging-colons-alist     . ((case-label after)
                                    (label after)
                                    (member-init-intro before)
                                    (inher-intro)))
     (c-offsets-alist . ((topmost-intro         . 0)
                         (topmost-intro-cont    . 0)
                         (substatement          . +)
                         (substatement-open     . 0)
                         (case-label            . 0)
                         (label                 . 0)
                         (access-label          . -)
                         (inclass               . +)
                         (inline-open           . 0)
                         (cpp-macro-cont        . ++)
                         (arglist-intro         . c-lineup-arglist-intro-after-paren)
                         (arglist-cont          . c-lineup-arglist)
                         (arglist-cont-nonempty . c-lineup-arglist)
                         (arglist-close         . c-lineup-arglist)
                         (inextern-lang         . -)
                         (statement-cont        . vh/clineup-array-init)))
     (c-cleanup-list . (empty-defun-braces
                        list-close-comma
                        scope-operator))
     (c-hanging-semi&comma-criteria . (c-semi&comma-inside-parenlist))))

(c-add-style "eracom-old" '(
     (c-basic-offset . 4)
     (c-comment-only-line-offset . 0)
     (c-block-comment-prefix . "*")
     (c-hanging-braces-alist     . ((substatement-open after)
                                    (brace-list-open   after)
                                    (brace-list-intro)
                                    (brace-entry-open  after)
                                    (brace-list-close  before)
                                    (block-close       . c-snug-do-while)
                                    (class-open        after)))
     (c-hanging-colons-alist     . ((case-label after)
                                    (label after)
                                    (member-init-intro before)
                                    (inher-intro)))
     (c-offsets-alist . ((topmost-intro         . 0)
                         (topmost-intro-cont    . 0)
                         (substatement          . +)
                         (substatement-open     . 0)
                         (case-label            . 0)
                         (label                 . 0)
                         (access-label          . -)
                         (inclass               . +)
                         (inline-open           . 0)
                         (cpp-macro-cont        . ++)
                         (arglist-intro         . c-lineup-arglist-intro-after-paren)
                         (arglist-cont          . c-lineup-arglist)
                         (arglist-cont-nonempty . c-lineup-arglist)
                         (arglist-close         . c-lineup-arglist)))
     (c-cleanup-list . (brace-else-brace
                        brace-elseif-brace
                        empty-defun-braces
                        list-close-comma
                        scope-operator))))

(defun vh/c-snug-array-close (syntax pos)
  "Dynamically calculate close-brace hanginess for array initializations.

See `c-hanging-braces-alist' for how to utilize this function as an
ACTION associated with `brace-list-close' syntax."
  (save-excursion
    (if (eq syntax 'brace-list-close)
        (match-parenthesis 0))
    (c-safe (c-forward-token-1 -1))
    (if (eq (char-after) ?\=)
        '(before)
      '(after))))

(defun vh/c-lineup-array-init (langelem)
  "Correct the indentation of array and structure initializer brace, when it is
reported as statement-cont.

Changes:
int a[] =             int a[] =
   {                  {
      1,2,3      ->      1,2,3
   };                 };"
  (let ((default-lineup (c-lineup-math langelem)))
    (save-excursion
      (goto-char (point-at-bol))
      (if (and (looking-at "\\s-*{")
               (progn (c-safe (c-backward-token-1 1))
                      (eq (char-after) ?\=)))
          0
        default-lineup))))

(defun vh/c-insert-include-prev-line (include-str offset-from-end)
  "Add INCLUDE-STR to the previous line, and leave cursor OFFSET-FROM-END
characters off from the end. The cursor position is where the header name goes.

TODO: Just use yasnippet. Not that useful anymore."
       (beginning-of-line)
       (insert "#include <.h>\n")
       (forward-line -1)
       (end-of-line)
       (backward-char 3))

(defun vh/c-add-header-file-protection (&optional c++-mode-p)
  "Add the statements that protect a header file against multiple inclusion.
when C++-MODE is set, the header file is assumed to be a C++ header, and no
extern \"C\" statements are added."
  (let ((nm (subst-char-in-string ?. ?_ (concat "INC_" (upcase (buffer-name))))))
	 (save-excursion
	   (goto-char (point-min))
	   (insert (concat (concat "#ifndef " nm) "\n"))
	   (insert (concat (concat "#define " nm) "\n\n"))
       (if (not c++-mode-p)
           (insert "#ifdef __cplusplus\nextern \"C\" {\n#endif /* __cplusplus */\n"))
	   (goto-char (point-max))
       (newline)
       (if c++-mode-p
           (insert (concat (concat "\n#endif // " nm) "\n"))
         (insert "\n#ifdef __cplusplus\n}\n#endif /* __cplusplus */\n\n")
         (insert (concat (concat "\n#endif /* " nm) " */\n"))))))


(provide 'c-helper)
