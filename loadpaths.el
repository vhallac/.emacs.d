(defun recursive-directory-list (path)
  (let* ((toplevel (directory-files path t))
         (dirs nil))
    (while toplevel
      (let ((file (car toplevel)))
        (unless (member
                 (file-name-nondirectory file)
                 '("." ".." "cvs" "CVS" "rcs" "RCS" ".svn" "emacs" "xemacs"))
          (if (file-directory-p file)
              (setq dirs (append dirs
                                 (recursive-directory-list file)))))
        (setq toplevel (cdr toplevel))))
    (setq dirs (append dirs (list path)))))

(defun get-compat-dirs ()
  (let* ((compat-dir (expand-file-name "emacs-compat"
                                       (or user-emacs-directory
                                           "~/.emacs.d/")))
         (major-compat-dir (expand-file-name
                            (concat "emacs-"
                                    (number-to-string emacs-major-version))
                            compat-dir))
         (default-compat-dir (expand-file-name "default"
                                               compat-dir)))
    (delq nil
          (list (and (file-directory-p major-compat-dir) major-compat-dir)
                (when (file-directory-p default-compat-dir) default-compat-dir)))))

;; My packages have load priority
(setq load-path (append
                 (get-compat-dirs)
                 (recursive-directory-list "~/.emacs.d/lisp")
                 (recursive-directory-list "~/.emacs.d/vhallac-lisp")
                 load-path))
