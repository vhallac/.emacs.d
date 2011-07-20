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

;; My packages have load priority
(setq load-path (append
                 (recursive-directory-list "~/.emacs.d/lisp")
                 (recursive-directory-list "~/.emacs.d/vhallac-lisp")
                 load-path))
