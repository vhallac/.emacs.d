;; TODO: This will need some simplification
;; TODO: This is windows only, I think
;; Setup for PLT Scheme
(defvar mzscheme-program (concat plt-dir "/mzscheme"))
(setenv "PLTHOME" plt-dir)
(setenv "PLTCOLLECTS" (concat plt-dir "collects"))
;;(setq quack-pltcollect-dirs (directory-files (concat plt-dir "collects") t))

(setq quack-pretty-lambda-p t)

(define-key scheme-mode-map [f1]
  (lambda ()
    (interactive)
    (ignore-errors
      (let ((symbol (thing-at-point 'symbol)))
        (info "(r5rs)")
        (Info-index symbol)))))
(define-key scheme-mode-map [f5]
  (lambda ()
    (interactive)
    (run-scheme mzscheme-program)))
(mapc (lambda (key-arg)
        (define-key scheme-mode-map (car key-arg)
          (eval `(lambda ()
                   (interactive)
                   (-test ,(cadr key-arg))))))
      '(([(control c) (control m)] nil)
        ([(control c) (h)]         :this)
        ([(control c) (e)]         :expand)
        ([(control c) (o)]         :expand-once)
        ([(control c) (*)]         :expand*)
        ([(control c) (p)]         :pp)))
(define-key scheme-mode-map [(control c) (x)] 'scheme-send-dwim)
(define-key scheme-mode-map [(control c) (\;)] 'insert-balanced-comments)
(define-key scheme-mode-map [(control c) (:)] 'remove-balanced-comments)
(define-key scheme-mode-map [(control c) (t)]
  (lambda (prefix)
    (interactive "P")
    (-trace "trace" prefix)))
(define-key scheme-mode-map [(control c) (T)]
  (lambda (prefix)
    (interactive "P")
    (-trace "trace-all" prefix)))

(add-hook 'scheme-mode-hook
          (lambda ()
            ;; Want this?
            (imenu-add-to-menubar "Symbols")
            (outline-minor-mode)
            (make-local-variable 'outline-regexp)
            (setq outline-regexp "^(.*")))

(add-hook 'Info-mode-hook
	  (lambda ()
	    (interactive)
	    (define-key Info-mode-map [(control c) (x)] 'scheme-send-dwim)))

;; Scheme-specific Functions
(defun insert-balanced-comments (arg)
  "Insert a set of balanced comments around the s-expression
containing the point.  If this command is invoked repeatedly
(without any other command occurring between invocations), the
comment progressively moves outward over enclosing expressions."
  (interactive "*p")
  (save-excursion
    (when (eq last-command this-command)
      (when (search-backward "#|" nil t)
        (save-excursion
          (delete-char 2)
          (while (and (< (point) (point-max)) (not (looking-at " *|#")))
            (forward-sexp))
          (replace-match ""))))
    (while (> arg 0)
      (backward-char 1)
      (cond ((looking-at ")") (incf arg))
            ((looking-at "(") (decf arg))))
    (insert "#|")
    (forward-sexp)
    (insert "|#")))

(defun remove-balanced-comments ()
  "Remove a set of balanced comments enclosing point."
  (interactive "*")
  (save-excursion
    (when (search-backward "#|" nil t)
      (delete-char 2)
      (while (and (< (point) (point-max)) (not (looking-at " *|#")))
	(forward-sexp))
      (replace-match ""))))

;; MzScheme Macro expansion
(defvar mzexpand-actions
  '(nil :this :expand :expand-once :expand* :pp))

(defvar mzexpand-cache nil)

(defun mzexpand-get-action ()
  (unless (eq (car mzexpand-cache) mzexpand-actions)
    (setq mzexpand-cache
          (cons mzexpand-actions
                (mapcar (lambda (a)
                          (list (replace-regexp-in-string
                                 "^:" "" (format "%s" a))
                                a))
                        mzexpand-actions))))
  (cdr (assoc (completing-read "Action? " mzexpand-cache nil t)
              (cdr mzexpand-cache))))

(defun -test (action)
  "Scheme syntax debugging. Uses Scheme code originally developed by
Eli Barzilay.  Actions: nil set current using sexp at point
 :this        show current
 :expand      expand current (possibly in a context)
 :expand-once expand one step
 :expand*     expand one step repeatedly
 :pp          pprint current"
  (interactive (mzexpand-get-action))
  (comint-send-string (get-buffer-process "*scheme*")
                      (format "(-test %S)" (or action
					       (sexp-at-point))))
  (pop-to-buffer "*scheme*" t)
  (other-window 1))

;; MzScheme Trace
(defun -trace (action &optional prefix)
  (interactive)
  (let ((symb nil))
    (if (or (equal action "trace")
	    (equal action "untrace"))
	(setq symb (symbol-at-point)))
    (if prefix
	(setq action (concat "un" action)))
    (comint-send-string (get-buffer-process "*scheme*")
			(if symb
			    (format "(%s %S)" action symb)
			  (format "(%s)" action))))
  (pop-to-buffer "*scheme*" t)
  (other-window 1))

