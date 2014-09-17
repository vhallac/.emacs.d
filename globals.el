;; Lazy mode on: Type y or n instead of full "yes" or "no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Control newly created frames. Frame height is calculated according
;; to font size in after-make-frame-functions callbacks (ui.el in
;; platform directories).
(setq default-frame-alist
      '((menu-bar-lines . 0)
	(width . 85)))

(add-hook 'after-make-frame-functions
          (lambda (frame)
             (when (window-system frame)
               (scroll-bar-mode -1)
               (set-cursor-color "light green"))
             (blink-cursor-mode -1)
             (setq transient-mark-mode nil)
             (menu-bar-mode -1)
             (tool-bar-mode -1)))

; Make sure the hooks are run if we are not in daemon mode
(if (not (daemonp))
    (add-hook 'after-init-hook
              (lambda ()
                (run-hook-with-args 'after-make-frame-functions
                                    (selected-frame)))))

;; no splash screen:
(setq inhibit-startup-message t)

;; Interactively do things
(ido-mode t)

;; Highlight mathing parenthesis
(show-paren-mode 1)

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
        (line-beginning-position 2)))))

(defun insert-date ()
  "Inserts the current date at point"
  (interactive)
  (insert (format-time-string "%d/%m/%Y")))

(defun match-parenthesis (arg)
  "Match the current character according to the syntax table.

   Based on the freely available match-paren.el by Kayvan Sylvan.
   I merged code from goto-matching-paren-or-insert and match-it.

   You can define new \"parentheses\" (matching pairs).
   Example: angle brackets. Add the following to your .emacs file:

    (modify-syntax-entry ?< \"(>\" )
    (modify-syntax-entry ?> \")<\" )

   You can set hot keys to perform matching with one keystroke.
   Example: f6 and Control-C 6.

    (global-set-key \"\\C-c6\" 'match-parenthesis)
    (global-set-key [f6] 'match-parenthesis)

   Simon Hawkin <cema@cs.umd.edu> 03/14/1998"
  (interactive "p")
  ;;The ?= can be anything that is not a ?\(or ?\)
  (let ((syntax (char-syntax (or (char-after) ?=)))
        (syntax2 (char-syntax (or (char-before) ?=))))
    (cond
     ((= syntax ?\()
      (forward-sexp 1) (backward-char))
     ((= syntax ?\))
      (forward-char) (backward-sexp 1))
     ((= syntax2 ?\()
      (backward-char) (forward-sexp 1) (backward-char))
     ((= syntax2 ?\))
      (backward-sexp 1))
     (t (message "No match")))))

;; browse-kill-ring mode for easy kill-ring history handling
;; Use M-y to paste from history
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

(add-hook 'before-save-hook
          'delete-trailing-whitespace)

(defun vh/home ()
  (interactive)
  (find-file "~/org/home.org"))

(defun vh/work ()
  (interactive)
  (find-file "~/org/work.org"))

(defun vh/refile ()
  (interactive)
  (find-file "~/org/refile.org"))

(put 'narrow-to-region 'disabled nil)

(server-start)

;; Try to get the whole character encoding mess to a sensible default
(set-default-coding-systems 'utf-8)
(setq current-language-environment "Latin-1"
      default-input-method "latin-1-prefix")

;; General settings for editing environment
(setq kill-do-not-save-duplicates t
      next-line-add-newlines nil
      require-final-newline t
      sentence-end-double-space nil
      tab-always-indent 'complete)

;; These become buffer local when set, so I use setq-default
(setq-default tab-width 4
              fill-column 80
              indent-tabs-mode nil
              case-fold-search nil)

;; We don't need no backups. Yee-haw!
(setq make-backup-files nil
      auto-save-default nil)

;; There are no scrollbars. I want to see location.
(setq line-number-mode t
      column-number-mode t)

;; Other odd bits
(eval-when-compile (require 'compile))
(setq compilation-scroll-output t
      pop-up-windows t)

(global-font-lock-mode 1)

(require 'escreen)
(require 'midnight)
(require 'uniquify)
;; Quack will be handling my scheming
(eval-after-load "scheme" '(require 'quack))
(require 'turkish-doubles)
(set-language-environment "Turkish")
(put 'dired-find-alternate-file 'disabled nil)
