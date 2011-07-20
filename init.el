;;
;; Set up the load paths
;;
(load-file "~/.emacs.d/loadpaths.el")

;;
;; Some macros to help with init scripts
;;
(load "startupfuncs.el")

;;
;; Load the platform dependent setup
;;
(load-files
 (concat "~/.emacs.d/platform/" (symbol-name system-type) "/")
 ".*\\.el")

;;
;; Load the customizations
;; I will try to keep this file empty, and move the customizations to various
;; init scripts.
;; I may go back to initsplit.el, but for now I will use setqs.
;;
(setq custom-file "~/.emacs.d/customizations.el")
(load custom-file)

;; Load the autoload and keyboard setups
(load-file "~/.emacs.d/autoloads.el")
(load-file "~/.emacs.d/keyboard.el")

;; Set up customizations:
;; Each file of the form customize-<file>.el in the ~/.emacs.d/config directory
;; is loaded via a call to (require '<file-name>) when <file> is loaded.
(mapcar (lambda (x)
          (let ((file-name
                 (replace-regexp-in-string "customize-\\|\.el" ""
                                           (file-name-nondirectory x))))
            (eval-after-load file-name
              `(load-file ,x))))
        (directory-files "~/.emacs.d/config" t "^customize-"))

;; Load the global config and tidbits
(load-file "~/.emacs.d/globals.el")

