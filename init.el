;;
;; Set up the load paths
;;
(load-file "~/.emacs.d/loadpaths.el")

;;
;; Some macros to help with init scripts
;;
;; XXX: How do you correctly handle a .el that defines both a macro and a function?
(eval-when-compile
  (load "startupfuncs.el"))
(autoload 'load-files "startupfuncs.el")
(autoload 'try-progn "startupfuncs.el")

;;
;; Initialize packages
;;
(try-progn
 "Cannot initialize package system"
 (load-file "~/.emacs.d/init-packages.el"))

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
(try-progn
 "Cannot load autoloads"
 (load-file "~/.emacs.d/autoloads.el"))
(try-progn
 "Cannot load keyboard setup"
 (load-file "~/.emacs.d/keyboard.el"))

;; Load package configuration
(try-progn
 "Cannot load configuration handler"
 (load-file "~/.emacs.d/config.el"))


;; Load the global config and tidbits
(try-progn
 "Cannot load globals"
 (load-file "~/.emacs.d/globals.el"))

;;
;; Load the per-machine setup:
;; This is never added to git repository. It may reveal information about
;; machine/user that you'd rather keep to yourself.
;; This the very last thing in startup to ensure it can override/undo standard behavior.
;;
(let ((local-config-file "~/.emacs-local-config.el"))
  (when (file-exists-p local-config-file)
    (try-progn
     "Cannot load local configuration"
     (load-file local-config-file))))
