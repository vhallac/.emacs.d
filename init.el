;;
;; Set up the load paths
;;
(load-file "~/.emacs.d/init/loadpaths.el")

;;
;; Some macros to help with init scripts
;;
(load "startupfuncs.el")

;;
;; Load the platform dependent setup
;;
(load-files
 (concat "~/.emacs.d/init/" (symbol-name system-type) "/")
 ".*\\.el")

;;
;; Load the customizations
;; I will try to keep this file empty, and move the customizations to various
;; init scripts.
;; I may go back to initsplit.el, but for now I will use setqs.
;;
(setq custom-file "~/.emacs.d/customizations.el")
(load custom-file)

;; Now, go through each file that ends with "_init.el" in the init directory,
;; and load them
(load-files "~/.emacs.d/init" ".*_init\\.el")



(put 'narrow-to-region 'disabled nil)

(server-start)

(set-default-coding-systems 'utf-8)
