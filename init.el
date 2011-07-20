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

;; Finally, set up customizations
;; Each file of the form customize-<file>.el in the ~/.emacs.d/config directory
;; is loaded via a call to (require '<file-name>) when <file> is loaded.
(mapcar (lambda (x)
          (let ((file-name
                 (replace-regexp-in-string "customize-\\|\.el" ""
                                           (file-name-nondirectory x))))
            (eval-after-load file-name
              `(load-file ,x))))
        (directory-files "~/.emacs.d/config" t "^customize-"))

(put 'narrow-to-region 'disabled nil)

(server-start)

(set-default-coding-systems 'utf-8)
