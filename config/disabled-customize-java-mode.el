; Do not throw exception if required files cannot be found.
; java-mode is defined in cc-mode in newer emacs versions, and java-mode in
; older ones.
; eclim may not be installed at all. Its usage is optional.

(eval-when-compile
  (require 'java-mode nil t)
  (require 'eclim nil t))

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

;; This function disappeared. Find a replacement.
(defun eclim--running-p () nil)

(add-to-list 'java-mode-hook
             '(lambda ()
                (c-set-style "java-custom")
                (when (eclim--running-p)
                  (eclim-mode))))
