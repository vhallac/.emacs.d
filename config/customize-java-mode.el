(eval-when-compile
  (require 'java-mode)
  (require 'eclim))

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

(add-to-list 'java-mode-hook
	     '(lambda ()
		(c-set-style "java-custom")
		(if (eclim--running-p)
		    (eclim-mode))))

