;; Redefine a function for a bugfix.
;; TODO: Occasionally check the eshell code to see if they've fixed it.
;; The bugfix is marked in the code below
(defun eshell-emit-prompt ()
  "Emit a prompt if eshell is being used interactively."
  (run-hooks 'eshell-before-prompt-hook)
  (if (not eshell-prompt-function)
      (set-marker eshell-last-output-end (point))
    (let ((prompt (funcall eshell-prompt-function)))
      (and eshell-highlight-prompt
	   (add-text-properties 0 (length prompt)
				'(read-only t
				  face eshell-prompt
                  ;; BEGIN: VH - This is the bugfix   -- (21/09/2013)
                  front-sticky (face read-only)
                  ;; END
				  rear-nonsticky (face read-only))
				prompt))
      (eshell-interactive-print prompt)))
  (run-hooks 'eshell-after-prompt-hook))
