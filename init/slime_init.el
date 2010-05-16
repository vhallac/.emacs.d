(autoload 'slime-mode "slime"
  "Superior LISP Interaction Mode" t)

(autoload 'slime "slime"
  "Superior LISP Interaction Mode" t)

(add-to-list 'lisp-mode-hook 'slime-mode)

(eval-after-load "slime.el"
  '(progn
     (slime-setup)
     (setq slime-lisp-implementations
           `((sbcl ("sbcl"))
             (cmucl ("lisp"))
             (openmcl ("openmcl"))
             (s48 ("scheme48") :init slime48-init-command)
             (s48-large ("scheme48" "-h" "80000000")
                        :init slime48-init-command)
             (abcl ("abcl"))
             ,@slime-lisp-implementations))))

;;  (setq inferior-lisp-program "sbcl")
