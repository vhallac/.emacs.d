;; ob.el - Customization for org-babel
(setq org-babel-min-lines-for-block-output 999
      org-babel-results-keyword "results")

(org-babel-do-load-languages
 'org-babel-load-languages
 '((ledger . t)
   ))
