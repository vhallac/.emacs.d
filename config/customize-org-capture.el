(eval-when-compile
  (require 'org-capture))

;; Remember and org-protocol setups
(setq org-default-notes-file "~/org/refile.org")

;;;  Load Org Remember Stuff
(setq org-capture-templates
      '(("w" "Web" entry
         (file+headline "~/org/home.org" "Firefox")
         "* TODO %c\n\n%i" :immediate-finish t)
        ("t" "TODO" entry
         (file+headline "~/org/refile.org" "Tasks")
         "* TODO %?\n  :PROPERTIES:\n  :CREATED: %U\n  :LINK: %a\n  :END:\n %i"
         :clock-in t :clock-resume t)
        ("n" "note" entry
         (file+headline "~/org/refile.org" "Notes")
         "* %? :NOTE:\n  %U\n  %a\n"
         :clock-in t :clock-resume t)
        ("a" "appointment" entry
         (file+headline "~/org/appointments.org" "Appointments")
         "* %? :APPOINTMENT:\n %U")
        ("l" "Ledger entries")
        ("li" "Credit Card" plain
         (file+olp "~/org/finance.org" "Expenses" "Review")
         "%(subst-char-in-string ?- ?/ (org-read-date)) %^{Payee}\n  Liabilities:CC:ING Bonus  \n  Expenses:%^{Account}  %^{Amount}\n")
        ("lc" "Cash" plain
         (file+olp "~/org/finance.org" "Expenses" "Review")
         "%(subst-char-in-string ?- ?/ (org-read-date)) * %^{Payee}\n  Assets:Cash\n  Expenses:%^{Account}  %^{Amount}\n")))
