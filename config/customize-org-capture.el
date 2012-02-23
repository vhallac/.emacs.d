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
         "* %? :APPOINTMENT:\n %U")))
