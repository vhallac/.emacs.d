;; Is this how we set it up?
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(setq
 org-M-RET-may-split-line '((default))
 org-agenda-custom-commands '(("D" "Daily Action List" agenda
                               ""
                               ((org-agenda-ndays 1)
                                (org-agenda-sorting-strategy ((agenda time-up priority-down tag-up)))
                                (org-deadline-warning-days 0)))
                              ("n" "Next actions" todo
                               #("NEXT" 0 4 (face org-warning))
                               ((org-agenda-sorting-strategy (time-up)))))
 org-agenda-files '("~/org/work.org" "~/org/home.org" "~/org/remember.org")
 org-agenda-include-all-todo t
 org-agenda-time-grid '((daily today) "----------------" (800 1000 1200 1400 1600 1800 2000))
 org-clock-in-resume t
 org-clock-persist t
 org-columns-default-format "%38ITEM(Details) %TAGS(Context) %7TODO(To Do) %5Effort(Estim){:} %6CLOCKSUM{Total}"
 org-enforce-todo-checkbox-dependencies t
 org-enforce-todo-dependencies t
 org-global-properties '(("Effort_ALL" . "0:0 0:10 0:20 0:30 1:00 2:00 3:00 4:00 8:00"))
 org-hide-leading-stars t
 org-log-done 'time
 org-publish-project-alist '(("org-notes-static"
                              :base-directory "~/org/notes"
                              :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
                              :publishing-directory "~/org-publish/"
                              :recursive t
                              :publishing-function org-publish-attachment)
                             ("org-notes"
                              :base-directory "~/org/notes"
                              :base-extension "org"
                              :publishing-directory "~/org-publish/")
                             ("org-notes-all"
                              :components ("org-notes" "org-notes-static")))
 org-publish-use-timestamps-flag nil
 org-return-follows-link t
 org-special-ctrl-a/e t
 org-use-fast-todo-selection t)

(defun home ()
  (interactive)
  (find-file "~/org/home.org"))

(defun work ()
  (interactive)
  (find-file "~/org/work.org"))

(add-hook 'org-mode-hook
	  (lambda ()
        (define-key org-mode-map [(control c) (control p)]
          '(lambda ()
             (interactive "")
             (org-publish-current-project)))
        (auto-fill-mode t)
        (vtidy-mode 1)))

;; From:
;; http://tsdh.wordpress.com/2008/11/14/calling-org-remember-from-inside-conkeror/
;; Integrate with conkeror
;; TODO: Change to autoload
(require 'org-protocol)

(setq org-remember-templates
 '(("Web" ?w "* TODO %c\n\n%i%!" "~/org/home.org" "Firefox" nil)
   ("TODO1"  ?T "* TODO %?\n   %i\n %a" nil nil nil)
   ("TODO2"  ?t "* TODO %?\n :PROPERTIES:\n :created: %U\n :link: %a\n :END:\n %i")))

(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)

;; For windows, remember to register the org-protocol:// handler via
;; Windows Registry Editor Version 5.00
;; [HKEY_CLASSES_ROOT\org-protocol]
;; @="URL:Org Protocol"
;; "URL Protocol"=""
;; [HKEY_CLASSES_ROOT\org-protocol\shell]
;; [HKEY_CLASSES_ROOT\org-protocol\shell\open]
;; [HKEY_CLASSES_ROOT\org-protocol\shell\open\command]
;; @="\"<path-to-emacs>/bin/emacsclient.exe\" \"%1\""

