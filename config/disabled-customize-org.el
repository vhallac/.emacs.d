(eval-when-compile
  (require 'org)
  (require 'org-archive)
  (require 'ox-html))

(setq
 org-enforce-todo-checkbox-dependencies t
 org-enforce-todo-dependencies t
 org-hide-leading-stars t
 org-log-done 'time
 org-log-reschedule 'note
 org-log-redeadline 'note
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
 org-use-fast-todo-selection t
 ;; TODO sequences
 org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
                     (sequence "WAITING(w@/!)" "|" "HOLD(h@/!)"
                               "CANCELLED(c@/!)" "PHONE" "MEETING")
                     (sequence "QUOTE(q!)" "QUOTED(Q!)" "|"
                               "APPROVED(A@)" "EXPIRED(E@)" "REJECTED(R@)")
                     (sequence "OPEN(O)" "|" "CLOSED(C)"))
 org-todo-keyword-faces '(("TODO"      :foreground "red"          :weight bold)
                          ("NEXT"      :foreground "blue"         :weight bold)
                          ("DONE"      :foreground "forest green" :weight bold)
                          ("WAITING"   :foreground "yellow"       :weight bold)
                          ("HOLD"      :foreground "goldenrod"    :weight bold)
                          ("CANCELLED" :foreground "orangered"    :weight bold)
                          ("PHONE"     :foreground "forest green" :weight bold)
                          ("MEETING"   :foreground "forest green" :weight bold)
                          ("QUOTE"     :foreground "hotpink"      :weight bold)
                          ("QUOTED"    :foreground "indianred1"   :weight bold)
                          ("APPROVED"  :foreground "forest green" :weight bold)
                          ("EXPIRED"   :foreground "olivedrab1"   :weight bold)
                          ("REJECTED"  :foreground "olivedrab"    :weight bold)
                          ("OPEN"      :foreground "magenta"      :weight bold)
                          ("CLOSED"    :foreground "forest green" :weight bold))
 org-todo-state-tags-triggers '(("CANCELLED" ("CANCELLED" . t))
                                ("WAITING" ("WAITING" . t))
                                ("HOLD" ("WAITING" . t) ("HOLD" . t))
                                (done ("WAITING") ("HOLD"))
                                ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                                ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                                ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))
 org-treat-S-cursor-todo-selection-as-state-change nil
 ;; Separate drawers for clocking and logs
 org-drawers '("PROPERTIES" "LOGBOOK" "CLOCK")
 org-completion-use-ido t
 ;; Targets include this file and any file contributing to the agenda - up to 5
 ;; levels deep
 org-refile-targets '((org-agenda-files :maxlevel . 5)
                      (nil :maxlevel . 5))
 ;; Targets start with the file name - allows creating level 1 tasks
 org-refile-use-outline-path 'file
 ;; Targets complete in steps so we start with filename, TAB shows the next
 ;; level of targets etc
 org-outline-path-complete-in-steps t
 ;; Allow refile to create parent tasks with confirmation
 org-refile-allow-creating-parent-nodes 'confirm
 ;; Column view and estimates
 ;; org-columns-default-format
 ;;   "%80ITEM(Task) %TAGS(Context) %7TODO(To Do) %10Effort(Estim){:} %10CLOCKSUM{Total}"
 org-columns-default-format "%80ITEM(Task) %10Effort(Estim){:} %10CLOCKSUM(Total){:}"
 org-global-properties '(("Effort_ALL" . "0:0 0:10 0:30 1:00 2:00 3:00 4:00 8:00"))
 org-time-clocksum-format '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)
 ;; Mark a task as DONE when archiving
 org-archive-mark-done nil
 org-html-head-extra "<style type=\"text/css\">
<!--/*--><![CDATA[/*><!--*/
pre.src {overflow-x: auto; }
.src { background-color: #f5deb3; color: #black;}
/*]]>*/-->
</style>"
 org-src-fontify-natively t
 org-time-clocksum-use-effort-durations t)

(define-key org-mode-map (kbd "C-c C-p") (lambda ()
                                           (interactive "")
                                           (org-publish-current-project)))
(define-key org-mode-map (kbd "C-c ;") nil)
;; I keep pressing this combo by mistake (when scheduling)
;; Disabling for now.
(define-key org-mode-map (kbd "C-c C-x C-s") nil)

(add-hook 'org-mode-hook
	  (lambda ()
	    ;; flyspell mode to spell check everywhere
	    (flyspell-mode 1)
	    (auto-fill-mode t)))

;; Add modules I want
(add-to-list 'org-modules 'org-habit)

;; I don't use this -- but set it in case I forget to specify a location in a
;; future template.
;; org-capture still uses this variable
; (setq org-remember-default-headline "Tasks")

;; For windows, remember to register the org-protocol:// handler via
;; Windows Registry Editor Version 5.00
;; [HKEY_CLASSES_ROOT\org-protocol]
;; @="URL:Org Protocol"
;; "URL Protocol"=""
;; [HKEY_CLASSES_ROOT\org-protocol\shell]
;; [HKEY_CLASSES_ROOT\org-protocol\shell\open]
;; [HKEY_CLASSES_ROOT\org-protocol\shell\open\command]
;; @="\"<path-to-emacs>/bin/emacsclient.exe\" \"%1\""
