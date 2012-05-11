(eval-when-compile
  (require 'org)
  (require 'org-publish)
  (require 'org-archive)
  (require 'org-html)
  (require 'yasnippet))

(setq
 org-enforce-todo-checkbox-dependencies t
 org-enforce-todo-dependencies t
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
 org-use-fast-todo-selection t
 org-time-stamp-rounding-minutes '(15 15)
 ;; TODO sequences
 org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
                     (sequence "WAITING(w@/!)" "SOMEDAY(s!)" "|"
                               "CANCELLED(c@/!)")
                     (sequence "QUOTE(q!)" "QUOTED(Q!)" "|"
                               "APPROVED(A@)" "EXPIRED(E@)" "REJECTED(R@)")
                     (sequence "OPEN(O)" "|" "CLOSED(C)"))
 org-todo-keyword-faces '(("TODO"      :foreground "red"          :weight bold)
                          ("NEXT"      :foreground "blue"         :weight bold)
                          ("DONE"      :foreground "forest green" :weight bold)
                          ("WAITING"   :foreground "yellow"       :weight bold)
                          ("SOMEDAY"   :foreground "goldenrod"    :weight bold)
                          ("CANCELLED" :foreground "orangered"    :weight bold)
                          ("QUOTE"     :foreground "hotpink"      :weight bold)
                          ("QUOTED"    :foreground "indianred1"   :weight bold)
                          ("APPROVED"  :foreground "forest green" :weight bold)
                          ("EXPIRED"   :foreground "olivedrab1"   :weight bold)
                          ("REJECTED"  :foreground "olivedrab"    :weight bold)
                          ("OPEN"      :foreground "magenta"      :weight bold)
                          ("CLOSED"    :foreground "forest green" :weight bold))
 org-todo-state-tags-triggers '(("CANCELLED" ("CANCELLED" . t))
                                ("WAITING" ("WAITING" . t))
                                ("SOMEDAY" ("WAITING" . t))
                                (done ("WAITING"))
                                ("TODO" ("WAITING") ("CANCELLED"))
                                ("NEXT" ("WAITING"))
                                ("DONE" ("WAITING") ("CANCELLED")))
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
 org-columns-default-format "%80ITEM(Task) %10Effort(Estim){:} %10CLOCKSUM{Total}"
 org-global-properties '(("Effort_ALL" . "0:0 0:10 0:30 1:00 2:00 3:00 4:00 8:00"))
 ;; Mark a task as DONE when archiving
 org-archive-mark-done nil
 org-return-follows-link t
 org-export-html-style-extra "<style type=\"text/css\">
<!--/*--><![CDATA[/*><!--*/
.src { background-color: #3f3f3f; color: #dcdccc; }
/*]]>*/-->
</style>")

(define-key org-mode-map (kbd "C-c C-p")
  (lambda ()
    (interactive "")
    (org-publish-current-project)))

(make-variable-buffer-local 'yas/trigger-key)

(add-hook 'org-mode-hook
	  (lambda ()
	    ;; yasnippet
	    (setq yas/trigger-key [tab])
	    (define-key yas/keymap [tab] 'yas/next-field)
	    ;; flyspell mode to spell check everywhere
	    (flyspell-mode 1)
	    (auto-fill-mode t)))

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