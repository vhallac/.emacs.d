;; This file contains my initialization for the emacs-wiki.el and planner.el 
;; scripts.

;; Use emacs-wiki
;(load "~/.emacs.common/emacs-wiki_init.el")
(load "~/.emacs.common/muse_init.el")

(require 'planner-experimental)
(require 'planner-tasks-overview)
(defalias 'new-task 'planner-create-task)
(defalias 'new-task-here 'planner-create-task-from-buffer)

(require 'remember)
(require 'remember-planner)

(custom-set-variables
 '(planner-annotation-functions (quote (planner-annotation-from-info planner-annotation-from-planner-note planner-annotation-from-planner planner-annotation-from-wiki planner-annotation-from-file)))
 '(planner-directory "~/Wiki/Plans" nil (planner))
 '(planner-initial-page "index")
 '(planner-project "PlannerWiki")
 '(planner-publishing-directory "~/EmacsWikiSite/plans")
 '(planner-renumber-notes-automatically t)
 '(planner-use-other-window nil)
 '(planner-use-task-numbers t)
 '(remember-annotation-functions (quote (org-remember-annotation)))
 '(remember-data-file "~/org/remember.org")
 '(remember-handler-functions (quote (org-remember-handler)))
 '(remember-leader-text "** ")
 '(remember-mode-hook (quote (org-remember-apply-template)))
 '(remember-planner-xref-p nil))
(custom-set-faces)
