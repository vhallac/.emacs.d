(eval-when-compile
  (require 'eclim))

(setq eclim-auto-save t 		; Need to save before analyzing class
      help-at-pt-display-when-idle t
      eclim-executable (concat eclipse-dir "eclim")
      eclimd-executable (concat eclipse-dir "eclimd")
      eclimd-workspace "~/work")

(help-at-pt-set-timer)
(add-hook 'eclim-mode-hook
	  (lambda ()
	    (require 'auto-complete-config)
	    (ac-config-default)
	    (add-to-list 'ac-sources 'ac-source-eclim))) ; ac-source-emacs-eclim is also available

(defconst eclimd-process-buffer-name "eclimd")

(defun start-eclimd (workspace-dir)
  (interactive "D")
  (make-comint eclimd-process-buffer-name
	       eclimd-executable
	       nil
	       (concat "-Dosgi.instance.area.default="
		       (replace-regexp-in-string "~" "@user.home" workspace-dir))))

(defun stop-eclimd ()
  (interactive)
  (let* ((eclimd-buffer (get-buffer (concat "*" eclimd-process-buffer-name "*")))
	 (eclimd-proc (get-buffer-process eclimd-buffer)))
    (when eclimd-proc
      (delete-process eclimd-proc))
    (when eclimd-buffer
      (kill-buffer eclimd-buffer))))

(start-eclimd eclimd-default-workspace)
