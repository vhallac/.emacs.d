;;; eclimd.el --- Start and stop eclimd from within emacs

;; Copyright (C) 2012 Vedat Hallac

;; Authors: Vedat Hallac
;; Version: 1.0
;; Created: 2012/05/11
;; Keywords: java, emacs-eclim

;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

(defgroup eclimd nil
  "eclimd customizations"
  :prefix "eclimd-"
  :group 'eclimd)

(defcustom eclimd-executable
  "eclimd"
  "Full path to eclimd executable - if it is not in `exec-path'"
  :group 'eclimd)

(defcustom eclimd-default-workspace
  "~/workspace"
  "The workspace to use with eclimd"
  :group 'eclimd)

(defconst eclimd-process-buffer-name "eclimd")

(defun have-eclimd-p ()
  (executable-find eclimd-executable))

(defun eclimd-running-p ()
  (let* ((eclimd-buffer (get-buffer (concat "*" eclimd-process-buffer-name "*")))
         (eclimd-proc (get-buffer-process eclimd-buffer)))
    (not (null eclimd-proc))))

(defun eclimd--match-process-output (regexp proc)
  "Wait for the given process to output a string that matches the specified regexp.
Return the string used for `match-string' if a match is found, and nil if the process is killed.

The caller must use `save-match-data' to preserve the match data if necessary."
  (let ((old-filter (process-filter proc))
	(old-sentinel (process-sentinel proc))
	(output "")
	(terminated-p))
    (set-process-filter proc (lambda (proc string)
			       (setf output (concat output string))
			       ;; Chain to the old filter
			       (if old-filter
				   (funcall old-filter proc string))))
    (set-process-sentinel proc (lambda (proc state)
				 (unless (eq 'run
					     (process-status proc))
				   (setf terminated-p t))))
    (while (and (not terminated-p)
		(not (string-match regexp output)))
      (accept-process-output proc))
    (set-process-sentinel proc old-sentinel)
    (set-process-filter proc old-filter)
    (and (not terminated-p) output)))

(defun wait-eclimd-start ()
  "Wait for the eclimd server to become active.
This function also waits for the eclimd server to report that it is started.
It returns the port it is listening on"
  (let* ((eclimd-buffer (get-buffer (concat "*" eclimd-process-buffer-name "*")))
         (proc (get-buffer-process eclimd-buffer))
	 (eclimd-start-regexp "Eclim Server Started on: \\(?:[0-9]+\\.\\)\\{3\\}[0-9]+:\\([0-9]+\\)"))
    (save-match-data
      (let ((output (eclimd--match-process-output eclimd-start-regexp proc)))
	(when output
	  (setq eclimd-port (match-string 1 output))
	  (message (concat "eclimd serving at port " eclimd-port)))))
    eclimd-port))

(defun start-eclimd (workspace-dir)
  (interactive (list (read-directory-name "Workspace directory: "
					    eclimd-default-workspace)))
  (when (and (have-eclimd-p)
	     (not (eclimd-running-p)))
    (message (concat "Starting eclimd for workspace: " workspace-dir "..."))
    (make-comint eclimd-process-buffer-name
		 eclimd-executable
		 nil
		 (concat "-Dosgi.instance.area.default="
			 (replace-regexp-in-string "~" "@user.home" workspace-dir)))
    (wait-eclimd-start)))

(defun stop-eclimd ()
  (interactive)
  (let* ((eclimd-buffer (get-buffer (concat "*" eclimd-process-buffer-name "*")))
         (eclimd-proc (get-buffer-process eclimd-buffer)))
    (when eclimd-proc
      (eclim/execute-command "shutdown")
      (eclimd--match-process-output "Process eclimd finished" eclimd-proc)
      (delete-process eclimd-proc))
    (when eclimd-buffer
      (kill-buffer eclimd-buffer))))

(provide 'eclimd)
