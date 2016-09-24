;;; init-elpa.el - initialize package.el stuff

;;; Copyright (C) 2012 Vedat Hallac <vedathallac@gmail.com>

;; This file is not (yet) part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

(when (not (require 'package nil t))
  ;; Cannot load the package.el file. Try to load the last one that works with
  ;; emacs23. This shouldn't happen, because I've got package.el generated by
  ;; backlog-package.el in the compat dir.
  (let ((elpa-dir (expand-file-name "elpa" "~/.emacs.d/")))
    (when (not (file-directory-p elpa-dir))
      (make-directory elpa-dir))
    (add-to-list 'load-path elpa-dir))

  (let ((buffer (url-retrieve-synchronously
                 "http://repo.or.cz/w/emacs.git/blob_plain/1a0a666f941c99882093d7bd08ced15033bc3f0c:/lisp/emacs-lisp/package.el")))
    (save-excursion
      (set-buffer buffer)
      (setq buffer-file-name
            (expand-file-name "package.el" (file-name-as-directory "~/.emacs.d/elpa/")))
      (goto-char (point-min))
      (search-forward-regexp "^;;;")
      (kill-region (point-min) (point-at-bol))
      (save-buffer)
      (kill-buffer (current-buffer))))
  ;; Bail out if it doesn't work a second time...
  (require 'package))

;; Initialize module if we made it this far.
;; Do not activate packages yet. It should happen after I've got a chance to
;; initialize package configuration variables.
(package-initialize t)

(mapc (lambda (elt)
        (add-to-list 'package-archives elt))
      '(("melpa" . "http://melpa.milkbox.net/packages/")
        ("tromey" . "http://tromey.com/elpa/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("copcu" . "http://cop.cuyuz.biz/elpa/")))

;; Fix load path for packages I need early.
;; Also check if they are installed, and install them if needed.
;; These packages may end up causing side effects before configuration. Check
;; their autoload files to be sure they are safe.
(let ((early-packages '("browse-kill-ring"
                        "escreen"
                        "color-theme"
                        "paredit"
                        "ace-window"
                        "ace-jump-mode"
                        "ace-jump-buffer"
                        "use-package"))
      refreshed-p)
  (mapc (lambda (pkg)
          (let ((pkg-symbol (intern pkg)))
            (when (not (package-installed-p pkg-symbol))
              (when (not refreshed-p)
                (package-refresh-contents)
                (setq refreshed-p t))
              (package-install pkg-symbol))
            (package-activate pkg-symbol nil)))
        early-packages))

;; Fudge needed for color-theme on emacs-23: it complains about missing themes directory
(defun elpa-dir (package-name)
  (expand-file-name (car
		     (file-name-all-completions package-name
						package-user-dir))
		     package-user-dir))

(when (< emacs-major-version 24)
  (let ((themes-dir (expand-file-name "themes" (elpa-dir "color-theme"))))
    (when (not (file-directory-p themes-dir))
      (make-directory themes-dir))))
