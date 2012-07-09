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

(add-to-list 'load-path (expand-file-name "elpa" "~/.emacs.d/"))

(when (not (require 'package nil t))
  ;; Cannot load the package.el file. Try to load one off the net.
  (let ((buffer (url-retrieve-synchronously
                 "http://tromey.com/elpa/package-install.el")))
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (re-search-forward "^$" nil 'move)
      (eval-region (point) (point-max))
      (kill-buffer (current-buffer))))
  ;; Bail out if it doesn't work a second time...
  (require 'package))

(mapc (lambda (elt)
        (add-to-list 'package-archives elt))
      '(("melpa" . "http://melpa.milkbox.net/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("tromey" . "http://tromey.com/elpa/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("copcu" . "http://cop.cuyuz.biz/elpa/")))
;; Packages I add will live in http://cop.cuyuz.biz/elpa

;; Fix load path for pacakges I need early
(defun elpa-dir (package-name)
  (expand-file-name (car
		     (file-name-all-completions package-name
						package-user-dir))
		     package-user-dir))

(let ((early-packages '("zenburn"
                        "browse-kill-ring"
                        "escreen"
                        "yasnippet"
                        "color-theme")))
  (mapc (lambda (pkg) (add-to-list 'load-path (elpa-dir pkg))) early-packages))
