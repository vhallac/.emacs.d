;;; lua-mode.el --- a major-mode for editing Lua scripts

;; Copyright (C) 1997, 2001, 2004, 2006, 2007 Free Software Foundation, Inc.

;; Author: 2006 Juergen Hoetzel <juergen@hoetzel.info>
;;         2004 various (support for Lua 5 and byte compilation)
;;         2001 Christian Vogler <cvogler@gradient.cis.upenn.edu>
;;         1997 Bret Mogilefsky <mogul-lua@gelatinous.com> starting from
;;              tcl-mode by Gregor Schmid <schmid@fb3-s7.math.tu-berlin.de>
;;              with tons of assistance from
;;              Paul Du Bois <pld-lua@gelatinous.com> and
;;              Aaron Smith <aaron-lua@gelatinous.com>.
;; URL:		http://lua-mode.luaforge.net/
;; Version:	20070703
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

(defconst lua-version "20100302"
  "Lua Mode version number.")

;; Keywords: languages, processes, tools


;;; Commentary:

;; Thanks to Tobias Polzin <polzin<at>gmx.de> for function indenting
;; patch: Indent "(" like "{"

;; Thanks to  Fabien <fleutot<at>gmail.com> for imenu patches.

;; Special Thanks to Simon Marshall <simonm@mail.esrin.esa.it> for
;; font-lock patches.

;; Additional font-lock highlighting and indentation tweaks by
;; Adam D. Moss <adam@gimp.org> <aspirin@icculus.org>

;; This file was written with emacs using Jamie Lokier's folding mode
;; That's what the funny ;;{{{ ;;}}} marks are there for

;;{{{ INSTALLATION:

;; To install, just drop this file into a directory on your load-path (and
;; byte-compile it).  To set up Emacs to automatically edit files ending in
;; ".lua" using lua-mode add the following to your ~/.emacs file (GNU
;; Emacs) or ~/.xemacs/init.el file (XEmacs):
;;    (setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
;;    (autoload 'lua-mode "lua-mode" "Lua editing mode." t)

;;}}}
;;{{{ Usage

;; Lua-mode supports c-mode style formatting and sending of
;; lines/regions/files to a Lua interpreter. An interpreter (see
;; variable `lua-default-application') will be started if you try to
;; send some code and none is running. You can use the process-buffer
;; (named after the application you chose) as if it were an
;; interactive shell. See the documentation for `comint.el' for
;; details.

;;}}}
;;{{{ Key-bindings

;; To see all the keybindings for Lua mode, look at `lua-setup-keymap'
;; or start `lua-mode' and type `\C-h m'.
;; The keybindings may seem strange, since I prefer to use them with
;; lua-prefix-key set to nil, but since those keybindings are already used
;; the default for `lua-prefix-key' is `\C-c', which is the conventional
;; prefix for major-mode commands.

;; You can customise the keybindings either by setting `lua-prefix-key'
;; or by putting the following in your .emacs
;;      (setq lua-mode-map (make-sparse-keymap))
;; and
;;      (define-key lua-mode-map <your-key> <function>)
;; for all the functions you need.

;;}}}

;;; Code:
(defconst lua-using-xemacs (string-match "XEmacs" emacs-version)
  "Nil unless using XEmacs).")

;; We need that !
(require 'comint)

;;{{{ variables

;; Local variables
(defgroup lua nil
  "Major mode for editing lua code."
  :prefix "lua-"
  :group 'languages)

(defcustom lua-default-application "lua"
  "Default application to run in lua subprocess."
  :type 'string
  :group 'lua)

(defcustom lua-default-command-switches (list "-i")
  "Command switches for `lua-default-application'.
Should be a list of strings."
  :type '(repeat string)
  :group 'lua)

(defcustom lua-always-show t
  "*Non-nil means display lua-process-buffer after sending a command."
  :type 'boolean
  :group 'lua)

(defcustom lua-search-url-prefix "http://www.lua.org/manual/5.1/manual.html#pdf-"
  "*URL at which to search for documentation on a word"
  :type 'string
  :group 'lua)

(defvar lua-process nil
  "The active Lua subprocess")

(defvar lua-process-buffer nil
  "Buffer used for communication with Lua subprocess")

(defvar lua-mode-map nil
  "Keymap used with lua-mode.")

(defvar lua-electric-flag t
"If t, electric actions (like automatic reindentation)  will happen when an electric
 key like `{' is pressed")
(make-variable-buffer-local 'lua-electric-flag)

(defcustom lua-prefix-key "\C-c"
  "Prefix for all lua-mode commands."
  :type 'string
  :group 'lua)

(defcustom lua-prompt-regexp "[^\n]*\\(>[\t ]+\\)+$"
  "Regexp which matches the Lua program's prompt."
  :group 'lua
  :type  'regexp
  )

(defcustom lua-traceback-line-re
  "^\\(?:[\t ]*\\|.*>[\t ]+\\)\\([^\n\t ]+\\):\\([0-9]+\\):"
  "Regular expression that describes tracebacks and errors."
  :group 'lua
  :type  'regexp
  )

(defcustom lua-jump-on-traceback t
  "*Jump to innermost traceback location in *lua* buffer.  When this
variable is non-nil and a traceback occurs when running Lua code in a
subprocess, jump immediately to the source code of the innermost
traceback location."
  :group 'lua
  :type 'boolean
  )

(defvar lua-mode-hook nil
  "Hooks called when Lua mode fires up.")

(defvar lua-region-start (make-marker)
  "Start of special region for Lua communication.")

(defvar lua-region-end (make-marker)
  "End of special region for Lua communication.")

(defvar lua-indent-level 3
  "Amount by which Lua subexpressions are indented.")

(defvar lua-mode-menu (make-sparse-keymap "Lua")
  "Keymap for lua-mode's menu.")

(defvar lua-xemacs-menu
  '(["Restart With Whole File" lua-restart-with-whole-file t]
    ["Kill Process" lua-kill-process t]
    ["Hide Process Buffer" lua-hide-process-buffer t]
    ["Show Process Buffer" lua-show-process-buffer t]
    ["Beginning Of Proc" lua-beginning-of-proc t]
    ["End Of Proc" lua-end-of-proc t]
    ["Set Lua-Region Start" lua-set-lua-region-start t]
    ["Set Lua-Region End" lua-set-lua-region-end t]
    ["Send Lua-Region" lua-send-lua-region t]
    ["Send Current Line" lua-send-current-line t]
    ["Send Region" lua-send-region t]
    ["Send Proc" lua-send-proc t]
    ["Send Buffer" lua-send-buffer t]
    ["Search Documentation" lua-search-documentation t])
  "XEmacs menu for Lua mode.")

(defvar lua-font-lock-keywords
  (eval-when-compile
    (list
     ;;
     ;; Function name declarations.
     '("^[ \t]*\\<\\(\\(local[ \t]+\\)?function\\)\\>[ \t]+\\(\\(\\sw:\\|\\sw\\.\\|\\sw_\\|\\sw\\)+\\)"
       (1 font-lock-keyword-face) (3 font-lock-function-name-face nil t))

     ;; Handle function names in assignments
     '("\\(\\(\\sw:\\|\\sw\\.\\|\\sw_\\|\\sw\\)+\\)[ \t]*=[ \t]*\\(function\\)\\>"
       (1 font-lock-function-name-face nil t) (3 font-lock-keyword-face))

     ; Highlight multi-line comment blocks; since font-lock-mode doesn't
     ; claim to handle the highlighting of multi-line expressions elegantly
     ; this works best with lazy-lock-mode if your Emacs supports it, e.g.
     ; try (setq font-lock-support-mode 'lazy-lock-mode) in your ~/.emacs

     ;; Comments or strings: Handle from a function
     `(lua-match-comment-or-string
       (1 font-lock-comment-face t t)
       (2 font-lock-string-face t t)
       (5 font-lock-comment-face t t)
       (7 font-lock-string-face t t))

     ;;
     ;; Keywords.
     ;; (concat "\\<"
     ;;         (regexp-opt '("and" "break" "do" "else" "elseif" "end" "false"
     ;;                       "for" "function" "if" "in" "local" "nil" "not"
     ;;                       "or" "repeat" "return" "then" "true" "until"
     ;;                       "while") t)
     ;;         "\\>")

     ; Insert expanded regexp-opt here for the benefit of those who
     ; don't have regexp-opt available.

     "\\<\\(and\\|break\\|do\\|e\\(lse\\(if\\)?\\|nd\\)\\|f\\(alse\\|or\\|unction\\)\\|i[fn]\\|local\\|n\\(il\\|ot\\)\\|or\\|re\\(peat\\|turn\\)\\|t\\(hen\\|rue\\)\\|until\\|while\\)\\>"

     "Default expressions to highlight in Lua mode.")))

(defvar lua-imenu-generic-expression
  '((nil "^[ \t]*\\(?:local[ \t]+\\)?function[ \t]+\\(\\(\\sw:\\|\\sw_\\|\\sw\\.\\|\\sw\\)+\\)" 1))
  "Imenu generic expression for lua-mode.  See `imenu-generic-expression'.")

(defvar lua-mode-abbrev-table nil
  "Abbreviation table used in lua-mode buffers.")

(defvar lua-sexp-alist '(("then" . "end")
                        ("function" . "end")
                        ("do" . "end")))

(define-abbrev-table 'lua-mode-abbrev-table
  '(
        ("end" "end" lua-indent-line 0)
        ("else" "else" lua-indent-line 0)
        ("elseif" "elseif" lua-indent-line 0)
        ))

(defconst lua-indent-whitespace " \t"
  "Character set that constitutes whitespace for indentation in lua.")

;;}}}
;;{{{ lua-make-temp-file

(eval-and-compile
  (defalias 'lua-make-temp-file
    (if (fboundp 'make-temp-file)
	'make-temp-file
      (lambda (prefix &optional dir-flag) ;; Simple implementation
	(expand-file-name
	 (make-temp-name prefix)
	 (if (fboundp 'temp-directory)
	     (temp-directory)
	   temporary-file-directory))))))

;;}}}
;;{{{ replace-in-string

(eval-and-compile
  (if (not (fboundp 'replace-in-string)) ;GNU emacs doesn't have it
      (defun replace-in-string  (string regexp newtext &optional literal)
	(replace-regexp-in-string regexp newtext string nil literal))))

;;}}}
;;{{{ lua-mode

;;;###autoload
(define-derived-mode lua-mode fundamental-mode "Lua"
  "Major mode for editing Lua code.
The following keys are bound:
\\{lua-mode-map}
"
  (let ((switches nil)
                  s)
    (setq comint-prompt-regexp lua-prompt-regexp)
    (make-local-variable 'lua-default-command-switches)
    (set (make-local-variable 'indent-line-function) 'lua-indent-line)
    (set (make-local-variable 'comment-start) "--")
    (set (make-local-variable 'comment-start-skip) "--")
    (set (make-local-variable 'font-lock-defaults)
         '(lua-font-lock-keywords
           t
           nil
           ((?_ . "w")
            (?+ . ".")
            (?* . ".")
            (?/ . ".")
            (?^ . ".")
            (?< . ".")
            (?> . ".")
            (?= . ".")
            (?~ . ".")
            (?- . ". 12")
            (?\n . ">")
            (?' . "\"")
            (?\" . "\"")
            ;; This might be better as punctuation, as
            ;; for C, but this way you can treat table
            ;; index as symbol.
            (?. . "_")) ; e.g. `io.string'
           ))
    (set (make-local-variable 'imenu-generic-expression)
                        lua-imenu-generic-expression)
    (setq local-abbrev-table lua-mode-abbrev-table)
    (abbrev-mode 1)
    (make-local-variable 'lua-default-eval)
    (or lua-mode-map
        (lua-setup-keymap))
    (use-local-map lua-mode-map)
    (if (and lua-using-xemacs
	     (featurep 'menubar)
	     current-menubar
	     (not (assoc "Lua" current-menubar)))
	(progn
	  (set-buffer-menubar (copy-sequence current-menubar))
	  (add-menu nil "Lua" lua-xemacs-menu)))
    ;; Append Lua menu to popup menu for XEmacs.
    (if (and lua-using-xemacs (boundp 'mode-popup-menu))
	(setq mode-popup-menu
	      (cons (concat mode-name " Mode Commands") lua-xemacs-menu)))

    ;; hideshow setup
    (unless (assq 'lua-mode hs-special-modes-alist)
      (add-to-list 'hs-special-modes-alist
		   `(lua-mode
		     ,(regexp-opt (mapcar 'car lua-sexp-alist) 'words);start
		     ,(regexp-opt (mapcar 'cdr lua-sexp-alist) 'words) ;end
		     nil lua-forward-sexp)))
    (run-hooks 'lua-mode-hook)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))

;;}}}
;;{{{ lua-setup-keymap

(defun lua-setup-keymap ()
  "Set up keymap for Lua mode.
If the variable `lua-prefix-key' is nil, the bindings go directly
to `lua-mode-map', otherwise they are prefixed with `lua-prefix-key'."
  (setq lua-mode-map (make-sparse-keymap))
  (define-key lua-mode-map [menu-bar lua-mode]
    (cons "Lua" lua-mode-menu))
  (define-key lua-mode-map "}" 'lua-electric-match)
  (define-key lua-mode-map "]" 'lua-electric-match)
  (define-key lua-mode-map ")" 'lua-electric-match)
  (define-key lua-mode-map (kbd "C-M-a") 'lua-beginning-of-proc)
  (define-key lua-mode-map (kbd "C-M-e") 'lua-end-of-proc)
  (let ((map (if lua-prefix-key
                                          (make-sparse-keymap)
                                        lua-mode-map)))

         ;; communication
         (define-key map "\C-c" 'comment-region)
	 (define-key map "\C-l" 'lua-send-buffer)
	 (define-key map "\C-f" 'lua-search-documentation)
         (if lua-prefix-key
                  (define-key lua-mode-map lua-prefix-key map))
         ))

;;}}}
;;{{{ lua-electric-match

(defun lua-electric-match (arg)
  "Insert character and adjust indentation."
  (interactive "P")
  (insert-char last-command-char (prefix-numeric-value arg))
  (if lua-electric-flag
      (lua-indent-line))
  (blink-matching-open))

;;}}}

;;{{{ private functions
(defun lua-syntax-status ()
  "Returns the syntactic status of the character after the point."
  (parse-partial-sexp (save-excursion (beginning-of-line) (point))
		      (point)))

(defun lua-string-p ()
  "Returns true if the point is in a string."
  (or (get-text-property (point) 'in-string)
      (elt (lua-syntax-status) 3)))

(defun lua-comment-p ()
  "Returns true if the point is in a comment."
  (or (get-text-property (point) 'in-comment)
      (elt (lua-syntax-status) 4)))

(defun lua-comment-or-string-p ()
  "Returns true if the point is in a comment or string."
  (or (get-text-property (point) 'in-string)
      (get-text-property (point) 'in-comment)
      (let ((parse-result (lua-syntax-status)))
        (or (elt parse-result 3) (elt parse-result 4)))))

;;}}}
;;{{{ lua-indent-line

(defun lua-indent-line ()
  "Indent current line for Lua mode.
Return the amount the indentation changed by."
  (let ((indent (max 0 (lua-calculate-indentation nil)))
        beg shift-amt
        (case-fold-search nil)
        (pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward lua-indent-whitespace)
    (setq shift-amt (- indent (current-column)))
    (when (not (zerop shift-amt))
      (delete-region beg (point))
      (indent-to indent))
    ;; If initial point was within line's indentation,
    ;; position after the indentation.  Else stay at same point in text.
    (if (> (- (point-max) pos) (point))
	(goto-char (- (point-max) pos)))
    shift-amt
    indent))

;;}}}
;;{{{ lua-find-regexp

(defun lua-find-regexp (direction regexp &optional limit ignore-p)
  "Searches for a regular expression in the direction specified.
Direction is one of 'forward and 'backward.
By default, matches in comments and strings are ignored, but what to ignore is
configurable by specifying ignore-p. If the regexp is found, returns point
position, nil otherwise.
ignore-p returns true if the match at the current point position should be
ignored, nil otherwise."
  (let ((ignore-func (or ignore-p 'lua-comment-or-string-p))
	(search-func (if (eq direction 'forward)
			 're-search-forward 're-search-backward))
	(case-fold-search nil))
    (catch 'found
      (while (funcall search-func regexp limit t)
        (save-excursion
          (goto-char (match-beginning 0))
          (when (not (funcall ignore-func))
            (throw 'found (point))))))))

;;}}}
;;{{{ lua-backwards-to-block-begin-or-end

(defun lua-backwards-to-block-begin-or-end ()
  "Move backwards to nearest block begin or end.  Returns nil if not successful."
  (interactive)
  (lua-find-regexp 'backward lua-block-regexp))

;;}}}
;;{{{ var. constants

(defconst lua-block-regexp
  (eval-when-compile
    ;; This is the code we used to generate the regexp:
    (concat
     "\\(\\<"
     (regexp-opt '("do" "function" "repeat" "then"
		   "else" "elseif" "end" "until") t)
     "\\>\\)\\|"
     (regexp-opt '("{" "(" "[" "]" ")" "}") t))

	))

(defconst lua-block-token-alist
  ;; FIXME: do is a really bad exception. It is sometimes an open, and sometimes
  ;; a middle. Need a better model for it. As it stands, the end of do lines up
  ;; agains the do itself, and not the beginning.
  '(("do"       "\\<for\\|while\\|end\\>"                     middle-or-open)
    ("function" "\\<end\\>"                                   open)
    ("repeat"   "\\<until\\>"                                 open)
    ("then"     "\\<if\\|\\(e\\(lse\\(if\\)?\\|nd\\)\\)\\>"   middle)
    ("{"        "}"                                           open)
    ("["        "]"                                           open)
    ("("        ")"                                           open)
    ("if"       "\\<then\\>"                                  open)
    ("for"      "\\<do\\>"                                    open)
    ("while"    "\\<do\\>"                                    open)
    ("else"     "\\<then\\>"                                  middle)
    ("elseif"   "\\<then\\>"                                  middle)
    ("end"      "\\<\\(do\\|function\\|then\\|else\\)\\>"     close)
    ("until"    "\\<repeat\\>"                                close)
    ("}"        "{"                                           close)
    ("]"        "\\["                                         close)
    (")"        "("                                           close)))


(defconst lua-indentation-modifier-regexp
    ;; This is the code we used to generate the regexp:
    (concat
;;     "\\(\\(?:[:a-zA-Z]* *=\\)* *function\\|\\<"
     "\\(\\<"
     ; n.b. "local function" is a bit of a hack, allowing only a single space
     ;;(regexp-opt '("do" "local function" "function" "repeat" "then" "if" "else" "elseif") t)
     (regexp-opt '("do" "function" "repeat" "then" "if" "else" "elseif" "for" "while") t)
     "\\>\\|"
     (regexp-opt '("{" "(" "["))
     "\\)\\|\\(\\<"
     (regexp-opt '("end" "until") t)
     "\\>\\|"
     (regexp-opt '("]" ")" "}"))
     "\\)")

    )

;;}}}
;;{{{ lua-find-matching-token-word

(defun lua-find-matching-token-word (token search-start &optional direction)
  (let* ((token-info (assoc token lua-block-token-alist))
         (match (car (cdr token-info)))
         (match-type (car (cdr (cdr token-info))))
         (search-direction (or direction
                               (if (eq match-type 'open) 'forward 'backward)))
         maybe-found-pos)
    ;; if we are searching forward from the token at the current point
    ;; (i.e. for a closing token), need to step one character forward
    ;; first, or the regexp will match the opening token.
    (if (eq search-direction 'forward) (forward-char 1))
    (if search-start (goto-char search-start))
    (catch 'found
      (while (lua-find-regexp search-direction lua-indentation-modifier-regexp)
        ;; have we found a valid matching token?
        (let ((found-token (match-string 0))
              (found-pos (match-beginning 0)))
          (let ((found-type (car (cdr (cdr (assoc found-token lua-block-token-alist))))))
            (if (not (string-match match found-token))
                ;; no - then there is a nested block. If we were looking for
                ;; a block begin token, found-token must be a block end
                ;; token; likewise, if we were looking for a block end token,
                ;; found-token must be a block begin token, otherwise there
                ;; is a grammatical error in the code.
                (if (not (and
                          (or (eq match-type 'middle)
                              (eq found-type 'middle)
                              (eq match-type 'middle-or-open)
                              (eq found-type 'middle-or-open)
                              (eq match-type found-type))
                          (lua-find-matching-token-word found-token nil
                                                        search-direction)))
                    (when maybe-found-pos
                      (goto-char maybe-found-pos)
                      (throw 'found maybe-found-pos)))
              ;; yes.
              ;; if it is a not a middle kind, report the location
              (when (not (or (eq found-type 'middle)
                             (eq found-type 'middle-or-open)))
                (throw 'found found-pos))
              ;; if it is a middle-or-open type, record location, but keep searching.
              ;; If we fail to complete the search, we'll report the location
              (when (eq found-type 'middle-or-open)
                (setq maybe-found-pos found-pos))
              ;; Cannot use tail recursion. too much nesting on long chains of
              ;; if/elseif. Will reset variables instead.
              (setq token found-token)
              (setq token-info (assoc token lua-block-token-alist))
              (setq match (car (cdr token-info)))
              (setq match-type (car (cdr (cdr token-info))))))))
    maybe-found-pos)))

;;}}}
;;{{{ lua-goto-matching-block-token

(defun lua-goto-matching-block-token (&optional search-start parse-start direction)
  "Find block begion/end token matching the one at the point.
This function moves the point to the token that matches the one
at the current point. Returns the point position of the first character of
the matching token if successful, nil otherwise."
  (if parse-start (goto-char parse-start))
  (let ((case-fold-search nil))
    (if (looking-at lua-indentation-modifier-regexp)
	(let ((position (lua-find-matching-token-word (match-string 0)
						      search-start direction)))
	  (and position
	       (goto-char position))))))


;; The following may be useful to speed up the search in the future.
;      (let ((token-type (char-syntax (string-to-char token-to-match)))
;	    matching-pos)
;	(cond ((eq token-type ?\()
;	       (setq matching-pos (scan-sexps (point) 1 (current-buffer) t))
;	       (when matching-pos (goto-char matching-pos)))

;	      ((eq token-type ?\))
;	       ;; need to move one char forward, because scan-sexps
;	       ;; expects the point to be one past the closing parenthesis
;	       (forward-char 1)
;	       (setq matching-pos (scan-sexps (point) -1 (current-buffer) t))
;	       (when matching-pos (goto-char matching-pos)))

;	      (t
;	       (lua-goto-matching-token-word token-to-match search-start)))))))


;;}}}
;;{{{ lua-goto-matching-block

(defun lua-goto-matching-block (&optional noreport)
  "Go to the keyword balancing the one under the point.
If the point is on a keyword/brace that starts a block, go to the
matching keyword that ends the block, and vice versa."
  (interactive)
  ;; search backward to the beginning of the keyword if necessary
  (if (eq (char-syntax (following-char)) ?w)
      (re-search-backward "\\<" nil t))
  (let ((position (lua-goto-matching-block-token)))
    (if (and (not position)
	     (not noreport))
	(error "Not on a block control keyword or brace.")
      position)))

;;}}}
;;{{{ lua-goto-nonblank-previous-line

(defun lua-goto-nonblank-previous-line ()
  "Puts the point at the first previous line that is not blank.
Returns the point, or nil if it reached the beginning of the buffer"
  (catch 'found
    (beginning-of-line)
    (while t
      (if (bobp) (throw 'found nil))
      (forward-char -1)
      (beginning-of-line)
      (if (not (looking-at "\\s *\\(--.*\\)?$")) (throw 'found (point))))))

;;}}}
;;{{{ lua-goto-nonblank-next-line

(defun lua-goto-nonblank-next-line ()
  "Puts the point at the first next line that is not blank.
Returns the point, or nil if it reached the end of the buffer"
  (catch 'found
    (end-of-line)
    (while t
      (forward-line)
      (if (eobp) (throw 'found nil))
      (beginning-of-line)
      (if (not (looking-at "\\s *\\(--.*\\)?$")) (throw 'found (point))))))

(eval-when-compile
  (defconst lua-operator-class
    "-+*/^.=<>~"))

;;}}}
;;{{{ var. constans

(defconst lua-cont-eol-regexp
  (eval-when-compile
    ;; expression used to generate the regexp
    (concat
     "\\(\\<"
     (regexp-opt '("and" "or" "not" "in" "for" "while"
		   "local" "function") t)
     "\\>\\|"
     "\\(^\\|[^" lua-operator-class "]\\)"
     (regexp-opt '("+" "-" "*" "/" "^" ".." "==" "=" "<" ">" "<=" ">=" "~=") t)
     "\\)"
     "\\s *\\=")

    ))


(defconst lua-cont-bol-regexp
  (eval-when-compile
    ;; expression used to generate the regexp
    (concat
     "\\=\\s *"
     "\\(\\<"
     (regexp-opt '("and" "or" "not") t)
     "\\>\\|"
     (regexp-opt '("+" "-" "*" "/" "^" ".." "==" "=" "<" ">" "<=" ">=" "~=") t)
     "\\($\\|[^" lua-operator-class "]\\)"
     "\\)")

    ))

;;}}}
;;{{{ lua-last-token-continues-p

(defun lua-last-token-continues-p ()
  "Returns true if the last token on this line is a continuation token."
  (let (line-begin
	line-end)
    (save-excursion
      (beginning-of-line)
      (setq line-begin (point))
      (end-of-line)
      (setq line-end (point))
      ;; we need to check whether the line ends in a comment and
      ;; skip that one.
      (while (lua-find-regexp 'backward "-" line-begin 'lua-string-p)
	(if (looking-at "--")
	    (setq line-end (point))))
      (goto-char line-end)
      (re-search-backward lua-cont-eol-regexp line-begin t))))

;;}}}
;;{{{ lua-first-token-continues-p

(defun lua-first-token-continues-p ()
  "Returns true if the first token on this line is a continuation token."
  (let (line-end)
    (save-excursion
      (end-of-line)
      (setq line-end (point))
      (beginning-of-line)
      (re-search-forward lua-cont-bol-regexp line-end t))))

;;}}}
;;{{{ lua-is-continuing-statement-p

(defun lua-is-continuing-statement-p (&optional parse-start)
  "Return nonnil if the line continues a statement.
More specifically, return the point in the line that is continued.
The criteria for a continuing statement are:

* the last token of the previous line is a continuing op,
  OR the first token of the current line is a continuing op

"
  (let ((prev-line nil))
    (save-excursion
      (if parse-start (goto-char parse-start))
      (save-excursion (setq prev-line (lua-goto-nonblank-previous-line)))
      (and prev-line
	   (or (lua-first-token-continues-p)
	       (and (goto-char prev-line)
		    ;; check last token of previous nonblank line
		    (lua-last-token-continues-p)))))))

;;}}}
;;{{{ lua-make-indentation-info-pair

(defun lua-make-indentation-info-pair ()
  "This is a helper function to lua-calculate-indentation-info. Don't
use standalone."
  (cond
   ;; function is a bit tricky to indent right. They can appear in a lot ot
   ;; different contexts. Until I find a shortcut, I'll leave it with a simple
   ;; relative indentation.
   ;; The special cases are for indenting according to the location of the
   ;; function. i.e.:
   ;;       (cons 'absolute (+ (current-column) lua-indent-level))
   ;; TODO: Fix this. It causes really ugly indentations for in-line functions.
   ((string-equal found-token "function")
    (cons 'relative lua-indent-level))

   ;; block openers
   ((member found-token (list "{" "(" "["))
	 (save-excursion
	   ;; expression follows -> indent at start of next expression
       ;; Last token on the line -> simple relative indent
	   (if (and (not (search-forward-regexp "[[:space:]]--" (line-end-position) t))
                (search-forward-regexp "[^[:space:]]" (line-end-position) t))
           (cons 'absolute (1- (current-column)))
         (cons 'relative lua-indent-level))))

   ;; These are not really block starters. They should not add to indentation.
   ;; The corresponding "then" and "do" handle the indentation.
   ((member found-token (list "if" "for" "while"))
     (cons 'relative 0))
	;; closing tokens follow: These are usually taken care of by
	;; lua-calculate-indentation-override.
    ;; elseif is a bit of a hack. It is not handled separately, but it needs to
    ;; nullify a previous then if on the same line.
   	((member found-token (list "until" "elseif"))
     (save-excursion
       (let ((line (line-number-at-pos)))
         (if (and (lua-goto-matching-block-token nil found-pos)
                  (= line (line-number-at-pos)))
             (cons 'remove-matching 0)
           (cons 'relative 0)))))

    ;; else is a special case; if its matching block token is on the same line,
    ;; instead of removing the matching token, it has to replace it, so that
    ;; either the next line will be indented correctly, or the end on the same
    ;; line will remove the effect of the else.
    ((string-equal found-token "else")
     (save-excursion
       (let ((line (line-number-at-pos)))
         (if (and (lua-goto-matching-block-token nil found-pos)
                  (= line (line-number-at-pos)))
             (cons 'replace-matching (cons 'relative lua-indent-level))
                   (cons 'relative lua-indent-level)))))

    ;; Block closers. If they are on the same line as their openers, they simply
    ;; eat up the matching indentation modifier. Otherwise, they pull
    ;; indentation back to the matching block opener.
	((member found-token (list ")" "}" "]" "end"))
     (save-excursion
       (let ((line (line-number-at-pos)))
         (lua-goto-matching-block-token nil found-pos)
         (if (/= line (line-number-at-pos))
             (cons 'absolute
                   (+ (current-indentation)
                      (lua-calculate-indentation-block-modifier
                       nil (point))))
           (cons 'remove-matching 0)))))

    ;; Everything else. This is from the original code: If opening a block
    ;; (match-data 1 exists), then push indentation one level up, if it is
    ;; closing a block, pull it one level down.
	(t
	 (cons 'relative (if (nth 2 (match-data))
                         ;; beginning of a block matched
                         lua-indent-level
                       ;; end of a block matched
                       (- lua-indent-level))))))


;;}}}
;;{{{ lua-calculate-indentation-info

(defun lua-cleanup-indentation-info (info)
  (when info
    (cond
     ( (eq 'remove-matching (caar info))
       (cdr (lua-cleanup-indentation-info (cdr info))))

     ( (eq 'replace-matching (caar info))
       (append (list (cdr (car info))) (cdr (lua-cleanup-indentation-info (cdr info)))))

     ( t
       (append (list (car info)) (lua-cleanup-indentation-info (cdr info)))))))
    ;; (if (eq 'remove-matching (caar info))
    ;;     (if (eq 'remove-matching (caadr info))
    ;;         (cdr (lua-cleanup-indentation-info (cdr info)))
    ;;       (lua-cleanup-indentation-info (cddr info)))
    ;;   (append (list (car info)) (lua-cleanup-indentation-info (cdr info))))))

(defun lua-calculate-indentation-info (&optional parse-start parse-end)
  "For each block token on the line, computes how it affects the indentation.
The effect of each token can be either a shift relative to the current
indentation level, or indentation to some absolute column. This information
is collected in a list of indentation info pairs, which denote absolute
and relative each, and the shift/column to indent to."
  (let ((combined-line-end (line-end-position))
        (start-indentation (current-indentation)))
    (save-excursion
      (while (lua-last-token-continues-p)
        (lua-goto-nonblank-next-line)
        (setq combined-line-end (line-end-position))))
    (let ((search-stop (if parse-end
                           (min parse-end combined-line-end)
                         combined-line-end))
          (indentation-info nil))
      (if parse-start (goto-char parse-start))
      (save-excursion
        (beginning-of-line)
        (while (lua-find-regexp 'forward lua-indentation-modifier-regexp
                                search-stop)
          (let ((found-token (match-string 0))
                (found-pos (match-beginning 0))
                (found-end (match-end 0))
                (data (match-data)))
            (setq indentation-info
                  (cons (lua-make-indentation-info-pair) indentation-info))))

        (or (and indentation-info (lua-cleanup-indentation-info indentation-info))
            (list (cons 'absolute start-indentation)))))))

;;}}}
;;{{{ lua-accumulate-indentation-info

(defun lua-accumulate-indentation-info (info)
  "Accumulates the indentation information previously calculated by
lua-calculate-indentation-info. Returns either the relative indentation
shift, or the absolute column to indent to."
  (let ((info-list (reverse info))
	(type 'relative)
	(accu 0))
    (mapcar (lambda (x)
	    (setq accu (if (eq 'absolute (car x))
			   (progn (setq type 'absolute)
				  (cdr x))
			 (+ accu (cdr x)))))
	  info-list)
    (cons type accu)))

;;}}}
;;{{{ lua-calculate-indentation-block-modifier

(defun lua-calculate-indentation-block-modifier (&optional parse-start
							   parse-end)
  "Return amount by which this line modifies the indentation.
Beginnings of blocks add lua-indent-level once each, and endings
of blocks subtract lua-indent-level once each. This function is used
to determine how the indentation of the following line relates to this
one."
  (if parse-start (goto-char parse-start))
  ;; First go back to the line that starts it all
  ;; lua-calculate-indentation-info will scan through the whole thing
  (while (lua-is-continuing-statement-p)
    (lua-goto-nonblank-previous-line))
  (let ((case-fold-search nil)
        (indentation-info (lua-accumulate-indentation-info
                           (lua-calculate-indentation-info nil parse-end))))
    (if (eq (car indentation-info) 'absolute)
        (- (cdr indentation-info) (current-indentation))
      (cdr indentation-info))))

;;}}}

;;{{{ lua-point-is-after-left-shifter-p

(defun lua-point-is-after-left-shifter-p ()
  "Check if point is at a left-shifter.
A left-shifter is a partial lua expression which should be ignored for line up purposes when closing a block. An example of this is:
   local a = function()
      ....
   end
   ^         ^
   |         +- not here
   +- Close here"
  (save-excursion
    (let ((old-point (point)))
      (beginning-of-line)
      (skip-chars-forward lua-indent-whitespace)
      (and
       (or (looking-at "local\\s +\\(?:\\(?:\\sw\\|\\s_\\)+\\s *\\(,\\s *\\(?:\\sw\\|\\s_\\)+\\s *\\)*=\\s *\\)?")
           ;; This is too generic, and will screw up a lot of indentations. Will need
           ;; a better regexp for assignments
           (looking-at "[^=]*=\\s *"))
       (= old-point (match-end 0))))))


;;}}}
;;{{{ lua-calculate-indentation-override

(defun lua-calculate-indentation-override (&optional parse-start)
  "Return amount, by which this line should be shifted left.
Look for an uninterrupted sequence of block-closing tokens that starts
at the beginning of the line. For each of these tokens, shift indentation
to the left by the amount specified in lua-indent-level."
  (let ((indentation-modifier 0)
        (case-fold-search nil)
        (block-token nil))
    (save-excursion
      (if parse-start (goto-char parse-start))
      (beginning-of-line)
      ;; Look for the last block closing token
      (skip-chars-forward lua-indent-whitespace)
      (if (and (not (lua-comment-or-string-p))
               (looking-at lua-indentation-modifier-regexp)
               (let ((token-info (assoc (match-string 0) lua-block-token-alist)))
                 (and token-info
                      (not (eq 'open (caddr token-info))))))
          (when (lua-goto-matching-block-token nil nil 'backward)
            ;; Exception cases: when the start of the line is an assignment,
            ;; go to the start of the assignment instead of the matching item
            (let ((block-start-column (current-column))
                  (block-start-point (point)))
              (if (lua-point-is-after-left-shifter-p)
                  (current-indentation)
                block-start-column)))))))

;;}}}
;;{{{ lua-calculate-indentation

(defun lua-calculate-indentation (&optional parse-start)
  "Return appropriate indentation for current line as Lua code.
In usual case returns an integer: the column to indent to."
  (let ((pos (point))
        (indent-amt 0))
    (save-excursion
      (let ((continuing-p (lua-is-continuing-statement-p)))
        (or (lua-calculate-indentation-override)
            (when (lua-goto-nonblank-previous-line)
              ;; the order of function calls here is important. block modifier
              ;; call may change the point to another line
              (let ((modifier
                     (lua-calculate-indentation-block-modifier nil (line-end-position))))
                (+ (if (and continuing-p (= 0 modifier))
                       lua-indent-level
                     modifier)
                   (current-indentation))))
            0)))))

;;}}}
;;{{{ lua-beginning-of-proc

(defun lua-beginning-of-proc (&optional arg)
  "Move backward to the beginning of a lua proc (or similar).
With argument, do it that many times.  Negative arg -N
means move forward to Nth following beginning of proc.
Returns t unless search stops due to beginning or end of buffer."
  (interactive "P")
  (or arg
      (setq arg 1))
  (let ((found nil)
                  (ret t))
    (if (and (< arg 0)
                                 (looking-at "^function[ \t]"))
                  (forward-char 1))
    (while (< arg 0)
      (if (re-search-forward "^function[ \t]" nil t)
                         (setq arg (1+ arg)
                                         found t)
                  (setq ret nil
                                  arg 0)))
    (if found
                  (beginning-of-line))
    (while (> arg 0)
      (if (re-search-backward "^function[ \t]" nil t)
                         (setq arg (1- arg))
                  (setq ret nil
                                  arg 0)))
    ret))

;;}}}
;;{{{ lua-end-of-proc

(defun lua-end-of-proc (&optional arg)
  "Move forward to next end of lua proc (or similar).
With argument, do it that many times.  Negative argument -N means move
back to Nth preceding end of proc.

This function just searches for a `end' at the beginning of a line."
  (interactive "P")
  (or arg
      (setq arg 1))
  (let ((found nil)
	(ret t))
    (if (and (< arg 0)
	     (not (bolp))
	     (save-excursion
	       (beginning-of-line)
	       (eq (following-char) ?})))
	(forward-char -1))
    (while (> arg 0)
      (if (re-search-forward "^end" nil t)
	  (setq arg (1- arg)
		found t)
	(setq ret nil
	      arg 0)))
    (while (< arg 0)
      (if (re-search-backward "^end" nil t)
	  (setq arg (1+ arg)
		found t)
	(setq ret nil
	      arg 0)))
    (if found
	(end-of-line))
    ret))

;;}}}
;;{{{ lua-start-process

(defun lua-start-process (name &optional program startfile &rest switches)
  "Start a lua process named NAME, running PROGRAM."
  (or switches
      (setq switches lua-default-command-switches))
  (setq program (or program name))
  (setq lua-process-buffer (apply 'make-comint name program startfile switches))
  (setq lua-process (get-buffer-process lua-process-buffer))
  ;; wait for prompt
  (with-current-buffer lua-process-buffer
    (while (not (lua-prompt-line))
      (accept-process-output (get-buffer-process (current-buffer)))
      (goto-char (point-max)))))

;;}}}
;;{{{ lua-kill-process

(defun lua-kill-process ()
  "Kill lua subprocess and its buffer."
  (interactive)
  (if lua-process-buffer
      (kill-buffer lua-process-buffer)))

;;}}}
;;{{{ lua-set-lua-region-start

(defun lua-set-lua-region-start (&optional arg)
  "Set start of region for use with `lua-send-lua-region'."
  (interactive)
  (set-marker lua-region-start (or arg (point))))

;;}}}
;;{{{ lua-set-lua-region-end

(defun lua-set-lua-region-end (&optional arg)
  "Set end of region for use with `lua-send-lua-region'."
  (interactive)
  (set-marker lua-region-end (or arg (point))))

;;}}}
;;{{{ lua-send-current-line

(defun lua-send-current-line ()
  "Send current line to lua subprocess, found in `lua-process'.
If `lua-process' is nil or dead, start a new process first."
  (interactive)
  (let ((start (save-excursion (beginning-of-line) (point)))
        (end (save-excursion (end-of-line) (point))))
    (lua-send-region start end)))

;;}}}
;;{{{ lua-send-region

(defun lua-send-region (start end)
  "Send region to lua subprocess."
  (interactive "r")
  ;; make temporary lua file
  (let ((tempfile (lua-make-temp-file "lua-"))
	(last-prompt nil)
	(prompt-found nil)
	(lua-stdin-line-offset (count-lines (point-min) start))
	(lua-stdin-buffer (current-buffer))
	current-prompt )
    (write-region start end tempfile)
    (or (and lua-process
	     (comint-check-proc lua-process-buffer))
	(lua-start-process lua-default-application))
    ;; kill lua process without query
    (if (fboundp 'process-kill-without-query)
	(process-kill-without-query lua-process))
    ;; send dofile(tempfile)
    (with-current-buffer lua-process-buffer
      (goto-char (point-max))
      (setq last-prompt (point-max))
      (comint-simple-send (get-buffer-process (current-buffer))
			  (format "dofile(\"%s\")"
				  (replace-in-string tempfile "\\\\" "\\\\\\\\" )))
      ;; wait for prompt
      (while (not prompt-found)
	(accept-process-output (get-buffer-process (current-buffer)))
	(goto-char (point-max))
	(setq prompt-found (and (lua-prompt-line) (< last-prompt (point-max)))))
    ;; remove temp. lua file
    (delete-file tempfile)
    (lua-postprocess-output-buffer lua-process-buffer last-prompt lua-stdin-line-offset)
    (if lua-always-show
	(display-buffer lua-process-buffer)))))

;;}}}
;;{{{ lua-postprocess-output-buffer

(defun lua-postprocess-output-buffer (buf start &optional lua-stdin-line-offset)
  "Highlight tracebacks found in buf. If an traceback occurred return
t, otherwise return nil.  BUF must exist."
  (let ((lua-stdin-line-offset (or lua-stdin-line-offset 0))
	line file bol err-p)
    (save-excursion
      (set-buffer buf)
      (goto-char start)
      (beginning-of-line)
      (if (re-search-forward lua-traceback-line-re nil t)
	  (setq file (match-string 1)
		line (string-to-int (match-string 2)))))
    (when (and lua-jump-on-traceback line)
      (beep)
      ;; TODO: highlight
      (lua-jump-to-traceback file line lua-stdin-line-offset)
      (setq err-p t))
    err-p))

;;}}}
;;{{{ lua-jump-to-tracebackw

(defun lua-jump-to-traceback (file line lua-stdin-line-offset)
  "Jump to the Lua code in FILE at LINE."
  ;; sanity check: temporary-file-directory
  (if (string= (substring file 0 3)  "...")
      (message "Lua traceback output truncated: customize 'temporary-file-directory' or increase 'LUA_IDSIZE' in 'luaconf.h'.")
    (let ((buffer (cond ((or (string-equal file tempfile) (string-equal file "stdin"))
		       (setq line (+ line lua-stdin-line-offset))
		       lua-stdin-buffer)
			(t (find-file-noselect file)))))
      (pop-to-buffer buffer)
      ;; Force Lua mode
      (if (not (eq major-mode 'lua-mode))
	  (lua-mode))
      ;; TODO fix offset when executing region
      (goto-line line)
      (message "Jumping to error in file %s on line %d" file line))))

;;}}}
;;{{{ lua-prompt-line

(defun lua-prompt-line ()
  (save-excursion
    (save-match-data
      (forward-line 0)
      (if (looking-at comint-prompt-regexp)
	  (match-end 0)))))

;;{{{ lua-send-lua-region
;;}}}

(defun lua-send-lua-region ()
  "Send preset lua region to lua subprocess."
  (interactive)
  (or (and lua-region-start lua-region-end)
      (error "lua-region not set"))
  (or (and lua-process
           (comint-check-proc lua-process-buffer))
      (lua-start-process lua-default-application))
  (comint-simple-send lua-process
                              (buffer-substring lua-region-start lua-region-end)
)
  (if lua-always-show
      (display-buffer lua-process-buffer)))

;;}}}
;;{{{ lua-send-proc

(defun lua-send-proc ()
  "Send proc around point to lua subprocess."
  (interactive)
  (let (beg end)
    (save-excursion
      (lua-beginning-of-proc)
      (setq beg (point))
      (lua-end-of-proc)
      (setq end (point)))
    (or (and lua-process
             (comint-check-proc lua-process-buffer))
        (lua-start-process lua-default-application))
    (comint-simple-send lua-process
                                (buffer-substring beg end))
    (if lua-always-show
        (display-buffer lua-process-buffer))))

;;}}}
;;{{{ lua-send-buffer

; This needs work... -Bret
(defun lua-send-buffer ()
  "Send whole buffer to lua subprocess."
  (interactive)
  (lua-send-region (point-min) (point-max)))

;;}}}
;;{{{ lua-restart-with-whole-file

(defun lua-restart-with-whole-file ()
  "Restart lua subprocess and send whole file as input."
  (interactive)
  (lua-kill-process)
  (lua-start-process lua-default-application)
  (lua-send-buffer))

;;}}}
;;{{{ lua-show-process-buffer

(defun lua-show-process-buffer ()
  "Make sure `lua-process-buffer' is being displayed."
  (interactive)
  (display-buffer lua-process-buffer))

;;}}}
;;{{{ lua-hide-process-buffer

(defun lua-hide-process-buffer ()
  "Delete all windows that display `lua-process-buffer'."
  (interactive)
  (delete-windows-on lua-process-buffer))

;;}}}
;;{{{ lua-search-documentation

(defun lua-search-documentation ()
  "Search Lua documentation for the word at the point."
  (interactive)
  (browse-url (concat lua-search-url-prefix (current-word t))))

;;}}}
;;{{{ lua-calculate-state

(defun lua-calculate-state (arg prevstate)
  ;; Calculate the new state of PREVSTATE, t or nil, based on arg. If
  ;; arg is nil or zero, toggle the state. If arg is negative, turn
  ;; the state off, and if arg is positive, turn the state on
  (if (or (not arg)
	  (zerop (setq arg (prefix-numeric-value arg))))
      (not prevstate)
    (> arg 0)))

;;}}}
;;{{{ lua-toggle-electric-state

(defun lua-toggle-electric-state (&optional arg)
  "Toggle the electric indentation feature.
Optional numeric ARG, if supplied, turns on electric indentation when
positive, turns it off when negative, and just toggles it when zero or
left out."
  (interactive "P")
  (setq lua-electric-flag (lua-calculate-state arg lua-electric-flag)))

;;}}}
;;{{{ lua-forward-sexp

(defun lua-forward-sexp (&optional count)
 "Forward to block end"
 (interactive "p")
 (save-match-data
 (let* ((count (or count 1))
	(stackheight 0)
	(block-start (mapcar 'car lua-sexp-alist))
	(block-end (mapcar 'cdr lua-sexp-alist))
	(block-regex (regexp-opt (append  block-start block-end) 'words))
	current-exp
	)
   (while (> count 0)
     ;; skip whitespace
     (skip-chars-forward " \t\n")
     (if (looking-at (regexp-opt block-start 'words))
	 (let ((keyword (match-string 1)))
	   (lua-find-matching-token-word keyword nil))
       ;; If the current keyword is not a "begin" keyword, then just
       ;; perform the normal forward-sexp.
       (forward-sexp 1))
     (setq count (1- count))))))


;;}}}
;;{{{ menu bar

(define-key lua-mode-menu [restart-with-whole-file]
  '("Restart With Whole File" .  lua-restart-with-whole-file))
(define-key lua-mode-menu [kill-process]
  '("Kill Process" . lua-kill-process))

(define-key lua-mode-menu [hide-process-buffer]
  '("Hide Process Buffer" . lua-hide-process-buffer))
(define-key lua-mode-menu [show-process-buffer]
  '("Show Process Buffer" . lua-show-process-buffer))

(define-key lua-mode-menu [end-of-proc]
  '("End Of Proc" . lua-end-of-proc))
(define-key lua-mode-menu [beginning-of-proc]
  '("Beginning Of Proc" . lua-beginning-of-proc))

(define-key lua-mode-menu [send-lua-region]
  '("Send Lua-Region" . lua-send-lua-region))
(define-key lua-mode-menu [set-lua-region-end]
  '("Set Lua-Region End" . lua-set-lua-region-end))
(define-key lua-mode-menu [set-lua-region-start]
  '("Set Lua-Region Start" . lua-set-lua-region-start))

(define-key lua-mode-menu [send-current-line]
  '("Send Current Line" . lua-send-current-line))
(define-key lua-mode-menu [send-region]
  '("Send Region" . lua-send-region))
(define-key lua-mode-menu [send-proc]
  '("Send Proc" . lua-send-proc))
(define-key lua-mode-menu [send-buffer]
  '("Send Buffer" . lua-send-buffer))
(define-key lua-mode-menu [search-documentation]
  '("Search Documentation" . lua-search-documentation))

;;}}}

;;{{{ syntax helper: handle comments and strings

(defun lua-match-comment-or-string (limit)
  (let ((start-point (point))
        (end-point limit)
        matched)
    (when (re-search-forward
           (concat"\\(?:\\(?:^\\|[^-]\\)\\(\\(?:--\\)?\\(\\[\\(=*\\)\\[\\(\\(?:.\\|\n\\)*?\\)\\]\\3\\]\\)\\)\\)" ;multi-line string or comment: 1: comment, 2: string, 4: body
                  "\\|"
                  "\\(--\\(.*?\\)$\\)"    ; single-line comment. 5: comment, 6: body
                  "\\|"
                  "\\(\\([\'\"]\\)\\(.*?\\)\\(?:\\8\\|$\\)\\)") ; string: 7: string, 9: body
           limit t)
      (setq matched t)
      ;; We will get rid of properties from search start to match start
      (if (> (match-beginning 0) start-point)
          (setq end-point (1- (match-beginning 0)))
        (setq end-point start-point))

      (let ((data (match-data)))
        ;; Get rid of either 1 or 2 in match-data
        (if (and (match-beginning 1)
                 (/= (match-beginning 1) (match-beginning 2)))
            (progn
              (setf (nth 4 data) nil)
              (setf (nth 5 data) nil))
          (setf (nth 2 data) nil)
          (setf (nth 3 data) nil))
        (set-match-data data))

      (cond
       ( (match-beginning 1)
         (add-text-properties (match-beginning 1) (match-end 1)
                              (list 'font-lock-multiline t))
         (add-text-properties (match-beginning 4) (match-end 4)
                              (list 'in-comment t)))
       ( (match-beginning 2)
         (add-text-properties (match-beginning 2) (match-end 2)
                              (list 'font-lock-multiline t))
         (add-text-properties (match-beginning 4) (match-end 4)
                              (list 'in-string t)))
       ( (match-beginning 5)
         (add-text-properties (match-beginning 6) (match-end 6)
                              (list 'in-comment t)))
       ( (match-beginning 7)
         (add-text-properties (match-beginning 9) (match-end 9)
                              (list 'in-string t)))))

    (when (>= end-point start-point)
      (remove-text-properties start-point end-point
                              (list 'font-lock-multiline nil
                                    'in-comment nil
                                    'in-string nil)))
    matched))

;;}}}

(provide 'lua-mode)


;;{{{ Emacs local variables

;; Local Variables:
;; folded-file: t
;; End:

;;}}}

;;; lua-mode.el ends here
