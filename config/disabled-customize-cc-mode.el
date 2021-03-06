(eval-when-compile
  (require 'cc-mode))

(defvar c-syntactic-context)
(defvar c-macro-cppflags)
(defvar c-macro-preprocessor)
(defvar c-macro-shrink-window-flag)
(declare-function match-parenthesis "global.el")
(declare-function choose-indent-type "choose-indent.el")

(message "Configuring C modes")
(require 'imenu)
(require 'c-helper)
(require 'completion)

;; TDA's C style
(c-add-style "tda" '(
     (c-basic-offset . 4)
     (c-comment-only-line-offset . 0)
     (c-block-comment-prefix . "*")
     (c-hanging-braces-alist     . ((substatement-open        before after)
                                    (brace-list-open          after)
                                    (brace-list-intro)
                                    (brace-entry-open         before)
                                    (brace-list-close  . c-snug-array-close)
                                    (block-close       . c-snug-do-while)
                                    (class-open               after)
                                    (class-close              before)))
     (c-hanging-colons-alist     . ((case-label after)
                                    (label after)
                                    (member-init-intro before)
                                    (inher-intro)))
     (c-offsets-alist . ((topmost-intro         . 0)
                         (topmost-intro-cont    . 0)
                         (substatement          . +)
                         (substatement-open     . 0)
                         (case-label            . 0)
                         (label                 . 0)
                         (access-label          . -)
                         (inclass               . +)
                         (inline-open           . 0)
                         (cpp-macro-cont        . ++)
                         (arglist-intro         . c-lineup-arglist-intro-after-paren)
                         (arglist-cont          . c-lineup-arglist)
                         (arglist-cont-nonempty . c-lineup-arglist)
                         (arglist-close         . c-lineup-arglist)
                         (inextern-lang         . -)
                         (statement-cont        . lineup-array-init)))
     (c-cleanup-list . (empty-defun-braces
                        list-close-comma
                        scope-operator
                        one-liner-defun
                        comment-close-slash))
     (c-hanging-semi&comma-criteria . (c-semi&comma-inside-parenlist))))

(c-add-style "eracom" '(
     (c-basic-offset . 4)
     (c-comment-only-line-offset . 0)
     (c-block-comment-prefix . "*")
     (c-hanging-braces-alist     . ((substatement-open        before after)
                                    (brace-list-open          after)
                                    (brace-list-intro)
                                    (brace-entry-open         before)
                                    (brace-list-close  . c-snug-array-close)
                                    (block-close       . c-snug-do-while)
                                    (class-open               after)
                                    (class-close              before)))
     (c-hanging-colons-alist     . ((case-label after)
                                    (label after)
                                    (member-init-intro before)
                                    (inher-intro)))
     (c-offsets-alist . ((topmost-intro         . 0)
                         (topmost-intro-cont    . 0)
                         (substatement          . +)
                         (substatement-open     . 0)
                         (case-label            . 0)
                         (label                 . 0)
                         (access-label          . -)
                         (inclass               . +)
                         (inline-open           . 0)
                         (cpp-macro-cont        . ++)
                         (arglist-intro         . c-lineup-arglist-intro-after-paren)
                         (arglist-cont          . c-lineup-arglist)
                         (arglist-cont-nonempty . c-lineup-arglist)
                         (arglist-close         . c-lineup-arglist)
                         (inextern-lang         . -)
                         (statement-cont        . lineup-array-init)))
     (c-cleanup-list . (empty-defun-braces
                        list-close-comma
                        scope-operator))
     (c-hanging-semi&comma-criteria . (c-semi&comma-inside-parenlist))))


(defun c-snug-array-close (syntax pos)
  "Dynamically calculate close-brace hanginess for array initializations.

See `c-hanging-braces-alist' for how to utilize this function as an
ACTION associated with `brace-list-close' syntax."
  (save-excursion
    (if (eq syntax 'brace-list-close)
        (match-parenthesis 0))
    (c-safe (c-forward-token-1 -1))
    (if (eq (char-after) ?\=)
        '(before)
      '(after))))

(defun lineup-array-init (langelem)
  "Correct the indentation of array and structure initializer brace, when it is
reported as statement-cont.

Changes:
int a[] =             int a[] =
   {                  {
      1,2,3      ->      1,2,3
   };                 };"
  (let ((default-lineup (c-lineup-math langelem)))
    (save-excursion
      (goto-char (point-at-bol))
      (if (and (looking-at "\\s-*{")
               (progn (c-safe (c-backward-token-1 1))
                      (eq (char-after) ?\=)))
          0
        default-lineup))))

(defun extended-c-electric-brace (arg)
  "Modify C-electric-brace behaviour to leave the array/struct initializers on the
same line as the '='.

Otherwise, the behaviour is the same as the c-electric-brace.
NOTE: This is no longer used, but left for future generations. ;-)"
  (interactive "*P")
  (let* ((lit-c-state-cache (c-parse-state))
         (lit-safepos (c-safe-position (point) lit-c-state-cache))
         (literal (c-in-literal lit-safepos)))
    ;; if we're in a literal, or we're not at the end of the line, or
    ;; a numeric arg is provided, or auto-newlining is turned off,
    ;; then just insert the character.
    (if (or literal
            arg
            (not (looking-at "[ \t]*$")))
        (self-insert-command (prefix-numeric-value arg))
      (let ((initializer nil))
        (save-excursion
          (c-safe (c-backward-token-1 1))
          (if (looking-at "=")
              (setq initializer t)))
        (if initializer
            (progn (indent-according-to-mode)
                   (insert "{")
                   (newline)
                   (indent-according-to-mode))
          (c-electric-brace arg))))))

(c-add-style "eracom-old" '(
     (c-basic-offset . 4)
     (c-comment-only-line-offset . 0)
     (c-block-comment-prefix . "*")
     (c-hanging-braces-alist     . ((substatement-open after)
                                    (brace-list-open   after)
                                    (brace-list-intro)
                                    (brace-entry-open  after)
                                    (brace-list-close  before)
                                    (block-close       . c-snug-do-while)
                                    (class-open        after)))
     (c-hanging-colons-alist     . ((case-label after)
                                    (label after)
                                    (member-init-intro before)
                                    (inher-intro)))
     (c-offsets-alist . ((topmost-intro         . 0)
                         (topmost-intro-cont    . 0)
                         (substatement          . +)
                         (substatement-open     . 0)
                         (case-label            . 0)
                         (label                 . 0)
                         (access-label          . -)
                         (inclass               . +)
                         (inline-open           . 0)
                         (cpp-macro-cont        . ++)
                         (arglist-intro         . c-lineup-arglist-intro-after-paren)
                         (arglist-cont          . c-lineup-arglist)
                         (arglist-cont-nonempty . c-lineup-arglist)
                         (arglist-close         . c-lineup-arglist)))
     (c-cleanup-list . (brace-else-brace
                        brace-elseif-brace
                        empty-defun-braces
                        list-close-comma
                        scope-operator))))

(defun goto-c-function-in-buffer ()
  "Jumps to the beginning of the function in the buffer"
  (interactive "")
  (imenu (imenu-choose-buffer-index "function: ")))

(defun insert-include-prev-line (include-str offset-from-end)
  "Add INCLUDE-STR to the previous line, and leave cursor OFFSET-FROM-END
characters off from the end. The cursor position is where the header name goes"
       (beginning-of-line)
       (insert "#include <.h>\n")
       (forward-line -1)
       (end-of-line)
       (backward-char 3))

(defun add-header-file-protection (&optional c++-mode)
  "Add the statements that protect a header file against multiple inclusion.
when C++-MODE is set, the header file is assumed to be a C++ header, and no
extern \"C\" statements are added."
  (let ((nm (subst-char-in-string ?. ?_ (concat "INC_" (upcase (buffer-name))))))
	 (save-excursion
	   (goto-char (point-min))
	   (insert (concat (concat "#ifndef " nm) "\n"))
	   (insert (concat (concat "#define " nm) "\n\n"))
       (if (not c++-mode)
           (insert "#ifdef __cplusplus\nextern \"C\" {\n#endif /* __cplusplus */\n"))
	   (goto-char (point-max))
       (newline)
       (if c++-mode
           (insert (concat (concat "\n#endif // " nm) "\n"))
         (insert "\n#ifdef __cplusplus\n}\n#endif /* __cplusplus */\n\n")
         (insert (concat (concat "\n#endif /* " nm) " */\n"))))))

(defun vedat-c-mode-hook ()
  (interactive "")
  (auto-fill-mode t)
  (c-toggle-auto-hungry-state 1)
;; Temporary
  (define-key global-map [(f1)]
    '(lambda () "Format ascii strings in char arrays"
	   (interactive "")
       (replace-regexp "\\(.\\)" "'\\1', ") ))

  (define-key global-map [(f2)]
    '(lambda () "Format HEX strings in char arrays"
	   (interactive "")
       (replace-regexp "\\([a-zA-Z0-9][a-zA-Z0-9]\\)" "0x\\1, ") ))
;; END: Temporary

  (define-key c-mode-map [(control c) (control i)]
    '(lambda ()
       (interactive "")
       (insert-include-prev-line "#include <.h>" 3)))
  (define-key c-mode-map [(control c) (i)]
    '(lambda ()
       (interactive "")
       (insert-include-prev-line "#include \".h\"" 3)))
  (define-key c++-mode-map [(control c) (control i)]
    '(lambda ()
       (interactive "")
       (insert-include-prev-line "#include <>" 1)))
  (define-key c++-mode-map [(control c) (i)]
    '(lambda ()
       (interactive "")
       (insert-include-prev-line "#include \"\"" 1)))
  (define-key c-mode-map [(control c) (control l)]
    '(lambda ()
       (interactive "")
       (add-header-file-protection)))
  (define-key c++-mode-map [(control c) (control l)]
    '(lambda ()
       (interactive "")
       (add-header-file-protection t)))
  (define-key c-mode-map [(control c) (g)]
	'goto-c-function-in-buffer)
  (define-key c-mode-map [(control c) (control f)] 'c-helper-find-file)
  (define-key c-mode-map [(control c) (control v)] 'c-helper-find-include-file)
  (setq imenu-sort-function 'imenu--sort-by-position)
  ;(imenu-add-to-menubar "Imenu")
  (choose-indent-type)
  ; 17 Mar 2009-VH: Just go with the default (choose-c-style)
  ; Minor modes I like in my C environment
  (completion-mode)
  ;(c-subword-mode)
  ;(camelCase-mode 1)
  (add-hook 'c-special-indent-hook 'ms-space-for-alignment nil t))

(add-hook 'c-mode-common-hook 'vedat-c-mode-hook)

(define-key global-map [(f7)] '(lambda ()
                                 (interactive "")
                                 (compile "make")))

(define-key global-map [(control f4)]
  '(lambda ()
     (interactive "")
     (if (> (length (device-frame-list)) 1)
         (delete-frame))))

(defun choose-c-style ()
    "Checks whether the file has old style or new style indentation."
    (interactive "")
    (save-excursion
      (if (find-old-eracom-style-line)
          (c-set-style "eracom-old")
        (c-set-style "eracom"))))

(defun find-old-eracom-style-line ()
  "Find the line containing an old eracom bracing style statement."
  (interactive "")
  (let* ((keywords '("if" "for" "while" "do" "switch"))
         (old-style-regexp (concat "^\\s-*\\("
                                   (mapconcat 'eval keywords "\\|")
                                   "\\).*{"))
         (old-style-point nil))
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (if (search-forward-regexp old-style-regexp nil t)
            (setq old-style-point (point)))))
    (if old-style-point
        (goto-char old-style-point)
      nil)))

(defun ms-space-for-alignment ()
  "Make the current line use tabs for indentation and spaces for alignment.

It is intended to be called from the hook
`c-special-indent-hook'.  It assumes that `indent-tabs-mode' is
non-nil and probably assumes that `c-basic-offset' is the same as
`tab-width'.

To enable it, do this, or similar:

    M-: (setq indent-tabs-mode t)
    M-: (setq tab-width 3)
    M-: (setq c-basic-offset tab-width)
    M-: (add-hook 'c-special-indent-hook 'ms-space-for-alignment nil t)"
  (save-excursion
      (let* ((indent-pos (progn (back-to-indentation) (point)))
             (indent-col (current-column))
             (syn-elt (car c-syntactic-context))
             (syn-sym (c-langelem-sym syn-elt)))
        (when (memq syn-sym '(arglist-cont-nonempty)) ;; <==============
          (let* ((syn-anchor (c-langelem-pos syn-elt))
                 (anchor-col (progn (goto-char syn-anchor)
                                    (back-to-indentation)
                                    (current-column)))
                 num-tabs)
        ;;
            (goto-char indent-pos)
            (delete-horizontal-space)
; Reported to be buggy!!!!
;            (insert-char ?\t (/ anchor-col tab-width))
;            (insert-char ?\  (- indent-col (current-column))))))))
            (insert-before-markers (make-string (/ anchor-col tab-width) ?\t))
            (insert-before-markers (make-string (- indent-col (current-column)) ?\ )))))))

(setq c-default-style "tda"
      c-echo-syntactic-information-p t
      c-electric-pound-behavior '(alignleft)
      c-font-lock-extra-types '("FILE" "\\<[A-Z]+_[A-Z]*[a-z]+\\sw*\\>" "\\<[A-Za-z]\\sw*[a-z]\\sw*T\\>" "\\sw+_\\(\\|s\\)t" "\\<\\(u\\|\\)int\\(8\\|16\\|32\\|64\\)\\>" "THR_HANDLE" "\\sw+_STATUS" "bool" "true" "false" "BOOL" "TRUE" "FALSE")
      c-indent-comments-syntactically-p t
      c-macro-cppflags "-I../include -I../../include -I../../../include -I ../../../../include -I../../../../../include"
      c-macro-preprocessor "e:\\cygwin\\lib\\gcc-lib\\i686-pc-cygwin\\2.95.2\\cpp -C"
      c-macro-shrink-window-flag t
      c-mode-common-hook '(vedat-c-mode-hook))

; Newer emacs versions define java-mode in cc-mode. Make sure java-mode is
; properly initialized.
(load-file "~/.emacs.d/config/customize-java-mode.el")
