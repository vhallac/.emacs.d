(eval-when-compile
  (require 'auto-complete))

;; Remove when ELPA auto-comlete defines these
(defun ac-semantic-candidates (prefix)
  (with-no-warnings
    (delete "" ; semantic sometimes returns an empty string
            (mapcar (lambda (elem)
                      (cons (semantic-tag-name elem)
                            (semantic-tag-clone elem)))
                    (ignore-errors
                      (or (semantic-analyze-possible-completions
                           (semantic-analyze-current-context))
                          (senator-find-tag-for-completion prefix)))))))

(defun ac-semantic-doc (symbol)
  (with-no-warnings
    (let* ((proto (semantic-format-tag-summarize-with-file symbol nil t))
           (doc (semantic-documentation-for-tag symbol))
           (res proto))
      (when doc
        (setq res (concat res "\n\n" doc)))
      res)))

(ac-define-source semantic
  '((available . (or (require 'semantic-ia nil t)
                     (require 'semantic/ia nil t)))
    (candidates . (ac-semantic-candidates ac-prefix))
    (document . ac-semantic-doc)
    (prefix . cc-member)
    (requires . 0)
    (symbol . "m")))

(ac-define-source semantic-raw
  '((available . (or (require 'semantic-ia nil t)
                     (require 'semantic/ia nil t)))
    (candidates . (ac-semantic-candidates ac-prefix))
    (document . ac-semantic-doc)
    (symbol . "s")))
