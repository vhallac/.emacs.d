(eval-when-compile
  (require 'ledger-mode))

(setq ledger-post-account-alignment-column 2
      ledger-clear-whole-transactions t
      ledger-complete-ignore-case t
      ledger-highlight-xact-under-point nil)

(defadvice ledger-add-transaction (after remove-extra-newlines activate)
  (when (looking-at "\n\n\n")
    (delete-char 2)))
