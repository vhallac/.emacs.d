(eval-when-compile
  (require 'ledger-mode))

(setq ledger-post-account-alignment-column 2
      ledger-clear-whole-transactions t
      ledger-complete-ignore-case t)

(defadvice ledger-add-transaction (after ledger-add-transaction-indent activate compile)
  "Indent the newly added transaction to ensure indent amount is correct."
  (let ((extens (ledger-find-xact-extents (point))))
    (when extens
      (apply #'indent-region extens))))
