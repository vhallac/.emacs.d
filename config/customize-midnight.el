(eval-when-compile
  (require 'midnight))

; ;kill buffers if they were last disabled more than 15 minutes ago
(setq clean-buffer-list-delay-special 900)

(defvar clean-buffer-list-timer nil
  "Stores clean-buffer-list timer if there is one. You can disable clean-buffer-list by (cancel-timer clean-buffer-list-timer).")

;; run clean-buffer-list every 2 hours
(setq clean-buffer-list-timer (run-at-time t 7200 'clean-buffer-list))

;; kill everything, clean-buffer-list is very intelligent at not killing
;; unsaved buffer.
(setq clean-buffer-list-kill-regexps
	  '("^.*$"))

;; keep these buffer untouched
(setq clean-buffer-list-kill-never-buffer-names '("*Messages*" "*cmd*" "*scratch*"
                                                  "*w3m*" "*w3m-cache*"
                                                  "*Group*")
      clean-buffer-list-kill-never-regexps '("^\\*EMMS Playlist\\*.*$"
                                             "^\\*Article "
                                             "^\\*Summary "))
