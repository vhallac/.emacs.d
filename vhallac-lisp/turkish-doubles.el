(require 'quail)

(quail-define-package
 "turkish-doubles" "Turkish" "TR<" t
 "Turkish (Türkçe) input method with double letter entry.

aa -> â
cc -> ç
gg -> ğ
ii -> ı
oo -> ö
uu -> ü
ss -> ş
AA -> Â
CC -> Ç
GG -> Ğ
II -> İ
OO -> Ö
UU -> Ü
SS -> Ş

Doubling the postfix separates the letter and postfix: e.g. aaa -> aa
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("AA" ?Â)
 ("aa" ?â)
 ("CC" ?Ç)
 ("cc" ?ç)
 ("GG" ?Ğ)
 ("gg" ?ğ)
 ("II" ?İ)
 ("ii" ?ı)
 ("OO" ?Ö)
 ("oo" ?ö)
 ("SS" ?Ş)
 ("ss" ?ş)
 ("UU" ?Ü)
 ("uu" ?ü)

 ("AAA" ["AA"])
 ("aaa" ["aa"])
 ("CCC" ["CC"])
 ("ccc" ["cc"])
 ("GGG" ["GG"])
 ("ggg" ["gg"])
 ("III" ["II"])
 ("iii" ["ii"])
 ("iiii" ["ıı"])
 ("OOO" ["OO"])
 ("ooo" ["oo"])
 ("SSS" ["SS"])
 ("sss" ["ss"])
 ("UUU" ["UU"])
 ("uuu" ["uu"])
 )

;; Replace turkish input method with the doubled-letters one above.
;; TODO: Better way to replace input method?
(let ((turkish-info (assoc "Turkish" language-info-alist)))
  (if turkish-info
      (setcdr (assoc 'input-method (cdr turkish-info)) "turkish-doubles")
    (eval-after-load "european"
      '(progn
	 (setq turkish-info (assoc "Turkish" language-info-alist))
	 (setcdr (assoc 'input-method (cdr turkish-info)) "turkish-doubles")))))

(provide 'turkish-doubles)
