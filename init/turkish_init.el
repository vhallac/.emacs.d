(require 'quail)

(quail-define-package
 "turkish-doubles" "Turkish" "TR<" t
 "Turkish (Türkçe) input method with double letter entry.

Note for I, ı, İ, i.

AA -> Â
CC -> Ç
GG -> Ğ
I  -> I
i  -> ı
II -> İ
ii -> i
OO -> Ö
SS -> Ş
UU -> Ü

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
      (setq turkish-info (assoc "Turkish" language-info-alist))
      (setcdr (assoc 'input-method (cdr turkish-info)) "turkish-doubles"))))
    
(set-language-environment "Turkish")