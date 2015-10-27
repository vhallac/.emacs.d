;; Various calculators I've met during my travels

(defun sum-digits (num)
  (if (< num 10)
      num
    (+ (% num 10) (sum-digits (/ num 10)))))

(defun recursive-sum-digits (num)
  (if (< num 10)
      num
    (recursive-sum-digits (sum-digits num))))

(defun make-vkn (prefix)
  (let ((adds '(9 8 7 6 5 4 3 2 1))
        (muls '(512 256 128 64 32 16 8 4 2))
        (nums (mapcar (lambda (x) (- x ?0)) (append prefix nil))))
    (let* ((c1 (cl-mapcar (lambda (x y z)
                            (recursive-sum-digits (* z (% (+ x y) 10))))
                          nums adds muls))
           (last-digit (% (apply '+ c1) 10)))
      (concat (mapcar (lambda (x) (+ ?0 x))
                      (append nums
                              (list (% (- 10 last-digit) 10))))))))

(provide 'misc-calculators)
