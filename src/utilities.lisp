(defun concat (&rest args) 
  (apply 'concatenate 'string args))

(defun randElem (l)
  (nth (random (length l)) l))

(defun range (start &optional (end '()))
  (if (null end) (range 0 start)
    (if (< start end) (cons start (range (+ 1 start) end))
      '())))

(defun random>0 (n)
  (let ( (x (random n)))
    (if (= x 0) (random>0 n) x)))

(defun >0 (val)
  (if (<= val 0) 1 val))

(defun selectItem (group index)
  (if (null group) '()
    (if (< index (caar group)) (car group)
      (selectItem (cdr group) (- index (caar group))))))

