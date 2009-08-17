(defun concat (&rest args) 
  (apply 'concatenate 'string args))

(defun randElem (l)
  (nth (random (length l)) l))

(defun range (start end)
  (if (< start end) (cons start (range (+ 1 start) end))
    '()))

(defun random>0 (n)
  (let ( (x (random n)))
    (if (= x 0) (random>0 n) x)))

(defun >0 (val)
  (if (<= val 0) 1 val))
