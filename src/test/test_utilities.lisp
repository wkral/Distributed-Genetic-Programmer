;(defun concat (&rest args) 
;  (apply 'concatenate 'string args))
;
;(defun randElem (l)
;  (nth (random (length l)) l))
;
;(defun range (start end)
;  (if (< start end) (cons start (range (+ 1 start) end))
;    '()))
;
;(defun random>0 (n)
;  (let ( (x (random n)))
;    (if (= x 0) (random>0 n) x)))
;
;(defun >0 (val)
;  (if (<= val 0) 1 val))

(load "lisp-unit.lisp")
(load "../utilities.lisp")

(use-package :lisp-unit)

(define-test test-concat
  (assert-equal "something" (concat "some" "thing"))
  (assert-equal "" (concat "" "" "" ""))
  (assert-equal "somethingelse" (concat "some" "thing" "else")))

(define-test test-range
  (assert-equal '(0 1 2 ) (range 3))
  (assert-equal '(0 1 2 3 4 ) (range 5))
  (assert-equal '(3 4) (range 3 5))
  (assert-equal '() (range 0))
  (assert-equal '() (range -1)))

(define-test test->0
  (assert-equal 5 (>0 5))
  (assert-equal 1 (>0 1))
  (assert-equal 1 (>0 0))
  (assert-equal 1 (>0 -1)))

(run-tests)
  