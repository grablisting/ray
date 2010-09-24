;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Ray Peters
;;
;; CS211, Scheme Homework #3
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; div
;;
;; This function takes two arguments, x and y, and divides
;; the first (x) by the second (y). It returns a list with
;; two atoms: the quotient and the remainder.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define divHelper
  (lambda (x y mark count)
    (cond ((zero? x) count)
          ((eq? (- y 1) mark) (divHelper (- x 1) y 0 (+ count 1)))
          (else (divHelper (- x 1) y (+ mark 1) count)))))
 
(define div
  (lambda (x y)
      (cons (divHelper x y 0 0) 
            (cons (- x (* (divHelper x y 0 0) y)) '()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; mygcd
;;
;; This function takes two arguments, x and y, and returns the
;; greatest common divisor.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define mygcd
  (lambda (x y)
    (car (div (car (cdr (div x y))) 
         (car (cdr (div y (car (cdr (div x y))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; dtb
;;
;; This function converts an integer, x, from decimal to binary.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define dtb
  (lambda (x)
    (cond ((zero? x) 0)
          ((zero? (- x 1)) 1)
          (else (cond (div x 2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; NumberOfNodes
;;
;; This function determines the number of nodes in a binary tree, x.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define NumberOfNodes
  (lambda (x)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; NumberOfLeaves
;;
;; This function returns the number of leaves of a given binary tree, x.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define NumberOfLeaves
  (lambda (x)
    ))