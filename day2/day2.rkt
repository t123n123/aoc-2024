#lang racket

(define vals
  (for/list ([line (file->lines "input")])
    (map string->number (string-split line))))

;; Part 1
(define (mono xs)
  (apply = (map (lambda (x y) (sgn (- x y))) (reverse (cdr (reverse xs))) (cdr xs))))

(define (diff xs)
  (andmap (lambda (x y) (<= 1 (abs (- x y)) 3)) (reverse (cdr (reverse xs))) (cdr xs)))

(define (safe xs)
  (and (mono xs) (diff xs)))

(displayln (count safe vals))

;; Part 2

(define (safe* xs ys)
  (if (null? ys) #f (or (safe (append xs (cdr ys))) (safe* (append xs (list (car ys))) (cdr ys)))))

(displayln (count (lambda (xs) (safe* (list) xs)) vals))
