#lang racket
(require racket/set)

(define file (open-input-file "input"))

(define n 1000)
(define ab
  (for/list ([_ n])
    (cons (read file) (read file))))
(define a (map car ab))
(define b (map cdr ab))

;; Part 1

(set! a (sort a <))
(set! b (sort b <))
(set! ab (map cons a b))

(apply +
       (for/list ([i ab])
         (abs (- (car i) (cdr i)))))

;; Part 2

(define ha (list->set a))

(apply +
       (for/list ([i b])
         (if (set-member? ha i) i 0)))
