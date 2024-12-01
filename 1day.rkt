#lang racket
(require racket/set)

(define file (open-input-file "./1day.input"))

(define n 1000)
(define a (list))
(define b (list))

(for ([_ n])
  (set! a (cons (read file) a))
  (set! b (cons (read file) b))
  )

;; Part 1

(set! a (sort a <))
(set! b (sort b <))

(apply + (for/list ([i (length a)])
           (abs (- (list-ref a i) (list-ref b i)))))

;; Part 2

(define ha (list->set a))

(apply + (for/list ([i (length a)])
           (if (set-member? ha (list-ref b i)) (list-ref b i) 0)))
