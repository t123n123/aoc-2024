#lang racket

(define file (open-input-file "./test.input"))

(define n (read file))

(define graph (make-hash '([0 . 4] [1 . 5] [2 . 6])))

(for ([i 10])
  (displayln (hash-ref graph i i)))

(apply + (for/list ([_ n])
           (read file)))
