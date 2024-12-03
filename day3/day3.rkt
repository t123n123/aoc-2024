#lang racket

(require megaparsack megaparsack/text)
(require data/applicative)
(require data/monad)

(define input (file->string "input"))

(define mult/p 
  (do 
    [res <- (many-until/p 
              any-char/p #:end 
              (try/p 
                (do
                  (string/p "mul")
                  (char/p #\( )
                  [x <- integer/p]
                  (char/p #\,)
                  [y <- integer/p]
                  (char/p #\))
                  (pure (* x y)))))]
    (pure (cadr res))  
  ))


;; Part 1

(apply + (parse-result! 
  (parse-string (many/p (try/p mult/p)) input)))

;; Part 2

(define do-dont-mult/p 
  (do 
    [res <- (many-until/p 
              any-char/p #:end 
              (or/p 
                (try/p 
                  (do
                    (string/p "mul")
                    (char/p #\( )
                    [x <- integer/p]
                    (char/p #\,)
                    [y <- integer/p]
                    (char/p #\))
                    (pure (* x y))))
                (try/p (do (string/p "do()")
                         (pure #t)))
                (try/p (do (string/p "don't()")
                         (pure #f)))
              ))]
    (pure (cadr res))  
  ))

(define (calculate ls f)
  (cond
    [(null? ls) 0]
    [(number? (car ls)) (+ (* f (car ls)) (calculate (cdr ls) f))]
    [(car ls) (calculate (cdr ls) 1)]
    [#t (calculate (cdr ls) 0)]
    )
  )
(calculate (parse-result! (parse-string (many/p (try/p do-dont-mult/p)) input)) 1)



