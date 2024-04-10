#lang racket

; Testing how to display hello
(begin
    (define x "hello")
    (printf "~a~%" x)
)

; hello_world function
(define (hello_world n)
    (if (= 1 n)
        (printf "hello world~%")
        (begin
            (printf "hello world~%")
            (hello_world (- n 1))
        )
    )
)

(hello_world 5)

; Factorial Function
(define (fac x)
    (if (= x 0)
        1
        (* x (fac (- x 1)))
    )
)

(fac 3)

; Add up
(define (add_up n)
    (if (= n 1)
        1
        (+ n (add_up (- n 1)))
    )
)

(add_up 10)

; printValue
(define (printValue symbol price shares)
    (printf "~a : ~a~%" symbol (* price shares))
)

(define symbol "APPL")
(define price 100)
(define shares 5)

(printValue symbol price shares)

; Compare values
(define (compareValue price shares threshold)
    (> (* price shares) threshold)
)

(define threshold 250)
(compareValue price shares threshold)