; 1. Write a Racket function count_list with type
; int list -> int
; that returns the number of items in a list. An item that is repeated is counted each time it
; appears in the list.
#lang racket

(define (count_list lst)
    (if (null? lst)
        0
        (+ 1 (count_list (cdr lst)))
    )
)

; 2. Write a Racket function sum_list with type
; int list -> int
; that returns the sum of all the elements within a list
(define (sum_list lst)
    (if (null? lst)
        0
        (begin
            (+ (car lst) (sum_list (cdr lst)))
        )
    )
)

; 3. Write a Racket function countdown with the type
; int -> int list
; that returns a list of numbers from its argument down to 1.
(define (countdown n)
    (if (< n 1)
        '()
        (cons n (countdown (sub1 n)))
    )
)

(define lst '(1 2 3 4 5))
(printf "Count of List: ~a\n" (count_list lst))
(printf "Sum of List: ~a\n" (sum_list lst))
(printf "Countdown from 5: ~a\n" (countdown 5))
(printf "Countdown from 10: ~a\n" (countdown 10))