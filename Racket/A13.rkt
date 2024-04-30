#lang racket

; Define a struct for cash that has a field for amount

; 1. Define a struct for stocks that has fields for symbol, price and number of shares
(struct cash (amount) #:transparent)
(struct stock (symbol price shares) #:transparent)


; 2. Create the following stocks
; appleStock with values ("AAPL", 100, 5)
; googleStock with values ("GOOG", 70, 15)
; facebookStock with values ("FB", 20, 8)
; ibmStock with values ("IBM", 30, 10)
(define appleStock (stock "AAPL" 100 5))
(define googleStock (stock "GOOG" 70 15))
(define facebookStock (stock "FB" 20 8))
(define ibmStock (stock "IBM" 30 10))

; 3. Create a cash struct with name myCash and value of 200
(define myCash (cash 200))

; 4. Define a function valueOf that takes one investment as its argument and:
;   if the investment is a Stock, returns the value of price x shares
;   if the investment is Cash, returns the amount of money in cash
(define (valueOf investment)
  (cond [(stock? investment) (* (stock-price investment) (stock-shares investment))]
        [(cash? investment) (cash-amount investment)]))

(valueOf appleStock)
(valueOf myCash)

; 5. Define a function compareValue that takes two investments as arguments and returns true if the first argument is greater than the second, and false otherwise.
(define (compareValue inv1 inv2)
  (> (valueOf inv1) (valueOf inv2)))

(compareValue appleStock myCash)
(compareValue myCash appleStock)

; 6. Define a function findMaxValue that takes a list of investments and returns the investment with the greatest value.
(define (findMaxValue investments)
  (cond [(null? (cdr investments)) (car investments)]
        [(compareValue (car investments) (findMaxValue (cdr investments))) (car investments)]
        [#t (findMaxValue (cdr investments))]))

(define investmentList (list appleStock googleStock facebookStock ibmStock myCash))
(findMaxValue investmentList)

; 7. Define a function findMaxValue2 that uses local bindings
(define (findMaxValue2 investments)
  (cond [(null? (cdr investments)) (car investments)]
        [#t (let ([maxTail (findMaxValue2 (cdr investments))])
              (if (compareValue (car investments) maxTail)
                  (car investments)
                  maxTail))]))

(findMaxValue2 investmentList)

; Map a function to all items in a list
(define (map f xs)
  (if (null? xs)
      null
      (cons (f (car xs)) (map f (cdr xs)))))

(map valueOf investmentList)

; Filter the items in a list
(define (filter f xs)
  (if (null? xs)
      null
      (if (f (car xs))
          (cons (car xs) (filter f (cdr xs)))
          (filter f (cdr xs)))))

; 8. Use filter to create a list of just investments with more than ten shares
(define (moreThanTenShares investment)
  (and (stock? investment) (> (stock-shares investment) 10)))

(moreThanTenShares facebookStock)
(moreThanTenShares googleStock)
(moreThanTenShares myCash)

(filter moreThanTenShares investmentList)

; Reduce the items in a list
(define (fold f acc xs)
  (if (null? xs)
      acc
      (fold f (f acc (car xs)) (cdr xs))))

; 9. Use fold to calculate the total value of all the items in investmentList
(define (sum x y) (+ x (valueOf y)))

(fold sum 0 investmentList)