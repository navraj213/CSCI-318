; 1. Write Racket expressions that do the following:
; 1. Defines a function printValue that takes a list consisting of symbol, price and shares as
; arguments and prints out the stock symbol and the value of price x shares
#lang racket

(define (symbol stock)
    (car stock)
)

(define (price stock)
    (car (cdr stock))
)

(define (shares stock)
    (car (cdr (cdr stock)))
)

(define (printValue lst)
  (printf "~a : ~a~%" (symbol lst) (* (price lst) (shares lst)))
)

(define (value stock)
    (* (price stock) (shares stock))
)

; 2. Binds the list ("AAPL", 100, 5) to the variable appleStock
(define appleStock '("AAPL" 100 5))

; 3. Calls the function printValue with the variable appleStock
(printValue appleStock)


; 2. Write Racket expressions that do the following:
; 1. Defines a function compareValue that takes two lists consisting of symbol, price and shares
; as arguments and returns true if the first argument has a greater value (price x shares) than the
; second argument, and false otherwise
(define (compareValue stock1 stock2)
    (if (> (value stock1) (value stock2))
    (symbol stock1)
    (symbol stock2))
)

; 2. Binds the list ("IBM", 30, 10) to the variable ibmStock
(define ibmStock '("IBM" 30 10))

; 3. Calls the function compareValue with appleStock and ibmStock as arguments
(compareValue appleStock ibmStock)


; 3. Write Racket expressions that do the following:
; 1. Defines a function findMaxValue that takes a list of lists consisting of symbol, price and
; shares as arguments and returns the stock with the greatest value (price x shares).
(define (findMaxValue stocks)
    (if (null? (cdr stocks))
    (car stocks)
    (let ([s1 (car stocks)]
        [maxTail (findMaxValue (cdr stocks))])
    (if (compareValue s1 maxTail)
        s1
        maxTail)))
)

; 2. Binds the list ("GOOG", 70, 15) to googleStock
(define googleStock '("GOOG" 70 15))

; 3. Binds the list ("FB", 20, 8) to facebookStock
(define facebookStock '("FB" 20 8))

; 4. Creates a list of all four stock lists (appleStock, ibmStock, googleStock and facebookStock)
; bound to stockList
(define stockList (list appleStock ibmStock googleStock facebookStock))

; 5. Calls findMaxValue with stockList as its argument
(findMaxValue stockList)