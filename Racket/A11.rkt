#lang racket/base

(define (symbol stock)
  (car stock))

(define (price stock)
  (car (cdr stock)))

(define (shares stock)
  (car (cdr (cdr stock))))

(define (value stock)
  (* (price stock) (shares stock)))

(define (printValue stock)
  (printf "~a : ~a~%" (symbol stock) (value stock)))

(define appleStock (list "AAPL" 100 5))

;(printValue appleStock)

(define (compareValue stock1 stock2)
  (> (value stock1) (value stock2)))

(define ibmStock (list "IBM" 30 10))

;(compareValue appleStock ibmStock)

(define (findMaxValue stocks)
  (if (null? (cdr stocks))
      (car stocks)
      (let ([s1 (car stocks)]
            [maxTail (findMaxValue (cdr stocks))])
        (if (compareValue s1 maxTail)
            s1
            maxTail))))

(define googleStock (list "GOOG" 70 15))
(define facebookStock (list "FB" 20 8))
(define stockList (list appleStock ibmStock googleStock facebookStock))

;(findMaxValue stockList)

(define (my-delay f)
  (mcons #f f))

(define (my-force th)
  (if (mcar th)
      (mcdr th)
      (begin (set-mcar! th #t)
             (set-mcdr! th ((mcdr th)))
             (mcdr th))))

(define appleValue (my-delay (lambda () (value appleStock))))
(my-force appleValue)
(my-force appleValue)

(define (compareValue2 s1Value s2Value)
  (> (my-force s1Value) (my-force s2Value)))


(define (findMaxValue2 stocks)
  (if (null? (cdr stocks))
      (car stocks)
      (let* ([s1 (car stocks)]
             [s1Value (my-delay (lambda () (value s1)))]
             [maxTail (findMaxValue2 (cdr stocks))]
             [maxTailValue (my-delay (lambda () (value maxTail)))])
        (if (compareValue2 s1Value maxTailValue)
            s1
            maxTail))))

(findMaxValue2 stockList)


(define (map f xs)
  (if (null? xs)
      null
      (cons (f (car xs)) (map f (cdr xs)))))

(define xs '(1 2 3 4))
(define f (lambda (x) (+ x 1)))
(map (lambda (x) (+ x 1)) xs)

(define (filter f xs)
  (if (null? xs)
      null
      (if (f (car xs))
          (cons (car xs) (filter f (cdr xs)))
          (filter f (cdr xs)))))

(filter (lambda (x) (< x 3)) xs)