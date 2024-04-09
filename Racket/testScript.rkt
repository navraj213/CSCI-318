; First check if u have installed racket in your system
; racket -v

; if u dont got it run 
; brew install --cask racket

; Extention
; https://marketplace.visualstudio.com/items?itemName=evzen-wybitul.magic-racket

#lang racket

(define (test-vscode)
  (define a 5)
  (define b 7)
  (define result (+ a b))
  (displayln (format "The result of adding ~a and ~a is ~a" a b result)))

(test-vscode)