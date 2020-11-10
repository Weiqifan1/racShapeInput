#lang racket
(require racket/contract) ; imports contract ingredients
(require "testcontracts.rkt")




(define/contract (our-div num denom)
  (number? (and/c number? (not/c zero?)) . -> . number?)
  (/ num denom))

;(our-div 42 0)

(define/contract (ourdiv2 num bignum)
  (-> integer? largenum? any/c)
  (/ num bignum))

;(define/contract (ourdiv2 #:mynum num
;                          #:mydenum denom)
;  (#:mynum number?
;   #:mydenum (and/c number? (< 5 #:mydenum)) . -> . number?)
;  (/ num denom))


(ourdiv2 2 7)
(ourdiv2 2 4)







;;slut
