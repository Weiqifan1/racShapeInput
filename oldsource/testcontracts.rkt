#lang racket
(require racket/contract)
; data definitions 
 
;(define id? symbol?)
;(define id-equal? eq?)
;(define-struct basic-customer (id name address) #:mutable)
 
; interface 
;(provide
; (contract-out
;  [id?                   (-> any/c boolean?)]
;  [id-equal?             (-> id? id? boolean?)]
;  [struct basic-customer ((id id?)
;                          (name string?)
;                          (address string?))]))
; end of interface




(define (largenum? x)
  (and/c integer? (< 5 x)))



(provide
 (contract-out
  [largenum? (-> any/c any/c)]))


















;;slut
