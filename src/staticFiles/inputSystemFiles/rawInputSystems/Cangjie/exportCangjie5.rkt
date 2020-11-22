#lang racket

(require racket/fasl)

(provide cangjie5CharHash cangjie5CodeHash)


(define (load-data path)
  (call-with-input-file path fasl->s-exp))


(define cangjie5CharHash (load-data "Cangjie5Maps/cangjie5CharHash.rktd"))
(define cangjie5CodeHash (load-data "Cangjie5Maps/cangjie5CodeHash.rktd"))

;(hash-ref readCharHash "我")
;(hash-ref readCodeHash "hqi")


;slut
